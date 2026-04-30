# ================================================================
# 03_prepare_SUS.R
# ----------------------------------------------------------------
# ETL completo da produção SUS (ambulatorial + hospitalar) para
# câncer do colo do útero. Três pipelines independentes:
#
#   1) SIA-PA (ambulatorial)        run_prepare_SUS()
#      - Baixa SIA-PA via microdatasus mês-a-mês, filtra por códigos
#        de interesse e salva chunks .parquet em
#        data-raw/sia_cc_chunks_sia_pa/.
#      - Consolida tudo em data-raw/sia_cc_completo.rds.
#      build_sia_cc_resumo()
#      - Resume sia_cc_completo em data/sia_cc_resumo.rds (1 linha
#        por categoria × geo × competência × procedimento × idade
#        × sexo).
#
#   2) SIH-RD (hospitalar — EZT)    run_prepare_SIH_RD_EZT()
#      - Mesma estrutura do SIA, mas para SIH-RD (Reduzida) com
#        códigos de tratamento (proxy de EZT em internação).
#      - Consolida em data-raw/sih_rd_ezt_completo.rds.
#      build_sih_rd_ezt_resumo()
#      - Resume em data/sih_rd_ezt_resumo.rds.
#
#   3) Resumo unificado para o app  build_sus_proc_resumo()
#      - Junta SIA + SIH em data/sus_proc_resumo.rds com 2 visões
#        ("care" = local de atendimento, "res" = local de residência)
#        e 2 métricas (total_all / total_25_64), já com merge na
#        hierarquia regional_sus_map.
#
# OBS importantes:
#  - Saída final consumida pelo app é sus_proc_resumo.rds (referida
#    como sia_cc_resumo na app.R por razões históricas — ver mapping).
#  - O ETL é incremental: chunks .parquet existentes não são
#    rebaixados (file.exists → next).
#  - Falhas no fetch_datasus retornam NULL e são puladas sem salvar.
# ================================================================

# ----------------------------------------------------------------
# Dependências
# - microdatasus: download de SIA/SIH do DATASUS
# - arrow: leitura/escrita de Parquet (chunks compactados em zstd)
# - data.table: agregações rápidas
# - dplyr: usado pontualmente para filter() com .data[[col]] (sintaxe NSE)
# - here: paths robustos (independem de cwd)
# ----------------------------------------------------------------
suppressWarnings({
  if (!requireNamespace("microdatasus", quietly = TRUE)) install.packages("microdatasus")
  if (!requireNamespace("data.table",   quietly = TRUE)) install.packages("data.table")
  if (!requireNamespace("here",         quietly = TRUE)) install.packages("here")
  if (!requireNamespace("arrow",        quietly = TRUE)) install.packages("arrow")
  if (!requireNamespace("dplyr",        quietly = TRUE)) install.packages("dplyr")
})

library(microdatasus)
library(data.table)
library(here)
library(arrow)
library(dplyr)

# Wrappers de path — toleram qualquer cwd dentro do projeto
path_data      <- function(...) here::here("data", ...)
path_data_raw  <- function(...) here::here("data-raw", ...)
path_sia_chunk <- function(...) path_data_raw("sia_cc_chunks_sia_pa", ...)


# ------------------------------------------------
# Parâmetros — UFs e janela temporal padrão (SIA + SIH)
# ------------------------------------------------

ufs_brasil <- c(
  "AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS",
  "MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC",
  "SP","SE","TO"
)

SUS_ufs_default  <- ufs_brasil
SUS_anos_default <- c(2024L, 2025L)
SUS_mes_ini      <- 1
SUS_mes_fim      <- 12

# ------------------------------------------------
# Códigos de procedimento de interesse (SIGTAP)
# ------------------------------------------------
# 10 categorias ao longo da linha de cuidado do CCU.
# Códigos seguem o padrão SIGTAP (10 dígitos como string).
# Mantidos em vetores separados por categoria para que a função
# build_sia_cc_resumo possa devolver `categoria` no agrupamento.

codigos_oci         <- c("0901010057", "0901010111", "0901010120", "0901010065")  # OCI - oferta de cuidado integrado
codigos_coleta      <- c("0201020033")                                              # coleta de material p/ exame citopatológico
codigos_cito        <- c("0203010019", "0203010086")                                # exames citopatológicos (lâmina)
codigos_colposcopia <- c("0211040029")                                              # colposcopia
codigos_biopsia     <- c("0201010666")                                              # biópsia (procedimento ambulatorial)
codigos_tratamento  <- c("0409060089", "0409060305", "0409060038")                  # EZT (exérese da zona de transformação) — proxy de tratamento ambulatorial
codigos_anatomo     <- c("0203020022", "0203020081")                                # anatomia patológica
codigos_cirurgia    <- c("0416060013", "0416060056", "0416060064", "0416060080", "0416060110", "0416060137")  # cirurgias de alta complexidade
codigos_radio       <- c("0304010421", "0304010430")                                # radioterapia
codigos_quimio      <- c("0304020184", "0304040045")                                # quimioterapia


# União sem repetição — usada como filtro inclusivo no SIA-PA bruto
todos_codigos_interesse <- unique(c(
  codigos_oci, codigos_coleta, codigos_cito, codigos_colposcopia,
  codigos_biopsia, codigos_tratamento, codigos_anatomo, codigos_cirurgia,
  codigos_radio, codigos_quimio
))

# Nome da coluna que carrega o código de procedimento no SIA-PA
coluna_procedimento_sia_pa <- "PA_PROC_ID"

# ------------------------------------------------
# Função principal — SIA-PA (download + filtro + consolidação)
# ------------------------------------------------
# Estratégia:
#   - Loop UF × ano × mês (≈324 iterações por ano para 27 UFs);
#   - Cada chunk é tentado individualmente com tryCatch — falhas
#     em UF/mês não interrompem o pipeline;
#   - SE o chunk já existe (Parquet salvo), pula imediatamente
#     (idempotência: rodar 2× não rebaixa);
#   - SOMENTE salva chunk se download retornou linhas E filtro
#     achou pelo menos 1 procedimento de interesse.
#   - Após o loop, abre todos os Parquets como dataset Arrow e
#     consolida em sia_cc_completo.rds (formato pesado, vivo em
#     data-raw/, NÃO em data/).

run_prepare_SUS <- function(
    anos    = SUS_anos_default,
    ufs     = SUS_ufs_default,
    mes_ini = SUS_mes_ini,
    mes_fim = SUS_mes_fim
) {
  message("==== SIA-PA / SUS — download + filtro por códigos (chunk + parquet) ====")
  
  # Garante diretórios alvo (idempotente — não falha se já existem)
  if (!dir.exists(path_data_raw()))  dir.create(path_data_raw(), recursive = TRUE)
  if (!dir.exists(path_sia_chunk())) dir.create(path_sia_chunk(), recursive = TRUE)

  # -------- Etapa 1: baixar + filtrar + salvar em Parquet --------
  # Loop UF × ano × mês — cada combinação vira 1 arquivo .parquet.
  for (ano in anos) {
    for (uf in ufs) {
      message(">> UF: ", uf, " | Ano: ", ano)

      # ------------------------------------------------------------
      # DOWNLOAD + FILTRO + SALVAMENTO SE E SOMENTE SE COMPLETO
      # Política: nunca salvar chunk parcial — se algo falhar,
      # pula e re-tenta na próxima execução.
      # ------------------------------------------------------------

      for (mes in seq.int(mes_ini, mes_fim)) {
        mm  <- sprintf("%02d", mes)
        arq <- sprintf("sia_pa_filtrado_%s_%d_%02d.parquet", uf, ano, mes)
        caminho <- path_sia_chunk(arq)

        # Idempotência: se chunk já foi salvo em rodada anterior, pula
        if (file.exists(caminho)) {
          message("   - Mês: ", mm, " -> já existe (", arq, "), pulando.")
          next
        }

        message("   - Mês: ", mm)

        # TENTA BAIXAR (se qualquer arquivo .dcc falhar, pode retornar NULL).
        # timeout grande (40 min) — UFs grandes têm SIA-PA pesado.
        dados_mes_bruto <- tryCatch(
          fetch_datasus(
            year_start         = ano,
            year_end           = ano,
            month_start        = mes,
            month_end          = mes,
            uf                 = uf,
            information_system = "SIA-PA",
            timeout            = 2400
          ),
          error = function(e) {
            message("     ERRO ao baixar dados: ", e$message)
            return(NULL)
          }
        )
        
        # ---- BLOQUEIO DO SALVAMENTO ----
        # Salvar arquivo parcial mascararia o problema na próxima
        # execução (file.exists → next). Por isso aqui é next sem save.
        if (is.null(dados_mes_bruto)) {
          message("     >>> Download incompleto — NÃO salvando chunk.")
          next
        }

        if (nrow(dados_mes_bruto) == 0) {
          message("     >>> Download retornou zero linhas — NÃO salvando chunk.")
          next
        }
        # ---------------------------------

        # Filtra apenas os códigos de interesse (~10 categorias do CCU)
        # Usa dplyr::filter com .data[[col]] (NSE) por compatibilidade
        # com o objeto retornado pelo microdatasus (data.frame).
        dados_filtrados_mes <- dplyr::filter(
          dados_mes_bruto,
          .data[[coluna_procedimento_sia_pa]] %in% todos_codigos_interesse
        )

        if (nrow(dados_filtrados_mes) == 0) {
          message("     Nenhum procedimento de interesse — NÃO salvando chunk.")
          rm(dados_mes_bruto, dados_filtrados_mes); gc()
          next
        }

        # Marcadores de procedência (UF/ano/mês) — úteis para debugar
        # depois de consolidados; prefixo "." para não colidir com colunas oficiais
        df_chunk <- as.data.table(dados_filtrados_mes)
        df_chunk[, `:=`(.ano = ano, .mes = mes, .uf = uf)]

        # Somente SALVA se tudo acima funcionou sem erro.
        # Compressão zstd: melhor razão velocidade/tamanho para Parquet.
        tryCatch({
          write_parquet(df_chunk, caminho, compression = "zstd")
          message("     ✔ Chunk salvo: ", arq, " (", nrow(df_chunk), " linhas)")
        }, error = function(e) {
          message("     ✖ ERRO ao salvar chunk ", arq, ": ", e$message)
        })

        # Libera memória entre meses (SIA-PA cru é GB-escala em UFs grandes)
        rm(dados_mes_bruto, dados_filtrados_mes, df_chunk); gc()
        Sys.sleep(1)  # cortesia ao DATASUS — evita rate-limit
      }
      
      message("UF ", uf, " concluída.")
    }
  }
  
  message("Processo de download + filtro concluído. Consolidando Parquets...")

  # -------- Etapa 2: consolidar Parquets em um único RDS --------
  # `open_dataset` faz lazy-binding via Arrow; `collect()` materializa.
  # Resultado vai para data-raw/ (NÃO data/) porque é arquivo pesado
  # de cache, não consumido diretamente pelo app.
  arquivos_parquet <- list.files(path_sia_chunk(), pattern = "\\.parquet$", full.names = TRUE)
  if (!length(arquivos_parquet)) {
    warning("Nenhum arquivo Parquet encontrado em ", path_sia_chunk())
    return(invisible(NULL))
  }

  message(">> Abrindo dataset a partir de ", length(arquivos_parquet), " arquivos Parquet...")
  ds <- open_dataset(path_sia_chunk(), format = "parquet")

  sia_cc_completo <- as.data.table(collect(ds))

  saveRDS(sia_cc_completo, path_data_raw("sia_cc_completo.rds"))
  message("==== FIM ====")
  message("Linhas totais em sia_cc_completo.rds: ", nrow(sia_cc_completo))
  message("Arquivo salvo em: ", path_data_raw("sia_cc_completo.rds"))


  invisible(sia_cc_completo)
}


# ------------------------------------------------
# Base resumida do SIA-PA para o app
# Gera: data/sia_cc_resumo.rds
# ------------------------------------------------
# Recebe o sia_cc_completo (granular linha-a-linha) e devolve uma
# tabela agregada por (categoria × geo × competência × procedimento
# × sexo × faixa etária). É o insumo para `build_sus_proc_resumo`.
#
# Robustez:
#  - Aceita layouts em que algumas colunas estejam ausentes
#    (preenche com NA do tipo certo);
#  - Padroniza geo_id (PA_UFMUN/PA_MUNPCN) com zfill 6;
#  - Deriva ano_cmp/mes_cmp a partir de PA_CMP (formato AAAAMM);
#  - Faixa etária em buckets de 5 anos (igual ao IBGE/GLOBOCAN);
#  - Une categoria via merge com lookup gerado a partir dos vetores
#    `codigos_*` definidos no topo do arquivo;
#  - Atribui nome do procedimento via SIGTAP (microdatasus::fetch_sigtab).
#
# Param ano_cmp: filtro opcional pré-agregação (mantém apenas a
# competência indicada). Útil em desenvolvimento.

build_sia_cc_resumo <- function(
    input_rds  = path_data_raw("sia_cc_completo.rds"),
    output_rds = path_data("sia_cc_resumo.rds"),
    ano_cmp    = NULL
) {
  if (!file.exists(input_rds)) {
    stop("Arquivo não encontrado: ", input_rds,
         " — rode run_prepare_SUS() antes.")
  }
  
  dt <- readRDS(input_rds)
  data.table::setDT(dt)

  # ---- garantir colunas necessárias ----
  # Layouts antigos do SIA podem não ter algumas colunas.
  # Em vez de falhar, preenche com NA do tipo correto.
  cols_chr <- c("PA_UFMUN", "PA_CMP", "PA_PROC_ID", "PA_SEXO", "PA_MUNPCN")
  cols_num <- c("PA_IDADE", "PA_QTDPRO", "PA_QTDAPR")

  missing_chr <- setdiff(cols_chr, names(dt))
  missing_num <- setdiff(cols_num, names(dt))

  if (length(missing_chr)) {
    for (v in missing_chr) dt[, (v) := NA_character_]
  }
  if (length(missing_num)) {
    for (v in missing_num) dt[, (v) := NA_real_]
  }

  # ---- normalizações básicas ----
  # zfill 6 nos códigos IBGE de município (alguns vintages perdem zero à esquerda).
  dt[, PA_UFMUN  := sprintf("%06s", as.character(PA_UFMUN))]
  dt[, PA_MUNPCN := sprintf("%06s", as.character(PA_MUNPCN))]
  dt[, PA_PROC_ID := as.character(PA_PROC_ID)]   # SIGTAP é texto (10 dígitos)
  dt[, PA_SEXO    := as.character(PA_SEXO)]

  dt[, PA_IDADE  := as.integer(PA_IDADE)]
  dt[, PA_QTDPRO := as.numeric(PA_QTDPRO)]        # qtd produzida
  dt[, PA_QTDAPR := as.numeric(PA_QTDAPR)]        # qtd aprovada (faturada) — preferida pelo app

  # ---- competência AAAAMM -> ano_cmp / mes_cmp ----
  # PA_CMP vem como "202401" (string AAAAMM). Decompõe em 2 inteiros
  # apenas quando o formato bate (regex ^[0-9]+$ + nchar=6).
  dt[, PA_CMP := as.character(PA_CMP)]
  dt[
    nchar(PA_CMP) == 6 & grepl("^[0-9]+$", PA_CMP),
    `:=`(
      ano_cmp = as.integer(substr(PA_CMP, 1, 4)),
      mes_cmp = as.integer(substr(PA_CMP, 5, 6))
    )
  ]
  # Garantia: se nenhum PA_CMP bateu o regex, criamos as colunas como NA
  if (!"ano_cmp" %in% names(dt)) dt[, ano_cmp := NA_integer_]
  if (!"mes_cmp" %in% names(dt)) dt[, mes_cmp := NA_integer_]

  # ---- filtro opcional por ano de competência ----
  if (!is.null(ano_cmp)) {
    ano_cmp_sel <- as.integer(ano_cmp)
    dt <- dt[ano_cmp == ano_cmp_sel]
  }



  # ---- categoria a partir da lista de códigos ----
  # Constrói lookup (codigo -> categoria) empilhando os 10 vetores
  # de códigos definidos no topo do arquivo. Merge à esquerda
  # mantém todas as linhas (categoria=NA se código fora dos 10 grupos).
  SUS_codigos_cat <- data.table::rbindlist(list(
    data.table::data.table(codigo = codigos_oci,        categoria = "oci"),
    data.table::data.table(codigo = codigos_coleta, categoria = "coleta"),
    data.table::data.table(codigo = codigos_cito,     categoria = "citologia"),
    data.table::data.table(codigo = codigos_colposcopia,     categoria = "colposcopia"),
    data.table::data.table(codigo = codigos_tratamento,     categoria = "tratamento"),
    data.table::data.table(codigo = codigos_biopsia,    categoria = "biopsia"),
    data.table::data.table(codigo = codigos_anatomo,    categoria = "anatomo"),
    data.table::data.table(codigo = codigos_cirurgia,   categoria = "cirurgia"),
    data.table::data.table(codigo = codigos_radio,      categoria = "radioterapia"),
    data.table::data.table(codigo = codigos_quimio,     categoria = "quimioterapia")
  ))
  SUS_codigos_cat[, codigo := as.character(codigo)]
  SUS_codigos_cat <- unique(SUS_codigos_cat)
  
  dt <- merge(
    dt,
    SUS_codigos_cat,
    by.x = "PA_PROC_ID",
    by.y = "codigo",
    all.x = TRUE
  )
  

  # --- Tabela SIGTAP para nome de procedimento --------------------------
  # Pega COD/nome_proced do SIGTAP via microdatasus e anexa por código.
  # Necessário para exibir labels legíveis nos cards/tabelas do app.
  sigtab <- microdatasus::fetch_sigtab()

  sigtab_dt <- data.table::as.data.table(sigtab)[
    ,
    .(
      PA_PROC_ID        = as.character(COD),
      nome_procedimento = as.character(nome_proced)
    )
  ]

  dt <- merge(
    dt,
    sigtab_dt,
    by = "PA_PROC_ID",
    all.x = TRUE
  )


  # ---- faixa etária 0-4, 5-9, ..., 85+ ----
  # Mesmos buckets quinquenais do IBGE/GLOBOCAN para alinhamento
  # com pop_municipio_regional. fcase é case-when do data.table.
  dt[
    ,
    faixa_idade := data.table::fifelse(
      is.na(PA_IDADE) | PA_IDADE < 0,
      NA_character_,
      data.table::fcase(
        PA_IDADE <= 4,  "0-4",
        PA_IDADE <= 9,  "5-9",
        PA_IDADE <= 14, "10-14",
        PA_IDADE <= 19, "15-19",
        PA_IDADE <= 24, "20-24",
        PA_IDADE <= 29, "25-29",
        PA_IDADE <= 34, "30-34",
        PA_IDADE <= 39, "35-39",
        PA_IDADE <= 44, "40-44",
        PA_IDADE <= 49, "45-49",
        PA_IDADE <= 54, "50-54",
        PA_IDADE <= 59, "55-59",
        PA_IDADE <= 64, "60-64",
        PA_IDADE <= 69, "65-69",
        PA_IDADE <= 74, "70-74",
        PA_IDADE <= 79, "75-79",
        PA_IDADE <= 84, "80-84",
        default = "85+"
      )
    )
  ]
  
  # ---- agregação: 1 linha por combinação ----
  # Mantém tanto o local de atendimento (PA_UFMUN) quanto o de
  # residência (PA_MUNPCN) — `build_sus_proc_resumo` separa em
  # geo_ref="care" e geo_ref="res" depois.
  dt_final <- dt[
    ,
    .(
      total_qtdpro = sum(PA_QTDPRO, na.rm = TRUE),  # produção (apresentada)
      total_qtdapr = sum(PA_QTDAPR, na.rm = TRUE)   # aprovada (faturada) — usada no app
    ),
    by = .(
      categoria,
      PA_UFMUN,           # local de atendimento (município do prestador)
      PA_CMP,
      PA_PROC_ID,
      nome_procedimento,
      PA_SEXO,
      PA_MUNPCN,          # local de residência (município do paciente)
      faixa_idade,
      ano_cmp,
      mes_cmp
    )
  ]

  saveRDS(dt_final, output_rds)
  message(">> sia_cc_resumo salvo em: ", output_rds,
          " (", nrow(dt_final), " linhas)")
  invisible(dt_final)
}


# ================================================================
# SIH-RD (Internações SUS) — EZT (excisão) via procedimento na RD
# ----------------------------------------------------------------
# Mesma estrutura do SIA-PA acima, mas para SIH (Reduzida).
# Por que existe: parte da produção de "tratamento" (EZT) é
# faturada como internação e portanto aparece no SIH em vez do SIA.
# Sem este pipeline, o app subestimaria EZT em hospitais.
#
# Diferenças vs SIA-PA:
#   - Coluna do procedimento: PROC_REA (não PA_PROC_ID);
#   - Código de competência vem em ANO_CMPT/MES_CMPT (não PA_CMP);
#   - 1 linha por internação (AIH) — agregação usa uniqueN(N_AIH)
#     se disponível, senão .N (cada linha = 1 internação).
#
# Caches:
#   data-raw/sih_rd_ezt_chunks_sih_rd/  (Parquets UF-ano-mês)
#   data-raw/sih_rd_ezt_completo.rds    (consolidado, granular)
#   data/sih_rd_ezt_resumo.rds          (para uso pelo app/ETL3)
# ================================================================

path_sih_rd_chunk <- function(...) path_data_raw("sih_rd_ezt_chunks_sih_rd", ...)

# ------------------------------------------------
# Parâmetros SIH-RD (EZT)
# ------------------------------------------------

# No SIH-RD, o procedimento realizado costuma estar em PROC_REA
coluna_procedimento_sih_rd <- "PROC_REA"

# Por padrão, reaproveitamos os códigos de "tratamento" do SIA.
# Se aparecer uma lista específica de EZT no SIH (códigos de internação
# diferentes), pode-se trocar aqui sem mexer na função principal.
codigos_ezt_sih_rd <- codigos_tratamento

# ------------------------------------------------
# Função principal: baixar + filtrar + chunk parquet (SIH-RD)
# ------------------------------------------------
# Espelha run_prepare_SUS para SIH-RD. Mesma política de chunk
# atomic-write (só salva se download e filtro tiveram sucesso).
# Adiciona uma checagem extra: se a coluna de procedimento esperada
# (`coluna_proc`) não existir no layout retornado, pula o mês —
# layouts de SIH-RD variaram historicamente.

run_prepare_SIH_RD_EZT <- function(
    anos    = SUS_anos_default,
    ufs     = SUS_ufs_default,
    mes_ini = SUS_mes_ini,
    mes_fim = SUS_mes_fim,
    codigos_ezt = codigos_ezt_sih_rd,
    coluna_proc = coluna_procedimento_sih_rd
) {
  message("==== SIH-RD / SUS — download + filtro EZT (chunk + parquet) ====")
  
  if (!dir.exists(path_data_raw()))     dir.create(path_data_raw(), recursive = TRUE)
  if (!dir.exists(path_sih_rd_chunk())) dir.create(path_sih_rd_chunk(), recursive = TRUE)
  
  # -------- Etapa 1: baixar + filtrar + salvar em Parquet --------
  for (ano in anos) {
    for (uf in ufs) {
      message(">> UF: ", uf, " | Ano: ", ano)
      
      for (mes in seq.int(mes_ini, mes_fim)) {
        mm  <- sprintf("%02d", mes)
        arq <- sprintf("sih_rd_ezt_filtrado_%s_%d_%02d.parquet", uf, ano, mes)
        caminho <- path_sih_rd_chunk(arq)
        
        if (file.exists(caminho)) {
          message("   - Mês: ", mm, " -> já existe (", arq, "), pulando.")
          next
        }
        
        message("   - Mês: ", mm)
        
        dados_mes_bruto <- tryCatch(
          fetch_datasus(
            year_start         = ano,
            year_end           = ano,
            month_start        = mes,
            month_end          = mes,
            uf                 = uf,
            information_system = "SIH-RD",
            timeout            = 2400
          ),
          error = function(e) {
            message("     ERRO ao baixar dados (SIH-RD): ", e$message)
            return(NULL)
          }
        )
        
        # ---- BLOQUEIO DO SALVAMENTO ----
        if (is.null(dados_mes_bruto)) {
          message("     >>> Download incompleto — NÃO salvando chunk.")
          next
        }
        
        if (nrow(dados_mes_bruto) == 0) {
          message("     >>> Download retornou zero linhas — NÃO salvando chunk.")
          next
        }
        # ---------------------------------
        
        # Layout SIH-RD pode variar — checa coluna esperada antes de filtrar
        if (!coluna_proc %in% names(dados_mes_bruto)) {
          message("     >>> Coluna ", coluna_proc, " não existe no SIH-RD baixado — NÃO salvando chunk.")
          rm(dados_mes_bruto); gc()
          next
        }

        # normaliza coluna de procedimento como character (evita problema de tipo)
        dados_mes_bruto[[coluna_proc]] <- as.character(dados_mes_bruto[[coluna_proc]])

        # Filtra apenas os códigos de EZT
        dados_filtrados_mes <- dplyr::filter(
          dados_mes_bruto,
          .data[[coluna_proc]] %in% as.character(codigos_ezt)
        )
        
        if (nrow(dados_filtrados_mes) == 0) {
          message("     Nenhum procedimento EZT no mês — NÃO salvando chunk.")
          rm(dados_mes_bruto, dados_filtrados_mes); gc()
          next
        }
        
        df_chunk <- as.data.table(dados_filtrados_mes)
        df_chunk[, `:=`(.ano = as.integer(ano), .mes = as.integer(mes), .uf = as.character(uf))]
        
        tryCatch({
          write_parquet(df_chunk, caminho, compression = "zstd")
          message("     ✔ Chunk salvo: ", arq, " (", nrow(df_chunk), " linhas)")
        }, error = function(e) {
          message("     ✖ ERRO ao salvar chunk ", arq, ": ", e$message)
        })
        
        rm(dados_mes_bruto, dados_filtrados_mes, df_chunk); gc()
        Sys.sleep(1)
      }
      
      message("UF ", uf, " concluída.")
    }
  }
  
  message("Processo SIH-RD (EZT) concluído. Consolidando Parquets...")
  
  # -------- Etapa 2: consolidar Parquets em um único RDS --------
  arquivos_parquet <- list.files(path_sih_rd_chunk(), pattern = "\\.parquet$", full.names = TRUE)
  if (!length(arquivos_parquet)) {
    warning("Nenhum arquivo Parquet encontrado em ", path_sih_rd_chunk())
    return(invisible(NULL))
  }
  
  message(">> Abrindo dataset a partir de ", length(arquivos_parquet), " arquivos Parquet...")
  ds <- open_dataset(path_sih_rd_chunk(), format = "parquet")
  
  sih_rd_ezt_completo <- as.data.table(collect(ds))
  
  saveRDS(sih_rd_ezt_completo, path_data_raw("sih_rd_ezt_completo.rds"))
  message("==== FIM (SIH-RD EZT) ====")
  message("Linhas totais em sih_rd_ezt_completo.rds: ", nrow(sih_rd_ezt_completo))
  message("Arquivo salvo em: ", path_data_raw("sih_rd_ezt_completo.rds"))
  
  invisible(sih_rd_ezt_completo)
}

# ------------------------------------------------
# Base resumida do SIH-RD (EZT) para o app
# Gera: data/sih_rd_ezt_resumo.rds
# ------------------------------------------------
# Análoga a build_sia_cc_resumo, mas com fallbacks específicos do SIH:
#  - Layouts diferentes podem ter ANO_CMPT/MES_CMPT ou só .ano/.mes
#    (do chunking nosso). A função tenta ANO_CMPT primeiro, cai em .ano.
#  - N_AIH (identificador da AIH) nem sempre está presente — quando
#    está, agregação usa uniqueN; quando não, .N (1 linha = 1 internação).
#  - Faixas etárias: mesmos buckets do SIA (compatibilidade).

build_sih_rd_ezt_resumo <- function(
    input_rds  = path_data_raw("sih_rd_ezt_completo.rds"),
    output_rds = path_data("sih_rd_ezt_resumo.rds"),
    ano_cmp    = NULL,
    coluna_proc = coluna_procedimento_sih_rd
) {
  if (!file.exists(input_rds)) {
    stop("Arquivo não encontrado: ", input_rds,
         " — rode run_prepare_SIH_RD_EZT() antes.")
  }
  
  dt <- readRDS(input_rds)
  data.table::setDT(dt)

  # ---- garantir colunas necessárias (com fallback) ----
  # Layouts SIH-RD variam por ano/CD-DBC. Em vez de stop(), preenche
  # com NA do tipo certo para que o pipeline continue.
  # Procedimento
  if (!coluna_proc %in% names(dt)) dt[, (coluna_proc) := NA_character_]

  # Competência (alguns layouts trazem ANO_CMPT e MES_CMPT)
  if (!"ANO_CMPT" %in% names(dt)) dt[, ANO_CMPT := NA_integer_]
  if (!"MES_CMPT" %in% names(dt)) dt[, MES_CMPT := NA_integer_]

  # Identificador AIH (nem sempre vem, depende do layout/extração)
  if (!"N_AIH" %in% names(dt)) dt[, N_AIH := NA_character_]

  # Sexo e idade (variam; mantemos os nomes mais comuns)
  if (!"SEXO" %in% names(dt))  dt[, SEXO := NA_character_]
  if (!"IDADE" %in% names(dt)) dt[, IDADE := NA_integer_]

  # Municípios (residência e movimento/atendimento)
  if (!"MUNIC_RES" %in% names(dt)) dt[, MUNIC_RES := NA_character_]
  if (!"MUNIC_MOV" %in% names(dt)) dt[, MUNIC_MOV := NA_character_]

  # UF do hospital pode vir como UF_ZI em alguns layouts
  if (!"UF_ZI" %in% names(dt)) dt[, UF_ZI := NA_character_]

  # ---- normalizações básicas ----
  dt[, (coluna_proc) := as.character(get(coluna_proc))]
  dt[, SEXO := as.character(SEXO)]
  dt[, IDADE := as.integer(IDADE)]
  
  dt[, MUNIC_RES := sprintf("%06s", as.character(MUNIC_RES))]
  dt[, MUNIC_MOV := sprintf("%06s", as.character(MUNIC_MOV))]
  dt[, UF_ZI     := as.character(UF_ZI)]
  dt[, N_AIH     := as.character(N_AIH)]
  
  dt[, ANO_CMPT := as.integer(ANO_CMPT)]
  dt[, MES_CMPT := as.integer(MES_CMPT)]
  
  # ---- cria ano_cmp/mes_cmp com fallback para as colunas .ano/.mes se vierem do chunk ----
  # Estratégia 2-níveis: usa ANO_CMPT/MES_CMPT (oficial) quando disponíveis;
  # senão, recorre aos marcadores de chunk (.ano/.mes) que adicionamos
  # em run_prepare_SIH_RD_EZT. Isso protege contra layouts antigos.
  dt[, ano_cmp := fifelse(!is.na(ANO_CMPT), ANO_CMPT, as.integer(.ano))]
  dt[, mes_cmp := fifelse(!is.na(MES_CMPT), MES_CMPT, as.integer(.mes))]
  
  # ---- filtro opcional por ano de competência ----
  if (!is.null(ano_cmp)) {
    ano_cmp_sel <- as.integer(ano_cmp)
    dt <- dt[ano_cmp == ano_cmp_sel]
  }
  
  # ---- faixa etária 0-4, 5-9, ..., 85+ (igual ao SIA) ----
  dt[
    ,
    faixa_idade := data.table::fifelse(
      is.na(IDADE) | IDADE < 0,
      NA_character_,
      data.table::fcase(
        IDADE <= 4,  "0-4",
        IDADE <= 9,  "5-9",
        IDADE <= 14, "10-14",
        IDADE <= 19, "15-19",
        IDADE <= 24, "20-24",
        IDADE <= 29, "25-29",
        IDADE <= 34, "30-34",
        IDADE <= 39, "35-39",
        IDADE <= 44, "40-44",
        IDADE <= 49, "45-49",
        IDADE <= 54, "50-54",
        IDADE <= 59, "55-59",
        IDADE <= 64, "60-64",
        IDADE <= 69, "65-69",
        IDADE <= 74, "70-74",
        IDADE <= 79, "75-79",
        IDADE <= 84, "80-84",
        default = "85+"
      )
    )
  ]
  
  # --- Tabela SIGTAP para nome de procedimento (mesma lógica do SIA) ---
  sigtab <- microdatasus::fetch_sigtab()
  sigtab_dt <- data.table::as.data.table(sigtab)[
    ,
    .(
      PROC_ID          = as.character(COD),
      nome_procedimento = as.character(nome_proced)
    )
  ]

  # Cria coluna PROC_ID padrão (o nome do campo varia entre layouts).
  # `get(coluna_proc)` resolve em runtime — por isso o argumento `coluna_proc`.
  dt[, PROC_ID := get(coluna_proc)]

  dt <- merge(
    dt,
    sigtab_dt,
    by = "PROC_ID",
    all.x = TRUE
  )

  # ---- agregação (AIH é 1 linha na RD, mas mantemos robusto) ----
  # n_internacoes: se N_AIH existir, conta uniqueN (proteção contra
  # duplicação por linha-procedimento); senão, usa .N (1 linha = 1 AIH).
  # Decisão dentro da agregação para evitar branch fora.
  dt_final <- dt[
    ,
    .(
      n_internacoes = data.table::fifelse(
        all(is.na(N_AIH)),
        .N,
        data.table::uniqueN(N_AIH)
      )
    ),
    by = .(
      .uf,
      ano_cmp,
      mes_cmp,
      PROC_ID,
      nome_procedimento,
      SEXO,
      faixa_idade,
      MUNIC_RES,
      MUNIC_MOV,
      UF_ZI
    )
  ]
  
  saveRDS(dt_final, output_rds)
  message(">> sih_rd_ezt_resumo salvo em: ", output_rds,
          " (", nrow(dt_final), " linhas)")
  invisible(dt_final)
}

# ------------------------------------------------
# Base SUS "resumo" (leve) para o app/BI — ano fixo (ex.: 2024)
# ------------------------------------------------
# Esta é a SAÍDA FINAL do pipeline SUS — o arquivo carregado pelo app.R.
#
# Estrutura:
#  - Sem ano/mês na base final (filtra no pré-processamento p/ ano_ref);
#  - 2 linhas por (geo_id, categoria, procedimento): geo_ref="care"
#    (PA_UFMUN/MUNIC_MOV) e geo_ref="res" (PA_MUNPCN/MUNIC_RES);
#  - 2 métricas: total_all (todas idades) e total_25_64
#    (faixa-alvo do rastreamento, hard-coded);
#  - Já faz merge com regional_sus_map (UF, Macro, Reg, Mun);
#  - Anexa SIH-RD com categoria="tratamento" (EZT em internação),
#    sistema="SIH" para distinção do SIA.
#
# Por que ano_ref fixo: o app exibe "produção 2024" como referência
# de capacidade; mudanças anuais são re-rodadas alterando este param.

build_sus_proc_resumo <- function(
    ano_ref     = 2024L,
    input_sia   = path_data("sia_cc_resumo.rds"),
    input_sih   = path_data("sih_rd_ezt_resumo.rds"),
    input_reg   = path_data("regional_sus_map.rds"),
    output_rds  = path_data("sus_proc_resumo.rds")
) {
  ano_ref <- as.integer(ano_ref)
  
  if (!file.exists(input_sia)) {
    stop("Arquivo não encontrado: ", input_sia, " — gere data/sia_cc_resumo.rds antes.")
  }
  if (!file.exists(input_reg)) {
    stop("Arquivo não encontrado: ", input_reg, " — gere data/regional_sus_map.rds antes.")
  }
  
  # -----------------------------
  # 1) REGIONAL MAP (somente colunas necessárias)
  # -----------------------------
  # Atenção: as colunas usadas aqui têm os nomes "humanos" do dataset
  # original ("Macrorregiao de Saude", "Regiao de Saude", "Municipio") —
  # NÃO o snake_case do `regional_sus_map` gerado em 02_prepare_BR.R.
  # Isto é proposital: o app filtra/exibe pelos nomes humanos.
  reg <- readRDS(input_reg)
  data.table::setDT(reg)

  # Garantir chave de merge
  if (!"geo_id" %in% names(reg)) stop("regional_sus_map: coluna 'geo_id' ausente.", call. = FALSE)
  reg[, geo_id := sprintf("%06s", as.character(geo_id))]

  # Manter apenas as 5 colunas usadas no app (filtros + display)
  keep_reg <- c("geo_id", "UF", "Macrorregiao de Saude", "Regiao de Saude", "Municipio")
  keep_reg <- keep_reg[keep_reg %in% names(reg)]
  reg_small <- reg[, ..keep_reg]
  
  # -----------------------------
  # 2) SIA (aprovado) -> care/res + total_all / total_25_64
  # -----------------------------
  # `total_qtdapr` (aprovada) é a métrica usada — representa produção
  # efetivamente faturada (proxy de "realizado").
  sia <- readRDS(input_sia)
  data.table::setDT(sia)

  # Schema validation defensivo: ETL muda → falha clara aqui em vez de NA silencioso
  needed_sia <- c("categoria","PA_UFMUN","PA_MUNPCN","faixa_idade","ano_cmp","total_qtdapr")
  miss_sia <- setdiff(needed_sia, names(sia))
  if (length(miss_sia)) {
    stop("sia_cc_resumo: faltam colunas: ", paste(miss_sia, collapse = ", "), call. = FALSE)
  }

  sia <- sia[ano_cmp == ano_ref]
  if (!nrow(sia)) {
    warning("SIA: nenhuma linha encontrada para ano_ref=", ano_ref)
  }

  # Faixas 25–64 (como strings já padronizadas no build_sia_cc_resumo)
  # Faixa hard-coded como referência epidemiológica do rastreamento CCU
  # (independente do que o usuário escolher no slider — é coluna de contexto).
  # NOTA: definida 2× por motivo histórico — duplicação inofensiva.
  faixa_25_64 <- c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")

  # Faixas 25–64 (como strings já padronizadas no build_sia_cc_resumo)
  faixa_25_64 <- c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")
  
  # care (PA_UFMUN) — local DE ATENDIMENTO (município do prestador SUS).
  # Visão "produção territorial": qual cidade ofertou o exame.
  sia_care <- sia[
    ,
    .(
      total_all   = sum(total_qtdapr, na.rm = TRUE),
      total_25_64 = sum(data.table::fifelse(faixa_idade %in% faixa_25_64, total_qtdapr, 0), na.rm = TRUE)
    ),
    by = .(categoria, PA_PROC_ID, nome_procedimento, geo_id = PA_UFMUN)
  ]
  sia_care[, `:=`(geo_ref = "care", sistema = "SIA")]

  # res (PA_MUNPCN) — local DE RESIDÊNCIA (município de moradia da paciente).
  # Visão "demanda da população": de onde vêm as pacientes atendidas.
  sia_res <- sia[
    ,
    .(
      total_all   = sum(total_qtdapr, na.rm = TRUE),
      total_25_64 = sum(data.table::fifelse(faixa_idade %in% faixa_25_64, total_qtdapr, 0), na.rm = TRUE)
    ),
    by = .(categoria, PA_PROC_ID, nome_procedimento, geo_id = PA_MUNPCN)
  ]
  sia_res[, `:=`(geo_ref = "res", sistema = "SIA")]

  # Empilha as 2 visões (uma sobre a outra) — usuário escolhe via input_global$sia_geo_ref
  sia_out <- data.table::rbindlist(list(sia_care, sia_res), use.names = TRUE, fill = TRUE)
  sia_out[, geo_id := sprintf("%06s", as.character(geo_id))]
  sia_out[, PA_PROC_ID := as.character(PA_PROC_ID)]
  sia_out[, nome_procedimento := as.character(nome_procedimento)]

  # Merge regional (já no data-raw, antes de salvar — economiza joins no app)
  sia_out <- merge(sia_out, reg_small, by = "geo_id", all.x = TRUE)
  
  
  # -----------------------------
  # 3) SIH-RD (EZT tipo 3) -> care/res + total_all / total_25_64
  #    Entra como categoria = "tratamento"
  #    Mantém PA_PROC_ID e nome_procedimento (a partir de PROC_ID)
  # -----------------------------
  # SIH é OPCIONAL — se input_sih não existir, o pipeline ainda
  # produz uma base "só SIA" (com warning). Isso facilita rodar o
  # pipeline antes de baixar o SIH-RD completo.
  sih_out <- data.table::data.table()  # default vazio (se não existir input_sih)

  if (file.exists(input_sih)) {
    sih <- readRDS(input_sih)
    data.table::setDT(sih)
    
    needed_sih <- c("ano_cmp","faixa_idade","MUNIC_RES","MUNIC_MOV","n_internacoes","PROC_ID","nome_procedimento")
    miss_sih <- setdiff(needed_sih, names(sih))
    if (length(miss_sih)) {
      stop("sih_rd_ezt_resumo: faltam colunas: ", paste(miss_sih, collapse = ", "), call. = FALSE)
    }
    
    sih <- sih[ano_cmp == ano_ref]
    if (!nrow(sih)) {
      warning("SIH: nenhuma linha encontrada para ano_ref=", ano_ref)
    }
    
    # care (MUNIC_MOV) — município do hospital onde foi feita a internação
    sih_care <- sih[
      ,
      .(
        total_all   = sum(n_internacoes, na.rm = TRUE),
        total_25_64 = sum(data.table::fifelse(faixa_idade %in% faixa_25_64, n_internacoes, 0), na.rm = TRUE)
      ),
      by = .(PA_PROC_ID = PROC_ID, nome_procedimento, geo_id = MUNIC_MOV)
    ]
    # categoria="tratamento" → no app, soma com SIA tratamento (visão única de EZT)
    sih_care[, `:=`(geo_ref = "care", sistema = "SIH", categoria = "tratamento")]

    # res (MUNIC_RES) — município de moradia da paciente internada
    sih_res <- sih[
      ,
      .(
        total_all   = sum(n_internacoes, na.rm = TRUE),
        total_25_64 = sum(data.table::fifelse(faixa_idade %in% faixa_25_64, n_internacoes, 0), na.rm = TRUE)
      ),
      by = .(PA_PROC_ID = PROC_ID, nome_procedimento, geo_id = MUNIC_RES)
    ]
    sih_res[, `:=`(geo_ref = "res", sistema = "SIH", categoria = "tratamento")]
    
    sih_out <- data.table::rbindlist(list(sih_care, sih_res), use.names = TRUE, fill = TRUE)
    sih_out[, geo_id := sprintf("%06s", as.character(geo_id))]
    sih_out[, PA_PROC_ID := as.character(PA_PROC_ID)]
    sih_out[, nome_procedimento := as.character(nome_procedimento)]
    
    sih_out <- merge(sih_out, reg_small, by = "geo_id", all.x = TRUE)
  } else {
    warning("input_sih não encontrado (", input_sih, "). Base final terá apenas SIA.")
  }
  
  
  # -----------------------------
  # 4) Junta tudo e salva
  # -----------------------------
  # rbindlist com use.names=TRUE+fill=TRUE — robusto a colunas
  # que existem só num dos lados (ex.: "categoria" no SIA é livre,
  # no SIH é fixo "tratamento"; SIH não tem PA_PROC_ID original).
  out <- data.table::rbindlist(list(sia_out, sih_out), use.names = TRUE, fill = TRUE)

  # Ordem de colunas (deixa amigável para inspeção em DT/print)
  col_order <- c(
    "sistema","categoria","PA_PROC_ID","nome_procedimento",
    "geo_ref","geo_id",
    "UF","Macrorregiao de Saude","Regiao de Saude","Municipio",
    "total_all","total_25_64"
  )
  # Anexa quaisquer colunas extras ao final (preservadas por segurança)
  col_order <- c(col_order, setdiff(names(out), col_order))
  data.table::setcolorder(out, col_order)

  saveRDS(out, output_rds)
  message(">> sus_proc_resumo salvo em: ", output_rds, " (", nrow(out), " linhas) — ano_ref=", ano_ref)
  invisible(out)
}


