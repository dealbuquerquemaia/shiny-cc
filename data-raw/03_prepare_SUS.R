# ================================================================
# 03_prepare_SUS.R
# SIA-PA (produção SUS) para câncer do colo do útero
# - Baixa e FILTRA por códigos de interesse (sem process_sia)
# - Salva chunks Parquet (UF-ano-mês)
# - Consolida em data-raw/sia_cc_completo.rds
# - Cria  data/sia_cc_resumo.rds
# ================================================================

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

path_data      <- function(...) here::here("data", ...)
path_data_raw  <- function(...) here::here("data-raw", ...)
path_sia_chunk <- function(...) path_data_raw("sia_cc_chunks_sia_pa", ...)


# ------------------------------------------------
# Parâmetros
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
# Códigos de procedimento de interesse
# ------------------------------------------------

codigos_oci <- c("0901010057", "0901010111", "0901010120", "0901010065")
codigos_coleta <- c("0201020033")
codigos_cito <- c("0203010019", "0203010086")
codigos_colposcopia <- c("0211040029")
codigos_biopsia <- c("0201010666")
codigos_tratamento <- c("0409060089", "0409060305", "0409060038")
codigos_anatomo <- c("0203020022", "0203020081")
codigos_cirurgia <- c("0416060013", "0416060056", "0416060064", "0416060080", "0416060110", "0416060137")
codigos_radio <- c("0304010421", "0304010430")
codigos_quimio <- c("0304020184", "0304040045")


todos_codigos_interesse <- unique(c(
  codigos_oci, codigos_coleta, codigos_cito, codigos_colposcopia, 
  codigos_biopsia, codigos_tratamento, codigos_anatomo, codigos_cirurgia, 
  codigos_radio, codigos_quimio
))

coluna_procedimento_sia_pa <- "PA_PROC_ID"

# ------------------------------------------------
# Função principal
# ------------------------------------------------

run_prepare_SUS <- function(
    anos    = SUS_anos_default,
    ufs     = SUS_ufs_default,
    mes_ini = SUS_mes_ini,
    mes_fim = SUS_mes_fim
) {
  message("==== SIA-PA / SUS — download + filtro por códigos (chunk + parquet) ====")
  
  if (!dir.exists(path_data_raw()))  dir.create(path_data_raw(), recursive = TRUE)
  if (!dir.exists(path_sia_chunk())) dir.create(path_sia_chunk(), recursive = TRUE)
  
  # -------- Etapa 1: baixar + filtrar + salvar em Parquet --------
  for (ano in anos) {
    for (uf in ufs) {
      message(">> UF: ", uf, " | Ano: ", ano)
      
      # ------------------------------------------------------------
      # DOWNLOAD + FILTRO + SALVAMENTO SE E SOMENTE SE COMPLETO
      # ------------------------------------------------------------
      
      for (mes in seq.int(mes_ini, mes_fim)) {
        mm  <- sprintf("%02d", mes)
        arq <- sprintf("sia_pa_filtrado_%s_%d_%02d.parquet", uf, ano, mes)
        caminho <- path_sia_chunk(arq)
        
        if (file.exists(caminho)) {
          message("   - Mês: ", mm, " -> já existe (", arq, "), pulando.")
          next
        }
        
        message("   - Mês: ", mm)
        
        # TENTA BAIXAR (se qualquer arquivo .dcc falhar, pode retornar NULL)
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
        if (is.null(dados_mes_bruto)) {
          message("     >>> Download incompleto — NÃO salvando chunk.")
          next
        }
        
        if (nrow(dados_mes_bruto) == 0) {
          message("     >>> Download retornou zero linhas — NÃO salvando chunk.")
          next
        }
        # ---------------------------------
        
        # Filtra apenas os códigos de interesse
        dados_filtrados_mes <- dplyr::filter(
          dados_mes_bruto,
          .data[[coluna_procedimento_sia_pa]] %in% todos_codigos_interesse
        )
        
        if (nrow(dados_filtrados_mes) == 0) {
          message("     Nenhum procedimento de interesse — NÃO salvando chunk.")
          rm(dados_mes_bruto, dados_filtrados_mes); gc()
          next
        }
        
        df_chunk <- as.data.table(dados_filtrados_mes)
        df_chunk[, `:=`(.ano = ano, .mes = mes, .uf = uf)]
        
        # Somente SALVA se tudo acima funcionou sem erro
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
  
  message("Processo de download + filtro concluído. Consolidando Parquets...")
  
  # -------- Etapa 2: consolidar Parquets em um único RDS --------
  
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
  dt[, PA_UFMUN  := sprintf("%06s", as.character(PA_UFMUN))]
  dt[, PA_MUNPCN := sprintf("%06s", as.character(PA_MUNPCN))]
  dt[, PA_PROC_ID := as.character(PA_PROC_ID)]
  dt[, PA_SEXO    := as.character(PA_SEXO)]
  
  dt[, PA_IDADE  := as.integer(PA_IDADE)]
  dt[, PA_QTDPRO := as.numeric(PA_QTDPRO)]
  dt[, PA_QTDAPR := as.numeric(PA_QTDAPR)]
  
  # ---- competência AAAAMM -> ano_cmp / mes_cmp ----
  dt[, PA_CMP := as.character(PA_CMP)]
  dt[
    nchar(PA_CMP) == 6 & grepl("^[0-9]+$", PA_CMP),
    `:=`(
      ano_cmp = as.integer(substr(PA_CMP, 1, 4)),
      mes_cmp = as.integer(substr(PA_CMP, 5, 6))
    )
  ]
  if (!"ano_cmp" %in% names(dt)) dt[, ano_cmp := NA_integer_]
  if (!"mes_cmp" %in% names(dt)) dt[, mes_cmp := NA_integer_]
  
  # ---- filtro opcional por ano de competência ----
  if (!is.null(ano_cmp)) {
    ano_cmp_sel <- as.integer(ano_cmp)
    dt <- dt[ano_cmp == ano_cmp_sel]
  }
  
  
  
  # ---- categoria a partir da lista de códigos ----
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
  dt_final <- dt[
    ,
    .(
      total_qtdpro = sum(PA_QTDPRO, na.rm = TRUE),
      total_qtdapr = sum(PA_QTDAPR, na.rm = TRUE)
    ),
    by = .(
      categoria,
      PA_UFMUN,
      PA_CMP,
      PA_PROC_ID,
      nome_procedimento,
      PA_SEXO,
      PA_MUNPCN,
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
# - Baixa por mês (UF/ano/mês)
# - Filtra por códigos de interesse (PROC_REA por padrão)
# - Salva chunks Parquet (UF-ano-mês)
# - Consolida em data-raw/sih_rd_ezt_completo.rds
# - Cria  data/sih_rd_ezt_resumo.rds
# ================================================================

path_sih_rd_chunk <- function(...) path_data_raw("sih_rd_ezt_chunks_sih_rd", ...)

# ------------------------------------------------
# Parâmetros SIH-RD (EZT)
# ------------------------------------------------

# No SIH-RD, o procedimento realizado costuma estar em PROC_REA
coluna_procedimento_sih_rd <- "PROC_REA"

# Por padrão, vamos reaproveitar os códigos de "tratamento" já definidos acima.
# Se você tiver uma lista específica para EZT no SIH, pode trocar aqui.
codigos_ezt_sih_rd <- codigos_tratamento

# ------------------------------------------------
# Função principal: baixar + filtrar + chunk parquet (SIH-RD)
# ------------------------------------------------

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
  
  # harmoniza nome do campo de procedimento para merge
  dt[, PROC_ID := get(coluna_proc)]
  
  dt <- merge(
    dt,
    sigtab_dt,
    by = "PROC_ID",
    all.x = TRUE
  )
  
  # ---- agregação (AIH é 1 linha na RD, mas mantemos robusto) ----
  # n_internacoes: se N_AIH existir, conta uniqueN; senão, usa .N (uma linha ~ uma internação)
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
# - Sem ano/mês na base final (filtra no pré-processamento)
# - Duas linhas por geo_id: care/res
# - Métricas: total_all e total_25_64 (25–64)
# - Já faz merge com regional_sus_map
# - Anexa SIH-RD (EZT tipo 3) como categoria separada
# Gera: data/sus_proc_resumo.rds
# ------------------------------------------------

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
  reg <- readRDS(input_reg)
  data.table::setDT(reg)
  
  # Garantir chaves
  if (!"geo_id" %in% names(reg)) stop("regional_sus_map: coluna 'geo_id' ausente.", call. = FALSE)
  reg[, geo_id := sprintf("%06s", as.character(geo_id))]
  
  # Manter apenas o que o app filtra hoje (nomes exatamente iguais ao dataset atual)
  keep_reg <- c("geo_id", "UF", "Macrorregiao de Saude", "Regiao de Saude", "Municipio")
  keep_reg <- keep_reg[keep_reg %in% names(reg)]
  reg_small <- reg[, ..keep_reg]
  
  # -----------------------------
  # 2) SIA (aprovado) -> care/res + total_all / total_25_64
  # -----------------------------
  sia <- readRDS(input_sia)
  data.table::setDT(sia)
  
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
  faixa_25_64 <- c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")
  
  # Faixas 25–64 (como strings já padronizadas no build_sia_cc_resumo)
  faixa_25_64 <- c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")
  
  # care (PA_UFMUN)
  sia_care <- sia[
    ,
    .(
      total_all   = sum(total_qtdapr, na.rm = TRUE),
      total_25_64 = sum(data.table::fifelse(faixa_idade %in% faixa_25_64, total_qtdapr, 0), na.rm = TRUE)
    ),
    by = .(categoria, PA_PROC_ID, nome_procedimento, geo_id = PA_UFMUN)
  ]
  sia_care[, `:=`(geo_ref = "care", sistema = "SIA")]
  
  # res (PA_MUNPCN)
  sia_res <- sia[
    ,
    .(
      total_all   = sum(total_qtdapr, na.rm = TRUE),
      total_25_64 = sum(data.table::fifelse(faixa_idade %in% faixa_25_64, total_qtdapr, 0), na.rm = TRUE)
    ),
    by = .(categoria, PA_PROC_ID, nome_procedimento, geo_id = PA_MUNPCN)
  ]
  sia_res[, `:=`(geo_ref = "res", sistema = "SIA")]
  
  sia_out <- data.table::rbindlist(list(sia_care, sia_res), use.names = TRUE, fill = TRUE)
  sia_out[, geo_id := sprintf("%06s", as.character(geo_id))]
  sia_out[, PA_PROC_ID := as.character(PA_PROC_ID)]
  sia_out[, nome_procedimento := as.character(nome_procedimento)]
  
  # Merge regional (já no data-raw)
  sia_out <- merge(sia_out, reg_small, by = "geo_id", all.x = TRUE)
  
  
  # -----------------------------
  # 3) SIH-RD (EZT tipo 3) -> care/res + total_all / total_25_64
  #    Entra como categoria = "tratamento"
  #    Mantém PA_PROC_ID e nome_procedimento (a partir de PROC_ID)
  # -----------------------------
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
    
    # care (MUNIC_MOV)
    sih_care <- sih[
      ,
      .(
        total_all   = sum(n_internacoes, na.rm = TRUE),
        total_25_64 = sum(data.table::fifelse(faixa_idade %in% faixa_25_64, n_internacoes, 0), na.rm = TRUE)
      ),
      by = .(PA_PROC_ID = PROC_ID, nome_procedimento, geo_id = MUNIC_MOV)
    ]
    sih_care[, `:=`(geo_ref = "care", sistema = "SIH", categoria = "tratamento")]
    
    # res (MUNIC_RES)
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
  out <- data.table::rbindlist(list(sia_out, sih_out), use.names = TRUE, fill = TRUE)
  
  # Ordem de colunas (deixa amigável)
  col_order <- c(
    "sistema","categoria","PA_PROC_ID","nome_procedimento",
    "geo_ref","geo_id",
    "UF","Macrorregiao de Saude","Regiao de Saude","Municipio",
    "total_all","total_25_64"
  )
  col_order <- c(col_order, setdiff(names(out), col_order))
  data.table::setcolorder(out, col_order)
  
  saveRDS(out, output_rds)
  message(">> sus_proc_resumo salvo em: ", output_rds, " (", nrow(out), " linhas) — ano_ref=", ano_ref)
  invisible(out)
}


