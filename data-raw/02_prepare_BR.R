# ================================================================
# 02_prepare_BR.R — ETL completo da base Brasil (Município/UF/Macro/Região)
# ================================================================
#
# ESCOPO
#   ETL específico do Brasil: cruza população feminina (IBGE) com
#   beneficiárias de planos privados (ANS) para derivar a população
#   "SUS-dependente" por município × faixa etária, e anexa a hierarquia
#   de regionalização do SUS (UF → Macrorregião de Saúde → Região de
#   Saúde → Município).
#
#   Esta é a base que alimenta o seletor "Brazil: total population
#   (IBGE) vs SUS-dependent (IBGE − ANS)" da sidebar e os filtros
#   geográficos em cascata de todos os módulos só-Brasil.
#
# ENTRADAS (em data-raw/)
#   - IBGE_pop.csv       — pop. feminina por município × faixa etária
#                          (formato wide, 1 linha = 1 município, ; Latin-1)
#   - ANS_pop.csv        — beneficiárias (Feminino) de planos por
#                          município × faixa etária (formato wide, ;)
#   - regional_sus.csv   — mapeamento município → região de saúde →
#                          macro → UF → região do país (UTF-8, vírgula)
#
# SAÍDAS (em data/)
#   - pop_municipio_faixas.rds            (só IBGE, long)
#   - ans_municipio_faixas.rds            (só ANS, long, com from/to)
#   - regional_sus_map.rds                (mapa hierárquico SUS)
#   - pop_municipio_faixas_total_sus.rds  (IBGE + pop_total + pop_sus)
#   - pop_municipio_regional.rds          (acima + regionalização) ← BASE CENTRAL
#
# COMO RODAR
#   source("data-raw/02_prepare_BR.R")
#   run_prepare_BR()
#
# OBSERVAÇÕES
#   - As faixas IBGE e ANS NÃO casam exatamente: IBGE tem "De 0 a 4
#     anos", ANS separa "Até 1 ano" + "1 a 4 anos". O bloco ANS combina
#     as duas para obter "De 0 a 4 anos" e fechar o merge por (from,to).
#   - "pop_sus" é definida como pmax(pop − beneficiárias, 0). É uma
#     PROXY para o público-alvo do SUS — supõe que cada beneficiária
#     ANS é uma usuária do privado (não capta uso simultâneo).
#   - Os dimensionais `df_dim_*` e o `df_cc_completo` (GLOBOCAN) são
#     gerados em outro script (`01_prepare_cc.R`); este aqui só cuida
#     da camada Brasil.
# ================================================================

# ----------------------------------------------------------------
# Setup: dependências mínimas e helpers de caminho
# ----------------------------------------------------------------
suppressWarnings({
  if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
  if (!requireNamespace("here", quietly = TRUE))        install.packages("here")
})

library(data.table)
library(here)

# Helpers de caminho — toleram a sessão estar em qualquer subpasta do projeto
path_data_raw <- function(...) here::here("data-raw", ...)
path_data     <- function(...) here::here("data", ...)

# ================================================================
# 1) IBGE — População feminina por FAIXAS etárias × Município
# ----------------------------------------------------------------
# Layout esperado (formato wide):
#   "Município" | "De 0 a 4 anos" | ... | "De 80 anos ou mais"
# A 1ª coluna traz "código municipio + nome" (ex.: "350010 Adamantina").
# Saída: data/pop_municipio_faixas.rds (formato long, com geo_id, faixa, from, to, pop, level)
# ================================================================

# Constantes de leitura — ajustar caso o arquivo bruto mude formato/encoding.
.IBGE_FILE <- "IBGE_pop.csv"   # ajuste se necessário
.IBGE_SEP  <- ";"              # ";" ou ","
.IBGE_ENC  <- "Latin-1"        # ou "UTF-8"

# parse_faixa_bounds(lbl)
# -------------------------------------------
# Converte um rótulo de faixa do IBGE (ex.: "De 0 a 4 anos", "De 80
# anos ou mais") em (from, to). Faz parsing por regex; falha com erro
# claro se o rótulo não bate em nenhum dos dois padrões — funciona
# como assert defensivo contra mudança no layout do CSV.
#   - faixa fechada → from/to literais
#   - "80 ou mais"  → from=80, to=200 (sentinela para "sem teto")
parse_faixa_bounds <- function(lbl) {
  lbl <- trimws(as.character(lbl))
  # Caso 1: "De N a M anos"
  if (grepl("^De\\s*\\d+\\s*a\\s*\\d+\\s+anos$", lbl, ignore.case = TRUE)) {
    nums <- regmatches(lbl, gregexpr("\\d+", lbl))[[1]]
    return(list(from = as.integer(nums[1]), to = as.integer(nums[2])))
  }
  # Caso 2: "De 80 anos ou mais" — sem teto superior; usamos 200 como sentinela
  if (grepl("^De\\s*80\\s+anos\\s+ou\\s+mais$", lbl, ignore.case = TRUE)) {
    return(list(from = 80L, to = 200L))
  }
  stop(sprintf("Rótulo de faixa não reconhecido: '%s'", lbl))
}

# run_etl_ibge()
# -------------------------------------------
# Lê IBGE_pop.csv (wide), derrete em long, parsa código/nome do
# município e from/to de cada faixa. Saída: data.table long com 1
# linha por (município, faixa) e nível "municipio".
run_etl_ibge <- function() {
  message(">> IBGE: lendo base por FAIXAS ...")
  wide <- fread(
    file        = path_data_raw(.IBGE_FILE),
    sep         = .IBGE_SEP,
    encoding    = .IBGE_ENC,
    fill        = TRUE,
    check.names = FALSE,   # preserva acentos e espaços nos nomes ("De 0 a 4 anos")
    showProgress = FALSE
  )
  setDT(wide)

  # Força a 1ª coluna a se chamar "Município" — robustez contra header
  # com BOM, espaço inicial ou variação de capitalização. Demais colunas
  # ficam intactas (são os rótulos das faixas etárias).
  setnames(wide, 1, "Município")

  # Remove linhas vazias (podem aparecer rodapés/totais soltos no CSV)
  wide <- wide[!is.na(`Município`) & nzchar(trimws(`Município`))]

  # Derrete: 1 linha por (município × faixa)
  faixa_cols <- setdiff(names(wide), "Município")
  long <- melt(
    wide,
    id.vars       = "Município",
    measure.vars  = faixa_cols,
    variable.name = "faixa",
    value.name    = "pop",
    variable.factor = FALSE
  )

  # Limpa formato pt-BR ("12.345" → 12345); ausentes viram zero (não NA,
  # para evitar propagação de NA na soma agregada a jusante).
  long[, pop := as.numeric(gsub("\\.", "", as.character(pop)))]
  long[is.na(pop), pop := 0]

  # Separa código IBGE de 6 dígitos (geo_id) e nome do município (geo_name).
  # Mantém acentos no nome — não usar clean_names aqui.
  long[, geo_id   := sub("^\\s*([0-9]{6}).*$", "\\1", `Município`)]
  long[, geo_name := trimws(sub("^\\s*[0-9]{6}\\s*", "", `Município`))]

  # Anexa from/to derivados do rótulo da faixa (parse_faixa_bounds)
  bounds <- lapply(long$faixa, parse_faixa_bounds)
  long[, from := vapply(bounds, function(x) x$from, integer(1))]
  long[, to   := vapply(bounds, function(x) x$to,   integer(1))]

  # Agrega por (município, faixa) — defensivo: garante 1 linha por chave
  # mesmo se a planilha tiver duplicatas.
  out <- long[, .(pop = sum(pop, na.rm = TRUE)),
              by = .(geo_id, geo_name, faixa, from, to)]
  out[, level := "municipio"]

  if (!dir.exists(path_data())) dir.create(path_data(), recursive = TRUE)
  saveRDS(out, path_data("pop_municipio_faixas.rds"))
  message(">> OK: data/pop_municipio_faixas.rds")

  invisible(out)
}

# ================================================================
# 2) ANS — Beneficiárias (Feminino) por Município × Faixa etária
# ----------------------------------------------------------------
# Layout esperado:
#   "Município" | "Até 1 ano" | "1 a 4 anos" | ... | "80 anos ou mais"
# Diferenças em relação ao IBGE:
#   - 1ª faixa é separada em "Até 1 ano" + "1 a 4 anos" (2 colunas).
#     O bloco abaixo COMBINA as duas para "De 0 a 4 anos" e fechar
#     a chave (from=0, to=4) que casa com o IBGE.
#   - Rótulos sem o prefixo "De" — usamos um mapa explícito em vez de
#     reutilizar parse_faixa_bounds.
# Saída: data/ans_municipio_faixas.rds (long, com from/to alinhados ao IBGE)
# ================================================================

# run_etl_ans()
# -------------------------------------------
# Lê ANS_pop.csv, derrete, mapeia faixas em (from, to), combina
# 0–0 + 1–4 → 0–4, e salva long. Falha com mensagem clara se aparecer
# rótulo de faixa sem mapeamento (assert defensivo).
run_etl_ans <- function() {
  message(">> ANS: lendo base padronizada ...")
  
  ans <- fread(
    file        = path_data_raw("ANS_pop.csv"),
    sep         = ";",
    encoding    = "Latin-1",
    fill        = TRUE,
    check.names = FALSE
  )

  # 1ª coluna = "Município" (mesmo padrão do bloco IBGE)
  setnames(ans, 1, "Município")

  # Limpa linhas vazias
  ans <- ans[!is.na(`Município`) & nzchar(trimws(`Município`))]

  # Derrete (município × faixa)
  faixa_cols <- setdiff(names(ans), "Município")
  long <- melt(
    ans,
    id.vars       = "Município",
    measure.vars  = faixa_cols,
    variable.name = "faixa",
    value.name    = "beneficiarios",
    variable.factor = FALSE
  )

  # Limpa formato pt-BR; ausentes → 0
  long[, beneficiarios := as.numeric(gsub("\\.", "", as.character(beneficiarios)))]
  long[is.na(beneficiarios), beneficiarios := 0]

  # Extrai geo_id (6 dígitos IBGE) e geo_name
  long[, geo_id   := sub("^\\s*([0-9]{6}).*$", "\\1", `Município`)]
  long[, geo_name := trimws(sub("^\\s*[0-9]{6}\\s*", "", `Município`))]

  # Normaliza espaços nos rótulos das faixas — evita NA silencioso no
  # merge por causa de espaço duplo ou espaço bizarro do CSV.
  long[, faixa := trimws(as.character(faixa))]
  long[, faixa := gsub("\\s+", " ", faixa)]

  # Mapa estático faixa-ANS → (from, to). Diferença vs IBGE:
  #   "Até 1 ano"  → from=0, to=0  (será combinada com 1–4 abaixo)
  #   "1 a 4 anos" → from=1, to=4  (idem)
  # Demais faixas alinham 1:1 com o grid IBGE; "80 anos ou mais"
  # usa to=200 como sentinela (mesmo padrão do bloco IBGE).
  mapa_faixas <- data.table(
    faixa = c(
      "Até 1 ano", "1 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos",
      "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos",
      "45 a 49 anos", "50 a 54 anos", "55 a 59 anos", "60 a 64 anos", "65 a 69 anos",
      "70 a 74 anos", "75 a 79 anos", "80 anos ou mais"
    ),
    from = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80),
    to   = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 200)
  )

  long <- merge(long, mapa_faixas, by = "faixa", all.x = TRUE)

  # Assert: nenhuma faixa pode ter ficado sem mapeamento. Lista as
  # faixas problemáticas na mensagem para facilitar correção do mapa.
  if (long[is.na(from) | is.na(to), .N] > 0L) {
    stop("ANS_pop.csv: existem faixas etárias sem mapeamento: ",
         paste(unique(long[is.na(from) | is.na(to), faixa]), collapse = " | "))
  }


  # Combina 0–0 ("Até 1 ano") + 1–4 ("1 a 4 anos") → "De 0 a 4 anos"
  # para alinhar com o grid IBGE (que tem só "De 0 a 4 anos").
  # Sem isso, o merge IBGE × ANS perderia a 1ª faixa.
  long[, faixa_comb := faixa]
  long[faixa %in% c("Até 1 ano", "1 a 4 anos"), faixa_comb := "De 0 a 4 anos"]
  long[faixa_comb == "De 0 a 4 anos", `:=`(from = 0L, to = 4L)]

  # Agrega beneficiárias na faixa combinada
  agg <- long[, .(beneficiarios = sum(beneficiarios, na.rm = TRUE)),
              by = .(geo_id, geo_name, faixa_comb, from, to)]
  setnames(agg, "faixa_comb", "faixa")
  agg[, level := "municipio"]

  if (!dir.exists(path_data())) dir.create(path_data(), recursive = TRUE)
  saveRDS(agg[, .(geo_id, geo_name, faixa, from, to, beneficiarios, level)],
          path_data("ans_municipio_faixas.rds"))

  message(">> OK: data/ans_municipio_faixas.rds gerado.")
  invisible(agg)
}

# ================================================================
# 3) Regionalização SUS — Mapa hierárquico (UF → Macro → Região → Município)
# ----------------------------------------------------------------
# Entrada: data-raw/regional_sus.csv (UTF-8, separador vírgula)
#   Originado da tabela oficial de regionalização do MS/SES, com
#   colunas em PT sem acentos e separação por vírgula.
# Saída: data/regional_sus_map.rds — usado em todo módulo só-Brasil
#         para a cascata UF → Macro → Região → Município.
# ================================================================

.REG_FILE <- "regional_sus.csv"

# run_etl_regional()
# -------------------------------------------
# Lê o CSV oficial, valida schema com `must_have_cols`-style assert
# inline, descarta a coluna de população (versão antiga), e renomeia
# para o esquema usado no app (snake_case + nomes consistentes
# `regiao_pais_*`/`uf_*`/`macro_*`/`regiao_*`/`mun_*`).
run_etl_regional <- function() {
  message(">> Regionalização: lendo CSV ...")
  reg <- fread(
    file        = path_data_raw(.REG_FILE),
    sep         = ",",
    encoding    = "UTF-8",
    fill        = TRUE,
    check.names = FALSE,   # preserva nomes oficiais com espaços ("Codigo UF" etc.)
    showProgress = FALSE
  )
  setDT(reg)

  # Schema esperado — usado como assert de "qual layout exato veio".
  # Inclui a coluna de população (que a gente descarta logo abaixo).
  expected <- c(
    "Codigo Regiao do Pais","Regiao do Pais",
    "Codigo UF","UF",
    "Codigo Macrorregiao de Saude","Macrorregiao de Saude",
    "Codigo Regiao de Saude","Regiao de Saude",
    "Codigo Municipio","Municipio",
    "Populacao Estimada IBGE 2022"
  )
  missing <- setdiff(expected, names(reg))
  if (length(missing)) {
    stop("Faltam colunas no regional_sus.csv: ", paste(missing, collapse = ", "))
  }

  # Descarta a coluna de população — a fonte canônica de pop é o IBGE
  # (run_etl_ibge), que tem desagregação por faixa etária. Aqui a coluna
  # serviria só ao total, e gerar 2 fontes de população é receita de drift.
  reg[, `Populacao Estimada IBGE 2022` := NULL]

  # Padroniza chave do município para 6 dígitos (zfill) — alinhamento
  # com o `geo_id` produzido por IBGE/ANS (mesmo formato).
  setnames(reg, old = "Codigo Municipio", new = "geo_id")
  reg[, geo_id := sprintf("%06s", trimws(as.character(geo_id)))]

  # Renomeia para o esquema usado no app: prefixos consistentes
  # `regiao_pais_*` / `uf_*` / `macro_*` / `regiao_*` / `mun_*`. Isto
  # facilita criar filtros em cascata e ler o código em todos os módulos.
  reg[, `:=`(
    regiao_pais_codigo = `Codigo Regiao do Pais`,
    regiao_pais_nome   = `Regiao do Pais`,
    uf_codigo          = `Codigo UF`,
    uf_sigla           = `UF`,
    macro_codigo       = `Codigo Macrorregiao de Saude`,
    macro_nome         = `Macrorregiao de Saude`,
    regiao_codigo      = `Codigo Regiao de Saude`,
    regiao_nome        = `Regiao de Saude`,
    mun_code6          = geo_id,
    mun_nome           = `Municipio`
  )]

  setorder(reg, geo_id)

  if (!dir.exists(path_data())) dir.create(path_data(), recursive = TRUE)
  saveRDS(reg, file = path_data("regional_sus_map.rds"))
  message(">> OK: data/regional_sus_map.rds gerado.")

  invisible(reg)
}

# ================================================================
# 4) Montagem — pop_total + pop_sus (+ regionalização)
# ----------------------------------------------------------------
# Combina IBGE (denominador total) com ANS (beneficiárias do privado)
# para derivar a proxy SUS-dependente (`pop_sus = max(pop_ibge − ANS, 0)`),
# e em seguida anexa a hierarquia SUS para gerar a base central
# `pop_municipio_regional`.
# ================================================================

# build_population_base(ibge, ans = NULL)
# -------------------------------------------
# Junta IBGE × ANS por (geo_id, from, to). Devolve data.table com
# `pop_total` (IBGE puro) e `pop_sus` (IBGE − ANS, clamp em 0).
#
# Argumentos:
#   ibge — saída de run_etl_ibge() (long)
#   ans  — saída de run_etl_ans() ou NULL. Se NULL, pop_sus = pop_total
#          (degenerated: roda como se ninguém tivesse plano privado).
#
# Detalhes:
#   - clamp em 0 via pmax(): evita pop_sus negativa em municípios com
#     mais beneficiárias declaradas do que população (raro, mas ocorre
#     quando há erro de digitação ou mismatch temporal entre IBGE e ANS).
#   - all.x=TRUE: município sem registro ANS não some — assume 0
#     beneficiárias (= pop_sus == pop_total).
build_population_base <- function(ibge, ans = NULL) {
  message(">> Construindo base populacional completa (todas as faixas) ...")

  # Caso degenerado: sem ANS, pop_sus colapsa em pop_total
  if (is.null(ans)) {
    ibge[, `:=`(pop_total = pop, pop_sus = pop)]
    return(ibge[, .(geo_id, geo_name, faixa, from, to, pop_total, pop_sus)])
  }

  # Merge por (geo_id, from, to) — depende do alinhamento de faixas
  # feito no bloco ANS (combinação 0–0 + 1–4 → 0–4).
  base <- merge(
    ibge[, .(geo_id, geo_name, faixa, from, to, pop)],
    ans[,  .(geo_id, from, to, beneficiarios)],
    by = c("geo_id", "from", "to"),
    all.x = TRUE
  )
  base[is.na(beneficiarios), beneficiarios := 0]

  base[, pop_total := pop]
  base[, pop_sus   := pmax(pop - beneficiarios, 0)]

  out <- base[, .(geo_id, geo_name, faixa, from, to, pop_total, pop_sus)]
  message(">> OK: base completa construída.")
  out
}

# build_population_with_regions(pop_base, reg)
# -------------------------------------------
# Anexa a hierarquia SUS (`regional_sus_map`) à base populacional.
# Resultado: data.table 1 linha por (município × faixa) com colunas de
# pop + colunas de regionalização. Esta é a `pop_municipio_regional`
# que vira `input_global()`-friendly e alimenta engine/módulos só-Brasil.
#
# Detalhes:
#   - `setcolorder` aplicado com filtro `%in% names(merged)`: caso
#     alguma coluna esperada não exista (mudança de schema), ela é
#     simplesmente omitida em vez de quebrar — útil em ambientes de
#     desenvolvimento, mas pode mascarar bug em produção.
build_population_with_regions <- function(pop_base, reg) {
  message(">> Construindo base populacional + regionalização ...")

  merged <- merge(pop_base, reg, by = "geo_id", all.x = TRUE)

  # Ordena colunas em uma sequência amigável (geo + pop + hierarquia
  # SUS). O filtro `%in%` torna a chamada idempotente caso o schema mude.
  setcolorder(merged, c(
    "geo_id","geo_name",
    "faixa","from","to","pop_total","pop_sus",
    "uf_sigla","uf_codigo",
    "regiao_pais_nome","regiao_pais_codigo",
    "macro_nome","macro_codigo",
    "regiao_nome","regiao_codigo",
    "mun_nome","mun_code6"
  )[c(
    "geo_id","geo_name","faixa","from","to","pop_total","pop_sus",
    "uf_sigla","uf_codigo",
    "regiao_pais_nome","regiao_pais_codigo",
    "macro_nome","macro_codigo",
    "regiao_nome","regiao_codigo",
    "mun_nome","mun_code6"
  ) %in% names(merged)])


  saveRDS(merged, path_data("pop_municipio_regional.rds"))
  message(">> OK: data/pop_municipio_regional.rds gerado.")
  invisible(merged)
}

# ================================================================
# 5) Função mestre — orquestra os 4 blocos acima
# ----------------------------------------------------------------
# Sequência: IBGE → ANS → Regional → pop_base (IBGE+ANS) → +regional.
# Salva também `pop_municipio_faixas_total_sus.rds` como subproduto
# (versão sem regionalização — usada pouco no app, mantida por
# compatibilidade e auditoria).
# ================================================================

# run_prepare_BR()
# -------------------------------------------
# Entry point único. Após rodar, todos os 5 .rds estão em data/.
# Re-executar é seguro: arquivos são sobrescritos, dir.create é
# idempotente, asserts param a execução cedo se o input mudou.
run_prepare_BR <- function() {
  message("==== ETL Brasil — início ====")

  # 1–3) ETLs primários (independentes entre si)
  ibge <- run_etl_ibge()
  ans  <- run_etl_ans()
  reg  <- run_etl_regional()

  # 4a) Combina IBGE × ANS → pop_total + pop_sus (sem hierarquia)
  pop_base <- build_population_base(ibge, ans)
  saveRDS(pop_base, path_data("pop_municipio_faixas_total_sus.rds"))
  message(">> OK: data/pop_municipio_faixas_total_sus.rds gerado.")

  # 4b) Anexa hierarquia SUS → BASE CENTRAL do app só-Brasil
  build_population_with_regions(pop_base, reg)

  message("==== ETL Brasil — concluído ====")
}
