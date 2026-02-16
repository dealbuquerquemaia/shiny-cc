# ================================================================
# 02_prepare_BR.R
# ETL completo da base Brasil (município, faixas etárias, SUS)
#
# Gera em data/:
#   pop_municipio_faixas.rds
#   ans_municipio_faixas.rds
#   regional_sus_map.rds
#   pop_municipio_faixas_total_sus.rds
#   pop_municipio_regional.rds
#
# Para rodar:
#   source("data-raw/02_prepare_BR.R")
#   run_prepare_BR()
# ================================================================

suppressWarnings({
  if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
  if (!requireNamespace("here", quietly = TRUE))        install.packages("here")
})

library(data.table)
library(here)

path_data_raw <- function(...) here::here("data-raw", ...)
path_data     <- function(...) here::here("data", ...)

# ================================================================
# 1) IBGE - População feminina por FAIXAS etárias (município)
# Arquivo limpo com colunas:
# "Município", "De 0 a 4 anos", ..., "De 80 anos ou mais"
# Saída: data/pop_municipio_faixas.rds
# ================================================================

.IBGE_FILE <- "IBGE_pop.csv"   # ajuste se necessário
.IBGE_SEP  <- ";"              # ";" ou ","
.IBGE_ENC  <- "Latin-1"        # ou "UTF-8"

parse_faixa_bounds <- function(lbl) {
  lbl <- trimws(as.character(lbl))
  # "De 0 a 4 anos"
  if (grepl("^De\\s*\\d+\\s*a\\s*\\d+\\s+anos$", lbl, ignore.case = TRUE)) {
    nums <- regmatches(lbl, gregexpr("\\d+", lbl))[[1]]
    return(list(from = as.integer(nums[1]), to = as.integer(nums[2])))
  }
  # "De 80 anos ou mais"
  if (grepl("^De\\s*80\\s+anos\\s+ou\\s+mais$", lbl, ignore.case = TRUE)) {
    return(list(from = 80L, to = 200L))
  }
  stop(sprintf("Rótulo de faixa não reconhecido: '%s'", lbl))
}

run_etl_ibge <- function() {
  message(">> IBGE: lendo base por FAIXAS ...")
  wide <- fread(
    file        = path_data_raw(.IBGE_FILE),
    sep         = .IBGE_SEP,
    encoding    = .IBGE_ENC,
    fill        = TRUE,
    check.names = FALSE,
    showProgress = FALSE
  )
  setDT(wide)
  
  # força a 1ª coluna a se chamar "Município" (sem mexer nas demais)
  setnames(wide, 1, "Município")
  
  # remove linhas vazias
  wide <- wide[!is.na(`Município`) & nzchar(trimws(`Município`))]
  
  # derrete faixas
  faixa_cols <- setdiff(names(wide), "Município")
  long <- melt(
    wide,
    id.vars       = "Município",
    measure.vars  = faixa_cols,
    variable.name = "faixa",
    value.name    = "pop",
    variable.factor = FALSE
  )
  
  # populações numéricas
  long[, pop := as.numeric(gsub("\\.", "", as.character(pop)))]
  long[is.na(pop), pop := 0]
  
  # separa código (6 dígitos) e nome (mantém acentos)
  long[, geo_id   := sub("^\\s*([0-9]{6}).*$", "\\1", `Município`)]
  long[, geo_name := trimws(sub("^\\s*[0-9]{6}\\s*", "", `Município`))]
  
  # calcula from/to da faixa
  bounds <- lapply(long$faixa, parse_faixa_bounds)
  long[, from := vapply(bounds, function(x) x$from, integer(1))]
  long[, to   := vapply(bounds, function(x) x$to,   integer(1))]
  
  # agrega (por segurança)
  out <- long[, .(pop = sum(pop, na.rm = TRUE)),
              by = .(geo_id, geo_name, faixa, from, to)]
  out[, level := "municipio"]
  
  if (!dir.exists(path_data())) dir.create(path_data(), recursive = TRUE)
  saveRDS(out, path_data("pop_municipio_faixas.rds"))
  message(">> OK: data/pop_municipio_faixas.rds")
  
  invisible(out)
}

# ================================================================
# 2) ANS - Beneficiárias (Feminino) por município e faixa etária
# Layout:
# Município | Até 1 ano | 1 a 4 anos | ... | 80 anos ou mais
# Saída: data/ans_municipio_faixas.rds
# ================================================================

run_etl_ans <- function() {
  message(">> ANS: lendo base padronizada ...")
  
  ans <- fread(
    file        = path_data_raw("ANS_pop.csv"),
    sep         = ";",
    encoding    = "Latin-1",
    fill        = TRUE,
    check.names = FALSE
  )
  
  # 1ª coluna = "Município"
  setnames(ans, 1, "Município")
  
  # remove linhas vazias
  ans <- ans[!is.na(`Município`) & nzchar(trimws(`Município`))]
  
  # derrete faixas
  faixa_cols <- setdiff(names(ans), "Município")
  long <- melt(
    ans,
    id.vars       = "Município",
    measure.vars  = faixa_cols,
    variable.name = "faixa",
    value.name    = "beneficiarios",
    variable.factor = FALSE
  )
  
  # beneficiárias numéricas
  long[, beneficiarios := as.numeric(gsub("\\.", "", as.character(beneficiarios)))]
  long[is.na(beneficiarios), beneficiarios := 0]
  
  # separa geo_id e geo_name
  long[, geo_id   := sub("^\\s*([0-9]{6}).*$", "\\1", `Município`)]
  long[, geo_name := trimws(sub("^\\s*[0-9]{6}\\s*", "", `Município`))]
  
  # padroniza rótulos de faixa (evita NA silencioso no merge)
  long[, faixa := trimws(as.character(faixa))]
  long[, faixa := gsub("\\s+", " ", faixa)]
  
  # mapa de faixas para from/to
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
  
  # falha se ficou faixa sem mapeamento
  if (long[is.na(from) | is.na(to), .N] > 0L) {
    stop("ANS_pop.csv: existem faixas etárias sem mapeamento: ",
         paste(unique(long[is.na(from) | is.na(to), faixa]), collapse = " | "))
  }
  
  
  # combinar 0–0 + 1–4 -> 0–4
  long[, faixa_comb := faixa]
  long[faixa %in% c("Até 1 ano", "1 a 4 anos"), faixa_comb := "De 0 a 4 anos"]
  long[faixa_comb == "De 0 a 4 anos", `:=`(from = 0L, to = 4L)]
  
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
# 3) Regionalização SUS — ETL
# Entrada: data-raw/regional_sus.csv
# Saída: data/regional_sus_map.rds
# ================================================================

.REG_FILE <- "regional_sus.csv"

run_etl_regional <- function() {
  message(">> Regionalização: lendo CSV ...")
  reg <- fread(
    file        = path_data_raw(.REG_FILE),
    sep         = ",",
    encoding    = "UTF-8",
    fill        = TRUE,
    check.names = FALSE,
    showProgress = FALSE
  )
  setDT(reg)
  
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
  
  reg[, `Populacao Estimada IBGE 2022` := NULL]
  
  setnames(reg, old = "Codigo Municipio", new = "geo_id")
  reg[, geo_id := sprintf("%06s", trimws(as.character(geo_id)))]
  
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
# 4) Montagem: população total + SUS-dependente + regionalização
# ================================================================

build_population_base <- function(ibge, ans = NULL) {
  message(">> Construindo base populacional completa (todas as faixas) ...")
  
  if (is.null(ans)) {
    ibge[, `:=`(pop_total = pop, pop_sus = pop)]
    return(ibge[, .(geo_id, geo_name, faixa, from, to, pop_total, pop_sus)])
  }
  
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

build_population_with_regions <- function(pop_base, reg) {
  message(">> Construindo base populacional + regionalização ...")
  
  merged <- merge(pop_base, reg, by = "geo_id", all.x = TRUE)
  
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
# 5) Função mestre
# ================================================================

run_prepare_BR <- function() {
  message("==== ETL Brasil — início ====")
  
  ibge <- run_etl_ibge()
  ans  <- run_etl_ans()
  reg  <- run_etl_regional()
  
  pop_base <- build_population_base(ibge, ans)
  saveRDS(pop_base, path_data("pop_municipio_faixas_total_sus.rds"))
  message(">> OK: data/pop_municipio_faixas_total_sus.rds gerado.")
  
  build_population_with_regions(pop_base, reg)
  
  message("==== ETL Brasil — concluído ====")
}
