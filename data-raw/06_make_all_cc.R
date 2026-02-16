# ===========================================================
# Shiny-CC — 06_make_all_cc.R
# Orquestrador do ETL e checagens (versão .rds + data-raw/)
# ===========================================================

run_file <- function(path, after = NULL) {
  message(">> Rodando: ", path)
  tryCatch({
    source(path, echo = TRUE, max.deparse.length = Inf)
    if (!is.null(after)) after()
    message("✔ Concluído: ", path)
  }, error = function(e) {
    message("✖ Falhou: ", path)
    stop(e)
  })
}

# 1) Preparo Globocan/WPP (gera df_cc_* .rds)
run_file("data-raw/01_prepare_cc.R")

# 2) Preparo Brasil (IBGE + ANS + regionalização)
#    gera pop_municipio_* .rds
run_file("data-raw/02_prepare_BR.R", after = function() run_prepare_BR())

# 3) Produção SUS (SIA-PA) — download + resumo
if (file.exists("data-raw/03_prepare_SUS.R")) {
  run_file(
    "data-raw/03_prepare_SUS.R",
    after = function() {
    #run_prepare_SUS()        # baixa + filtra + consolida
    #build_sia_cc_resumo()    # gera a base enxuta para o app
    #run_prepare_SIH_RD_EZT()
    #build_sih_rd_ezt_resumo()
      build_sus_proc_resumo(ano_ref = 2024L)  
      
    }
  )
}

# 4) Checagens e relatórios (usa todas as bases .rds disponíveis)
run_file("data-raw/04_checks_cc.R")

# 5) Dicionários e dimensões (df_dim_*.rds)
run_file("data-raw/05_glossary_cc.R")

# 6) Resumo final ----------------------------------------------------------
message("\n====== RESUMO ======")

if (file.exists("data/df_cc_completo.rds")) {
  df_cc_completo <- readRDS("data/df_cc_completo.rds")
  cat("df_cc_completo:", nrow(df_cc_completo), "linhas\n")
}

if (file.exists("data/df_cc_taxas.rds")) {
  df_cc_taxas <- readRDS("data/df_cc_taxas.rds")
  cat("df_cc_taxas:    ", nrow(df_cc_taxas), "linhas\n")
}

if (file.exists("data/df_dim_country.rds")) {
  df_dim_country <- readRDS("data/df_dim_country.rds")
  cat("Países (df_dim_country):     ", nrow(df_dim_country), "\n")
}

if (file.exists("data/df_dim_age.rds")) {
  df_dim_age <- readRDS("data/df_dim_age.rds")
  cat("Faixas etárias (df_dim_age): ", nrow(df_dim_age), "\n")
}

message("====================\n")
