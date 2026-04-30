# ===========================================================
# Shiny-CC — 06_make_all_cc.R
# -----------------------------------------------------------
# Orquestrador (master script) do pipeline de ETL + QA do app.
#
# Escopo: dispara, em sequência, os scripts standalone de
#   data-raw/ (01, 02, 03, 04, 05) que populam data/*.rds com
#   as bases consumidas pelo Shiny.
#
# Entrada: cwd no root do projeto (caminhos são relativos a
#   `data-raw/...`). Cada sub-script é responsável pelas suas
#   próprias entradas (ver README + cabeçalho de cada script).
#
# Saída: efeitos colaterais em data/ (e data-raw/ para
#   intermediários do SUS) + bloco de RESUMO no console.
#
# Observações:
#   - Re-execução é idempotente: cada sub-script grava .rds
#     com saveRDS (sobrescreve) e os de SUS pulam chunks já
#     baixados (file.exists → next).
#   - 03_prepare_SUS.R é OPCIONAL — se o arquivo não existir,
#     o bloco SUS é pulado silenciosamente (permite buildar
#     o app só com bases globais/Globocan). Os 4 demais são
#     obrigatórios.
#   - Run 03 está parcialmente comentado por padrão (apenas
#     build_sih_rd_ezt_resumo + build_sus_proc_resumo): os
#     downloads pesados (run_prepare_SUS / run_prepare_SIH_RD_EZT
#     / build_sia_cc_resumo) são tipicamente rodados uma vez
#     "manualmente" e não a cada make. Ajustar conforme a
#     necessidade do build.
# ===========================================================

# -----------------------------------------------------------
# Helper: executa um sub-script com tratamento de erro e
#   um callback opcional (`after`) para chamar funções
#   exportadas pelo script logo após o source.
# -----------------------------------------------------------
run_file <- function(path, after = NULL) {
  message(">> Rodando: ", path)
  tryCatch({
    # echo=TRUE imprime cada expressão executada (útil para
    # debug em CI/log offline); max.deparse.length=Inf evita
    # truncar comandos longos no console.
    source(path, echo = TRUE, max.deparse.length = Inf)
    if (!is.null(after)) after()
    message("✔ Concluído: ", path)
  }, error = function(e) {
    # Re-emite o erro para interromper o pipeline — falhas em
    # ETL upstream NÃO devem ser silenciadas (a ordem dos
    # scripts pressupõe que o anterior tenha completado).
    message("✖ Falhou: ", path)
    stop(e)
  })
}

# -----------------------------------------------------------
# 1) Preparo Globocan/WPP — gera df_cc_completo.rds + df_cc_taxas.rds
# -----------------------------------------------------------
# Script de top-level: o source já dispara os blocos 1–6.
# Não precisa de `after` (não há função mestre exportada).
run_file("data-raw/01_prepare_cc.R")

# -----------------------------------------------------------
# 2) Preparo Brasil — IBGE + ANS + regionalização SUS
# -----------------------------------------------------------
# Script só DEFINE funções; precisa do callback para chamar
# `run_prepare_BR()`, a função mestre que orquestra IBGE →
# ANS → Regional → pop_base → +regional e salva 5 .rds.
run_file("data-raw/02_prepare_BR.R", after = function() run_prepare_BR())

# -----------------------------------------------------------
# 3) Produção SUS (SIA-PA + SIH-RD) — OPCIONAL
# -----------------------------------------------------------
# Bloco opcional: pulado silenciosamente se o script não
# existir (permite buildar o app só com bases globais).
# Por padrão, dispara apenas os agregadores rápidos:
#   - build_sih_rd_ezt_resumo: agrega SIH/EZT já baixado
#   - build_sus_proc_resumo:    saída final consumida pelo app
# Os downloads via microdatasus (run_prepare_SUS / run_prepare_SIH_RD_EZT
# / build_sia_cc_resumo) ficam comentados por serem caros
# (~10–15 min) e tipicamente já populados em `data-raw/sia_cc_chunks_*/`.
# Descomentar se for build "do zero".
if (file.exists("data-raw/03_prepare_SUS.R")) {
  run_file(
    "data-raw/03_prepare_SUS.R",
    after = function() {
    # run_prepare_SUS()        # baixa + filtra + consolida SIA-PA (microdatasus)
    # build_sia_cc_resumo()    # agrega SIA → data/sia_cc_resumo.rds (intermediário)
    # run_prepare_SIH_RD_EZT() # baixa + filtra + consolida SIH-RD (microdatasus)
    build_sih_rd_ezt_resumo()              # agrega SIH → data/sih_rd_ezt_resumo.rds (intermediário)
    build_sus_proc_resumo(ano_ref = 2024L) # saída final → data/sus_proc_resumo.rds (consumida pelo app)
    }
  )
}

# -----------------------------------------------------------
# 4) Checagens e relatórios (QA) — usa todas as bases disponíveis
# -----------------------------------------------------------
# Script standalone: lê os .rds gerados em 1–3 e produz CSVs
# de anomalias + sumário em reports/. Bloco BR é tolerante
# a .rds ausentes (permite rodar em ambientes parciais).
run_file("data-raw/04_checks_cc.R")

# -----------------------------------------------------------
# 5) Dicionários e dimensões — gera df_dim_*.rds
# -----------------------------------------------------------
# Roda APÓS 01 (precisa de df_cc_completo.rds) e idealmente
# APÓS 04 (para garantir QA antes de empacotar dimensionais).
run_file("data-raw/05_glossary_cc.R")

# -----------------------------------------------------------
# 6) Resumo final no console
# -----------------------------------------------------------
# Loga no console o nº de linhas dos .rds principais para
# inspeção visual rápida pós-build. Tolerante a ausência
# (silencioso se algum .rds não existir).
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
