# ===========================================================
# Shiny-CC — 05_glossary_cc.R
# -----------------------------------------------------------
# Gera tabelas de dimensão ("dicionários") a partir das bases
# já consolidadas por `01_prepare_cc.R`. Cada dimensão é a
# lista canônica de valores únicos para uma chave estrangeira
# usada nos módulos do app.
#
# ESCOPO:
# - Roda APÓS `01_prepare_cc.R` (precisa de df_cc_completo e
#   df_cc_taxas já gravados em `data/`).
# - Standalone: NÃO dá `source()` no app, NÃO modifica
#   `df_cc_completo`/`df_cc_taxas`. Lê e gera os 5 .rds.
#
# ENTRADA (em `data/`):
# - df_cc_completo.rds — base GLOBOCAN com população,
#   incidência e mortalidade (gerada por 01).
# - df_cc_taxas.rds   — taxas agregadas (carregada apenas
#   por consistência; este script não a usa para gerar
#   dimensões — fica pronta caso 06 precise).
#
# SAÍDA (5 .rds em `data/`, todos compressão `xz`):
# - df_dim_country.rds — dim países: (population_code, population_name).
# - df_dim_age.rds     — dim faixas etárias: (age, age_code) com
#   ORDEM CANÔNICA do GLOBOCAN preservada (não alfabética).
# - df_dim_type.rds    — dim tipo de métrica: (type_code, type)
#   com Incidence/Mortality.
# - df_dim_cancer.rds  — dim câncer: (cancer_code, cancer).
#   ATENÇÃO: gerada mas NÃO consumida por `app.R` (carga
#   em app.R cobre apenas country/age/type/year). Subproduto
#   reservado para futuro suporte a outros sítios.
# - df_dim_year.rds    — dim anos de projeção: (year_prediction).
#
# OBSERVAÇÕES:
# - `age_order` é hard-coded e duplica `AGE_ORDER` em
#   `R/00_constants_cc.R` (mesmo motivo de 04: o script
#   roda standalone). Centralizar AGE_ORDER em um lugar
#   compartilhado eliminaria a duplicação.
# - O assert pós-merge (linhas ~62) garante que toda faixa
#   em `age_order` casa com algum `age_code` no GLOBOCAN.
#   Falha alta-decibel se aparecerem rótulos novos (ex.:
#   "90+") em vintages futuros do GCO.
# - Compressão `xz` mantém as dimensões em arquivos pequenos
#   (kB) — overhead de leitura desprezível no startup do app.
# ===========================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fs)  # para dir_create()
})

# Garante que `data/` existe (idempotente — útil em primeira execução fresca)
if (!dir.exists("data")) dir_create("data")

# -----------------------------------------------------------
# 1) Carregar objetos principais
# -----------------------------------------------------------
# As 2 bases servem como FONTES das dimensões. df_cc_completo
# tem todas as combinações (país × faixa × tipo × câncer × ano);
# df_cc_taxas é carregada por padronização — futura expansão
# pode usar suas colunas (ASR-World, etc.) como fonte de
# atributos extras nas dimensões.

df_cc_completo <- readRDS("data/df_cc_completo.rds")
df_cc_taxas    <- readRDS("data/df_cc_taxas.rds")

setDT(df_cc_completo)
setDT(df_cc_taxas)

# -----------------------------------------------------------
# 2) Dimensão de países
# -----------------------------------------------------------
# Distinct (population_code, population_name) ordenado pelo
# nome (ordem alfabética é amigável ao seletor da sidebar).
# Inclui o "World" (1001) re-derivado em 01_prepare_cc.R.

df_dim_country <- unique(
  df_cc_completo[, .(population_code, population_name)]
)[order(population_name)]

# -----------------------------------------------------------
# 3) Dimensão de idades
# -----------------------------------------------------------
# Faixas etárias precisam preservar a ORDEM CANÔNICA do
# GLOBOCAN (0-4 → 5-9 → … → 85+) — não a ordem alfabética
# (que colocaria "10-14" antes de "5-9"). Estratégia:
#  (a) define `age_order` na ordem desejada;
#  (b) extrai (age, age_code) de df_cc_completo;
#  (c) `merge` com `all.x = TRUE` preserva a ordem do
#      data.table à esquerda (= `age_order`);
#  (d) assert pós-merge: nenhum age_code pode ficar NA
#      (significaria que `age_order` tem rótulo que o
#      GLOBOCAN não conhece — mudança de schema).

age_order <- c(
  "0-4","5-9","10-14","15-19","20-24",
  "25-29","30-34","35-39","40-44","45-49",
  "50-54","55-59","60-64","65-69","70-74",
  "75-79","80-84","85+"
)

age_map <- unique(df_cc_completo[, .(age_code, age)])[order(age_code)]
df_dim_age <- merge(
  data.table(age = age_order),
  age_map,
  by = "age",
  all.x = TRUE
)
# Defesa contra mudança silenciosa no schema GLOBOCAN
if (df_dim_age[is.na(age_code), .N] > 0L) {
  stop(
    "df_dim_age: há faixas etárias sem age_code (NA): ",
    paste(df_dim_age[is.na(age_code), age], collapse = ", ")
  )
}




# -----------------------------------------------------------
# 4) Dimensão de tipo (Incidence / Mortality)
# -----------------------------------------------------------
# Distinct (type_code, type). GLOBOCAN usa code 1=Incidence
# e code 2=Mortality (alinhado com `SEX_LABELS`/codificações
# institucionais do IARC). Ordenado por code.

df_dim_type <- unique(
  df_cc_completo[, .(type_code, type)]
)[order(type_code)]

# -----------------------------------------------------------
# 5) Dimensão de câncer
# -----------------------------------------------------------
# Distinct (cancer_code, cancer). No app atual o filtro é
# fixo em "Cervix uteri" (CCU), então este dicionário é um
# SUBPRODUTO — gerado por completude/futuro-proofing, mas
# NÃO carregado em `app.R`. Útil se o app for estendido para
# outros sítios.

df_dim_cancer <- unique(
  df_cc_completo[, .(cancer_code, cancer)]
)[order(cancer_code)]

# -----------------------------------------------------------
# 6) Dimensão de ano de projeção
# -----------------------------------------------------------
# Distinct dos `year_prediction` presentes em df_cc_completo
# (tipicamente 2022 e 2025 — pop observada e pop projetada).
# Ordenado crescente para o seletor de ano (caso usado).

df_dim_year <- data.table(
  year_prediction = sort(unique(df_cc_completo$year_prediction))
)

# -----------------------------------------------------------
# 7) Salvar dimensões em .rds
# -----------------------------------------------------------
# Compressão `xz` é coerente com os outros .rds em `data/`
# (perfil offline-friendly: arquivos pequenos a custo de
# tempo de gravação levemente maior — irrelevante em ETL).

saveRDS(df_dim_country, "data/df_dim_country.rds", compress = "xz")
saveRDS(df_dim_age,     "data/df_dim_age.rds",     compress = "xz")
saveRDS(df_dim_type,    "data/df_dim_type.rds",    compress = "xz")
saveRDS(df_dim_cancer,  "data/df_dim_cancer.rds",  compress = "xz")  # subproduto: app.R não lê
saveRDS(df_dim_year,    "data/df_dim_year.rds",    compress = "xz")

message("✔ Dicionários gerados e salvos em data/df_dim_*.rds")
