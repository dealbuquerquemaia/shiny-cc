# ===========================================================
# Shiny-CC — 05_glossary_cc.R
# Gera dicionários (dimensões) a partir dos objetos .rds
# ===========================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fs)
})

if (!dir.exists("data")) dir_create("data")

# -----------------------------------------------------------
# 1) Carregar objetos principais
# -----------------------------------------------------------

df_cc_completo <- readRDS("data/df_cc_completo.rds")
df_cc_taxas    <- readRDS("data/df_cc_taxas.rds")

setDT(df_cc_completo)
setDT(df_cc_taxas)

# -----------------------------------------------------------
# 2) Dimensão de países
# -----------------------------------------------------------

df_dim_country <- unique(
  df_cc_completo[, .(population_code, population_name)]
)[order(population_name)]

# -----------------------------------------------------------
# 3) Dimensão de idades
# (seguindo a ordem desejada, não a ordem alfabética)
# -----------------------------------------------------------

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
if (df_dim_age[is.na(age_code), .N] > 0L) {
  stop(
    "df_dim_age: há faixas etárias sem age_code (NA): ",
    paste(df_dim_age[is.na(age_code), age], collapse = ", ")
  )
}




# -----------------------------------------------------------
# 4) Dimensão de tipo (Incidence / Mortality)
# -----------------------------------------------------------

df_dim_type <- unique(
  df_cc_completo[, .(type_code, type)]
)[order(type_code)]

# -----------------------------------------------------------
# 5) Dimensão de câncer
# -----------------------------------------------------------

df_dim_cancer <- unique(
  df_cc_completo[, .(cancer_code, cancer)]
)[order(cancer_code)]

# -----------------------------------------------------------
# 6) Dimensão de ano de projeção
# -----------------------------------------------------------

df_dim_year <- data.table(
  year_prediction = sort(unique(df_cc_completo$year_prediction))
)

# -----------------------------------------------------------
# 7) Salvar dimensões em .rds
# -----------------------------------------------------------

saveRDS(df_dim_country, "data/df_dim_country.rds", compress = "xz")
saveRDS(df_dim_age,     "data/df_dim_age.rds",     compress = "xz")
saveRDS(df_dim_type,    "data/df_dim_type.rds",    compress = "xz")
saveRDS(df_dim_cancer,  "data/df_dim_cancer.rds",  compress = "xz")
saveRDS(df_dim_year,    "data/df_dim_year.rds",    compress = "xz")

message("✔ Dicionários gerados e salvos em data/df_dim_*.rds")
