# ===========================================================
# Shiny-CC — 01_prepare_cc.R
# Preparo das bases “brutas” -> objetos .rds
# Escopo: somente mulheres (Female) + faixas etárias 0–4, 5–9, ...

# ===========================================================

# Pacotes -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(janitor)
  library(usethis)
})

options(stringsAsFactors = FALSE, scipen = 999)

# Helpers -----------------------------------------------------------------
fix_population_code <- function(code, year = NULL) {
  code <- as.integer(code)
  # 900 -> 1001 (World), vetorizado
  code <- fifelse(code == 900L, 1001L, code)
  # 156 -> 160 somente quando year == 2025, vetorizado
  if (!is.null(year)) {
    if (!(length(year) %in% c(1L, length(code)))) {
      stop("fix_population_code: 'year' com comprimento incompatível.")
    }
    code <- fifelse(year == 2025L & code == 156L, 160L, code)
  }
    code
}


must_have_cols <- function(dt, cols) {
  miss <- setdiff(cols, names(dt))
  if (length(miss)) stop("Colunas faltantes: ", paste(miss, collapse = ", "))
}

# Caminhos (mantém convenção data-raw/) ----------------------------------
path_wpp_csv <- "data-raw/WPP2019_POP_ANNUAL_POPULATION.csv"   # csv2 (;)
path_globo   <- "data-raw/Globocan_2022_cervical.xlsx"            # csv (,)
path_inc     <- "data-raw/dataset-inc-females-in-2022-cervix-uteri.xlsx"
path_mort    <- "data-raw/dataset-mort-females-in-2022-cervix-uteri.xlsx"

# ------------------------------------------------------------------------
# 1) População (WPP) — somente mulheres, por país + faixa etária (2022/2025)
# ------------------------------------------------------------------------
wpp_all <- fread(path_wpp_csv, sep = ";", showProgress = FALSE)
must_have_cols(wpp_all, c("population_code","population","sex_code","sex",
                          "age_code","age","year","pop"))

# Filtra somente mulheres (sex_code == 2 ou sex == "female")
wpp_all <- wpp_all[sex_code == 2L | tolower(as.character(sex)) == "female"]
wpp_all[, population_code := fix_population_code(as.integer(population_code), year)]
wpp_all[, sex := "Female"]      # padroniza rotulo
wpp_all[, sex_code := 2L]       # padroniza código

# 2022 e 2025, agregando por país + idade (soma pop em caso de fragmentação)
wpp_2022 <- wpp_all[year == 2022,
                    .(pop_2022 = sum(as.numeric(pop), na.rm = TRUE)),
                    by = .(population_code, sex_code, sex, age_code, age)
]
wpp_2025 <- wpp_all[year == 2025,
                    .(pop_2025 = sum(as.numeric(pop), na.rm = TRUE)),
                    by = .(population_code, sex_code, sex, age_code, age)
]

# Junta 2022 x 2025 na mesma granularidade
df_infos_pop <- merge(
  wpp_2022, wpp_2025,
  by = c("population_code","sex_code","sex","age_code","age"),
  all = TRUE
)

# ------------------------------------------------------------------------
# 2) Globocan — câncer do Colo do Útero (CSV) — somente mulheres, por país + idade
# ------------------------------------------------------------------------
# --- POR este bloco (inteiro) ---
globo_raw <- readxl::read_xlsx(path_globo) |>
  janitor::clean_names() |>
  data.table::as.data.table()

must_have_cols(globo_raw, c("population_code","population",
                            "cancer_code","cancer","type_code","type",
                            "sex_code","sex","age_code","age",
                            "year","prediction","cases_2022"))

# Padroniza códigos e filtros
globo_raw[, population_code := fix_population_code(as.integer(population_code), year)]
globo_raw <- globo_raw[sex_code == 2L | tolower(as.character(sex)) == "female"]
globo_raw[, `:=`(
  sex = "Female",
  sex_code = 2L,
  year_prediction = year
)]
globo_raw[, year := NULL]

# Agrega "World" (1001) por estratos — apenas feminino
world_agg <- globo_raw[
  , .(prediction = sum(prediction, na.rm = TRUE),
      cases_2022 = sum(cases_2022, na.rm = TRUE)),
  by = .(cancer_code, cancer, type_code, type,
         sex_code, sex, age_code, age, year_prediction)
][
  , `:=`(population_code = 1001L,
         population      = "World")
]


# Empilha garantindo o mesmo conjunto de colunas
df_infos_epidemiologicas_cc <- data.table::rbindlist(
  list(globo_raw, world_agg),
  use.names = TRUE,
  fill = TRUE
)
# Lookup de nomes de países a partir do Globocan (para fallback)
globo_names <- unique(df_infos_epidemiologicas_cc[
  , .(population_code, population_globocan = population)
])

# ------------------------------------------------------------------------
# 3) Labels (nomes das populações) e anexo aos dados de população
# ------------------------------------------------------------------------
labels_raw <- read_xlsx(path_inc) |> clean_names()
must_have_cols(labels_raw, c("country","label","sex"))

labels_dt <- as.data.table(labels_raw)[
  , .(population_code = fix_population_code(as.integer(country)),
      population_name = as.character(label))
]
# Unicidade por código
setorder(labels_dt, population_code, population_name)
labels_dt <- unique(labels_dt, by = "population_code")


# Anexa nomes aos dados de população (e fallback para Globocan se faltar)
df_infos_pop <- merge(df_infos_pop, labels_dt, by = "population_code", all.x = TRUE)
df_infos_pop <- merge(df_infos_pop, globo_names, by = "population_code", all.x = TRUE)
df_infos_pop[, population_name := fifelse(
  is.na(population_name), population_globocan, population_name
)]
df_infos_pop[, population_globocan := NULL]

# Garantias de chave única
stopifnot(!any(duplicated(df_infos_pop[, .(population_code, sex_code, sex, age_code)])))


# ------------------------------------------------------------------------
# 4) Join final: epidemiologia (Globocan) x população (WPP) por chaves cheias
# ------------------------------------------------------------------------
df_cc_completo <- merge(
  df_infos_epidemiologicas_cc,
  df_infos_pop[
    # NÃO traga sex/age aqui para evitar colisão de nomes
    , .(population_code, sex_code, age_code, population_name, pop_2022, pop_2025)
  ],
  by = c("population_code","sex_code","age_code"),
  all.x = TRUE, allow.cartesian = FALSE
)[
  , .(population_code, population_name,
      sex_code, sex, age_code, age,           # 'sex' e 'age' ficam da base epidemiológica
      pop_2022, pop_2025,
      cancer_code, cancer, type_code, type,
      year_prediction, prediction, cases_2022)
]

# sanity: população faltante (esperado só em casos específicos)
if (df_cc_completo[is.na(pop_2022) & is.na(pop_2025), .N] > 0L) {
  warning("População faltante (pop_2022 e pop_2025) para alguns estratos após o merge.")
}
stopifnot(!any(duplicated(df_cc_completo[, .(population_code, sex_code, age_code,
                                             cancer_code, type_code, year_prediction)])))



# Persistência ------------------------------------------------------------
saveRDS(df_cc_completo,
        "data/df_cc_completo.rds", compress = "xz")

message("-> Gravado: data/df_cc_completo.rds (",
        nrow(df_cc_completo), " linhas)")

# ------------------------------------------------------------------------
# 5) Taxas agregadas (incidência e mortalidade) — nível país (sem idade)
# ------------------------------------------------------------------------
inc_raw <- read_xlsx(path_inc) |> clean_names()
mort_raw <- read_xlsx(path_mort) |> clean_names()

# --- POR este bloco (inteiro) ---
pick_number_col <- function(dt) {
  nm <- names(dt)
  
  # casos comuns após clean_names()
  cand <- intersect(
    c("number", "number_7", "number_10", "number...7", "number...10"),
    nm
  )
  if (length(cand) > 0L) return(cand[1L])
  
  # fallback geral: qualquer coluna que comece com "number"
  cand2 <- grep("^number", nm, value = TRUE)
  if (length(cand2) > 0L) return(cand2[1L])
  
  stop("Coluna 'number' não encontrada. Nomes disponíveis: ", paste(nm, collapse = ", "))
}

inc_num_col  <- pick_number_col(inc_raw)
mort_num_col <- pick_number_col(mort_raw)

must_have_cols(inc_raw,  c("country","label","sex","cancer_code", inc_num_col, "crude_rate","asr_world"))
must_have_cols(mort_raw, c("country","label","sex","cancer_code", mort_num_col, "crude_rate","asr_world"))


# As planilhas já são só de mulheres -> não filtrar por sexo,
# para não perder o World se a coluna 'sex' vier em branco ou com outro código.
df_cc_incidencia <- as.data.table(inc_raw)[
  ,
  .(population_code       = fix_population_code(as.integer(country)),
    population_name       = as.character(label),
    sex_code              = 2L,  # feminino fixo
    cancer_code           = cancer_code,
    number_incidence      = as.numeric(get(inc_num_col)),
    incidence_crude_rate  = as.numeric(crude_rate),
    incidence_asr_world   = as.numeric(asr_world))
]

df_cc_mortalidade <- as.data.table(mort_raw)[
  ,
  .(population_code       = fix_population_code(as.integer(country)),
    population_name       = as.character(label),
    sex_code              = 2L,
    cancer_code           = cancer_code,
    number_mortality      = as.numeric(get(mort_num_col)),
    mortality_crude_rate  = as.numeric(crude_rate),
    mortality_asr_world   = as.numeric(asr_world))
]



# Full join por chaves canônicas (explicitamente) + unifica population_name
df_cc_taxas <- merge(
  df_cc_incidencia,
  df_cc_mortalidade,
  by = c("population_code", "sex_code", "cancer_code"),
  all = TRUE,
  suffixes = c(".inc", ".mort")
)

# unifica nome (prioriza incidência; fallback mortalidade)
df_cc_taxas[, population_name := fifelse(
  !is.na(population_name.inc) & nzchar(population_name.inc),
  population_name.inc,
  population_name.mort
)]
df_cc_taxas[, c("population_name.inc", "population_name.mort") := NULL]

# Persistência
saveRDS(df_cc_taxas,
        "data/df_cc_taxas.rds", compress = "xz")

message("-> Gravado: data/df_cc_taxas.rds (",
        nrow(df_cc_taxas), " linhas)")


# ------------------------------------------------------------------------
# 6) Sanidade mínima
# ------------------------------------------------------------------------
if (!1001L %in% df_cc_completo$population_code) {
  warning("World (1001) ausente em df_cc_completo.")
}
if (df_cc_completo[is.na(pop_2022) & is.na(pop_2025), .N] > 0L) {
  message("Atenção: existem estratos com pop_2022 e pop_2025 ausentes após o join. Verificar mapeamento de códigos.")
}


# Fim ---------------------------------------------------------------------


                       