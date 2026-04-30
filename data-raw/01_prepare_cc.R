# ===========================================================
# Shiny-CC — data-raw/01_prepare_cc.R
# Preparo das bases globais (GLOBOCAN + WPP) -> objetos .rds
# -----------------------------------------------------------
# ESCOPO
#   Somente mulheres (Female) + faixas etárias 0–4, 5–9, ...
#
# ENTRADA (planilhas em data-raw/)
#   - WPP2019_POP_ANNUAL_POPULATION.csv ............ população por país × idade × ano (ONU/WPP)
#   - Globocan_2022_cervical.xlsx .................. casos de câncer de colo (GLOBOCAN 2022)
#   - dataset-inc-females-in-2022-cervix-uteri.xlsx  agregados de incidência por país
#   - dataset-mort-females-in-2022-cervix-uteri.xlsx agregados de mortalidade por país
#
# SAÍDA (em data/)
#   - df_cc_completo.rds : merge granular GLOBOCAN × WPP por país × faixa etária × tipo
#   - df_cc_taxas.rds    : taxas agregadas (incidência + mortalidade) por país
#
# OBSERVAÇÕES
#   - Códigos populacionais são "consertados" via fix_population_code():
#       * 900 (World, vintage antigo) -> 1001 (World atual em GLOBOCAN)
#       * 156 -> 160 quando year == 2025 (mudança de código WPP em 2025)
#   - "World" (1001) é re-derivado por agregação dos países para garantir consistência
#     com os totais por estrato no GLOBOCAN.
#   - Os dimensionais (df_dim_country, df_dim_age, df_dim_type, df_dim_year) NÃO são
#     gerados aqui — provavelmente em outro script (verificar 02/06).
# ===========================================================

# Pacotes -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(janitor)
  library(usethis)
})

options(stringsAsFactors = FALSE, scipen = 999)

# Helpers compartilhados com o app (cc_check_schema, %||%, etc.).
# `R/01_utils_cc.R` contém só definições de função no top-level — nenhum
# efeito colateral; cwd na raiz do projeto (premissa do header).
source("R/01_utils_cc.R")

# Helpers -----------------------------------------------------------------

# Padroniza códigos populacionais entre vintages diferentes do GLOBOCAN/WPP.
#   - 900  (World, vintage antigo) -> 1001 (World atual)
#   - 156  (China, vintage WPP)    -> 160  apenas para year == 2025
# `year` pode ser escalar ou vetor do mesmo comprimento de `code`.
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


# Validação de schema das planilhas de entrada via `cc_check_schema()`
# (definido em R/01_utils_cc.R). Default on_fail = "stop": falha cedo se
# faltar coluna esperada, defensivo contra mudanças silenciosas de schema.

# Caminhos (mantém convenção data-raw/) ----------------------------------
path_wpp_csv <- "data-raw/WPP2019_POP_ANNUAL_POPULATION.csv"   # csv2 (;)
path_globo   <- "data-raw/Globocan_2022_cervical.xlsx"            # csv (,)
path_inc     <- "data-raw/dataset-inc-females-in-2022-cervix-uteri.xlsx"
path_mort    <- "data-raw/dataset-mort-females-in-2022-cervix-uteri.xlsx"

# ------------------------------------------------------------------------
# 1) População (WPP) — somente mulheres, por país + faixa etária (2022/2025)
# ------------------------------------------------------------------------
# CSV separado por ";" (formato europeu). Após carregar, deixa-se apenas
# o sexo feminino e padronizam-se rótulo/código. As populações dos anos
# 2022 e 2025 entram em colunas distintas (pop_2022, pop_2025).

wpp_all <- fread(path_wpp_csv, sep = ";", showProgress = FALSE)
cc_check_schema(wpp_all,
                c("population_code","population","sex_code","sex",
                  "age_code","age","year","pop"),
                "wpp_all")

# Filtra somente mulheres (sex_code == 2 ou sex == "female")
wpp_all <- wpp_all[sex_code == 2L | tolower(as.character(sex)) == "female"]
wpp_all[, population_code := fix_population_code(as.integer(population_code), year)]
wpp_all[, sex := "Female"]      # padroniza rotulo
wpp_all[, sex_code := 2L]       # padroniza código

# 2022 e 2025, agregando por país + idade (soma pop em caso de fragmentação,
# ex.: códigos populacionais que viraram um único após fix_population_code).
wpp_2022 <- wpp_all[year == 2022,
                    .(pop_2022 = sum(as.numeric(pop), na.rm = TRUE)),
                    by = .(population_code, sex_code, sex, age_code, age)
]
wpp_2025 <- wpp_all[year == 2025,
                    .(pop_2025 = sum(as.numeric(pop), na.rm = TRUE)),
                    by = .(population_code, sex_code, sex, age_code, age)
]

# Junta 2022 x 2025 na mesma granularidade — full join (all = TRUE) para
# preservar países que existem em só um dos vintages.
df_infos_pop <- merge(
  wpp_2022, wpp_2025,
  by = c("population_code","sex_code","sex","age_code","age"),
  all = TRUE
)

# ------------------------------------------------------------------------
# 2) Globocan — câncer do Colo do Útero (CSV) — somente mulheres, por país + idade
# ------------------------------------------------------------------------
# Apesar do nome `path_globo` apontar para .xlsx, lemos via readxl. Após
# `clean_names()` os nomes ficam em snake_case. Renomeia-se `year` para
# `year_prediction` (que é o ano-alvo da projeção GLOBOCAN, ex.: 2050).
# "World" (1001) é re-derivado por agregação dos países, garantindo
# consistência com qualquer recorte que o usuário fizer no app.

globo_raw <- readxl::read_xlsx(path_globo) |>
  janitor::clean_names() |>
  data.table::as.data.table()

cc_check_schema(globo_raw,
                c("population_code","population",
                  "cancer_code","cancer","type_code","type",
                  "sex_code","sex","age_code","age",
                  "year","prediction","cases_2022"),
                "globo_raw")

# Padroniza códigos e filtros (mesma lógica do bloco WPP)
globo_raw[, population_code := fix_population_code(as.integer(population_code), year)]
globo_raw <- globo_raw[sex_code == 2L | tolower(as.character(sex)) == "female"]
globo_raw[, `:=`(
  sex = "Female",
  sex_code = 2L,
  year_prediction = year   # ano-alvo da predição (renomeação)
)]
globo_raw[, year := NULL]

# Agrega "World" (1001) por estratos — apenas feminino. Usa a soma direta
# dos países; pode haver ligeira diferença em relação ao "World" oficial
# do GLOBOCAN, mas a garantia de aditividade é mais útil para o app.
world_agg <- globo_raw[
  , .(prediction = sum(prediction, na.rm = TRUE),
      cases_2022 = sum(cases_2022, na.rm = TRUE)),
  by = .(cancer_code, cancer, type_code, type,
         sex_code, sex, age_code, age, year_prediction)
][
  , `:=`(population_code = 1001L,
         population      = "World")
]


# Empilha países + World (use.names + fill = TRUE para tolerar colunas
# extras existentes só em uma das tabelas)
df_infos_epidemiologicas_cc <- data.table::rbindlist(
  list(globo_raw, world_agg),
  use.names = TRUE,
  fill = TRUE
)
# Lookup de nomes de países a partir do Globocan (para fallback caso a
# tabela de labels — bloco 3 — não traga nome para algum código)
globo_names <- unique(df_infos_epidemiologicas_cc[
  , .(population_code, population_globocan = population)
])

# ------------------------------------------------------------------------
# 3) Labels (nomes das populações) e anexo aos dados de população
# ------------------------------------------------------------------------
# Os nomes oficiais (rótulos curtos para a UI) vêm da planilha de incidência.
# `country` ali é, na prática, um `population_code`. Fluxo:
#   labels da planilha (preferencial) -> fallback para o nome do GLOBOCAN.

labels_raw <- read_xlsx(path_inc) |> clean_names()
cc_check_schema(labels_raw, c("country","label","sex"), "labels_raw")

labels_dt <- as.data.table(labels_raw)[
  , .(population_code = fix_population_code(as.integer(country)),
      population_name = as.character(label))
]
# Unicidade por código (algum país pode aparecer várias vezes se houver
# múltiplos cancer/sex). setorder + unique garante 1 nome canônico por código.
setorder(labels_dt, population_code, population_name)
labels_dt <- unique(labels_dt, by = "population_code")


# Anexa nomes aos dados de população: 1º) labels oficiais; 2º) nome do
# GLOBOCAN como fallback. Em seguida descarta-se a coluna auxiliar.
df_infos_pop <- merge(df_infos_pop, labels_dt, by = "population_code", all.x = TRUE)
df_infos_pop <- merge(df_infos_pop, globo_names, by = "population_code", all.x = TRUE)
df_infos_pop[, population_name := fifelse(
  is.na(population_name), population_globocan, population_name
)]
df_infos_pop[, population_globocan := NULL]

# Garantia de chave única (assertiva — para por exceção se algo escapar)
stopifnot(!any(duplicated(df_infos_pop[, .(population_code, sex_code, sex, age_code)])))


# ------------------------------------------------------------------------
# 4) Join final: epidemiologia (Globocan) x população (WPP) por chaves cheias
# ------------------------------------------------------------------------
# Left join para preservar todas as linhas da base epidemiológica. Os
# campos `sex` e `age` ficam intencionalmente os da base epidemiológica
# (mesmos rótulos do GLOBOCAN); por isso só `sex_code`/`age_code` são
# trazidos do lado da população, evitando colisão.

df_cc_completo <- merge(
  df_infos_epidemiologicas_cc,
  df_infos_pop[
    # NÃO traga sex/age aqui para evitar colisão de nomes
    , .(population_code, sex_code, age_code, population_name, pop_2022, pop_2025)
  ],
  by = c("population_code","sex_code","age_code"),
  all.x = TRUE, allow.cartesian = FALSE
)[
  # Reordena colunas para layout final (rótulos primeiro, métricas depois)
  , .(population_code, population_name,
      sex_code, sex, age_code, age,           # 'sex' e 'age' ficam da base epidemiológica
      pop_2022, pop_2025,
      cancer_code, cancer, type_code, type,
      year_prediction, prediction, cases_2022)
]

# Sanity check: aviso (não erro) se algum estrato ficou sem população em
# ambos os anos. É esperado em casos específicos (ex.: territórios sem
# correspondência no WPP). Erro se duplicar a chave canônica.
if (df_cc_completo[is.na(pop_2022) & is.na(pop_2025), .N] > 0L) {
  warning("População faltante (pop_2022 e pop_2025) para alguns estratos após o merge.")
}
stopifnot(!any(duplicated(df_cc_completo[, .(population_code, sex_code, age_code,
                                             cancer_code, type_code, year_prediction)])))



# Persistência ------------------------------------------------------------
# Compressão xz é mais lenta de gravar mas reduz o .rds em ~30-40%.
saveRDS(df_cc_completo,
        "data/df_cc_completo.rds", compress = "xz")

message("-> Gravado: data/df_cc_completo.rds (",
        nrow(df_cc_completo), " linhas)")

# ------------------------------------------------------------------------
# 5) Taxas agregadas (incidência e mortalidade) — nível país (sem idade)
# ------------------------------------------------------------------------
# Estes dados alimentam a aba **Epidemiology** (cards de casos absolutos
# + ASR, gráficos do país selecionado). Diferente de df_cc_completo:
# aqui as métricas já vêm AGREGADAS por país (sem desagregação por idade).

inc_raw <- read_xlsx(path_inc) |> clean_names()
mort_raw <- read_xlsx(path_mort) |> clean_names()

# Helper: descobre qual coluna tem o NÚMERO DE CASOS/ÓBITOS.
# A planilha do GCO/IARC às vezes vem com colunas duplicadas (number,
# number_7, number_10, number...7), porque o Excel renomeia colunas
# repetidas e o `clean_names()` ajusta de formas distintas conforme
# o vintage do arquivo. Tentamos uma lista conhecida; em último caso,
# pegamos a primeira coluna que começa com "number".
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

cc_check_schema(inc_raw,
                c("country","label","sex","cancer_code", inc_num_col, "crude_rate","asr_world"),
                "inc_raw")
cc_check_schema(mort_raw,
                c("country","label","sex","cancer_code", mort_num_col, "crude_rate","asr_world"),
                "mort_raw")


# As planilhas já são só de mulheres -> não filtrar por sexo,
# para não perder o World se a coluna 'sex' vier em branco ou com outro código.
# `get(inc_num_col)` resolve o nome dinâmico capturado por pick_number_col().
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



# Full join (all = TRUE) por chaves canônicas. `suffixes` é necessário
# porque `population_name` aparece nos dois lados.
df_cc_taxas <- merge(
  df_cc_incidencia,
  df_cc_mortalidade,
  by = c("population_code", "sex_code", "cancer_code"),
  all = TRUE,
  suffixes = c(".inc", ".mort")
)

# Unifica `population_name` (prioriza o vindo de incidência; fallback para
# o de mortalidade se o primeiro estiver vazio/NA). nzchar() trata strings
# vazias como ausência.
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
# Verificações leves (warning/message, não param a execução). Servem como
# pista no console quando o ETL é re-rodado depois de atualização de fonte.

if (!1001L %in% df_cc_completo$population_code) {
  warning("World (1001) ausente em df_cc_completo.")
}
if (df_cc_completo[is.na(pop_2022) & is.na(pop_2025), .N] > 0L) {
  message("Atenção: existem estratos com pop_2022 e pop_2025 ausentes após o join. Verificar mapeamento de códigos.")
}


# Fim ---------------------------------------------------------------------


                       