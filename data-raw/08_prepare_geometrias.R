# ===========================================================
# 08_prepare_geometrias.R
# Baixa geometrias do geobr, simplifica e salva em data/
# Executar uma única vez (ou ao atualizar geometrias).
#
# Saídas:
#   data/geo_estados.rds
#   data/geo_macrorregioes.rds
#   data/geo_regioes_saude.rds
#   data/geo_municipios.rds
# ===========================================================

suppressPackageStartupMessages({
  library(geobr)
  library(sf)
  library(rmapshaper)
  library(data.table)
})

path_data     <- function(...) file.path("data", ...)
path_data_raw <- function(...) file.path("data-raw", ...)

# Regionalização SUS (para adicionar hierarquia nas geometrias)
regional_sus_map <- readRDS(path_data("regional_sus_map.rds"))
setDT(regional_sus_map)

# Lookup único município → hierarquia (sem duplicatas de faixas etárias)
mun_hier <- unique(regional_sus_map[, .(
  mun_code6,
  mun_nome,
  uf_sigla,
  macro_codigo,
  macro_nome,
  regiao_codigo,
  regiao_nome,
  `Macrorregiao de Saude`,
  `Regiao de Saude`,
  `Municipio`
)])

# ----------------------------------------------------------
# 1) Estados (27 polígonos)
# ----------------------------------------------------------
message(">> Baixando geometrias de estados ...")
geo_estados_raw <- geobr::read_state(year = 2020, showProgress = FALSE)

geo_estados <- geo_estados_raw[, c("code_state", "abbrev_state", "name_state", "geom")]
names(geo_estados)[names(geo_estados) == "code_state"]   <- "uf_codigo"
names(geo_estados)[names(geo_estados) == "abbrev_state"] <- "uf_sigla"
names(geo_estados)[names(geo_estados) == "name_state"]   <- "uf_nome"
names(geo_estados)[names(geo_estados) == "geom"]         <- "geometry"
sf::st_geometry(geo_estados) <- "geometry"

geo_estados <- rmapshaper::ms_simplify(geo_estados, keep = 0.05, keep_shapes = TRUE)
saveRDS(geo_estados, path_data("geo_estados.rds"))
message(">> OK: data/geo_estados.rds — ", nrow(geo_estados), " estados")

# ----------------------------------------------------------
# 2) Municípios (base para macrorregiões e regiões)
# ----------------------------------------------------------
message(">> Baixando geometrias de municípios ...")
geo_mun_raw <- geobr::read_municipality(year = 2020, showProgress = FALSE)

geo_mun_sf <- geo_mun_raw[, c("code_muni", "name_muni", "geom")]
names(geo_mun_sf)[names(geo_mun_sf) == "geom"] <- "geometry"
sf::st_geometry(geo_mun_sf) <- "geometry"

# Adicionar código 6 dígitos (IBGE 7 → 6)
geo_mun_sf$mun_code6 <- substr(as.character(geo_mun_sf$code_muni), 1, 6)

# Join com hierarquia SUS
mun_hier_df <- as.data.frame(mun_hier)
geo_mun_sf  <- merge(geo_mun_sf, mun_hier_df, by = "mun_code6", all.x = TRUE)

# Simplificar
geo_municipios <- rmapshaper::ms_simplify(geo_mun_sf, keep = 0.05, keep_shapes = TRUE)
saveRDS(geo_municipios, path_data("geo_municipios.rds"))
message(">> OK: data/geo_municipios.rds — ", nrow(geo_municipios), " municípios")

# ----------------------------------------------------------
# 3) Macrorregiões de saúde (derivado dos municípios)
# ----------------------------------------------------------
message(">> Derivando geometrias de macrorregiões ...")
geo_mun_macro <- geo_municipios[
  !is.na(geo_municipios$macro_nome) & nzchar(geo_municipios$macro_nome),
]

geo_macrorregioes <- aggregate(
  geo_mun_macro["geometry"],
  by = list(
    macro_codigo = geo_mun_macro$macro_codigo,
    macro_nome   = geo_mun_macro$macro_nome,
    uf_sigla     = geo_mun_macro$uf_sigla
  ),
  FUN = sf::st_union
)
geo_macrorregioes <- sf::st_as_sf(geo_macrorregioes)
geo_macrorregioes <- rmapshaper::ms_simplify(geo_macrorregioes, keep = 0.05, keep_shapes = TRUE)

saveRDS(geo_macrorregioes, path_data("geo_macrorregioes.rds"))
message(">> OK: data/geo_macrorregioes.rds — ", nrow(geo_macrorregioes), " macrorregiões")

# ----------------------------------------------------------
# 4) Regiões de saúde (derivado dos municípios, agrupa por regiao_nome)
# ----------------------------------------------------------
message(">> Derivando geometrias de regiões de saúde ...")
geo_mun_reg <- geo_municipios[
  !is.na(geo_municipios$regiao_nome) & nzchar(geo_municipios$regiao_nome),
]

geo_regioes_saude <- aggregate(
  geo_mun_reg["geometry"],
  by = list(
    regiao_codigo = geo_mun_reg$regiao_codigo,
    regiao_nome   = geo_mun_reg$regiao_nome,
    macro_nome    = geo_mun_reg$macro_nome,
    uf_sigla      = geo_mun_reg$uf_sigla
  ),
  FUN = sf::st_union
)
geo_regioes_saude <- sf::st_as_sf(geo_regioes_saude)
geo_regioes_saude <- rmapshaper::ms_simplify(geo_regioes_saude, keep = 0.05, keep_shapes = TRUE)

saveRDS(geo_regioes_saude, path_data("geo_regioes_saude.rds"))
message(">> OK: data/geo_regioes_saude.rds — ", nrow(geo_regioes_saude), " regiões de saúde")

message("==== 08_prepare_geometrias — concluído ====")
