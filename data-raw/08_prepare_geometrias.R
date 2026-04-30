# ===========================================================
# 08_prepare_geometrias.R
# -----------------------------------------------------------
# Escopo: ETL de geometrias do Brasil para o módulo `mod_maps`.
#         Baixa polígonos do `geobr`, simplifica via `rmapshaper`,
#         enriquece com a hierarquia SUS (regional_sus_map) e
#         salva 4 arquivos `.rds` (sf) em data/.
#
# Entrada:
#   data/regional_sus_map.rds      (gerado por 02_prepare_BR.R)
#   geobr::read_state(year=2020)   (download em runtime)
#   geobr::read_municipality(year=2020)
#
# Saída (em data/):
#   data/geo_estados.rds         — sf 27 polígonos UF
#   data/geo_municipios.rds      — sf ~5.5k polígonos municipais (com hierarquia SUS)
#   data/geo_macrorregioes.rds   — sf ~120 macrorregiões (st_union de municípios)
#   data/geo_regioes_saude.rds   — sf ~440 regiões de saúde (st_union de municípios)
#
# Particularidades:
#   - Standalone: NÃO está em 06_make_all_cc.R (lacuna do orquestrador,
#     mesma situação de 04/05/07).
#   - Macrorregião e região de saúde NÃO existem no geobr — são derivadas
#     dos municípios via `aggregate(st_union(...))` usando a hierarquia
#     do `regional_sus_map`. Decisão arquitetural: ÚNICA fonte de verdade
#     da hierarquia SUS é o `regional_sus_map.rds` (gerado em 02).
#   - IBGE usa código de 7 dígitos; o app/SUS usa 6 dígitos. `mun_code6`
#     é derivado por `substr(., 1, 6)` para casar com regional_sus_map.
#   - Simplificação `keep = 0.05` (5% dos vértices) + `keep_shapes = TRUE`
#     (impede colapso de polígonos pequenos como ilhas/distritos isolados).
#     Tradeoff peso × fidelidade — calibrado para o leaflet do `mod_maps`.
#   - `year = 2020` hardcoded — alinha com vintage do regional_sus_map.
#     Se atualizar (ex.: malha pós-Censo 2022), conferir compat com 02.
# ===========================================================

# --- Pacotes (silencioso para reduzir ruído no console do ETL) ------------
suppressPackageStartupMessages({
  library(geobr)         # downloads de geometrias oficiais brasileiras
  library(sf)            # tipos sf + operações geométricas (st_union)
  library(rmapshaper)    # simplificação topológica (preserva fronteiras)
  library(data.table)    # data.table na manipulação do regional_sus_map
})

# --- Helpers de path -------------------------------------------------------
# Diferem de `02_prepare_BR.R` (que usa `here::here`) — aqui são wrappers
# simples sobre `file.path`, exigindo cwd na raiz do projeto.
# `path_data_raw` é definida mas atualmente NÃO é chamada (futuro-proofing).
path_data     <- function(...) file.path("data", ...)
path_data_raw <- function(...) file.path("data-raw", ...)

# --- Hierarquia SUS (lookup município → macro/região/UF) -------------------
# Carregamos o `regional_sus_map.rds` (saída de 02) para anexar a hierarquia
# SUS aos polígonos municipais. Sem isso, não dá para derivar macro/região.
regional_sus_map <- readRDS(path_data("regional_sus_map.rds"))
setDT(regional_sus_map)

# `regional_sus_map` é granular por (mun_code6 × faixa etária) na origem.
# `unique(...)` deduplica para 1 linha por município, com as 10 colunas
# necessárias: códigos numéricos + nomes nativos + nomes oficiais SUS
# (com acentos) que o `mod_maps` usa nos tooltips.
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
# Via `geobr::read_state`. Saída tem colunas em inglês — renomeamos
# para o padrão pt-BR usado pelo app (`uf_codigo`, `uf_sigla`, `uf_nome`)
# e a geometria de `geom` para `geometry` (convenção sf moderna).
# ----------------------------------------------------------
message(">> Baixando geometrias de estados ...")
geo_estados_raw <- geobr::read_state(year = 2020, showProgress = FALSE)

# Subset + rename (substituímos os 4 nomes em sequência via lookup posicional)
geo_estados <- geo_estados_raw[, c("code_state", "abbrev_state", "name_state", "geom")]
names(geo_estados)[names(geo_estados) == "code_state"]   <- "uf_codigo"
names(geo_estados)[names(geo_estados) == "abbrev_state"] <- "uf_sigla"
names(geo_estados)[names(geo_estados) == "name_state"]   <- "uf_nome"
names(geo_estados)[names(geo_estados) == "geom"]         <- "geometry"
# Reativa a coluna `geometry` como ativa do objeto sf (necessário após rename).
sf::st_geometry(geo_estados) <- "geometry"

# Simplificação 5% — reduz ~95% dos vértices preservando topologia.
# `keep_shapes=TRUE` impede que ilhas/polígonos pequenos colapsem para vazio.
geo_estados <- rmapshaper::ms_simplify(geo_estados, keep = 0.05, keep_shapes = TRUE)
saveRDS(geo_estados, path_data("geo_estados.rds"))
message(">> OK: data/geo_estados.rds — ", nrow(geo_estados), " estados")

# ----------------------------------------------------------
# 2) Municípios (base para macrorregiões e regiões)
# ----------------------------------------------------------
# Os polígonos municipais cumprem 2 papéis aqui:
#   (a) saída `geo_municipios.rds` (camada de granularidade fina no mapa);
#   (b) base para derivar macrorregiões e regiões de saúde (blocos 3 e 4)
#       via `aggregate(st_union, by = ...)`.
# ----------------------------------------------------------
message(">> Baixando geometrias de municípios ...")
geo_mun_raw <- geobr::read_municipality(year = 2020, showProgress = FALSE)

geo_mun_sf <- geo_mun_raw[, c("code_muni", "name_muni", "geom")]
names(geo_mun_sf)[names(geo_mun_sf) == "geom"] <- "geometry"
sf::st_geometry(geo_mun_sf) <- "geometry"

# IBGE entrega `code_muni` com 7 dígitos; o app/SUS usa 6.
# `substr(., 1, 6)` é a chave para casar com `regional_sus_map$mun_code6`.
# (O 7º dígito é o verificador — descartar é seguro.)
geo_mun_sf$mun_code6 <- substr(as.character(geo_mun_sf$code_muni), 1, 6)

# Join com hierarquia SUS (left join: preserva polígonos sem hierarquia
# cadastrada, embora idealmente todos cadastrem). `as.data.frame(mun_hier)`
# evita conflitos sf×data.table no merge.
mun_hier_df <- as.data.frame(mun_hier)
geo_mun_sf  <- merge(geo_mun_sf, mun_hier_df, by = "mun_code6", all.x = TRUE)

# Simplificar e salvar (mesmos parâmetros 0.05/keep_shapes do bloco 1).
geo_municipios <- rmapshaper::ms_simplify(geo_mun_sf, keep = 0.05, keep_shapes = TRUE)
saveRDS(geo_municipios, path_data("geo_municipios.rds"))
message(">> OK: data/geo_municipios.rds — ", nrow(geo_municipios), " municípios")

# ----------------------------------------------------------
# 3) Macrorregiões de saúde (derivado dos municípios)
# ----------------------------------------------------------
# Não há malha oficial de macrorregiões no `geobr`; gerada aqui pela
# união topológica dos municípios pertencentes a cada macro.
# Filtro `!is.na(macro_nome) & nzchar(macro_nome)` é "belt & suspenders":
# remove tanto NAs (municípios sem hierarquia no regional_sus_map) quanto
# strings vazias (defesa extra).
# ----------------------------------------------------------
message(">> Derivando geometrias de macrorregiões ...")
geo_mun_macro <- geo_municipios[
  !is.na(geo_municipios$macro_nome) & nzchar(geo_municipios$macro_nome),
]

# `aggregate.sf` — método específico de sf que aplica `st_union` na geometria
# por chave. Chave = (macro_codigo, macro_nome, uf_sigla):
# o `uf_sigla` na chave é uma proteção extra (em tese uma macro não atravessa
# UF, mas se o regional_sus_map tiver inconsistência, o group-by não as funde).
geo_macrorregioes <- aggregate(
  geo_mun_macro["geometry"],
  by = list(
    macro_codigo = geo_mun_macro$macro_codigo,
    macro_nome   = geo_mun_macro$macro_nome,
    uf_sigla     = geo_mun_macro$uf_sigla
  ),
  FUN = sf::st_union
)
# `aggregate` devolve um `data.frame` com geometria — re-promovemos a sf.
geo_macrorregioes <- sf::st_as_sf(geo_macrorregioes)
geo_macrorregioes <- rmapshaper::ms_simplify(geo_macrorregioes, keep = 0.05, keep_shapes = TRUE)

saveRDS(geo_macrorregioes, path_data("geo_macrorregioes.rds"))
message(">> OK: data/geo_macrorregioes.rds — ", nrow(geo_macrorregioes), " macrorregiões")

# ----------------------------------------------------------
# 4) Regiões de saúde (derivado dos municípios, agrupa por regiao_nome)
# ----------------------------------------------------------
# Mesmo padrão do bloco 3, agora chave = (regiao_codigo, regiao_nome,
# macro_nome, uf_sigla). O `macro_nome` na chave permite herdar a macro
# correspondente em cada região agregada (útil para tooltips e drill-up).
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
