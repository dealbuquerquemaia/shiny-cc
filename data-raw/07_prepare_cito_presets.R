# ===========================================================
# data-raw/07_prepare_cito_presets.R
#
# Gera `data/cito_presets.rds` — lista nomeada com parâmetros do
# modelo de citologia por fonte (`inca2019`, `siscan`) e, no caso
# do SISCAN, também por UF. É o consumido pela sidebar (`mod_filters`)
# quando o usuário escolhe "Cytology" como método de rastreamento.
#
# Escopo: script standalone (não dá `source` no app). Roda manualmente
# ou via `06_make_all_cc.R` (atualmente NÃO está no pipeline mestre —
# ver Observações).
#
# Entrada (em `data-raw/`):
#   - tabela_uf_prest_categorias.xlsx — exportação SISCAN 2022–2024
#     (1 linha por UF + linhas-totais com `uf_prest = NA` que são
#     descartadas). Colunas usadas: `uf_prest`, `uf_prest_num`,
#     `Total`, `insatisfatoria_rejeitada`, `ASC-H+`,
#     `outras_alteracoes`, `Negativo`.
#
# Saída (em `data/`):
#   - cito_presets.rds — lista de 2 níveis:
#       cito_presets$inca2019$brasil   → list (15 params, fonte INCA 2019)
#       cito_presets$siscan$brasil     → list (15 params: 4 SISCAN + 11 INCA)
#       cito_presets$siscan$<uf_nome>  → list (15 params: 4 SISCAN UF + 11 INCA)
#     Onde <uf_nome> é o nome SEM acento (ex.: "Sao Paulo", "Para").
#
# Parâmetros derivados do SISCAN (variam por UF — calculados aqui):
#   unsatisfactory_pct, res_asch_pct, res_other_pct, res_neg_pct
#
# Parâmetros SEM dados SISCAN por UF (usam INCA 2019 como fallback
# para todas as UFs — atualizar aqui quando disponível):
#   first_time_pct, colpo_asch_pct, colpo_other_follow_pct,
#   biopsy_pos_asch_pct, biopsy_pos_other_pct,
#   b_asch_nic23_pct, b_asch_cancer_pct, b_asch_neg_nic1_pct,
#   b_other_nic23_pct, b_other_cancer_pct, b_other_neg_nic1_pct
#
# Observações:
#   - Mapeamento de nomes acento→sem-acento é hard-coded em `uf_name_map`
#     (27 UFs). Casa com `pop_municipio_regional$UF` (sem acento) — se
#     algum dia o pipeline BR mudar para preservar acento, sincronizar aqui.
#   - "Brasil SISCAN" é re-derivado por SOMA de contagens das UFs (não
#     usa eventual linha-total da planilha) — garante aditividade.
#   - `make_params(siscan_part, fallback = inca2019)` apenas SOBRESCREVE
#     no fallback as 4 chaves SISCAN: os outros 11 campos vêm do INCA.
#   - Os metadados de label/UI dos presets (`label`, `por_uf`) ficam em
#     `R/00_constants_cc.R` (`CITO_PRESETS_META`); este script só popula
#     o `.rds` com os valores numéricos.
# ===========================================================

library(readxl)

# -----------------------------------------------------------
# 0) Parâmetros INCA 2019 (base / fallback para tudo sem dados SISCAN)
#
# Conjunto completo dos 15 parâmetros do modelo de citologia. Usado
# de duas formas:
#   (a) Preset "inca2019" puro (consumido como referência nacional).
#   (b) Fallback: o `make_params()` substitui apenas as 4 chaves
#       SISCAN nesta lista, mantendo os outros 11 valores nacionais.
#
# Valores em PERCENTUAL (escala 0–100). Fonte: ENCC/INCA 2019.
# -----------------------------------------------------------
inca2019 <- list(
  first_time_pct         = 6.0,
  unsatisfactory_pct     = 1.2,
  res_asch_pct           = 1.4,
  res_other_pct          = 2.8,
  res_neg_pct            = 95.8,
  colpo_asch_pct         = 100.0,
  colpo_other_follow_pct = 20.9,
  biopsy_pos_asch_pct    = 33.3,
  biopsy_pos_other_pct   = 33.3,
  b_asch_nic23_pct       = 70.0,
  b_asch_cancer_pct      = 15.0,
  b_asch_neg_nic1_pct    = 15.0,
  b_other_nic23_pct      = 70.0,
  b_other_cancer_pct     = 15.0,
  b_other_neg_nic1_pct   = 15.0
)

# -----------------------------------------------------------
# 1) Leitura e processamento do SISCAN
#    Fonte: SISCAN 2022–2024 (tabela_uf_prest_categorias.xlsx).
#    A planilha vem com 1 linha por UF + linhas adicionais de
#    totais/percentuais (que aparecem com `uf_prest = NA` e são
#    descartadas no filtro logo abaixo).
# -----------------------------------------------------------
raw <- read_excel("data-raw/tabela_uf_prest_categorias.xlsx")

# Descarta linhas de totais/percentuais (mantém só as 27 UFs).
raw <- raw[!is.na(raw$uf_prest), ]

# Mapeamento UF: nome COM acento (SISCAN) → SEM acento (chave usada
# em `pop_municipio_regional$UF` e nos filtros do app). Hard-coded
# para garantir match exato — sem dependência de iconv/stringi.
uf_name_map <- c(
  "Rondônia"            = "Rondonia",
  "Acre"                = "Acre",
  "Amazonas"            = "Amazonas",
  "Roraima"             = "Roraima",
  "Pará"                = "Para",
  "Amapá"               = "Amapa",
  "Tocantins"           = "Tocantins",
  "Maranhão"            = "Maranhao",
  "Piauí"               = "Piaui",
  "Ceará"               = "Ceara",
  "Rio Grande do Norte" = "Rio Grande do Norte",
  "Paraíba"             = "Paraiba",
  "Pernambuco"          = "Pernambuco",
  "Alagoas"             = "Alagoas",
  "Sergipe"             = "Sergipe",
  "Bahia"               = "Bahia",
  "Minas Gerais"        = "Minas Gerais",
  "Espírito Santo"      = "Espirito Santo",
  "Rio de Janeiro"      = "Rio de Janeiro",
  "São Paulo"           = "Sao Paulo",
  "Paraná"              = "Parana",
  "Santa Catarina"      = "Santa Catarina",
  "Rio Grande do Sul"   = "Rio Grande do Sul",
  "Mato Grosso do Sul"  = "Mato Grosso do Sul",
  "Mato Grosso"         = "Mato Grosso",
  "Goiás"               = "Goias",
  "Distrito Federal"    = "Distrito Federal"
)

# Calcula os 4 parâmetros SISCAN a partir de contagens absolutas:
#   - unsatisfactory_pct: % insatisfatórios sobre o TOTAL.
#   - res_asch_pct / res_other_pct / res_neg_pct: % sobre os
#     SATISFATÓRIOS (total − insatisfatórios), seguindo a
#     definição usada pelo modelo (`cc_workup_metrics`).
# Retorna lista nomeada de 4 entradas (numéricas, escala 0–100,
# arredondadas a 3 casas decimais).
calc_siscan_params <- function(asch, outras, negativo, insatisf, total) {
  satisf        <- unname(total - insatisf)
  unsatisf_pct  <- unname(insatisf / total * 100)
  res_asch_pct  <- unname(asch     / satisf * 100)
  res_other_pct <- unname(outras   / satisf * 100)
  res_neg_pct   <- unname(negativo / satisf * 100)
  list(
    unsatisfactory_pct = round(unsatisf_pct,  3),
    res_asch_pct       = round(res_asch_pct,  3),
    res_other_pct      = round(res_other_pct, 3),
    res_neg_pct        = round(res_neg_pct,   3)
  )
}

# Monta o conjunto completo de 15 parâmetros combinando:
#   - `siscan_part` — lista com as 4 chaves SISCAN (saída de `calc_siscan_params`);
#   - `fallback`    — INCA 2019 (default), de onde vêm os 11 campos restantes.
# A operação é uma sobrescrita SELETIVA: só os nomes em `siscan_part`
# substituem o fallback. Isso preserva os outros 11 campos no INCA
# até que dados SISCAN por UF estejam disponíveis (ver header).
make_params <- function(siscan_part, fallback = inca2019) {
  p <- fallback
  for (nm in names(siscan_part)) p[[nm]] <- siscan_part[[nm]]
  p
}

# -----------------------------------------------------------
# 2) Brasil total (soma das UFs)
#    Re-derivado por SOMA das contagens das 27 UFs (não usa eventual
#    linha-total da planilha) para garantir aditividade: o "Brasil"
#    SISCAN bate com a agregação que o app faria ao selecionar todas
#    as UFs.
# -----------------------------------------------------------
br_totals <- colSums(raw[, c("Total", "insatisfatoria_rejeitada",
                              "ASC-H+", "outras_alteracoes", "Negativo")])

siscan_br <- calc_siscan_params(
  asch    = br_totals["ASC-H+"],
  outras  = br_totals["outras_alteracoes"],
  negativo = br_totals["Negativo"],
  insatisf = br_totals["insatisfatoria_rejeitada"],
  total    = br_totals["Total"]
)

# -----------------------------------------------------------
# 3) Por UF
#    Itera linha-a-linha, calcula os 4 params SISCAN da UF e empacota
#    com fallback INCA. Se aparecer UF não mapeada (ex.: typo na fonte
#    ou nova divisão administrativa), emite `warning` e segue — não
#    interrompe a geração das outras UFs.
# -----------------------------------------------------------
siscan_por_uf <- list()

for (i in seq_len(nrow(raw))) {
  uf_acento <- raw$uf_prest_num[i]
  uf_key    <- uf_name_map[uf_acento]

  if (is.na(uf_key)) {
    warning("UF sem mapeamento: ", uf_acento)
    next
  }

  siscan_uf <- calc_siscan_params(
    asch     = raw$`ASC-H+`[i],
    outras   = raw$outras_alteracoes[i],
    negativo = raw$Negativo[i],
    insatisf = raw$insatisfatoria_rejeitada[i],
    total    = raw$Total[i]
  )

  siscan_por_uf[[uf_key]] <- make_params(siscan_uf)
}

# -----------------------------------------------------------
# 4) Monta objeto final
#    Estrutura espelha `CITO_PRESETS_META` (em `00_constants_cc.R`):
#      - inca2019.por_uf = FALSE → só `$brasil`
#      - siscan.por_uf   = TRUE  → `$brasil` + 1 entrada por UF
#    O lookup no app é: cito_presets[[source]][[uf_or_brasil]].
# -----------------------------------------------------------
cito_presets <- list(

  inca2019 = list(
    brasil = inca2019
    # sem subdivisão por UF: INCA 2019 aplica o mesmo valor nacional em qualquer seleção
  ),

  siscan = c(
    list(brasil = make_params(siscan_br)),
    siscan_por_uf
  )

)

# -----------------------------------------------------------
# 5) Salva
#    `saveRDS` sem `compress = "xz"` aqui (objeto pequeno, < 50 KB)
#    — re-executar é idempotente (sobrescreve sem efeito colateral
#    fora de `data/cito_presets.rds`).
# -----------------------------------------------------------
saveRDS(cito_presets, "data/cito_presets.rds")

cat("cito_presets.rds gerado com sucesso.\n")
cat("Fontes disponíveis:", paste(names(cito_presets), collapse = ", "), "\n")
cat("UFs no SISCAN:", length(siscan_por_uf), "\n")
cat("Campos por preset:", length(inca2019), "\n")
