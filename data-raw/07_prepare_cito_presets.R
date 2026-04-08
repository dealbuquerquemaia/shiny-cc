# ===========================================================
# data-raw/07_prepare_cito_presets.R
#
# Gera data/cito_presets.rds — lista com parâmetros de citologia
# por fonte (inca2019, siscan) e, no caso do SISCAN, por UF.
#
# Estrutura do objeto gerado:
#   cito_presets$inca2019$brasil   → lista com todos os parâmetros
#   cito_presets$siscan$brasil     → lista com todos os parâmetros
#   cito_presets$siscan$<uf_nome>  → lista com todos os parâmetros
#
# Parâmetros derivados do SISCAN (variam por UF):
#   unsatisfactory_pct, res_asch_pct, res_other_pct, res_neg_pct
#
# Parâmetros SEM dados SISCAN por UF (usam INCA 2019 como fallback
# para todos os estados — atualizar aqui quando disponível):
#   first_time_pct, colpo_asch_pct, colpo_other_follow_pct,
#   biopsy_pos_asch_pct, biopsy_pos_other_pct,
#   b_asch_nic23_pct, b_asch_cancer_pct, b_asch_neg_nic1_pct,
#   b_other_nic23_pct, b_other_cancer_pct, b_other_neg_nic1_pct
# ===========================================================

library(readxl)

# -----------------------------------------------------------
# 0) Parâmetros INCA 2019 (base / fallback para tudo sem dados SISCAN)
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
# 1) Leitura e processamento do SISCAN (tabela_uf_prest_categorias.xlsx)
#    Fonte: SISCAN 2022-2024
# -----------------------------------------------------------
raw <- read_excel("data-raw/tabela_uf_prest_categorias.xlsx")

# Remove linhas de totais/percentuais (uf_prest = NA)
raw <- raw[!is.na(raw$uf_prest), ]

# Mapeamento: nome com acento (SISCAN) → nome sem acento (pop_municipio_regional$UF)
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

# Função: calcula os 4 parâmetros SISCAN a partir de contagens
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

# Função: monta parâmetros completos combinando SISCAN + fallback INCA
make_params <- function(siscan_part, fallback = inca2019) {
  p <- fallback
  for (nm in names(siscan_part)) p[[nm]] <- siscan_part[[nm]]
  p
}

# -----------------------------------------------------------
# 2) Brasil total (soma das UFs)
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
# -----------------------------------------------------------
saveRDS(cito_presets, "data/cito_presets.rds")

cat("cito_presets.rds gerado com sucesso.\n")
cat("Fontes disponíveis:", paste(names(cito_presets), collapse = ", "), "\n")
cat("UFs no SISCAN:", length(siscan_por_uf), "\n")
cat("Campos por preset:", length(inca2019), "\n")
