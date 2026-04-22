# ===========================================================
# data-raw/09_prepare_peers.R
#
# Gera data/peers.rds — tabela mestre para a aba Peer Analysis.
#
# Conteúdo:
#   - Features de clustering por município (pop + faixa etária)
#   - Grupo PAM (k ótimo por silhouette) + grupo_label
#   - Necessidade estimada anual (parâmetros INCA 2019)
#   - Produção real (SIA, geo_ref = "res")
#   - Scores por procedimento e score_geral
#   - Ranking dentro do grupo, métricas de gap, flag prioritario
#
# Rodar antes de iniciar o app para gerar/atualizar o arquivo.
# ===========================================================

library(data.table)
library(cluster)

# -----------------------------------------------------------
# 1) Carregar dados
# -----------------------------------------------------------
pop_mun <- readRDS("data/pop_municipio_regional.rds")
sia     <- readRDS("data/sus_proc_resumo.rds")
cito_p  <- readRDS("data/cito_presets.rds")

setDT(pop_mun)
setDT(sia)

# Inspeção defensiva — garante que mapeamentos estão corretos
cat("=== pop_municipio_regional colunas ===\n")
cat(names(pop_mun), sep = ", "); cat("\n")

cat("\n=== faixas etárias disponíveis ===\n")
print(unique(pop_mun[, .(faixa, from, to)])[order(from)])

cat("\n=== sia geo_ref valores únicos ===\n")
print(unique(sia$geo_ref))

cat("\n=== sia categoria valores únicos ===\n")
print(unique(sia$categoria))

# -----------------------------------------------------------
# 2) Agregar população por município — faixa 25–64
# -----------------------------------------------------------

pop_alvo <- pop_mun[from >= 25 & to <= 64,
  .(
    pop_alvo_total = sum(pop_total),
    pop_sus_total  = sum(pop_sus)
  ),
  by = .(
    cod_municipio  = mun_code6,
    nome_municipio = mun_nome,
    uf             = uf_sigla,
    nome_regiao    = regiao_nome,
    nome_macro     = macro_nome
  )
]

# Bandas etárias (proporção do pop_alvo_total)
pop_25_34 <- pop_mun[from >= 25 & to <= 34,
  .(pop_25_34 = sum(pop_total)), by = .(cod_municipio = mun_code6)]

pop_35_49 <- pop_mun[from >= 35 & to <= 49,
  .(pop_35_49 = sum(pop_total)), by = .(cod_municipio = mun_code6)]

pop_50_64 <- pop_mun[from >= 50 & to <= 64,
  .(pop_50_64 = sum(pop_total)), by = .(cod_municipio = mun_code6)]

# Merge das bandas
mun_pop <- pop_alvo[pop_25_34, on = "cod_municipio", nomatch = NA]
mun_pop <- mun_pop[pop_35_49, on = "cod_municipio", nomatch = NA]
mun_pop <- mun_pop[pop_50_64, on = "cod_municipio", nomatch = NA]

# Proporções
mun_pop[, prop_sus   := pop_sus_total  / pop_alvo_total]
mun_pop[, prop_25_34 := pop_25_34 / pop_alvo_total]
mun_pop[, prop_35_49 := pop_35_49 / pop_alvo_total]
mun_pop[, prop_50_64 := pop_50_64 / pop_alvo_total]

# Remover municípios com dados incompletos ou população zero
feats_cols <- c("pop_alvo_total", "pop_sus_total",
                "prop_sus", "prop_25_34", "prop_35_49", "prop_50_64")
mun_pop <- mun_pop[complete.cases(mun_pop[, ..feats_cols])]
mun_pop <- mun_pop[pop_alvo_total > 0]

cat(sprintf("\nMunicípios válidos para clustering: %d\n", nrow(mun_pop)))

# -----------------------------------------------------------
# 3) Clustering PAM — k ótimo por silhouette médio
# -----------------------------------------------------------

features   <- as.matrix(mun_pop[, ..feats_cols])
feat_scaled <- scale(features)

set.seed(42)

# Pré-computar a matriz de distâncias uma vez (usada em todas as iterações)
cat("Calculando matriz de distâncias...\n")
diss_mat <- dist(feat_scaled)

k_range  <- 3:12
sil_vals <- setNames(numeric(length(k_range)), as.character(k_range))

cat("Calculando silhouette para k = 3..12 (pode levar alguns minutos)...\n")

for (k in k_range) {
  cat(sprintf("  k = %d... ", k)); flush.console()
  pam_k <- pam(diss_mat, k = k, diss = TRUE, nstart = 5)
  sil_k  <- silhouette(pam_k$clustering, diss_mat)
  sil_vals[as.character(k)] <- mean(sil_k[, "sil_width"])
  cat("ok\n"); flush.console()
}

cat("\nSilhouette médio por k:\n")
print(round(sil_vals, 4))

k_opt <- as.integer(names(which.max(sil_vals)))
cat(sprintf("k ótimo selecionado: %d\n", k_opt))

pam_final <- pam(feat_scaled, k = k_opt, nstart = 5)
mun_pop[, grupo := as.integer(pam_final$clustering)]

# -----------------------------------------------------------
# 4) Labels de grupo baseados nos centroides
# -----------------------------------------------------------

centroids <- mun_pop[, .(
  pop_alvo_med = median(pop_alvo_total),
  prop_sus_med = median(prop_sus)
), by = grupo][order(pop_alvo_med)]

centroids[, size_rank       := seq_len(.N)]
centroids[, grupo_label     := paste0("Group ", size_rank)]
centroids[, grupo_size_rank := size_rank]

# Merge labels de volta
mun_pop <- centroids[, .(grupo, grupo_label, grupo_size_rank)][mun_pop, on = "grupo"]

cat("\nDistribuição por grupo:\n")
print(mun_pop[, .N, by = .(grupo, grupo_label)][order(grupo)])

# -----------------------------------------------------------
# 5) Necessidade estimada anual (parâmetros INCA 2019)
# -----------------------------------------------------------

inca <- cito_p$inca2019$brasil

# Cobertura-alvo de referência (mesma lógica usada no summary: pop_sel * cov)
cov_ref <- 0.70

# Parâmetros de volume da citologia (mesmos do engine em 02_engine_capacity_cc.R)
ft  <- inca$first_time_pct     / 100   # % exames de primeira vez
uns <- inca$unsatisfactory_pct / 100   # % exames insatisfatórios (repetição)

# Taxa de encaminhamento para colposcopia (por citologia realizada)
taxa_colpo <- (
  inca$res_asch_pct  * inca$colpo_asch_pct         +
  inca$res_other_pct * inca$colpo_other_follow_pct
) / 1e4

# Taxa de biópsia (por colposcopia — média ponderada pelos dois braços)
num_b <- (inca$biopsy_pos_asch_pct  * inca$res_asch_pct  * inca$colpo_asch_pct +
          inca$biopsy_pos_other_pct * inca$res_other_pct * inca$colpo_other_follow_pct)
den_b <- (inca$res_asch_pct  * inca$colpo_asch_pct +
          inca$res_other_pct * inca$colpo_other_follow_pct)
taxa_biopsia <- (num_b / den_b) / 100

# Taxa NIC2+ (por biópsia — média ponderada pelos dois braços)
num_n <- (inca$b_asch_nic23_pct  * inca$biopsy_pos_asch_pct  * inca$res_asch_pct  * inca$colpo_asch_pct +
          inca$b_other_nic23_pct * inca$biopsy_pos_other_pct * inca$res_other_pct * inca$colpo_other_follow_pct)
den_n <- (inca$biopsy_pos_asch_pct  * inca$res_asch_pct  * inca$colpo_asch_pct +
          inca$biopsy_pos_other_pct * inca$res_other_pct * inca$colpo_other_follow_pct)
taxa_nic2mais <- (num_n / den_n) / 100

cat(sprintf(
  "\nTaxas INCA 2019:\n  colpo/cito    = %.5f\n  biopsia/colpo = %.4f\n  nic2+/biopsia = %.4f\n",
  taxa_colpo, taxa_biopsia, taxa_nic2mais
))

cat(sprintf(
  "Parâmetros de volume (INCA 2019):\n  cobertura-alvo      = %.0f%%\n  first_time_pct (ft) = %.1f%%\n  unsatisf_pct  (uns) = %.1f%%\n",
  cov_ref * 100, ft * 100, uns * 100
))

# Necessidade anual por município — mesma fórmula do engine (cc_engine_run):
#   eligible   = pop_alvo * cobertura
#   need_cito  = ((E / 3) + (E * ft)) * (1 + uns)
mun_pop[, eligible_sus := pop_sus_total * cov_ref]
mun_pop[, need_cito    := ((eligible_sus / 3) + (eligible_sus * ft)) * (1 + uns)]
mun_pop[, need_colpo   := need_cito    * taxa_colpo]
mun_pop[, need_biopsia := need_colpo   * taxa_biopsia]
mun_pop[, need_ezt     := need_biopsia * taxa_nic2mais]

# -----------------------------------------------------------
# 6) Produção real (SIA, geo_ref = "res")
# -----------------------------------------------------------

sia_res <- sia[geo_ref == "res"]

# Agregar por município e categoria
agg_sia <- function(cat_name) {
  sia_res[categoria == cat_name,
    .(val = sum(total_all, na.rm = TRUE)),
    by = .(cod_municipio = geo_id)
  ]
}

sia_cito    <- agg_sia("citologia")
sia_colpo   <- agg_sia("colposcopia")
sia_biopsia <- agg_sia("biopsia")
sia_ezt     <- agg_sia("tratamento")

cat(sprintf(
  "\nMunicípios com produção real (SIA/res): cito=%d, colpo=%d, biopsia=%d, ezt=%d\n",
  nrow(sia_cito), nrow(sia_colpo), nrow(sia_biopsia), nrow(sia_ezt)
))

# Left join: manter todos os municípios do mun_pop
peers <- copy(mun_pop)
peers[sia_cito,    real_cito    := i.val, on = "cod_municipio"]
peers[sia_colpo,   real_colpo   := i.val, on = "cod_municipio"]
peers[sia_biopsia, real_biopsia := i.val, on = "cod_municipio"]
peers[sia_ezt,     real_ezt     := i.val, on = "cod_municipio"]

# Municípios sem registro no SIA recebem produção zero (não são excluídos do score)
peers[is.na(real_cito),    real_cito    := 0]
peers[is.na(real_colpo),   real_colpo   := 0]
peers[is.na(real_biopsia), real_biopsia := 0]
peers[is.na(real_ezt),     real_ezt     := 0]

# -----------------------------------------------------------
# 7) Scores por procedimento e score geral
# -----------------------------------------------------------

score_col <- function(real, need) pmin(real / need, 1)

peers[, score_cito    := score_col(real_cito,    need_cito)]
peers[, score_colpo   := score_col(real_colpo,   need_colpo)]
peers[, score_biopsia := score_col(real_biopsia, need_biopsia)]
peers[, score_ezt     := score_col(real_ezt,     need_ezt)]

# Tratar Inf/NaN (need = 0 ou real/need impossível)
score_cols <- c("score_cito", "score_colpo", "score_biopsia", "score_ezt")
for (col in score_cols) {
  peers[!is.finite(get(col)), (col) := NA_real_]
}

# Score geral: na.rm = TRUE pois NAs só ocorrem quando need = 0 (município sem pop)
peers[, score_geral := rowMeans(.SD, na.rm = TRUE), .SDcols = score_cols]

# -----------------------------------------------------------
# 8) Ranking dentro do grupo
# -----------------------------------------------------------

peers[, rank_grupo    := frank(score_geral, ties.method = "min", na.last = "keep"),
      by = grupo]
peers[, n_grupo       := .N, by = grupo]
peers[, mediana_grupo := median(score_geral, na.rm = TRUE), by = grupo]

# Threshold dos 25% melhores performers do grupo
# (como score maior = melhor cobertura, top-25% = percentil 75 do score)
peers[, p_top25_grupo := quantile(score_geral, 0.75, na.rm = TRUE), by = grupo]

peers[, gap_mediana := mediana_grupo  - score_geral]
peers[, gap_top25   := p_top25_grupo  - score_geral]

# Prioritário: pior desempenho (menor rank = pior: rank 1 = menor score)
# rank crescente: 1 = menor score_geral = pior
peers[, prioritario := !is.na(rank_grupo) & rank_grupo <= pmax(5L, ceiling(n_grupo * 0.10))]

cat(sprintf(
  "\nResumo final:\n  Total municípios : %d\n  Com score_geral  : %d\n  Prioritários     : %d\n",
  nrow(peers),
  sum(!is.na(peers$score_geral)),
  sum(peers$prioritario, na.rm = TRUE)
))

# -----------------------------------------------------------
# 9) Salvar
# -----------------------------------------------------------
saveRDS(peers, "data/peers.rds")
cat("\ndata/peers.rds gerado com sucesso.\n")
cat("Colunas:\n"); cat(names(peers), sep = ", "); cat("\n")
