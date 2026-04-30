# ===========================================================
# data-raw/09_prepare_peers.R
#
# Gera data/peers.rds — tabela mestre para a aba Peer Analysis.
#
# Escopo:
#   Pipeline standalone que constrói "peer groups" de municípios
#   brasileiros (clusters PAM por estrutura demográfica + perfil SUS) e
#   atribui scores de cobertura por procedimento. Saída é uma tabela
#   plana 1 linha = 1 município, com tudo pré-calculado para a aba
#   `mod_peers` consumir sem fazer cluster em runtime.
#
# Entrada (em `data/`):
#   - pop_municipio_regional.rds  (gerado em 02_prepare_BR.R)
#   - sus_proc_resumo.rds         (gerado em 03_prepare_SUS.R; objeto
#                                  histórico chamado `sia_cc_resumo`
#                                  em app.R)
#   - cito_presets.rds            (gerado em 07_prepare_cito_presets.R;
#                                  usado só para puxar params INCA 2019)
#
# Saída (em `data/`):
#   - peers.rds                   (data.table municipal — schema final
#                                  na seção 9 do script)
#
# Conteúdo da saída:
#   - Features de clustering por município (pop_alvo, % SUS-dependente,
#     proporções por banda etária 25-34/35-49/50-64).
#   - Grupo PAM (k ótimo selecionado por silhouette médio) + label
#     legível ordenado por tamanho ("Group 1" = menor).
#   - Necessidade anual estimada (cenário INCA 2019, cobertura 70%,
#     intervalo de citologia 3 anos) por procedimento.
#   - Produção real (SIA — visão "res" = município de residência).
#   - Scores 0–1 por procedimento (real/need com cap 1) + score_geral
#     (média dos 4 procedimentos, ignorando NAs onde need=0).
#   - Ranking dentro do grupo + gap vs mediana + gap vs top-25%.
#   - Flag `prioritario` (10% piores do grupo, mínimo 5 municípios).
#
# Observações importantes:
#   - É a primeira tabela do projeto que mistura clustering não-
#     supervisionado com cenário epi pré-calculado. A sidebar do app
#     NÃO afeta peers (cenário é fixo) — decisão de UX para que
#     grupos peer permaneçam estáveis entre sessões.
#   - Cenário hard-coded é INCA 2019 / cobertura 70% / faixa 25–64 /
#     intervalo de citologia 3 anos. Se mudar parâmetros INCA aqui,
#     todos os scores municipais mudam — re-rodar o script.
#   - É um dos 4 standalones NÃO incluídos em `06_make_all_cc.R`
#     (junto com 04, 05, 07, 08). Para regerar: chamar `source(...)`
#     direto, com cwd na raiz e DEPOIS de 02 + 03 + 07.
#   - Custo de execução: ~2–5 min (pré-cálculo da matriz de distância
#     dominante; PAM em si é rápido). Idempotente.
# ===========================================================

library(data.table)
library(cluster)

# Helper compartilhado com o engine: cc_workup_volumes() consolida
# a fórmula `((E/ciclo) + (E*ft)) * (1+uns)`. Source aqui evita
# drift com R/02_engine_capacity_cc.R. cwd esperado = raiz do projeto.
source("R/02_engine_capacity_cc.R")

# -----------------------------------------------------------
# 1) Carregar dados
#
# As 3 bases vêm de scripts ETL anteriores. `setDT` é idempotente
# (no-op se já for data.table). Os 4 `cat()`/`print()` seguintes são
# inspeções defensivas — não afetam a saída, mas servem para detectar
# rapidamente mudanças de schema (faixa nova, geo_ref novo, categoria
# nova) antes que o pipeline silencie a falha em joins downstream.
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
#
# Constrói a tabela `mun_pop` (1 linha por município) com:
#   - `pop_alvo_total`: soma da pop feminina IBGE 25–64
#   - `pop_sus_total` : soma da pop SUS-dependente (IBGE - ANS, clamp ≥ 0)
#   - 3 bandas etárias (25-34 / 35-49 / 50-64) — derivadas e convertidas
#     em PROPORÇÃO sobre `pop_alvo_total` (perfil etário do município).
#   - `prop_sus`     : % da pop-alvo que depende exclusivamente do SUS.
#
# Por que proporções (e não absolutos)? Para que o clustering capture
# perfil estrutural do município (jovem vs envelhecido; SUS vs misto)
# independente do tamanho. O tamanho entra separado via `pop_alvo_total`
# e `pop_sus_total` em escala absoluta — `scale()` no bloco 3 normaliza.
#
# Faixa 25–64 é hard-coded — coerente com cenário fixo INCA documentado
# no header. Não responde aos sliders da sidebar (decisão de UX).
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
# (PAM falha com NAs; município com pop_alvo=0 não tem cenário de necessidade)
feats_cols <- c("pop_alvo_total", "pop_sus_total",
                "prop_sus", "prop_25_34", "prop_35_49", "prop_50_64")
mun_pop <- mun_pop[complete.cases(mun_pop[, ..feats_cols])]
mun_pop <- mun_pop[pop_alvo_total > 0]

cat(sprintf("\nMunicípios válidos para clustering: %d\n", nrow(mun_pop)))

# -----------------------------------------------------------
# 3) Clustering PAM — k ótimo por silhouette médio
#
# `scale()` z-score em cada coluna (média 0, sd 1) — necessário porque
# `pop_alvo_total` está em milhares e as proporções em [0, 1]; sem
# scaling, a distância seria dominada por pop.
#
# `set.seed(42)` torna a clusterização determinística (PAM tem nstart
# aleatório).
#
# Estratégia: pré-computa `dist(feat_scaled)` UMA vez (operação O(n²),
# cara para ~5,5k municípios) e reusa em todas as iterações de PAM.
# `pam(diss = TRUE)` aceita matriz de distância pronta. Sem isso, cada
# `pam()` recomputaria a matriz internamente.
#
# Loop k = 3..12 com `silhouette()` médio como métrica de qualidade.
# `which.max` seleciona o k com maior silhouette médio (separação entre
# grupos / coesão interna). Faixa 3..12 é heurística — abaixo de 3 o
# clustering vira "grandes/médios/pequenos"; acima de 12 grupos ficam
# tão pequenos que o ranking interno perde sentido.
#
# `pam_final` re-roda PAM com k_opt usando a matriz `feat_scaled` (não
# a `diss_mat`) — assim o objeto retornado tem `medoids` interpretáveis
# em escala original via `feat_scaled[medoids, ]`.
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
#
# `grupo` (saída do PAM) é um int aleatório (1..k_opt) sem semântica
# de ordem. Aqui ele é re-rotulado como "Group 1..k" ORDENADO POR
# TAMANHO MEDIANO — "Group 1" sempre é o de menores municípios e
# "Group k_opt" o de maiores. Estabilidade entre re-runs (com mesmo
# seed) é garantida pela ordem fixa de PAM + ordenação determinística
# aqui.
#
# `prop_sus_med` é calculada para inspeção visual (não vai pra label
# final), facilita avaliar se o ordenamento por tamanho também
# correlaciona com perfil SUS.
#
# `grupo_size_rank` redundante com `size_rank` — mantido como coluna
# explícita no schema final por compat com o módulo (que filtra/
# ordena por essa coluna).
# -----------------------------------------------------------

centroids <- mun_pop[, .(
  pop_alvo_med = median(pop_alvo_total),
  prop_sus_med = median(prop_sus)
), by = grupo][order(pop_alvo_med)]

centroids[, size_rank       := seq_len(.N)]
centroids[, grupo_label     := paste0("Group ", size_rank)]
centroids[, grupo_size_rank := size_rank]

# Merge labels de volta — `right join` (centroids[mun_pop]) preserva
# todas as linhas de `mun_pop` sem perder municípios.
mun_pop <- centroids[, .(grupo, grupo_label, grupo_size_rank)][mun_pop, on = "grupo"]

cat("\nDistribuição por grupo:\n")
print(mun_pop[, .N, by = .(grupo, grupo_label)][order(grupo)])

# -----------------------------------------------------------
# 5) Necessidade estimada anual (parâmetros INCA 2019)
#
# Replica a lógica do engine (`cc_engine_run` + `cc_workup_metrics`)
# em forma vetorizada — para cada município, calcula necessidade anual
# de citologias, colposcopias, biópsias e EZTs.
#
# Cenário:
#   - `cov_ref = 0.70` (cobertura 70%, hard-coded — ver header)
#   - INCA 2019 nacional para todos os parâmetros (mesma lookup que o
#     preset `cito_p$inca2019$brasil` em runtime)
#   - Intervalo 3 anos (citologia padrão), expresso pelo divisor `/3`
#     na fórmula `((E/3) + (E*ft)) * (1+uns)`
#
# `taxa_*` são taxas EFETIVAS médias ponderadas pelos 2 braços do
# fluxo cito (ASC-H+ vs outras alterações), porque INCA 2019 separa
# positividade de colposcopia/biópsia/NIC2+ por braço. A média
# ponderada é matematicamente equivalente a aplicar cada braço
# separado e somar — escolhe-se o atalho aqui porque o output só
# precisa de valores totais por município.
#
# IMPORTANTE: se a fórmula no engine mudar (`02_engine_capacity_cc.R`
# blocos 5–6), atualizar aqui — ATÉ existir um helper compartilhado
# (item de débito técnico já listado nas observações cruzadas).
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
#   eligible   = pop_sus * cobertura
#   need_cito  = cc_workup_volumes(E, params = {ciclo=3, ft, uns})$volume_anual
#     → 1ª vez no programa: E*ft (todas no ano de entrada)
#     → rastreio de manutenção: E/3 (intervalo 3 anos)
#     → multiplica (1+uns) para repor exames insatisfatórios
# `pop_sus_total` (não `pop_alvo_total`) é a base do denominador —
# coerente com a lógica do app: necessidade SUS, não total.
mun_pop[, eligible_sus := pop_sus_total * cov_ref]
mun_pop[, need_cito    := cc_workup_volumes(
  eligible = eligible_sus,
  params   = list(ciclo = 3, ft = ft, uns = uns)
)$volume_anual]
mun_pop[, need_colpo   := need_cito    * taxa_colpo]
mun_pop[, need_biopsia := need_colpo   * taxa_biopsia]
mun_pop[, need_ezt     := need_biopsia * taxa_nic2mais]

# -----------------------------------------------------------
# 6) Produção real (SIA, geo_ref = "res")
#
# Filtra SIA pela visão "res" (município de RESIDÊNCIA), porque o
# objetivo é medir cobertura DA POPULAÇÃO de cada município —
# independentemente de onde foi atendida. A visão "care" (atendimento)
# inflaria municípios-polo (referência regional) e zeraria os de
# residência sem rede própria.
#
# `total_all` (todas as idades) é usado em vez de `total_25_64`
# porque o SIA não restringe rastreio à faixa-alvo no campo de idade
# (mulheres fora da faixa também fazem citologia rotina). A
# necessidade `need_*` calculada em (5) é restrita a 25–64 — então o
# `score = real/need` pode passar de 1 (município com rastreio "fora
# da faixa" tem oferta excedente). O `pmin(., 1)` em (7) faz o cap.
# -----------------------------------------------------------

sia_res <- sia[geo_ref == "res"]

# Agregar por município e categoria — closure sobre `sia_res` (não
# precisa passar como argumento). Uma chamada por categoria de procedimento.
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
# `copy(mun_pop)` evita modificar o objeto original por referência
# (data.table é mutável; sem `copy`, alterações em `peers` afetariam
# `mun_pop` em variável separada). O update-join (`X[Y, col := i.col]`)
# cria as 4 colunas `real_*` sem aumentar nº de linhas.
peers <- copy(mun_pop)
peers[sia_cito,    real_cito    := i.val, on = "cod_municipio"]
peers[sia_colpo,   real_colpo   := i.val, on = "cod_municipio"]
peers[sia_biopsia, real_biopsia := i.val, on = "cod_municipio"]
peers[sia_ezt,     real_ezt     := i.val, on = "cod_municipio"]

# Municípios sem registro no SIA recebem produção zero (não são excluídos do score)
# Ausência no SIA = "0 procedimentos realizados" — interpretação
# adequada (município sem oferta nenhuma vs missing). Sem isso, score
# seria NA e o município sumiria do ranking.
peers[is.na(real_cito),    real_cito    := 0]
peers[is.na(real_colpo),   real_colpo   := 0]
peers[is.na(real_biopsia), real_biopsia := 0]
peers[is.na(real_ezt),     real_ezt     := 0]

# -----------------------------------------------------------
# 7) Scores por procedimento e score geral
#
# `score_col(real, need) = pmin(real/need, 1)` — cap em 1 evita que
# municípios com oferta excedente (ver bloco 6 sobre faixa fora 25–64)
# distorçam a média geral pra cima. Score = 1 significa "atende ou
# excede a necessidade".
#
# Tratamento de Inf/NaN: ocorre quando `need = 0` (sem pop SUS-alvo)
# ou `0/0`. Convertido para NA — não pune o município (rowMeans com
# na.rm=TRUE pula esse procedimento na média).
#
# `score_geral = rowMeans` dos 4 procedimentos. Tratamento simétrico
# (cada procedimento pesa 1/4). Se o município tiver TODOS os 4 NA
# (need=0 em tudo, ou seja, pop SUS=0), `score_geral` também vira NA
# e cai fora dos rankings em (8) por `na.last="keep"`.
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
#
# `frank(score_geral, ties.method="min", na.last="keep")`:
#   - rank crescente: 1 = menor score = PIOR cobertura
#   - `ties.method="min"`: empates recebem o menor rank possível
#   - `na.last="keep"`: NA fica NA (não vira último rank)
#
# `gap_mediana` e `gap_top25` são DIFERENÇAS positivas para municípios
# abaixo da referência (mediana ou p75 do grupo). Negativas para os
# acima — útil para visualizar "quanto falta" no app.
#
# `prioritario`: pior desempenho relativo do grupo. Critério: 10% mais
# baixos por rank, com piso de 5 municípios (grupos pequenos não ficam
# sem prioritários). `!is.na(rank_grupo)` garante que municípios com
# score_geral=NA (sem necessidade modelada) não viram prioritários.
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
#
# Sem `compress`: o objeto é poucos MB (5,5k linhas × ~20 colunas), e
# acelerar leitura no startup do app importa mais que o tamanho.
#
# Schema final do `data/peers.rds` (consumido por `mod_peers`):
#   Identificação:
#     - cod_municipio (chr 6 dígitos), nome_municipio, uf,
#       nome_regiao, nome_macro
#   Features de clustering (escala original):
#     - pop_alvo_total, pop_sus_total, prop_sus,
#       prop_25_34, prop_35_49, prop_50_64
#   Grupo:
#     - grupo (int 1..k_opt, ordem aleatória do PAM)
#     - grupo_label ("Group 1..k_opt", ordenado por tamanho)
#     - grupo_size_rank (idem)
#   Cenário INCA 2019 (cobertura 70%, intervalo 3 anos):
#     - eligible_sus, need_cito, need_colpo, need_biopsia, need_ezt
#   Produção real SIA (visão "res", todas idades):
#     - real_cito, real_colpo, real_biopsia, real_ezt
#   Scores (0–1, capped):
#     - score_cito, score_colpo, score_biopsia, score_ezt, score_geral
#   Ranking dentro do grupo:
#     - rank_grupo, n_grupo, mediana_grupo, p_top25_grupo
#     - gap_mediana, gap_top25
#     - prioritario (logical)
# -----------------------------------------------------------
saveRDS(peers, "data/peers.rds")
cat("\ndata/peers.rds gerado com sucesso.\n")
cat("Colunas:\n"); cat(names(peers), sep = ", "); cat("\n")
