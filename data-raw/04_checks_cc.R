# ===========================================================
# Shiny-CC — 04_checks_cc.R
# Checagens de sanidade (QA) dos objetos .rds gerados pelos
# scripts 01–03. Roda offline; gera relatórios leves em
# `reports/` (CSV por anomalia + 1 sumário .txt). NÃO modifica
# nenhum dado em `data/` — só lê e diagnostica.
# -----------------------------------------------------------
# Escopo (3 famílias de checagens + sumário):
#   1) Globocan/WPP        — `df_cc_completo`, `df_cc_taxas`
#                            (10 sub-checks: schema, sexo,
#                            faixa etária, NAs, duplicatas,
#                            World, coerência World vs soma,
#                            coerência incidência sheet,
#                            taxas, epi-sem-pop)
#   2) Brasil — IBGE/ANS   — `pop_municipio_faixas`,
#                            `ans_municipio_faixas`,
#                            `pop_municipio_faixas_total_sus`,
#                            `regional_sus_map`,
#                            `pop_municipio_regional`
#                            (5 sub-checks)
#   3) Produção SUS (SIA)  — `sia_cc_completo` (intermediário,
#                            opcional), `sia_cc_resumo`
#                            (2 sub-checks)
#   4) Sumário             — linhas, dimensões únicas etc.
#                            salvo em
#                            `reports/checks_cc_summary.txt`.
#
# Convenção de saída:
#   ok("...")   → mensagem verde no console (sem efeito p/ app)
#   warn("...") → aviso amarelo (anomalia documentada em CSV)
#   fail("...") → erro vermelho via `stop()` — quebra schema
#
# Cada anomalia detectada (NAs, duplicatas, divergências) é
# despejada em `reports/<nome>.csv` para inspeção offline.
# Se o data.table de anomalias estiver vazio, nenhum arquivo
# é escrito.
#
# Observações:
# - Os bases Brasil são opcionais (`required = FALSE`): o
#   script roda mesmo sem elas (aviso, não erro).
# - `sia_cc_completo.rds` (intermediário) é só "ok-presente":
#   conferências de fato são em `sia_cc_resumo.rds`.
# - **Schema unificado**: `expect_cols()` agora é wrapper sobre
#   `cc_check_schema()` (R/01_utils_cc.R), o helper único usado
#   também por 01_prepare_cc.R e R/16_mod_capacidade.R (TASK-05).
# - **Hard-code de faixas etárias** (1.3 e 3.2) duplica
#   `AGE_ORDER` de `R/00_constants_cc.R`. Documentar.
# ===========================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fs)
})

options(stringsAsFactors = FALSE, scipen = 999)

# Helpers compartilhados com o app (cc_check_schema, %||%, etc.).
# `R/01_utils_cc.R` é só definições de função no top-level — nenhum efeito
# colateral; cwd na raiz do projeto.
source("R/01_utils_cc.R")

# -------- Helpers --------------------------------------------------------
# 3 níveis de severidade alinhados a logging tradicional.
# `fail()` é o único que interrompe — usado quando o schema
# está quebrado (impede confiança no `.rds` inteiro).
ok   <- function(msg) cat(sprintf("✔ %s\n", msg))
warn <- function(msg) cat(sprintf("⚠ %s\n", msg))
fail <- function(msg) stop(sprintf("✖ %s", msg), call. = FALSE)

# Asserção de schema. `nm` é só para legenda no console.
# Wrapper local sobre `cc_check_schema()` (R/01_utils_cc.R) — delega a
# validação ao helper unificado e reapresenta o erro via `fail()` para
# preservar o prefixo ✖ no relatório do script. `ok()` é emitido em sucesso.
expect_cols <- function(dt, cols, nm) {
  tryCatch(
    cc_check_schema(dt, cols, nm, on_fail = "stop"),
    error = function(e) fail(conditionMessage(e))
  )
  ok(sprintf("[%s] colunas essenciais presentes", nm))
}

# Salva data.table como CSV em `reports/` (cria pasta na 1ª
# chamada). NULL ou 0 linhas → no-op silencioso (mantém o
# console limpo nos casos em que a checagem passa). Quando
# escreve, emite warn() para dar visibilidade no console.
write_report <- function(dt, file) {
  if (!dir_exists("reports")) dir_create("reports")
  if (is.null(dt) || !nrow(dt)) return(invisible(NULL))
  fwrite(dt, file)
  warn(sprintf("Relatório salvo: %s (%d linhas)", file, nrow(dt)))
}

# Null-coalescing — definido em `R/01_utils_cc.R` (já carregado via
# source no topo). Mantido aqui também por defesa contra reordenação.
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Loader defensivo:
# - `required = TRUE`  → fail() se arquivo ausente.
# - `required = FALSE` → warn() + return(NULL); chamadores
#   testam `is.null(x)` antes de seguir.
# Coage data.frame → data.table (consistência interna do
# script, que usa data.table em todas as agregações).
load_rds_if_exists <- function(path, required = TRUE, nm = NULL) {
  nm <- nm %||% basename(path)
  if (!file.exists(path)) {
    msg <- sprintf("Arquivo não encontrado: %s", path)
    if (required) fail(msg) else { warn(msg); return(NULL) }
  }
  x <- readRDS(path)
  if (is.data.table(x)) return(x)
  if (is.data.frame(x)) return(as.data.table(x))
  x
}



# ===========================================================
# 1) Objetos Globocan/WPP (df_cc_*)
# ===========================================================
# Saídas de `data-raw/01_prepare_cc.R`. São OBRIGATÓRIAS — o
# app não roda sem elas; daí `required = TRUE`. (As bases BR
# ficam atrás de `required = FALSE`.)
# `setDT()` redundante: `load_rds_if_exists` já coage, mas é
# inofensivo e protege contra regressão se o helper mudar.

df_cc_completo <- load_rds_if_exists("data/df_cc_completo.rds",
                                                 required = TRUE,
                                                 nm = "df_cc_completo")
df_cc_taxas    <- load_rds_if_exists("data/df_cc_taxas.rds",
                                                 required = TRUE,
                                                 nm = "df_cc_taxas")

data.table::setDT(df_cc_completo)
data.table::setDT(df_cc_taxas)


ok("Objetos Globocan/WPP carregados.")

# -------- 1.1 Colunas essenciais -----------------------------------------
# Schema canônico das duas saídas. Se uma coluna sumir, é
# `fail()` imediato — o resto do script depende destas colunas.
expect_cols(
  df_cc_completo,
  c("population_code","population_name","sex_code","sex",
    "age_code","age","pop_2022","pop_2025",
    "cancer_code","cancer","type_code","type",
    "year_prediction","prediction","cases_2022"),
  "df_cc_completo"
)

expect_cols(
  df_cc_taxas,
  c("population_code","population_name","sex_code","cancer_code",
    "number_incidence","incidence_crude_rate","incidence_asr_world",
    "number_mortality","mortality_crude_rate","mortality_asr_world"),
  "df_cc_taxas"
)

# -------- 1.2 Sexo (somente Female) --------------------------------------
# A planilha GLOBOCAN é filtrada a Female no script 01.
# Se sex_code != 2 ou rótulo != "Female" → fail (algo deu
# errado no filtro, base inutilizável).
u_sex      <- unique(df_cc_completo$sex)
u_sex_code <- unique(df_cc_completo$sex_code)
if (!all(u_sex %in% c("Female")))  fail(sprintf("Há rótulos de sexo inesperados: %s", paste(u_sex, collapse=", ")))
if (!all(u_sex_code %in% 2L))      fail(sprintf("Há sex_code diferente de 2: %s", paste(u_sex_code, collapse=", ")))
ok("Somente Female presente em df_cc_completo.")

# -------- 1.3 Faixas etárias esperadas -----------------------------------
# Lista hard-coded (18 faixas quinquenais 0–4 ... 85+) que
# DUPLICA `AGE_ORDER` em `R/00_constants_cc.R`. Mantido aqui
# para o script rodar standalone — anotado em observações.
# Falta de faixa = warn (não fail), porque alguns países podem
# não publicar todas as faixas em todos os anos GLOBOCAN.
expected_age <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                  "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
missing_age <- setdiff(expected_age, unique(df_cc_completo$age))
if (length(missing_age)) {
  warn(sprintf("Faixas etárias ausentes em df_cc_completo: %s", paste(missing_age, collapse=", ")))
  write_report(data.table(missing_age = missing_age), "reports/ages_missing.csv")
} else {
  ok("Cobertura completa das faixas etárias esperadas.")
}

# -------- 1.4 NAs e valores não positivos --------------------------------
# 2 problemas distintos exportados em CSVs separados (mais
# fácil tratar separadamente — NA vem de full join, negativo
# de bug de parsing).
na_pop <- df_cc_completo[is.na(pop_2022) | is.na(pop_2025)]
write_report(na_pop, "reports/pop_na.csv")
if (nrow(na_pop) == 0) ok("Sem NAs em pop_2022/pop_2025.")

neg_pop <- df_cc_completo[(pop_2022 < 0) | (pop_2025 < 0)]
write_report(neg_pop, "reports/pop_negative.csv")
if (nrow(neg_pop) == 0) ok("Sem populações negativas.")

# -------- 1.5 Duplicidades de chave --------------------------------------
# Chave canônica: (país × sexo × faixa × câncer × tipo × ano).
# `df_cc_completo` deve ter exatamente 1 linha por chave —
# duplicata indica falha no join do bloco 4 de `01_prepare_cc`.
dup_key <- df_cc_completo[
  , .N, by = .(population_code, sex_code, age_code, cancer_code, type_code, year_prediction)
][N > 1]
write_report(dup_key, "reports/duplicates_epi_key.csv")
if (nrow(dup_key) == 0) ok("Sem duplicatas na chave epidemiológica canônica.")

# -------- 1.6 Presença de World (1001) -----------------------------------
# World (1001) é "país" sintético derivado por agregação no
# script 01. Ausência = aviso (alguns recortes podem omitir).
if (!1001L %in% df_cc_completo$population_code) {
  warn("World (1001) ausente em df_cc_completo.")
} else {
  ok("World (1001) presente em df_cc_completo.")
}

# -------- 1.7 Coerência World = soma dos países --------------------------
# World 1001 deve ser EXATAMENTE a soma dos países por idade
# (foi construído assim no bloco 2 do script 01). Tolerância
# 1e-6 captura erro de ponto flutuante; qualquer drift maior
# indica bug.
tol <- 1e-6
inc_2022 <- df_cc_completo[type == "Incidence" & year_prediction == 2022]

world_by_age <- inc_2022[population_code == 1001L,
                         .(cases_world = sum(cases_2022, na.rm = TRUE),
                           pred_world  = sum(prediction, na.rm = TRUE)),
                         by = .(age_code, age)]

sum_countries_by_age <- inc_2022[population_code != 1001L,
                                 .(cases_sum = sum(cases_2022, na.rm = TRUE),
                                   pred_sum  = sum(prediction, na.rm = TRUE)),
                                 by = .(age_code, age)]

cmp_world <- merge(world_by_age, sum_countries_by_age, by = c("age_code","age"), all = TRUE)
cmp_world[, `:=`(
  rel_diff_cases = abs(cases_world - cases_sum) / pmax(1, cases_world),
  rel_diff_pred  = abs(pred_world  - pred_sum)  / pmax(1, pred_world)
)]

world_mismatch <- cmp_world[(rel_diff_cases > tol) | (rel_diff_pred > tol)]
write_report(world_mismatch, "reports/world_vs_sumcountries_incidence_2022.csv")
if (nrow(world_mismatch) == 0) ok("World 1001 (Incidence/2022): consistente com soma dos países por idade.")

# -------- 1.8 Coerência com planilha agregada (incidência) ---------------
# Compara: (a) soma das idades em `df_cc_completo` × país;
#          (b) `number_incidence` em `df_cc_taxas` (oficial agregado).
# Tolerância 5% — ad-hoc, generosa de propósito porque há
# pequenas diferenças esperadas (vintages diferentes, "—"
# tratado como 0 em uma e NA na outra). >5% → CSV para revisão.
inc_country <- inc_2022[
  , .(cases_2022_sum = sum(cases_2022, na.rm = TRUE)),
  by = .(population_code, population_name)
]

tax_inc <- df_cc_taxas[
  , .(population_code, population_name, number_incidence)
]

cmp_inc <- merge(inc_country, tax_inc, by = "population_code", all.x = TRUE)
cmp_inc[, abs_diff := abs(cases_2022_sum - number_incidence)]
cmp_inc[, rel_diff := abs_diff / pmax(1, number_incidence)]
inc_mismatch <- cmp_inc[is.na(number_incidence) | rel_diff > 0.05]

write_report(inc_mismatch, "reports/incidence_country_vs_sheet.csv")
if (nrow(inc_mismatch) == 0) ok("Incidência por país (soma das idades) consistente com planilha agregada (±5%).")

# -------- 1.9 Sanidade das taxas -----------------------------------------
# Taxas brutas/ASR não podem ser negativas — sanity check
# trivial, captura bug de parsing do GCO/IARC ("—" vira -1
# em algumas planilhas se mal-mapeado).
rate_issues <- df_cc_taxas[
  (incidence_crude_rate < 0) | (incidence_asr_world < 0) |
    (mortality_crude_rate < 0) | (mortality_asr_world < 0)
]
write_report(rate_issues, "reports/rates_negative.csv")
if (nrow(rate_issues) == 0) ok("Sem taxas negativas nas planilhas agregadas.")

# -------- 1.10 Epi sem população -----------------------------------------
# Combinação (país × faixa) que tem epi mas não tem pop.
# Espelha 1.4 mas agregado por chave (mais fácil ler em CSV).
# `unique()` deduplica — várias linhas de tipos/anos com a
# mesma falta de pop colapsam em 1.
epi_no_pop <- df_cc_completo[
  is.na(pop_2022) | is.na(pop_2025),
  .(population_code, population_name, age, age_code, n = .N)
]
epi_no_pop <- unique(epi_no_pop)
write_report(epi_no_pop, "reports/epi_without_population.csv")
if (nrow(epi_no_pop) == 0) ok("Todos os registros epidemiológicos possuem população 2022/2025.")

# ===========================================================
# 2) Bases Brasil — IBGE / ANS / regionalização
# ===========================================================
# Saídas de `data-raw/02_prepare_BR.R`. Todas OPCIONAIS (a app
# pode rodar para outros países sem essas bases). Se o
# `.rds` está ausente, o bloco do mesmo número é integralmente
# pulado — o resto continua.

# 2.1 IBGE faixas
# Único check de sanidade: pop não-negativa. (NAs existem por
# design — município sem dado vira 0 no script 02.)
pop_mun_faixas <- load_rds_if_exists("data/pop_municipio_faixas.rds",
                                     required = FALSE,
                                     nm = "pop_municipio_faixas")
if (!is.null(pop_mun_faixas)) {
  expect_cols(pop_mun_faixas,
              c("geo_id","geo_name","faixa","from","to","pop","level"),
              "pop_municipio_faixas")
  neg_ibge <- pop_mun_faixas[pop < 0]
  write_report(neg_ibge, "reports/ibge_pop_negative.csv")
  if (nrow(neg_ibge) == 0) ok("IBGE: sem populações negativas.")
}

# 2.2 ANS faixas
# `beneficiarios` (não `pop`) — schema próprio. Sanity:
# não-negativos.
ans_mun_faixas <- load_rds_if_exists("data/ans_municipio_faixas.rds",
                                     required = FALSE,
                                     nm = "ans_municipio_faixas")
if (!is.null(ans_mun_faixas)) {
  expect_cols(ans_mun_faixas,
              c("geo_id","geo_name","faixa","from","to","beneficiarios","level"),
              "ans_municipio_faixas")
  neg_ans <- ans_mun_faixas[beneficiarios < 0]
  write_report(neg_ans, "reports/ans_beneficiarios_negative.csv")
  if (nrow(neg_ans) == 0) ok("ANS: sem beneficiários negativos.")
}

# 2.3 Base total_sus (IBGE × ANS combinado, sem regionalização)
# 3 checks empilhados:
#   (a) sanity: nenhum negativo;
#   (b) consistência: pop_sus <= pop_total (clamp em 0 do
#       script 02 deveria garantir);
#   (c) cross-check IBGE: pop_total deve casar com IBGE puro
#       em ±1 (folga p/ arredondamento, ainda que a fonte seja
#       a mesma). Só roda se IBGE também foi carregado.
pop_mun_total_sus <- load_rds_if_exists("data/pop_municipio_faixas_total_sus.rds",
                                        required = FALSE,
                                        nm = "pop_municipio_faixas_total_sus")
if (!is.null(pop_mun_total_sus)) {
  expect_cols(pop_mun_total_sus,
              c("geo_id","geo_name","faixa","from","to","pop_total","pop_sus"),
              "pop_municipio_faixas_total_sus")
  neg_total <- pop_mun_total_sus[pop_total < 0 | pop_sus < 0]
  write_report(neg_total, "reports/pop_total_sus_negative.csv")
  if (nrow(neg_total) == 0) ok("pop_total/pop_sus: sem valores negativos.")
  inco_sus <- pop_mun_total_sus[pop_sus > pop_total]
  write_report(inco_sus, "reports/pop_sus_greater_than_total.csv")
  if (nrow(inco_sus) == 0) ok("pop_sus <= pop_total para todos os registros.")

  # Cross-check só roda se IBGE puro foi carregado em 2.1
  if (!is.null(pop_mun_faixas)) {
    cmp_pop <- merge(
      pop_mun_total_sus[, .(geo_id, from, to, pop_total)],
      pop_mun_faixas[, .(geo_id, from, to, pop)],
      by = c("geo_id","from","to"),
      all.x = TRUE
    )
    cmp_pop[, diff := pop_total - pop]
    prob_pop <- cmp_pop[abs(diff) > 1]
    write_report(prob_pop, "reports/pop_total_vs_ibge.csv")
    if (nrow(prob_pop) == 0) ok("pop_total consistente com IBGE (diferença absoluta <= 1).")
  }
}

# 2.4 Regionalização
# Schema rico (11 colunas hierárquicas). Único sanity adicional:
# `geo_id` sem ausência/string vazia — ele é a chave de join
# em todo o app só-Brasil.
regional_map <- load_rds_if_exists("data/regional_sus_map.rds",
                                   required = FALSE,
                                   nm = "regional_sus_map")
if (!is.null(regional_map)) {
  expect_cols(regional_map,
              c("geo_id",
                "uf_sigla","uf_codigo",
                "regiao_pais_nome","regiao_pais_codigo",
                "macro_nome","macro_codigo",
                "regiao_nome","regiao_codigo",
                "mun_nome","mun_code6"),
              "regional_sus_map")

  na_geo <- regional_map[is.na(geo_id) | geo_id == ""]
  write_report(na_geo, "reports/regional_geo_id_missing.csv")
  if (nrow(na_geo) == 0) ok("regional_sus_map: sem geo_id faltante.")
}

# 2.5 População + regionalização (BASE CENTRAL DO APP só-BR)
# Cross-check de tamanho: ao mergear total_sus × regional_map,
# linhas só somem se um município tiver geo_id sem hierarquia.
# `n_reg < n_base` = aviso (perda silenciosa); `==` ou `>` é OK.
pop_mun_regional <- load_rds_if_exists("data/pop_municipio_regional.rds",
                                       required = FALSE,
                                       nm = "pop_municipio_regional")
if (!is.null(pop_mun_regional)) {
  expect_cols(pop_mun_regional,
              c("geo_id","geo_name","faixa","from","to","pop_total","pop_sus"),
              "pop_municipio_regional")
  if (!is.null(pop_mun_total_sus)) {
    n_base   <- nrow(pop_mun_total_sus)
    n_reg    <- nrow(pop_mun_regional)
    if (n_reg < n_base) warn(sprintf("pop_municipio_regional tem menos linhas (%d) que pop_municipio_faixas_total_sus (%d).", n_reg, n_base))
    else ok("pop_municipio_regional: número de linhas >= base total_sus.")
  }
}

# ===========================================================
# 3) Produção SUS (SIA)
# ===========================================================
# Atenção: este bloco confere o intermediário `sia_cc_resumo`
# (saída de `build_sia_cc_resumo` em 03), NÃO o
# `sus_proc_resumo.rds` que o app carrega (esse é a saída
# unificada SIA+SIH). Os checks aqui valem para o intermediário.

# 3.1 Base completa (opcional, só confere se existe e é data.frame)
# `sia_cc_completo.rds` mora em `data-raw/`, não `data/` —
# é insumo intermediário. Único check: tipo é tabular.
sia_cc_completo <- load_rds_if_exists("data-raw/sia_cc_completo.rds",
                                      required = FALSE,
                                      nm = "sia_cc_completo")
if (!is.null(sia_cc_completo)) {
  if (is.data.table(sia_cc_completo) || is.data.frame(sia_cc_completo)) {
    ok(sprintf("sia_cc_completo carregado (%d linhas).", nrow(sia_cc_completo)))
  } else {
    warn("sia_cc_completo não é data.frame/data.table.")
  }
}

# 3.2 Base resumida (intermediária — `data/sia_cc_resumo.rds`)
# 3 checks: schema, quantidades não-negativas, meses
# de competência válidos (1–12), faixa_idade no domínio
# canônico ou NA (NA tolerado para registros sem idade).
# Faixas etárias hard-coded (DUPLICA `AGE_ORDER` de
# `00_constants_cc.R` — mesmo problema do 1.3).
sia_cc_resumo <- load_rds_if_exists("data/sia_cc_resumo.rds",
                                    required = FALSE,
                                    nm = "sia_cc_resumo")

if (!is.null(sia_cc_resumo)) {
  expect_cols(
    sia_cc_resumo,
    c(
      "categoria",
      "PA_UFMUN",
      "PA_CMP",
      "PA_PROC_ID",
      "nome_procedimento",
      "PA_SEXO",
      "PA_MUNPCN",
      "faixa_idade",
      "total_qtdpro",
      "total_qtdapr",
      "ano_cmp",
      "mes_cmp"
    ),
    "sia_cc_resumo"
  )


  # quantidades não-negativas (apresentada e aprovada)
  neg_qtd <- sia_cc_resumo[total_qtdpro < 0 | total_qtdapr < 0]
  write_report(neg_qtd, "reports/sia_qtd_negative.csv")
  if (nrow(neg_qtd) == 0) ok("SIA: total_qtdpro/total_qtdapr sem valores negativos.")


  # meses 1–12 (competência) — defesa contra parsing de PA_CMP
  meses_invalidos_cmp <- unique(sia_cc_resumo[mes_cmp < 1 | mes_cmp > 12, mes_cmp])
  if (length(meses_invalidos_cmp)) {
    warn(sprintf("SIA: mes_cmp fora de 1-12: %s",
                 paste(meses_invalidos_cmp, collapse = ", ")))
  } else {
    ok("SIA: mes_cmp entre 1 e 12.")
  }


  # faixas etárias válidas (opcional — NA tolerado)
  # Mesmo vetor de 1.3 — replicação confirma necessidade
  # de constante única (AGE_ORDER já existe).
  faixas_validas <- c(
    "0-4","5-9","10-14","15-19","20-24","25-29","30-34",
    "35-39","40-44","45-49","50-54","55-59","60-64",
    "65-69","70-74","75-79","80-84","85+"
  )
  faixas_invalidas <- unique(
    sia_cc_resumo[!is.na(faixa_idade) & !(faixa_idade %in% faixas_validas), faixa_idade]
  )
  if (length(faixas_invalidas)) {
    warn(sprintf("SIA: faixa_idade com valores fora do esperado: %s",
                 paste(faixas_invalidas, collapse = ", ")))
  } else {
    ok("SIA: faixa_idade dentro dos intervalos esperados (ou NA).")
  }

}


# ===========================================================
# 4) Sumário final
# ===========================================================
# Sumário em texto plano salvo em `reports/checks_cc_summary.txt`.
# Não cobre as bases BR/SIA — limita-se ao escopo Globocan/WPP
# (o "núcleo obrigatório"). Os 3 helpers `safe_*` retornam
# "NA" como string quando o objeto é NULL — útil porque o
# sumário deve sair mesmo se algum bloco anterior falhou
# silenciosamente.

safe_nrow <- function(x) if (is.null(x)) "NA" else nrow(x)

safe_unique_chr <- function(x) {
  if (is.null(x)) return("NA")
  paste(sort(unique(as.character(x))), collapse = ", ")
}

safe_unique_int_n <- function(x) {
  if (is.null(x)) return("NA")
  as.character(length(unique(x)))
}

summary_txt <- c(
  sprintf("Data: %s", Sys.time()),
  sprintf("Linhas df_cc_completo: %s", safe_nrow(df_cc_completo)),
  sprintf("Linhas df_cc_taxas: %s", safe_nrow(df_cc_taxas)),
  sprintf("Faixas etárias únicas (Globocan): %s", safe_unique_chr(df_cc_completo$age)),
  sprintf("Países (codes) únicos: %s", safe_unique_int_n(df_cc_completo$population_code)),
  sprintf("Anos em df_cc_completo: %s", safe_unique_chr(df_cc_completo$year_prediction)),
  sprintf("Tipos únicos: %s", safe_unique_chr(df_cc_completo$type))
)


if (!dir_exists("reports")) dir_create("reports")
writeLines(summary_txt, "reports/checks_cc_summary.txt")
ok("Resumo salvo em reports/checks_cc_summary.txt")

# Fim ---------------------------------------------------------------------
