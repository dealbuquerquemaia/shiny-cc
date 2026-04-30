# ===========================================================
# Shiny-cc — 02_engine_capacity_cc.R
# -----------------------------------------------------------
# MOTOR (engine) de cálculo do painel. Converte a seleção do
# usuário em necessidade anual de procedimentos e recursos.
#
# Pipeline (ordem de chamada — ver seção 7 `cc_engine_run`):
#   1) cc_engine_settings()    → normaliza + aplica defaults e devolve cfg (list)
#   2) cc_pop_in_range()       → população-alvo na faixa etária (GLOBOCAN, BR ou manual)
#   3) cc_workup_metrics()     → fluxo clínico (HPV via modelo_hpv() ou Citologia inline)
#   4) cc_hr_metrics()         → demanda de RH/equipamentos a partir de metrics
#   5) cc_engine_run()         → orquestra (1)→(4) e devolve lista com tudo
#
# Utilitários associados:
#   - cc_engine_summary_dt()   → achata o resultado em data.table de 1 linha
#   - cc_capacity_from_sia()   → totais de produção SUS/SIA 2024 (usado na aba Capacity)
#   - mod_capacity_compare()   → cruza necessidade (engine) × realizado (SIA)
#   - modelo_hpv()             → função de fluxo clínico do HPV (reutilizada no export PDF)
#
# Dependências implícitas:
#   - HPV_DEFAULTS, CITO_DEFAULTS, BASE_ANO   → 00_constants_cc.R
#   - age_band_min(), age_band_max()          → 01_utils_cc.R
#
# Uso básico:
#   cfg <- cc_engine_settings(country_code = 1001L)
#   res <- cc_engine_run(df_cc_completo, cfg,
#                        pop_mun_regional = pop_municipio_regional)
# ===========================================================


# -----------------------------------------------------------
# 1) Engine settings
#    Constrói a lista cfg que todo o pipeline consome. Aceita
#    NULL em qualquer parâmetro de % — cai no default correspondente
#    (HPV_DEFAULTS / CITO_DEFAULTS). Percentuais são mantidos em %
#    aqui; a divisão por 100 acontece dentro de cc_workup_metrics().
# -----------------------------------------------------------

cc_engine_settings <- function(
    country_code,
    pop_mode          = c("globocan", "other"),
    coverage          = 70,      # %
    screen_method     = c("hpv", "cytology"),
    target_age_min    = 25,
    target_age_max    = 64,
    custom_pop        = NA_real_,  # usado quando pop_mode == "other"
    
        # ---------- HPV (percentuais em %) ----------
    p16_18            = NULL,
    poutros           = NULL,
    pneg              = NULL,
    cito_out_pos      = NULL,
    cito_out_neg      = NULL,
    colpo16_pos       = NULL,
    colpo16_neg       = NULL,
    colpoout_pos      = NULL,
    colpoout_neg      = NULL,
    b16_neg_nic1      = NULL,
    b16_nic23         = NULL,
    b16_cancer        = NULL,
    bo_neg_nic1       = NULL,
    bo_nic23          = NULL,
    bo_cancer         = NULL,
    hpv_followup_pos_pct = NULL,

    
    # ---------- Citologia (percentuais em %) ----------
    first_time_pct         = NULL,
    unsatisfactory_pct     = NULL,
    res_asch_pct           = NULL,
    res_other_pct          = NULL,
    res_neg_pct            = NULL,
    colpo_asch_pct         = NULL,
    colpo_other_follow_pct = NULL,
    biopsy_pos_asch_pct    = NULL,
    biopsy_pos_other_pct   = NULL,
    b_asch_nic23_pct        = NULL,
    b_asch_cancer_pct       = NULL,
    b_asch_neg_nic1_pct     = NULL,
    b_other_nic23_pct       = NULL,
    b_other_cancer_pct      = NULL,
    b_other_neg_nic1_pct    = NULL,
    
    # ---------- Capacidade anual (baseline) ----------
    cap_colpo_device  = 5760,
    cap_colpo_med     = 2880,
    cap_citopato      = 14400,
    cap_patol_med     = 7200,
    
    # ---------------- BRASIL (subnacional) ----------------
    is_brazil         = FALSE,
    br_pop_tipo       = c("total", "sus"),
    filt_uf           = NULL,
    filt_macro        = NULL,
    filt_reg          = NULL,
    filt_mun          = NULL
) {
  # --- Coerção e validação das entradas escalares ---
  coverage <- suppressWarnings(as.numeric(coverage))
  if (!is.finite(coverage)) coverage <- NA_real_

  screen_method <- match.arg(screen_method)

  target_age_min <- suppressWarnings(as.numeric(target_age_min))
  target_age_max <- suppressWarnings(as.numeric(target_age_max))
  if (!is.finite(target_age_min) || !is.finite(target_age_max) || target_age_min > target_age_max) {
    stop("cc_engine_settings(): 'target_age_min' deve ser <= 'target_age_max'.", call. = FALSE)
  }
  
  custom_pop <- suppressWarnings(as.numeric(custom_pop))
  if (!is.finite(custom_pop)) custom_pop <- NA_real_
  
  # ---------- Defaults HPV (centralizado em HPV_DEFAULTS) ----------
  # Para cada parâmetro HPV: se chegar NULL/NA/não-numérico, usa o valor correspondente
  # de HPV_DEFAULTS (definido em 00_constants_cc.R).
  if (!exists("HPV_DEFAULTS", inherits = TRUE)) {
    stop("cc_engine_settings(): objeto 'HPV_DEFAULTS' não encontrado.", call. = FALSE)
  }
  d_hpv <- get("HPV_DEFAULTS", inherits = TRUE)
  hpv_or_def <- function(x, def) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 0L || !is.finite(x) || is.na(x)) def else x
  }
  
  p16_18       <- hpv_or_def(p16_18,       d_hpv$p16_18)
  poutros      <- hpv_or_def(poutros,      d_hpv$poutros)
  pneg         <- hpv_or_def(pneg,         d_hpv$pneg)
  cito_out_pos <- hpv_or_def(cito_out_pos, d_hpv$cito_out_pos)
  cito_out_neg <- hpv_or_def(cito_out_neg, d_hpv$cito_out_neg)
  colpo16_pos  <- hpv_or_def(colpo16_pos,  d_hpv$colpo16_pos)
  colpo16_neg  <- hpv_or_def(colpo16_neg,  d_hpv$colpo16_neg)
  colpoout_pos <- hpv_or_def(colpoout_pos, d_hpv$colpoout_pos)
  colpoout_neg <- hpv_or_def(colpoout_neg, d_hpv$colpoout_neg)
  b16_neg_nic1 <- hpv_or_def(b16_neg_nic1, d_hpv$b16_neg_nic1)
  b16_nic23    <- hpv_or_def(b16_nic23,    d_hpv$b16_nic23)
  b16_cancer   <- hpv_or_def(b16_cancer,   d_hpv$b16_cancer)
  bo_neg_nic1  <- hpv_or_def(bo_neg_nic1,  d_hpv$bo_neg_nic1)
  bo_nic23     <- hpv_or_def(bo_nic23,     d_hpv$bo_nic23)
  bo_cancer    <- hpv_or_def(bo_cancer,    d_hpv$bo_cancer)
  hpv_followup_pos_pct <- hpv_or_def(hpv_followup_pos_pct, d_hpv$hpv_followup_pos_pct)
  
  # ---------- Defaults Citologia (centralizado em CITO_DEFAULTS) ----------
  # Mesma lógica dos defaults HPV: parâmetro NULL/NA → valor de CITO_DEFAULTS.
  if (!exists("CITO_DEFAULTS", inherits = TRUE)) {
    stop("cc_engine_settings(): objeto 'CITO_DEFAULTS' não encontrado.", call. = FALSE)
  }
  d_cito <- get("CITO_DEFAULTS", inherits = TRUE)
  cito_or_def <- function(x, def) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 0L || !is.finite(x) || is.na(x)) def else x
  }
  
  
  first_time_pct         <- cito_or_def(first_time_pct,         d_cito$first_time_pct)
  unsatisfactory_pct     <- cito_or_def(unsatisfactory_pct,     d_cito$unsatisfactory_pct)
  
  res_asch_pct           <- cito_or_def(res_asch_pct,           d_cito$res_asch_pct)
  res_other_pct          <- cito_or_def(res_other_pct,          d_cito$res_other_pct)
  res_neg_pct            <- cito_or_def(res_neg_pct,            d_cito$res_neg_pct)
  
  colpo_asch_pct         <- cito_or_def(colpo_asch_pct,         d_cito$colpo_asch_pct)
  colpo_other_follow_pct <- cito_or_def(colpo_other_follow_pct, d_cito$colpo_other_follow_pct)
  
  biopsy_pos_asch_pct    <- cito_or_def(biopsy_pos_asch_pct,    d_cito$biopsy_pos_asch_pct)
  biopsy_pos_other_pct   <- cito_or_def(biopsy_pos_other_pct,   d_cito$biopsy_pos_other_pct)
  
  b_asch_nic23_pct        <- cito_or_def(b_asch_nic23_pct,        d_cito$b_asch_nic23_pct)
  b_asch_cancer_pct       <- cito_or_def(b_asch_cancer_pct,       d_cito$b_asch_cancer_pct)
  b_asch_neg_nic1_pct     <- cito_or_def(b_asch_neg_nic1_pct,     d_cito$b_asch_neg_nic1_pct)
  
  b_other_nic23_pct       <- cito_or_def(b_other_nic23_pct,       d_cito$b_other_nic23_pct)
  b_other_cancer_pct      <- cito_or_def(b_other_cancer_pct,      d_cito$b_other_cancer_pct)
  b_other_neg_nic1_pct    <- cito_or_def(b_other_neg_nic1_pct,    d_cito$b_other_neg_nic1_pct)


  # --- Retorno: lista `cfg` (consumida pelo restante do pipeline) ---
  # Todos os % permanecem em escala 0–100; conversão p/ proporção ocorre em cc_workup_metrics().
  list(
    country_code     = as.integer(country_code),
    pop_mode         = match.arg(pop_mode),
    coverage         = coverage,
    screen_method    = screen_method,
    target_age_min   = target_age_min,
    target_age_max   = target_age_max,
    custom_pop       = custom_pop,
    
    p16_18           = as.numeric(p16_18),
    poutros          = as.numeric(poutros),
    pneg             = as.numeric(pneg),
    cito_out_pos     = as.numeric(cito_out_pos),
    cito_out_neg     = as.numeric(cito_out_neg),
    colpo16_pos      = as.numeric(colpo16_pos),
    colpo16_neg      = as.numeric(colpo16_neg),
    colpoout_pos     = as.numeric(colpoout_pos),
    colpoout_neg     = as.numeric(colpoout_neg),
    b16_neg_nic1     = as.numeric(b16_neg_nic1),
    b16_nic23        = as.numeric(b16_nic23),
    b16_cancer       = as.numeric(b16_cancer),
    bo_neg_nic1      = as.numeric(bo_neg_nic1),
    bo_nic23         = as.numeric(bo_nic23),
    bo_cancer        = as.numeric(bo_cancer),
    hpv_followup_pos_pct = as.numeric(hpv_followup_pos_pct),
    
    first_time_pct         = as.numeric(first_time_pct),
    unsatisfactory_pct     = as.numeric(unsatisfactory_pct),
    
    res_asch_pct           = as.numeric(res_asch_pct),
    res_other_pct          = as.numeric(res_other_pct),
    res_neg_pct            = as.numeric(res_neg_pct),
    
    colpo_asch_pct         = as.numeric(colpo_asch_pct),
    colpo_other_follow_pct = as.numeric(colpo_other_follow_pct),
    
    biopsy_pos_asch_pct    = as.numeric(biopsy_pos_asch_pct),
    biopsy_pos_other_pct   = as.numeric(biopsy_pos_other_pct),
    
    b_asch_nic23_pct        = as.numeric(b_asch_nic23_pct),
    b_asch_cancer_pct       = as.numeric(b_asch_cancer_pct),
    b_asch_neg_nic1_pct     = as.numeric(b_asch_neg_nic1_pct),
    
    b_other_nic23_pct       = as.numeric(b_other_nic23_pct),
    b_other_cancer_pct      = as.numeric(b_other_cancer_pct),
    b_other_neg_nic1_pct    = as.numeric(b_other_neg_nic1_pct),
    
    cap_colpo_device = as.numeric(cap_colpo_device),
    cap_colpo_med    = as.numeric(cap_colpo_med),
    cap_citopato     = as.numeric(cap_citopato),
    cap_patol_med    = as.numeric(cap_patol_med),
    
    is_brazil        = isTRUE(is_brazil),
    br_pop_tipo      = match.arg(br_pop_tipo),
    filt_uf          = filt_uf,
    filt_macro       = filt_macro,
    filt_reg         = filt_reg,
    filt_mun         = filt_mun
  )
}


# -----------------------------------------------------------
# 1.b) cc_cfg_from_input(g, br_code)
#      Wrapper único que traduz a saída do reactive `filters`
#      (mod_filters_cc) em `cfg` consumível pelo engine. Substitui o
#      bloco `reactive({ cc_engine_settings(...) })` antes replicado
#      em 5 módulos (Summary / Equipment / Pathway / Capacity / Compare).
#
#      A função é PURA (não reactive) — o caller mantém o `reactive(...)`
#      em volta. Aceita lista vazia / campos NULL e aplica defesa:
#        - país: coage para integer; fallback para `br_code` se ausente
#        - is_brazil:  derivado de (country_code == br_code)
#        - coverage:   coage para numérico; clamp em [0, 100]
#        - método:     prefere `screen_method`; aceita `programa`
#                      legado ("HPV"/"Citologia") como retrocompat
#        - faixa etária: prefere `target_age_*`; aceita `proto1_age_*`
#                      legado como retrocompat
#        - HPV (16) + Cito (16): repassa NA → engine cai em HPV_DEFAULTS
#                      / CITO_DEFAULTS
#        - capacidades: já anualizadas em `g`; NA → engine cai em
#                      BASE_ANO via cc_hr_metrics
#        - filtros Brasil: filt_uf/macro/reg/mun + br_pop_tipo
#
#      Não consome `g$is_brazil` (redundante) nem `g$sia_geo_ref`
#      (consumido fora do engine). custom_pop é lido de `g$custom_pop`
#      (chave emitida por mod_filters_cc); aceita `g$custom_pop_main`
#      como retrocompat para módulos que liam o nome antigo.
# -----------------------------------------------------------

cc_cfg_from_input <- function(g, br_code = 1001L) {
  if (is.null(g)) g <- list()

  # Helper local: NULL/length 0/NA → default; senão retorna x
  val_or <- function(x, default) {
    if (is.null(x) || length(x) == 0L || all(is.na(x))) default else x
  }

  # -- País (fallback para br_code; nunca devolve NA)
  country_code <- suppressWarnings(as.integer(val_or(g$pais_sel, br_code)))
  if (is.na(country_code)) country_code <- as.integer(br_code)

  # -- Modo de população (globocan ou other)
  pop_mode <- as.character(val_or(g$pop_mode, "globocan"))
  if (!pop_mode %in% c("globocan", "other")) pop_mode <- "globocan"

  # -- custom_pop só faz sentido em pop_mode = "other".
  #    Lê `g$custom_pop` (chave emitida pelo mod_filters); se ausente,
  #    aceita `g$custom_pop_main` como retrocompat.
  custom_pop <- NA_real_
  if (identical(pop_mode, "other")) {
    cp <- val_or(g$custom_pop, val_or(g$custom_pop_main, NA_real_))
    custom_pop <- suppressWarnings(as.numeric(cp))
    if (!is.finite(custom_pop)) custom_pop <- NA_real_
  }

  # -- Cobertura: clamp em [0, 100]
  coverage <- suppressWarnings(as.numeric(val_or(g$coverage, 70)))
  if (!is.finite(coverage)) coverage <- 70
  coverage <- max(0, min(100, coverage))

  # -- Método: novo (screen_method) com fallback para `programa` legado
  screen_method <- val_or(g$screen_method, NULL)
  if (is.null(screen_method)) {
    prog <- val_or(g$programa, "HPV")
    screen_method <- if (identical(as.character(prog), "Citologia")) "cytology" else "hpv"
  }
  screen_method <- as.character(screen_method)
  if (!screen_method %in% c("hpv", "cytology")) screen_method <- "hpv"

  # -- Faixa etária: novo (target_age_*) com fallback para proto1_age_* legado
  target_age_min <- suppressWarnings(as.numeric(
    val_or(g$target_age_min, val_or(g$proto1_age_min, 25))
  ))
  target_age_max <- suppressWarnings(as.numeric(
    val_or(g$target_age_max, val_or(g$proto1_age_max, 64))
  ))
  if (!is.finite(target_age_min)) target_age_min <- 25
  if (!is.finite(target_age_max)) target_age_max <- 64

  # -- is_brazil derivado (ignora g$is_brazil que é redundante)
  is_br <- isTRUE(country_code == as.integer(br_code))

  # -- br_pop_tipo (total/sus)
  br_pop_tipo <- as.character(val_or(g$br_pop_tipo, "total"))
  if (!br_pop_tipo %in% c("total", "sus")) br_pop_tipo <- "total"

  # -- Repassa para cc_engine_settings (que aplica defaults HPV/Cito)
  cc_engine_settings(
    country_code     = country_code,
    pop_mode         = pop_mode,
    coverage         = coverage,
    screen_method    = screen_method,
    target_age_min   = target_age_min,
    target_age_max   = target_age_max,
    custom_pop       = custom_pop,

    # HPV — NA → engine cai em HPV_DEFAULTS
    p16_18           = val_or(g$p16_18,           NA_real_),
    poutros          = val_or(g$poutros,          NA_real_),
    pneg             = val_or(g$pneg,             NA_real_),
    cito_out_pos     = val_or(g$cito_out_pos,     NA_real_),
    cito_out_neg     = val_or(g$cito_out_neg,     NA_real_),
    colpo16_pos      = val_or(g$colpo16_pos,      NA_real_),
    colpo16_neg      = val_or(g$colpo16_neg,      NA_real_),
    colpoout_pos     = val_or(g$colpoout_pos,     NA_real_),
    colpoout_neg     = val_or(g$colpoout_neg,     NA_real_),
    b16_neg_nic1     = val_or(g$b16_neg_nic1,     NA_real_),
    b16_nic23        = val_or(g$b16_nic23,        NA_real_),
    b16_cancer       = val_or(g$b16_cancer,       NA_real_),
    bo_neg_nic1      = val_or(g$bo_neg_nic1,      NA_real_),
    bo_nic23         = val_or(g$bo_nic23,         NA_real_),
    bo_cancer        = val_or(g$bo_cancer,        NA_real_),
    hpv_followup_pos_pct = val_or(g$hpv_followup_pos_pct, NA_real_),

    # Citologia — NA → engine cai em CITO_DEFAULTS
    first_time_pct         = val_or(g$first_time_pct,         NA_real_),
    unsatisfactory_pct     = val_or(g$unsatisfactory_pct,     NA_real_),
    res_asch_pct           = val_or(g$res_asch_pct,           NA_real_),
    res_other_pct          = val_or(g$res_other_pct,          NA_real_),
    res_neg_pct            = val_or(g$res_neg_pct,            NA_real_),
    colpo_asch_pct         = val_or(g$colpo_asch_pct,         NA_real_),
    colpo_other_follow_pct = val_or(g$colpo_other_follow_pct, NA_real_),
    biopsy_pos_asch_pct    = val_or(g$biopsy_pos_asch_pct,    NA_real_),
    biopsy_pos_other_pct   = val_or(g$biopsy_pos_other_pct,   NA_real_),
    b_asch_nic23_pct       = val_or(g$b_asch_nic23_pct,       NA_real_),
    b_asch_cancer_pct      = val_or(g$b_asch_cancer_pct,      NA_real_),
    b_asch_neg_nic1_pct    = val_or(g$b_asch_neg_nic1_pct,    NA_real_),
    b_other_nic23_pct      = val_or(g$b_other_nic23_pct,      NA_real_),
    b_other_cancer_pct     = val_or(g$b_other_cancer_pct,     NA_real_),
    b_other_neg_nic1_pct   = val_or(g$b_other_neg_nic1_pct,   NA_real_),

    # Capacidades já anualizadas; NA → engine cai em BASE_ANO via cc_hr_metrics
    cap_colpo_device = val_or(g$cap_colpo_device, NA_real_),
    cap_colpo_med    = val_or(g$cap_colpo_med,    NA_real_),
    cap_citopato     = val_or(g$cap_citopato,     NA_real_),
    cap_patol_med    = val_or(g$cap_patol_med,    NA_real_),

    # Brasil subnacional
    is_brazil        = is_br,
    br_pop_tipo      = br_pop_tipo,
    filt_uf          = val_or(g$filt_uf,    NULL),
    filt_macro       = val_or(g$filt_macro, NULL),
    filt_reg         = val_or(g$filt_reg,   NULL),
    filt_mun         = val_or(g$filt_mun,   NULL)
  )
}




# -----------------------------------------------------------
# 2) POP 2025 por idade (GLOBOCAN/WPP)
#    Extrai a população feminina projetada p/ 2025 por faixa
#    etária de um país. Usa as linhas de "Incidence" apenas
#    (cada par age×country se repete em Incidence/Mortality
#    com a mesma pop_2025; first() elimina a duplicação).
# -----------------------------------------------------------

cc_pop_by_age <- function(df_completo, country_code) {
  data.table::setDT(df_completo)
  df_completo[
    population_code == country_code & type == "Incidence",
    .(pop_2025 = data.table::first(pop_2025)),
    by = .(age_code, age)
  ][order(age_code)]
}

# -----------------------------------------------------------
# 3) POP Brasil (IBGE/ANS) por faixa etária, com filtros
#    Soma pop_total OU pop_sus (conforme cfg$br_pop_tipo) no
#    subset geográfico (UF / Macro / Região / Município) e
#    intersecção da faixa etária [age_min, age_max]. Devolve
#    NA_real_ se cfg não for Brasil ou se pop_mun_regional for NULL.
# -----------------------------------------------------------

cc_br_pop_in_range <- function(pop_mun_regional, cfg, age_min, age_max) {
  if (is.null(pop_mun_regional) || !isTRUE(cfg$is_brazil)) {
    return(NA_real_)
  }
  
  dt <- data.table::as.data.table(pop_mun_regional)
  
  # coluna de população: total x SUS-dependente
  pop_col <- if (cfg$br_pop_tipo == "sus" && "pop_sus" %in% names(dt)) {
    "pop_sus"
  } else {
    "pop_total"
  }
  
  # faixa etária (interseção com [age_min, age_max])
  dt <- dt[to >= age_min & from <= age_max]
  
  
  # filtros geográficos
  if (!is.null(cfg$filt_uf) && length(cfg$filt_uf))
    dt <- dt[UF %in% cfg$filt_uf]
  if (!is.null(cfg$filt_macro) && length(cfg$filt_macro))
    dt <- dt[`Macrorregiao de Saude` %in% cfg$filt_macro]
  if (!is.null(cfg$filt_reg) && length(cfg$filt_reg))
    dt <- dt[`Regiao de Saude` %in% cfg$filt_reg]
  if (!is.null(cfg$filt_mun) && length(cfg$filt_mun))
    dt <- dt[Municipio %in% cfg$filt_mun]
  
  if (!nrow(dt)) return(0)

  sum(dt[[pop_col]], na.rm = TRUE)
}

# -----------------------------------------------------------
# 3b) Wrapper unificado de população-alvo
#     Dispatch entre 3 modos:
#       - "other"   → usa custom_pop (entrada manual do usuário)
#       - Brasil    → cc_br_pop_in_range() (IBGE/ANS, subnacional)
#       - GLOBOCAN  → cc_pop_by_age() e intersecção com [age_min, age_max]
#                     usando os parsers age_band_min/max (01_utils_cc.R).
# -----------------------------------------------------------
cc_pop_in_range <- function(df_completo, cfg, pop_mun_regional, age_min, age_max, custom_pop = NA_real_) {
  # Modo manual: usa diretamente custom_pop (fallback 0).
  if (isTRUE(cfg$pop_mode == "other")) {
    x <- suppressWarnings(as.numeric(custom_pop))
    if (is.na(x) || x < 0) x <- 0
    return(x)
  }

  # Modo Brasil: delega para o agregador IBGE/ANS.
  if (isTRUE(cfg$is_brazil) && !is.null(pop_mun_regional)) {
    return(cc_br_pop_in_range(pop_mun_regional, cfg, age_min = age_min, age_max = age_max))
  }

  # Modo GLOBOCAN (default): soma faixas etárias que intersectam [age_min, age_max].
  df <- cc_pop_by_age(df_completo, cfg$country_code)
  if (!nrow(df)) return(0)

  df[, age_min_i := age_band_min(age)]
  df[, age_max_i := age_band_max(age)]

  rows <- df[age_max_i >= age_min & age_min_i <= age_max]
  sum(rows$pop_2025, na.rm = TRUE)
}


# -----------------------------------------------------------
# 4) Métricas de workup (CCU: HPV ou Citologia)
#    Dispatch pelo cfg$screen_method:
#      - "hpv"      → chama modelo_hpv() (seção 11 deste arquivo)
#      - "cytology" → calcula inline (modelo de citologia com
#                     resultados em 3 categorias: ASC-H+, outras,
#                     negativos; follow-up pós-colpo/biópsia/EZT)
#
#    Args:
#      N_rastreada — número anual de testes de rastreamento (HPV
#                    ou Pap) já processados no cc_engine_run().
#      eligible    — população elegível coberta (pop_selecionada *
#                    cobertura). Usada no modelo de citologia para
#                    derivar primeiros exames (`first_time_pct`) e
#                    insatisfatórios (`unsatisfactory_pct`).
#                    Defaults para N quando ausente.
#    Retorna: data.table de 1 linha com colunas do fluxo completo
#    (colpo_indicada, biopsia_indicada, ezt, cytologies_total_all,
#    colposcopies_total_all, follow-ups, etc.).
# -----------------------------------------------------------

# -----------------------------------------------------------
# 4a) cc_workup_volumes(eligible, params)
#
#     Helper único para a fórmula consolidada de volume anual
#     de citologias de rastreamento. Substitui 3 réplicas da
#     fórmula `((E/ciclo) + (E*ft)) * (1 + uns)` espalhadas em:
#       - cc_workup_metrics() (braço Cito, abaixo)
#       - cc_engine_run()     (braço Cito, seção 7)
#       - data-raw/09_prepare_peers.R (cenário INCA pré-calculado)
#
#     Fórmula:
#       volume_anual = (E/ciclo + E*ft) * (1 + uns)
#         E     — N elegíveis (escalar OU vetor; peers usa vetor)
#         ciclo — intervalo do ciclo de rastreamento, em anos
#         ft    — fração de "primeiros exames" (proporção 0–1)
#         uns   — fração de exames insatisfatórios (proporção 0–1)
#
#     Args:
#       eligible — numeric (escalar ou vetor)
#       params   — list com {ciclo, ft, uns}; defaults aplicados
#                  para cada campo ausente: ciclo = 3, ft = 0,
#                  uns = 0
#     Retorno: list com 4 componentes (mesma forma de `eligible`):
#       volume_anual    — volume anual final (incluindo reforço)
#       volume_ciclo    — E / ciclo (rastreamento cíclico)
#       exames_followup — E * ft (parcela "primeiros exames")
#       exames_insat    — base * uns (reforço por insatisfatórios)
# -----------------------------------------------------------
cc_workup_volumes <- function(eligible, params = list()) {
  # Sanitização do E (aceita escalar ou vetor)
  E <- suppressWarnings(as.numeric(eligible))
  E[!is.finite(E)] <- 0
  E[E < 0]         <- 0

  # ciclo — escalar > 0 (default 3 = citologia padrão BR)
  ciclo <- suppressWarnings(as.numeric(params$ciclo))
  if (length(ciclo) != 1L || !is.finite(ciclo) || ciclo <= 0) ciclo <- 3

  # ft — escalar em [0,1] (default 0)
  ft <- suppressWarnings(as.numeric(params$ft))
  if (length(ft) != 1L || !is.finite(ft) || ft < 0) ft <- 0
  if (ft > 1) ft <- 1

  # uns — escalar em [0,1] (default 0)
  uns <- suppressWarnings(as.numeric(params$uns))
  if (length(uns) != 1L || !is.finite(uns) || uns < 0) uns <- 0
  if (uns > 1) uns <- 1

  volume_ciclo    <- E / ciclo
  exames_followup <- E * ft
  base_exams      <- volume_ciclo + exames_followup
  exames_insat    <- base_exams * uns
  # Mantém a forma `base * (1 + uns)` (idêntica a workup_metrics e a
  # 09_prepare_peers pré-refator). engine_run pré-refator usava
  # `base + base*uns` — pode diferir 1 ULP, mas os snapshots batem
  # ao 4º decimal (limite do KPI exibido).
  volume_anual    <- base_exams * (1 + uns)

  list(
    volume_anual    = volume_anual,
    volume_ciclo    = volume_ciclo,
    exames_followup = exames_followup,
    exames_insat    = exames_insat
  )
}


cc_workup_metrics <- function(N_rastreada, cfg, eligible = NULL) {
  # Sanitização: N e E viram 0/N quando inválidos.
  N <- suppressWarnings(as.numeric(N_rastreada))
  if (!is.finite(N) || is.na(N) || N < 0) N <- 0
  E <- suppressWarnings(as.numeric(eligible))
  if (!is.finite(E) || is.na(E) || E < 0) E <- NA_real_
  if (!is.finite(E) || is.na(E)) E <- N
  
  
  
  method <- as.character(cfg$screen_method)
  if (is.na(method) || !nzchar(method)) method <- "hpv"
  if (!method %chin% c("hpv", "cytology")) {
    stop("cc_workup_metrics(): cfg$screen_method deve ser 'hpv' ou 'cytology'.", call. = FALSE)
  }
  
  if (identical(method, "hpv")) {
    # -------------------- Braço HPV --------------------
    # Delega o modelo clínico para modelo_hpv() (seção 11).
    if (!exists("modelo_hpv", mode = "function", inherits = TRUE)) {
      stop("cc_workup_metrics(): função 'modelo_hpv' não encontrada.", call. = FALSE)
    }

    # Defaults (cfg pode vir já preenchido por cc_engine_settings; aqui é rede de segurança).
    if (!exists("HPV_DEFAULTS", inherits = TRUE)) {
      stop("cc_workup_metrics(): objeto 'HPV_DEFAULTS' não encontrado.", call. = FALSE)
    }
    d <- get("HPV_DEFAULTS", inherits = TRUE)

    # get_pct: converte % (0–100) em proporção (0–1) e aplica fallback ao default.
    get_pct <- function(x_cfg, x_def) {
      x <- suppressWarnings(as.numeric(x_cfg))
      if (!is.finite(x) || is.na(x)) x <- x_def
      x / 100
    }
    
    p16_18       <- get_pct(cfg$p16_18,       d$p16_18)
    poutros      <- get_pct(cfg$poutros,      d$poutros)
    pneg         <- get_pct(cfg$pneg,         d$pneg)
    cito_out_pos <- get_pct(cfg$cito_out_pos, d$cito_out_pos)
    cito_out_neg <- get_pct(cfg$cito_out_neg, d$cito_out_neg)
    colpo16_pos  <- get_pct(cfg$colpo16_pos,  d$colpo16_pos)
    colpo16_neg  <- get_pct(cfg$colpo16_neg,  d$colpo16_neg)
    colpoout_pos <- get_pct(cfg$colpoout_pos, d$colpoout_pos)
    colpoout_neg <- get_pct(cfg$colpoout_neg, d$colpoout_neg)
    b16_neg_nic1 <- get_pct(cfg$b16_neg_nic1, d$b16_neg_nic1)
    b16_nic23    <- get_pct(cfg$b16_nic23,    d$b16_nic23)
    b16_cancer   <- get_pct(cfg$b16_cancer,   d$b16_cancer)
    bo_neg_nic1  <- get_pct(cfg$bo_neg_nic1,  d$bo_neg_nic1)
    bo_nic23     <- get_pct(cfg$bo_nic23,     d$bo_nic23)
    bo_cancer    <- get_pct(cfg$bo_cancer,    d$bo_cancer)
    fu_pos       <- get_pct(cfg$hpv_followup_pos_pct, d$hpv_followup_pos_pct)
    
    
    res <- modelo_hpv(
      N = N,
      p16_18 = p16_18, poutros = poutros, pneg = pneg,
      cito_out_pos = cito_out_pos, cito_out_neg = cito_out_neg,
      colpo16_pos = colpo16_pos, colpo16_neg = colpo16_neg,
      colpoout_pos = colpoout_pos, colpoout_neg = colpoout_neg,
      b16_neg_nic1 = b16_neg_nic1, b16_nic23 = b16_nic23, b16_cancer = b16_cancer,
      bo_neg_nic1 = bo_neg_nic1, bo_nic23 = bo_nic23, bo_cancer = bo_cancer, 
      hpv_followup_pos = fu_pos
    )
    data.table::setDT(res)
    return(res[])
  }
  
  # -------------------- Braço Citologia --------------------
  # Modelo inline (não há função `modelo_cito` separada — todo o fluxo é aqui).
  # Estrutura do fluxo:
  #   1) volume anual de Paps (screening + primeiros exames + reforço por insatisfatórios)
  #   2) resultados: ASC-H+ / outras alterações / negativos
  #   3) colposcopia (taxas distintas por tipo de resultado)
  #   4) biópsia (positividade diferente por braço)
  #   5) desfechos da biópsia (NIC2/3 → EZT, câncer → alta complexidade, NIC1/neg → fu)
  #   6) follow-up: colpo negativo, biópsia NIC1, 6 cito + 2 colpo por EZT (pós-tratamento)
  if (!exists("CITO_DEFAULTS", inherits = TRUE)) {
    stop("cc_workup_metrics(): objeto 'CITO_DEFAULTS' não encontrado.", call. = FALSE)
  }
  d <- get("CITO_DEFAULTS", inherits = TRUE)

  get_pct <- function(x_cfg, x_def) {
    x <- suppressWarnings(as.numeric(x_cfg))
    if (!is.finite(x) || is.na(x)) x <- x_def
    x / 100
  }
  
  ft   <- get_pct(cfg$first_time_pct,         d$first_time_pct)
  uns  <- get_pct(cfg$unsatisfactory_pct,     d$unsatisfactory_pct)
  
  p_asch  <- get_pct(cfg$res_asch_pct,        d$res_asch_pct)
  p_other <- get_pct(cfg$res_other_pct,       d$res_other_pct)
  p_neg   <- get_pct(cfg$res_neg_pct,         d$res_neg_pct)
  
  p_col_asch  <- get_pct(cfg$colpo_asch_pct,         d$colpo_asch_pct)
  p_col_other <- get_pct(cfg$colpo_other_follow_pct, d$colpo_other_follow_pct)
  
  p_bpos_asch  <- get_pct(cfg$biopsy_pos_asch_pct,  d$biopsy_pos_asch_pct)
  p_bpos_other <- get_pct(cfg$biopsy_pos_other_pct, d$biopsy_pos_other_pct)
  
  p_asch_nic23 <- get_pct(cfg$b_asch_nic23_pct,      d$b_asch_nic23_pct)
  p_asch_can   <- get_pct(cfg$b_asch_cancer_pct,     d$b_asch_cancer_pct)
  p_asch_neg1  <- get_pct(cfg$b_asch_neg_nic1_pct,   d$b_asch_neg_nic1_pct)
  
  p_oth_nic23  <- get_pct(cfg$b_other_nic23_pct,     d$b_other_nic23_pct)
  p_oth_can    <- get_pct(cfg$b_other_cancer_pct,    d$b_other_cancer_pct)
  p_oth_neg1   <- get_pct(cfg$b_other_neg_nic1_pct,  d$b_other_neg_nic1_pct)
  
  # (1) volume anual de citologias de rastreamento.
  #     Lógica: rastreamento a cada 3 anos (E/3) + parcela de "primeiros exames" (E*ft)
  #     e um reforço proporcional a % de insatisfatórios (multiplica por 1+uns).
  #     Delegado ao helper cc_workup_volumes() (seção 4a) para evitar drift
  #     com cc_engine_run() e data-raw/09_prepare_peers.R.
  n_cyt_screen <- cc_workup_volumes(
    eligible = E,
    params   = list(ciclo = 3, ft = ft, uns = uns)
  )$volume_anual
  
  # (2) resultados (sobre o screening realizado)
  n_asch  <- n_cyt_screen * p_asch
  n_other <- n_cyt_screen * p_other
  n_neg   <- pmax(n_cyt_screen - n_asch - n_other, 0)

  # Citologia diagnóstica: repetição em 6 meses p/ mulheres com "outras alterações".
  n_cyt_diag <- n_other

  # (3) colposcopia — taxa distinta por tipo de alteração
  n_col_asch  <- n_asch * p_col_asch
  n_col_other <- n_other * p_col_other
  n_col_total <- n_col_asch + n_col_other

  # (4) biópsia (positividade dentre colposcopias realizadas)
  n_b_asch  <- n_col_asch  * p_bpos_asch
  n_b_other <- n_col_other * p_bpos_other
  n_b_total <- n_b_asch + n_b_other

  # (5) desfechos (dentre biópsias positivas)
  n_nic23_asch <- n_b_asch  * p_asch_nic23
  n_can_asch   <- n_b_asch  * p_asch_can
  n_neg1_asch  <- n_b_asch  * p_asch_neg1
  
  n_nic23_oth  <- n_b_other * p_oth_nic23
  n_can_oth    <- n_b_other * p_oth_can
  n_neg1_oth   <- n_b_other * p_oth_neg1
  
  n_nic23 <- n_nic23_asch + n_nic23_oth
  n_can   <- n_can_asch   + n_can_oth
  
  # (6) Follow-up — citologias
  # 6a) pós-colposcopia negativa (= colpo indicada − biópsia indicada)
  n_colpo_neg <- (n_col_asch - n_b_asch) + (n_col_other - n_b_other)
  n_colpo_neg <- pmax(n_colpo_neg, 0)

  # 6b) pós-biópsia com NIC1/negativo (dentre biópsias realizadas)
  n_biopsy_nic1 <- n_neg1_asch + n_neg1_oth

  # 6c) pós-EZT: 6 citologias de seguimento por EZT (protocolo nacional)
  n_ezt_fu_cyt <- n_nic23 * 6

  followup_cytologies <- n_colpo_neg + n_biopsy_nic1 + n_ezt_fu_cyt

  # Follow-up — colposcopias
  # Reaproveita n_colpo_neg (controle das negativas) + 2 colpos por EZT.
  n_ezt_fu_colpo <- n_nic23 * 2
  followup_colposcopies <- n_colpo_neg + n_ezt_fu_colpo

  # Totais "all-in" (usados em cc_hr_metrics e mod_capacity_compare)
  cytologies_total_all   <- n_cyt_screen + n_cyt_diag + followup_cytologies
  colposcopies_total_all <- n_col_total + followup_colposcopies
  
  
  
  out <- data.table::data.table(
    rastreada          = n_cyt_screen,
    cito_reflexa       = NA_real_,
    
    colpo_indicada     = n_col_total,
    biopsia_indicada   = n_b_total,
    
    ezt_1              = n_nic23,
    ezt_2              = 0,
    ezt_3              = 0,
    ezt                = n_nic23,
    
    alta_complexidade  = n_can,
    retorno_1ano       = followup_cytologies,
    
    cit_rastreamento   = n_cyt_screen,
    cit_diagnostica    = n_cyt_diag,
    ap_biopsia         = n_b_total,
    ap_peca_cir        = 0,
    
    followup_cytologies     = followup_cytologies,
    followup_colposcopies   = followup_colposcopies,
    cytologies_total_all    = cytologies_total_all,
    colposcopies_total_all  = colposcopies_total_all
  )
  
  
  
  
  out[]
  
}

# -----------------------------------------------------------
# 5) Recursos humanos (CCU) e equipamentos
#    Converte a demanda anual de procedimentos (de `metrics`)
#    em nº de colposcópios, colposcopistas, citopatologistas e
#    patologistas necessários, dividindo pela capacidade anual
#    de cada recurso (vem de `cfg$cap_*`, ou fallback em BASE_ANO).
#
#    Regra de prioridade para o volume:
#      - Colposcopia:  colposcopies_total_all → colpo_indicada
#      - Citologia:    cytologies_total_all → cit_rastreamento+cit_diagnostica
#                       → cito_reflexa → rastreada
#      - AP:           ap_biopsia + ap_peca_cir → biopsia_indicada
#
#    Retorna um data.table de 1 linha com `*_demand`, `cap_*` e `*_needed`
#    (nº de recursos = demand / cap_anual).
# -----------------------------------------------------------

cc_hr_metrics <- function(metrics, cfg) {
  dt <- data.table::as.data.table(metrics)

  # Baseline institucional (BASE_ANO em 00_constants_cc.R) — usado apenas se cfg$cap_* for inválido.
  base <- NULL
  if (exists("BASE_ANO", inherits = TRUE)) base <- get("BASE_ANO", inherits = TRUE)
  
  # capacidades anuais: prioriza cfg; se inválido, cai no BASE_ANO
  cap_colpo_device <- suppressWarnings(as.numeric(cfg$cap_colpo_device))
  cap_colpo_med    <- suppressWarnings(as.numeric(cfg$cap_colpo_med))
  cap_citopato     <- suppressWarnings(as.numeric(cfg$cap_citopato))
  cap_patol_med    <- suppressWarnings(as.numeric(cfg$cap_patol_med))
  
  if ((!is.finite(cap_colpo_device) || is.na(cap_colpo_device) || cap_colpo_device <= 0) && !is.null(base)) cap_colpo_device <- as.numeric(base$colpo_device)
  if ((!is.finite(cap_colpo_med)    || is.na(cap_colpo_med)    || cap_colpo_med    <= 0) && !is.null(base)) cap_colpo_med    <- as.numeric(base$colpo_med)
  if ((!is.finite(cap_citopato)     || is.na(cap_citopato)     || cap_citopato     <= 0) && !is.null(base)) cap_citopato     <- as.numeric(base$citopato)
  if ((!is.finite(cap_patol_med)    || is.na(cap_patol_med)    || cap_patol_med    <= 0) && !is.null(base)) cap_patol_med    <- as.numeric(base$patol_med)
  
  # volumes
  # Colposcopia: prioriza colposcopies_total_all (inicial + follow-up);
  # fallback: colpo_indicada (comportamento antigo)
  colpo_n <- if ("colposcopies_total_all" %in% names(dt)) {
    suppressWarnings(as.numeric(dt$colposcopies_total_all[1L]))
  } else if ("colpo_indicada" %in% names(dt)) {
    suppressWarnings(as.numeric(dt$colpo_indicada[1L]))
  } else 0
  if (!is.finite(colpo_n) || is.na(colpo_n) || colpo_n < 0) colpo_n <- 0
  
  biopsia_n <- if ("biopsia_indicada" %in% names(dt)) suppressWarnings(as.numeric(dt$biopsia_indicada[1L])) else 0
  
  # Citologia: prioriza cytologies_total_all (screening + diagnóstica + follow-up);
  # fallbacks em ordem: cit_rastreamento + cit_diagnostica → cito_reflexa → rastreada
  cito_n <- 0
  if ("cytologies_total_all" %in% names(dt)) {
    cito_n <- suppressWarnings(as.numeric(dt$cytologies_total_all[1L]))
  } else if ("cit_rastreamento" %in% names(dt) || "cit_diagnostica" %in% names(dt)) {
    cito_n <- (if ("cit_rastreamento" %in% names(dt)) suppressWarnings(as.numeric(dt$cit_rastreamento[1L])) else 0) +
      (if ("cit_diagnostica" %in% names(dt))  suppressWarnings(as.numeric(dt$cit_diagnostica[1L]))  else 0)
  } else if ("cito_reflexa" %in% names(dt)) {
    cito_n <- suppressWarnings(as.numeric(dt$cito_reflexa[1L]))
  } else if ("rastreada" %in% names(dt)) {
    cito_n <- suppressWarnings(as.numeric(dt$rastreada[1L]))
  }
  if (!is.finite(cito_n) || is.na(cito_n) || cito_n < 0) cito_n <- 0
  
  # AP: se tiver ap_biopsia/ap_peca_cir usa soma; senão assume 1 AP por biópsia
  ap_n <- NA_real_
  if ("ap_biopsia" %in% names(dt) || "ap_peca_cir" %in% names(dt)) {
    ap_n <- (if ("ap_biopsia" %in% names(dt)) suppressWarnings(as.numeric(dt$ap_biopsia[1L])) else 0) +
      (if ("ap_peca_cir" %in% names(dt)) suppressWarnings(as.numeric(dt$ap_peca_cir[1L])) else 0)
  } else {
    ap_n <- biopsia_n
  }
  if (!is.finite(ap_n) || is.na(ap_n) || ap_n < 0) ap_n <- 0
  
  cap_ok <- function(x) is.finite(x) && !is.na(x) && x > 0

  out <- data.table::data.table(
    colpo_demand         = colpo_n,
    cito_demand          = cito_n,
    ap_demand            = ap_n,
    cap_colpo_device     = if (cap_ok(cap_colpo_device)) cap_colpo_device else NA_real_,
    cap_colpo_med        = if (cap_ok(cap_colpo_med))    cap_colpo_med    else NA_real_,
    cap_citopato         = if (cap_ok(cap_citopato))     cap_citopato     else NA_real_,
    cap_patol_med        = if (cap_ok(cap_patol_med))    cap_patol_med    else NA_real_,
    colpo_devices_needed = if (cap_ok(cap_colpo_device)) colpo_n / cap_colpo_device else NA_real_,
    colpo_med_needed     = if (cap_ok(cap_colpo_med))    colpo_n / cap_colpo_med    else NA_real_,
    citopato_needed      = if (cap_ok(cap_citopato))     cito_n  / cap_citopato     else NA_real_,
    patol_med_needed     = if (cap_ok(cap_patol_med))    ap_n    / cap_patol_med    else NA_real_
  )
  out[]
}



# -----------------------------------------------------------
# 7) FUNÇÃO PRINCIPAL — cc_engine_run()
#    Orquestra todo o pipeline. Chamada pela maioria dos módulos
#    (mod_resumo_geral, mod_fluxo, mod_capacidade, etc.) e também
#    pelo export de PDF em app.R.
#
#    Entradas:
#      df_completo      — base GLOBOCAN (df_cc_completo)
#      cfg              — lista produzida por cc_engine_settings()
#      pop_mun_regional — tabela IBGE/ANS (apenas Brasil; NULL p/ outros)
#
#    Saída: lista com
#      country_code, pop_mode, is_brazil, screen_method,
#      target_age_min/max, pop_selected, coverage,
#      screened_per_year, metrics (data.table), hr (data.table)
# -----------------------------------------------------------

cc_engine_run <- function(df_completo, cfg, pop_mun_regional = NULL) {
  data.table::setDT(df_completo)
  
  # -----------------------------
  # 1) População-alvo (CCU)
  # -----------------------------
  age_min <- suppressWarnings(as.numeric(cfg$target_age_min))
  age_max <- suppressWarnings(as.numeric(cfg$target_age_max))
  if (!is.finite(age_min) || is.na(age_min)) age_min <- 25
  if (!is.finite(age_max) || is.na(age_max)) age_max <- 64
  if (age_min > age_max) stop("cc_engine_run(): target_age_min > target_age_max.", call. = FALSE)
  
  pop_sel <- cc_pop_in_range(
    df_completo      = df_completo,
    cfg              = cfg,
    pop_mun_regional = pop_mun_regional,
    age_min          = age_min,
    age_max          = age_max,
    custom_pop       = cfg$custom_pop
  )
  if (!is.finite(pop_sel) || is.na(pop_sel) || pop_sel < 0) pop_sel <- 0
  
  cov <- suppressWarnings(as.numeric(cfg$coverage)) / 100
  if (!is.finite(cov) || is.na(cov) || cov < 0) cov <- 0
  if (cov > 1) cov <- 1
  
  screen_method <- as.character(cfg$screen_method)
  if (is.na(screen_method) || !nzchar(screen_method)) screen_method <- "hpv"
  if (!screen_method %chin% c("hpv", "cytology")) {
    stop("cc_engine_run(): cfg$screen_method deve ser 'hpv' ou 'cytology'.", call. = FALSE)
  }
  
  eligible <- pop_sel * cov

  # Volume anual de exames de rastreamento (regra por método):
  #   HPV      → coorte elegível / intervalo de 5 anos
  #   Citologia → idem ao cálculo dentro de cc_workup_metrics() (intervalo 3 anos +
  #               parcela "primeira vez" + reforço por insatisfatórios).
  #   Delegado ao helper cc_workup_volumes() (seção 4a) — o helper
  #   sanitiza ft/uns internamente.
  if (identical(screen_method, "hpv")) {
    screened_per_year <- eligible / 5
  } else {
    screened_per_year <- cc_workup_volumes(
      eligible = eligible,
      params   = list(
        ciclo = 3,
        ft    = suppressWarnings(as.numeric(cfg$first_time_pct))     / 100,
        uns   = suppressWarnings(as.numeric(cfg$unsatisfactory_pct)) / 100
      )
    )$volume_anual
  }
  
  
  
  
  # -----------------------------
  # 2) Workup (HPV ou citologia)
  # -----------------------------
  metrics <- cc_workup_metrics(
    N_rastreada = screened_per_year,
    cfg         = cfg,
    eligible    = eligible
  )
  
  data.table::setDT(metrics)
  
  metrics[, `:=`(
    screen_method     = screen_method,
    target_age_min    = age_min,
    target_age_max    = age_max,
    pop_selected      = pop_sel,
    coverage_pct      = cov * 100,
    screened_per_year = screened_per_year,
    eligible          = eligible
    
  )]
  
  # -----------------------------
  # 3) RH (CCU)
  # -----------------------------
  hr <- cc_hr_metrics(metrics = metrics, cfg = cfg)
  
  # -----------------------------
  # 4) Retorno
  # -----------------------------
  list(
    country_code      = cfg$country_code,
    pop_mode          = cfg$pop_mode,
    is_brazil         = isTRUE(cfg$is_brazil),
    screen_method     = screen_method,
    target_age_min    = age_min,
    target_age_max    = age_max,
    pop_selected      = pop_sel,
    coverage          = cov * 100,
    screened_per_year = screened_per_year,
    metrics           = metrics,
    hr                = hr
  )
}



# -----------------------------------------------------------
# 8) Resumo "achatado" em data.table (CCU)
#    Conveniência: converte a lista devolvida por cc_engine_run()
#    em data.table de 1 linha com todas as métricas em colunas.
#    Usada quando é preciso passar o cenário para um `bind_rows`
#    (ex.: comparação entre países, export tabular).
# -----------------------------------------------------------

cc_engine_summary_dt <- function(res) {
  m <- res$metrics
  data.table::setDT(m)
  
  get1 <- function(col) {
    if (!is.null(m) && nrow(m) > 0 && col %chin% names(m)) as.numeric(m[[col]][1L]) else NA_real_
  }
  
  data.table::data.table(
    country_code        = res$country_code,
    pop_mode            = res$pop_mode,
    is_brazil           = isTRUE(res$is_brazil),
    
    screen_method       = res$screen_method,
    target_age_min      = res$target_age_min,
    target_age_max      = res$target_age_max,
    
    pop_selected        = res$pop_selected,
    coverage_percent    = res$coverage,
    screened_per_year   = res$screened_per_year,
    eligible            = get1("eligible"),
    
    
    # Saídas comuns / HPV (quando screen_method == "hpv")
    cito_reflexa        = get1("cito_reflexa"),
    colpo_indicada      = get1("colpo_indicada"),
    biopsia_indicada    = get1("biopsia_indicada"),
    ezt                 = get1("ezt"),
    alta_complexidade   = get1("alta_complexidade"),
    retorno_1ano        = get1("retorno_1ano"),
    followup_colposcopy = get1("followup_colposcopy"),
    
    # Novas métricas (citologia)
    followup_cytologies    = get1("followup_cytologies"),
    followup_colposcopies  = get1("followup_colposcopies"),
    
    # Citologia (quando screen_method == "cytology")
    cit_rastreamento    = get1("cit_rastreamento"),
    cit_diagnostica     = get1("cit_diagnostica"),
    
    
    # Mantidas por compatibilidade (preenche a partir das "indicadas")
    colposcopia         = get1("colpo_indicada"),
    biopsia             = get1("biopsia_indicada"),
    
    # Citologia: EZT (novos nomes)
    ezt_1               = get1("ezt_1"),
    ezt_2               = get1("ezt_2"),
    ezt_3               = get1("ezt_3"),
    
    ap_biopsia          = get1("ap_biopsia"),
    ap_peca_cir         = get1("ap_peca_cir")
  )
}



# -----------------------------------------------------------
# 9) Volumes SUS (SIA) para CCU — produção realizada
#    Soma procedimentos realizados no SIA-SUS por categoria.
#    Aceita dois formatos de entrada (backwards-compatible):
#      - CASO 1 (NOVO): dataset "sus_proc_resumo" já agregado por
#        categoria (coleta, citologia, colposcopia, biópsia, etc.),
#        com coluna `total_all`. Soma por `categoria`.
#      - CASO 2 (ANTIGO): dataset micro com `ano_cmp` e `PA_PROC_ID`
#        (códigos SIGTAP). Filtra pelo ano e soma por código.
#
#    IMPORTANTE: filtros geográficos (UF, município, etc.) devem
#    ser aplicados ANTES (filtrando sia_cc_resumo no chamador).
#    `sia_cc_resumo` pode ser um data.frame/data.table em memória
#    OU um caminho para um .rds (readRDS é chamado internamente).
# -----------------------------------------------------------

cc_capacity_from_sia <- function(sia_cc_resumo, ano_cmp_ref = 2024L) {
  
  # Permite passar:
  # 1) um data.frame/data.table já em memória, OU
  # 2) um caminho para .rds (ex.: path_data("sus_proc_resumo.rds") ou path_data("sia_cc_resumo.rds"))
  if (is.character(sia_cc_resumo) && length(sia_cc_resumo) == 1L) {
    dt <- tryCatch(readRDS(sia_cc_resumo), error = function(e) NULL)
    if (!is.null(dt)) {
      dt <- data.table::as.data.table(dt)
    } else {
      dt <- data.table::as.data.table(sia_cc_resumo)
    }
  } else {
    dt <- data.table::as.data.table(sia_cc_resumo)
  }
  
  
  # ------------------------------------------------------------
  # Caso 1 (NOVO): dataset SUS "resumo" já filtrado para o ano
  # Espera colunas: categoria, total_all (e opcional total_25_64),
  # e pode conter geo_ref/geo_id/PA_PROC_ID/nome_procedimento etc.
  # Aqui a agregação é feita pela categoria (mais robusto e inclui SIH como tratamento).
  # ------------------------------------------------------------
  if ("categoria" %in% names(dt) && "total_all" %in% names(dt) && !"ano_cmp" %in% names(dt)) {
    # soma por categoria usando total_all
    soma_cat <- function(cat) {
      x <- dt[categoria %chin% cat, sum(total_all, na.rm = TRUE)]
      as.numeric(x)
    }
    
    out <- list(
      coleta_total        = soma_cat("coleta"),
      citologia_total     = soma_cat("citologia"),
      colposcopia_total   = soma_cat("colposcopia"),
      biopsia_total       = soma_cat("biopsia"),
      anatomo_total       = soma_cat("anatomo"),
      cirurgia_total      = soma_cat("cirurgia"),
      radioterapia_total  = soma_cat("radioterapia"),
      quimioterapia_total = soma_cat("quimioterapia"),
      tratamento_total    = soma_cat("tratamento")
    )
    
    # Se estiver tudo zerado (ex.: filtro geográfico deixou vazio), devolve NA (mantém padrão antigo)
    if (all(vapply(out, function(z) is.na(z) || !is.finite(z) || z == 0, logical(1)))) {
      return(list(
        coleta_total         = NA_real_,
        citologia_total      = NA_real_,
        colposcopia_total    = NA_real_,
        biopsia_total        = NA_real_,
        anatomo_total        = NA_real_,
        cirurgia_total       = NA_real_,
        radioterapia_total   = NA_real_,
        quimioterapia_total  = NA_real_,
        tratamento_total     = NA_real_
      ))
    }
    
    return(out)
  }
  
  # ------------------------------------------------------------
  # Caso 2 (ANTIGO): sia_cc_resumo com ano_cmp (micro/mesmo agregado por procedimento)
  # Mantém a lógica anterior por códigos SIGTAP
  # ------------------------------------------------------------
  if (!"ano_cmp" %in% names(dt)) {
    stop("cc_capacity_from_sia(): dataset não tem 'ano_cmp' e também não parece ser o novo SUS resumo (faltam 'categoria'/'total_all').", call. = FALSE)
  }
  
  dt <- dt[ano_cmp == as.integer(ano_cmp_ref)]
  if (!nrow(dt)) {
    return(list(
      coleta_total         = NA_real_,
      citologia_total      = NA_real_,
      colposcopia_total    = NA_real_,
      biopsia_total        = NA_real_,
      anatomo_total        = NA_real_,
      cirurgia_total       = NA_real_,
      radioterapia_total   = NA_real_,
      quimioterapia_total  = NA_real_,
      tratamento_total     = NA_real_
    ))
  }
  
  vol_col <- if ("total_qtdapr" %in% names(dt)) "total_qtdapr" else "total_qtdpro"
  dt[, PA_PROC_ID := as.character(PA_PROC_ID)]
  
  # ==========================================================
  # CÓDIGOS (CCU) — alinhar com 03_prepare_SUS.R
  # ==========================================================
  proc_coleta     <- c("0201020033")
  proc_citologia  <- c("0203010019", "0203010086")
  proc_colpo      <- c("0211040029")
  proc_biopsia    <- c("0201010666")
  proc_tratamento <- c("0409060089", "0409060305", "0409060038")
  proc_anatomo    <- c("0203020022", "0203020081")
  proc_cirurgia   <- c("0416060013", "0416060056", "0416060064", "0416060080", "0416060110", "0416060137")
  proc_radio      <- c("0304010421", "0304010430")
  proc_quimio     <- c("0304020184", "0304040045")
  
  soma_proc <- function(cods) {
    x <- dt[PA_PROC_ID %in% cods, sum(get(vol_col), na.rm = TRUE)]
    as.numeric(x)
  }
  
  out <- list(
    coleta_total        = soma_proc(proc_coleta),
    citologia_total     = soma_proc(proc_citologia),
    colposcopia_total   = soma_proc(proc_colpo),
    biopsia_total       = soma_proc(proc_biopsia),
    anatomo_total       = soma_proc(proc_anatomo),
    cirurgia_total      = soma_proc(proc_cirurgia),
    radioterapia_total  = soma_proc(proc_radio),
    quimioterapia_total = soma_proc(proc_quimio),
    tratamento_total    = soma_proc(proc_tratamento)
  )
  
  out
}

# -----------------------------------------------------------
# 10) Necessidade (engine) × produção realizada (SIA)
#     Monta um data.table de 5 linhas (citologia, colposcopia,
#     biópsia, anatomo, tratamento) com:
#       - needed   → demanda vinda de `engine_res$metrics`
#       - realized → produção SIA (lista vinda de cc_capacity_from_sia)
#       - ratio_realized_needed, coverage_percent
#     É o insumo direto da aba Capacity (16_mod_capacidade.R).
# -----------------------------------------------------------

mod_capacity_compare <- function(engine_res, sia_cap) {
  if (is.null(engine_res) || is.null(sia_cap)) {
    return(data.table::data.table())
  }
  
  m <- engine_res$metrics
  if (is.null(m) || !nrow(m)) {
    return(data.table::data.table())
  }
  data.table::setDT(m)
  
  realized <- sia_cap
  if (!is.list(realized)) realized <- as.list(realized)
  
  method <- as.character(engine_res$screen_method)[1L]
  if (is.na(method) || !nzchar(method)) method <- "hpv"
  
  # --- needed (engine) ---
  getm <- function(col) {
    if (col %chin% names(m)) suppressWarnings(as.numeric(m[[col]][1L])) else NA_real_
  }
  
  # Citologia total (SIA não tem "teste HPV"; então aqui comparamos citologias)
  citologia_needed <- if ("cytologies_total_all" %chin% names(m)) {
    getm("cytologies_total_all")
  } else if (identical(method, "cytology")) {
    (if (is.finite(getm("cit_rastreamento"))) getm("cit_rastreamento") else 0) +
      (if (is.finite(getm("cit_diagnostica"))) getm("cit_diagnostica") else 0)
  } else {
    getm("cito_reflexa")
  }

  colpo_needed <- if ("colposcopies_total_all" %chin% names(m)) {
    getm("colposcopies_total_all")
  } else {
    getm("colpo_indicada")
  }
  biopsia_needed <- getm("biopsia_indicada")
  
  # AP: se existir ap_biopsia/ap_peca_cir, usa soma; senão assume ~1 AP por biópsia
  anatomo_needed <- if ("ap_biopsia" %chin% names(m) || "ap_peca_cir" %chin% names(m)) {
    (if (is.finite(getm("ap_biopsia"))) getm("ap_biopsia") else 0) +
      (if (is.finite(getm("ap_peca_cir"))) getm("ap_peca_cir") else 0)
  } else {
    biopsia_needed
  }
  
  tratamento_needed <- getm("ezt")
  
  out <- data.table::data.table(
    item = c("citologia_total", "colposcopia_total", "biopsia_total", "anatomo_total", "tratamento_total"),
    needed = c(citologia_needed, colpo_needed, biopsia_needed, anatomo_needed, tratamento_needed),
    realized = c(
      suppressWarnings(as.numeric(realized$citologia_total)),
      suppressWarnings(as.numeric(realized$colposcopia_total)),
      suppressWarnings(as.numeric(realized$biopsia_total)),
      suppressWarnings(as.numeric(realized$anatomo_total)),
      suppressWarnings(as.numeric(realized$tratamento_total))
    )
  )
  
  out[, `:=`(
    ratio_realized_needed = data.table::fifelse(!is.na(needed) & needed > 0, realized / needed, NA_real_),
    coverage_percent      = data.table::fifelse(!is.na(needed) & needed > 0, 100 * realized / needed, NA_real_)
  )][]
  
  out
}

# -----------------------------------------------------------
# 11) Modelo clínico HPV
#     Função pura (sem efeitos colaterais) que implementa o
#     fluxo de rastreamento HPV:
#       N rastreadas → 3 braços de resultado HPV
#         - 16/18+         → direto para colposcopia
#         - HR-HPV+ outros → citologia reflexa → colposcopia se positiva
#         - HPV−           → alta (com proporção de seguimento)
#       → biópsia (taxas distintas por braço)
#       → desfechos (NIC1/neg, NIC2/3 → EZT, câncer → alta complexidade)
#       → follow-up (retorno + colposcopia proporcional a hpv_followup_pos)
#
#     Todos os parâmetros `p*`, `cito_out_*`, `colpo*`, `b*` e
#     `hpv_followup_pos` são proporções (0–1), JÁ divididos por 100
#     (a divisão acontece em cc_workup_metrics()).
#
#     Retorna data.table de 1 linha (colunas usadas em cc_hr_metrics,
#     mod_capacity_compare, render_pathway_hpv).
# -----------------------------------------------------------

modelo_hpv <- function(N,
                       p16_18, poutros, pneg,
                       cito_out_pos, cito_out_neg,
                       colpo16_pos, colpo16_neg,
                       colpoout_pos, colpoout_neg,
                       b16_neg_nic1, b16_nic23, b16_cancer,
                       bo_neg_nic1,  bo_nic23,  bo_cancer,  hpv_followup_pos) {
  
  # Renormaliza pesos (garante soma = 1 mesmo se p16_18+poutros+pneg ≠ 1)
  tot <- p16_18 + poutros + pneg
  if (!is.finite(tot) || is.na(tot) || tot <= 0) tot <- 1

  w16  <- p16_18 / tot
  wout <- poutros / tot

  # Braços HPV em número absoluto
  n16  <- N * w16      # HPV 16/18+  → colposcopia imediata
  nout <- N * wout     # outros HR+  → citologia reflexa

  # Citologia reflexa (apenas braço "outros"): o total equivale a nout
  cito_reflexa <- nout
  cito_pos_outros <- nout * cito_out_pos
  cito_neg_outros <- nout * cito_out_neg

  # Colposcopia = HPV 16/18+ (todos) + braço "outros" com citologia positiva
  colpo_indicada <- n16 + cito_pos_outros
  
  colpo_pos <- n16 * colpo16_pos + cito_pos_outros * colpoout_pos
  colpo_neg <- pmax(colpo_indicada - colpo_pos, 0)
  
  biopsia_indicada <- colpo_pos
  
  biop_16  <- n16 * colpo16_pos
  biop_out <- cito_pos_outros * colpoout_pos
  
  biop_neg_16  <- biop_16  * b16_neg_nic1
  ezt_16       <- biop_16  * b16_nic23
  cancer_16    <- biop_16  * b16_cancer
  
  biop_neg_out <- biop_out * bo_neg_nic1
  ezt_out      <- biop_out * bo_nic23
  cancer_out   <- biop_out * bo_cancer
  
  ezt  <- ezt_16 + ezt_out
  alta <- cancer_16 + cancer_out
  
  # Retorno em 1 ano: biópsias negativas + colpo negativas + cito reflexa negativas + 2× EZT (controle pós-tratamento)
  retorno_1ano <- biop_neg_16 + biop_neg_out + colpo_neg + cito_neg_outros + 2*ezt
  followup_colposcopy <- retorno_1ano * hpv_followup_pos

  # Totais "all-in" para cálculo de recursos (incluem follow-up)
  # Nota: HPV não tem "citologia diagnóstica"; a única citologia é a reflexa.
  cytologies_total_all   <- cito_reflexa
  colposcopies_total_all <- colpo_indicada + followup_colposcopy
  
  data.table::data.table(
    rastreada         = N,
    cito_reflexa      = cito_reflexa,
    colpo_indicada    = colpo_indicada,
    biopsia_indicada  = biopsia_indicada,
    ezt               = ezt,
    alta_complexidade = alta,
    retorno_1ano      = retorno_1ano,
    followup_colposcopy = followup_colposcopy,
    cytologies_total_all    = cytologies_total_all,
    colposcopies_total_all  = colposcopies_total_all
    
  )
}

