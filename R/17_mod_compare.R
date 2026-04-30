# ===========================================================
# 17_mod_compare.R — aba "Compare"
# -----------------------------------------------------------
# Renderiza dois cenários lado a lado:
#   • Scenario A — herda a configuração atual da sidebar (input_global)
#   • Scenario B — usa um painel próprio (à direita) com inputs com sufixo "_b"
#
# Geografia (país + filtros Brasil) é COMPARTILHADA: B sempre herda de A.
# Apenas População customizada (custom_pop) também é herdada — exibida read-only.
# Cobertura, faixa etária, método e parâmetros são editáveis no painel B.
#
# Pipeline (server):
#   input_global  ──► cfg_a ──► res_a ──► dt_a ─┐
#   input$*_b     ──► cfg_b ──► res_b ──► dt_b ─┴──► tabela comparativa (Δ B vs A)
#
# UI:
#   • compare-results    — header + scenario_badges + compare_table
#   • scen-b-panel       — accordion (Population / Protocol / Coverage / Resources)
#                          espelha a sidebar mas com IDs sufixados "_b"
#
# Padrões replicados de outros módulos:
#   • val_or / fmt_or_dash         (idem 11/14/16) — candidatos a utils
#   • get_*_preset_params          (espelha load_*_preset de 10_mod_filters_cc.R)
#   • lock_pair_b / lock_triplet_b (espelha lock_pair/lock_triplet de 10)
#   • cfg_a                         (espelha cfg de 11_mod_resumo_geral.R)
# ===========================================================

mod_compare_ui <- function(id) {
  ns <- NS(id)

  # Layout em 2 colunas: resultados à esquerda, painel do Scenario B à direita
  # (controle de largura em www/style.css → .compare-layout)
  div(
    class = "compare-layout",

    # ── Área de resultados (col. esquerda) ─────────────────────────────
    div(
      class = "compare-results",
      div(
        class = "cc-page-header",
        div(class = "cc-page-title", "Compare"),
        # Subtítulo dinâmico: descreve a geografia compartilhada (output abaixo)
        div(class = "cc-page-subtitle", uiOutput(ns("result_desc")))
      ),
      # Badges resumindo método/idade/cobertura de A e B
      uiOutput(ns("scenario_badges")),
      # Tabela comparativa A | B | Δ B vs A
      uiOutput(ns("compare_table"))
    ),
    # ── Painel Scenario B (col. direita) ───────────────────────────────
    # Accordion paralelo à sidebar, mas só com o que é editável no cenário B.
    # País / filtros Brasil ficam travados (geo_locked) — herdam de A.
    div(
      class = "scen-b-panel",
      div(class = "scen-b-header", "Scenario B"),
      div(class = "scen-b-subheader", "Geography is shared with Scenario A."),
      uiOutput(ns("geo_locked")),  # mostra a geografia herdada (read-only)

      # ── Acordeão 1: Population ──────────────────────────────────────
      # População é sempre herdada (GLOBOCAN ou custom_pop). Render no server.
      tags$details(
        class = "scen-b-acc", open = NA,
        tags$summary("Population"),
        div(
          class = "scen-b-acc-body",
          uiOutput(ns("pop_b_ui"))
        )
      ),
      
      # ── Acordeão 2: Screening protocol (com Customize) ─────────────
      # Espelha 10_mod_filters_cc.R, mas com IDs sufixados "_b"
      # e dois conditionalPanels que dependem de input$screen_method_b
      tags$details(
        class = "scen-b-acc", open = NA,
        tags$summary("Screening protocol"),
        div(
          class = "scen-b-acc-body",

          # Método de rastreamento do cenário B
          radioButtons(ns("screen_method_b"), NULL,
                       choices  = c("HPV test" = "hpv", "Cytology" = "cytology"),
                       selected = "hpv"
          ),
          fluidRow(
            column(6, selectInput(ns("target_age_min_b"), "Age from",
                                  choices  = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70),
                                  selected = 25
            )),
            column(6, selectInput(ns("target_age_max_b"), "Age to",
                                  choices  = c(24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74),
                                  selected = 64
            ))
          ),
          
          # ── HPV: seletor de fonte + painel Customize ────────────────────
          # Choices construídas a partir de HPV_PRESETS (00_constants_cc.R) +
          # opção "Customize" que abre o sub-painel com os 16 inputs HPV.
          conditionalPanel(
            condition = sprintf("input['%s'] == 'hpv'", ns("screen_method_b")),
            {
              hpv_choices_b <- c(
                setNames(names(HPV_PRESETS), sapply(HPV_PRESETS, `[[`, "label")),
                "Customize" = "custom"
              )
              radioButtons(ns("hpv_param_source_b"), "Parameter source",
                           choices  = hpv_choices_b,
                           selected = names(HPV_PRESETS)[1])
            },
            # Painel "Customize HPV" — só aparece quando fonte = custom
            conditionalPanel(
              condition = sprintf("input['%s'] == 'custom'", ns("hpv_param_source_b")),
              tags$div(
                style = "border:1px solid #ddd; padding:10px; border-radius:8px; margin-top:8px; background:#fbfdfd;",
                
                fluidRow(
                  column(6, checkboxInput(ns("lock_prop_b"),   "Lock proportions", value = TRUE)),
                  column(6, actionButton(ns("reset_params_b"), "Reset", class = "btn-secondary btn-sm"))
                ),
                br(),
                
                h5("HPV prevalence"),
                numericInput(ns("p16_18_b"),  "HPV 16/18 (%)",  value = round(HPV_DEFAULTS$p16_18,  2), min = 0, max = 100, step = 0.01),
                numericInput(ns("poutros_b"), "HPV other (%)",  value = round(HPV_DEFAULTS$poutros, 2), min = 0, max = 100, step = 0.01),
                numericInput(ns("pneg_b"),    "Negative (%)",   value = round(HPV_DEFAULTS$pneg,    2), min = 0, max = 100, step = 0.01),
                
                h5("Cytology result (HPV other)"),
                numericInput(ns("cito_out_pos_b"), "Positive (%)", value = round(HPV_DEFAULTS$cito_out_pos, 2), min = 0, max = 100, step = 0.01),
                numericInput(ns("cito_out_neg_b"), "Negative (%)", value = round(HPV_DEFAULTS$cito_out_neg, 2), min = 0, max = 100, step = 0.01),
                
                h5("Colposcopy (HPV 16/18)"),
                numericInput(ns("colpo16_pos_b"), "Positive (%)", value = round(HPV_DEFAULTS$colpo16_pos, 2), min = 0, max = 100, step = 0.01),
                numericInput(ns("colpo16_neg_b"), "Negative (%)", value = round(HPV_DEFAULTS$colpo16_neg, 2), min = 0, max = 100, step = 0.01),
                
                h5("Colposcopy (HPV other)"),
                numericInput(ns("colpoout_pos_b"), "Positive (%)", value = round(HPV_DEFAULTS$colpoout_pos, 2), min = 0, max = 100, step = 0.01),
                numericInput(ns("colpoout_neg_b"), "Negative (%)", value = round(HPV_DEFAULTS$colpoout_neg, 2), min = 0, max = 100, step = 0.01),
                
                h5("Biopsy (HPV 16/18)"),
                numericInput(ns("b16_neg_nic1_b"), "Negative / CIN1 (%)", value = round(HPV_DEFAULTS$b16_neg_nic1, 2), min = 0, max = 100, step = 0.01),
                numericInput(ns("b16_nic23_b"),    "CIN2 / CIN3 (%)",     value = round(HPV_DEFAULTS$b16_nic23,    2), min = 0, max = 100, step = 0.01),
                numericInput(ns("b16_cancer_b"),   "Cancer (%)",          value = round(HPV_DEFAULTS$b16_cancer,   2), min = 0, max = 100, step = 0.01),
                
                h5("Biopsy (HPV other)"),
                numericInput(ns("bo_neg_nic1_b"), "Negative / CIN1 (%)", value = round(HPV_DEFAULTS$bo_neg_nic1, 2), min = 0, max = 100, step = 0.01),
                numericInput(ns("bo_nic23_b"),    "CIN2 / CIN3 (%)",     value = round(HPV_DEFAULTS$bo_nic23,    2), min = 0, max = 100, step = 0.01),
                numericInput(ns("bo_cancer_b"),   "Cancer (%)",          value = round(HPV_DEFAULTS$bo_cancer,   2), min = 0, max = 100, step = 0.01),
                
                h5("Follow-up HPV"),
                numericInput(ns("hpv_followup_pos_pct_b"), "HPV positivity at follow-up (%)",
                             value = round(HPV_DEFAULTS$hpv_followup_pos_pct, 2), min = 0, max = 100, step = 0.01),
                
                uiOutput(ns("params_alert_b"))
              )
            )
          ),
          
          # ── Citologia: seletor de fonte + painel Customize ──────────────
          # Choices a partir de CITO_PRESETS_META (00_constants_cc.R) — flag
          # `por_uf` controla se a fonte aceita filtragem por UF (ex.: SISCAN).
          # Nota: cenário B NÃO permite escolher UF independente — a UF é
          # herdada de A, e get_cito_preset_params() resolve para "brasil"
          # quando 0 ou >1 UFs estão selecionadas (mesmo fallback de 10).
          conditionalPanel(
            condition = sprintf("input['%s'] == 'cytology'", ns("screen_method_b")),
            {
              cito_choices_b <- c(
                setNames(names(CITO_PRESETS_META), sapply(CITO_PRESETS_META, `[[`, "label")),
                "Customize" = "custom"
              )
              radioButtons(ns("cito_param_source_b"), "Parameter source",
                           choices  = cito_choices_b,
                           selected = names(CITO_PRESETS_META)[1])
            },
            # Painel "Customize Cytology" — só aparece quando fonte = custom
            conditionalPanel(
              condition = sprintf("input['%s'] == 'custom'", ns("cito_param_source_b")),
              tags$div(
                style = "border:1px solid #ddd; padding:10px; border-radius:8px; margin-top:8px; background:#fbfdfd;",
                
                fluidRow(
                  column(6, checkboxInput(ns("lock_prop_cito_b"),   "Lock proportions", value = TRUE)),
                  column(6, actionButton(ns("reset_params_cito_b"), "Reset", class = "btn-secondary btn-sm"))
                ),
                br(),
                
                h5("Exam volume"),
                numericInput(ns("first_time_pct_b"),     "First-time exams (%)",    value = CITO_DEFAULTS$first_time_pct,     min = 0, max = 100, step = 0.01),
                numericInput(ns("unsatisfactory_pct_b"), "Unsatisfactory (%)",      value = CITO_DEFAULTS$unsatisfactory_pct, min = 0, max = 100, step = 0.01),
                
                h5("Cytology result"),
                numericInput(ns("res_asch_pct_b"),  "HSIL / ASC-H / AOI / AIS / Ca (%)", value = CITO_DEFAULTS$res_asch_pct,  min = 0, max = 100, step = 0.01),
                numericInput(ns("res_other_pct_b"), "Other abnormalities (%)",           value = CITO_DEFAULTS$res_other_pct, min = 0, max = 100, step = 0.01),
                numericInput(ns("res_neg_pct_b"),   "Negative (%)",                      value = CITO_DEFAULTS$res_neg_pct,   min = 0, max = 100, step = 0.01),
                
                h5("Colposcopy referral"),
                numericInput(ns("colpo_asch_pct_b"),         "After HSIL / ASC-H / AOI / AIS / Ca (%)", value = CITO_DEFAULTS$colpo_asch_pct,         min = 0, max = 100, step = 0.01),
                numericInput(ns("colpo_other_follow_pct_b"), "After other abnormalities (%)",           value = CITO_DEFAULTS$colpo_other_follow_pct, min = 0, max = 100, step = 0.01),
                
                h5("Colposcopy positivity"),
                numericInput(ns("biopsy_pos_asch_pct_b"),  "HSIL / ASC-H arm (%)",       value = CITO_DEFAULTS$biopsy_pos_asch_pct,  min = 0, max = 100, step = 0.01),
                numericInput(ns("biopsy_pos_other_pct_b"), "Other abnormalities arm (%)", value = CITO_DEFAULTS$biopsy_pos_other_pct, min = 0, max = 100, step = 0.01),
                
                h5("Biopsy result (HSIL / ASC-H arm)"),
                numericInput(ns("b_asch_neg_nic1_pct_b"), "Negative / CIN1 (%)", value = CITO_DEFAULTS$b_asch_neg_nic1_pct, min = 0, max = 100, step = 0.01),
                numericInput(ns("b_asch_nic23_pct_b"),    "CIN2 / CIN3 (%)",     value = CITO_DEFAULTS$b_asch_nic23_pct,    min = 0, max = 100, step = 0.01),
                numericInput(ns("b_asch_cancer_pct_b"),   "Cancer (%)",          value = CITO_DEFAULTS$b_asch_cancer_pct,   min = 0, max = 100, step = 0.01),
                
                h5("Biopsy result (Other arm)"),
                numericInput(ns("b_other_neg_nic1_pct_b"), "Negative / CIN1 (%)", value = CITO_DEFAULTS$b_other_neg_nic1_pct, min = 0, max = 100, step = 0.01),
                numericInput(ns("b_other_nic23_pct_b"),    "CIN2 / CIN3 (%)",     value = CITO_DEFAULTS$b_other_nic23_pct,    min = 0, max = 100, step = 0.01),
                numericInput(ns("b_other_cancer_pct_b"),   "Cancer (%)",          value = CITO_DEFAULTS$b_other_cancer_pct,   min = 0, max = 100, step = 0.01),
                
                uiOutput(ns("params_alert_cito_b"))
              )
            )
          )
        )
      ),
      
      # ── Acordeão 3: Screening coverage ──────────────────────────────
      # Slider 0–100% — independente do A
      tags$details(
        class = "scen-b-acc", open = NA,
        tags$summary("Screening coverage"),
        div(
          class = "scen-b-acc-body",
          sliderInput(ns("coverage_b"), NULL,
                      min = 0, max = 100, value = 70, step = 5, post = "%"
          )
        )
      ),

      # ── Acordeão 4: Resources (capacidades) ─────────────────────────
      # Mesma estrutura da sidebar (10): inputs em unidade configurável,
      # anualizados depois em cfg_b via cap_mult_b.
      tags$details(
        class = "scen-b-acc",
        tags$summary("Resources"),
        div(
          class = "scen-b-acc-body",
          radioButtons(ns("cap_unidade_b"), "Capacity unit:",
                       choices  = c("Day" = "dia", "Week" = "semana", "Month" = "mes", "Year" = "ano"),
                       selected = "ano",
                       inline   = TRUE
          ),
          numericInput(ns("cap_colposcopio_b"),  "Colposcope (per unit)",      value = 5760, min = 1, step = 1),
          numericInput(ns("cap_colposcopista_b"),"Colposcopist (per unit)",    value = 2880, min = 1, step = 1),
          numericInput(ns("cap_citopato_b"),      "Cytopathologist (per unit)", value = 14400, min = 1, step = 1),
          numericInput(ns("cap_patologista_b"),   "Pathologist (per unit)",     value = 7200, min = 1, step = 1)
        )
      )
    )
  )
}


# ===========================================================
# SERVER
# ===========================================================

mod_compare_server <- function(id, input_global, df_completo, dim_country, pop_mun_regional,
                               cito_presets = NULL) {
  moduleServer(id, function(input, output, session) {

    # ===========================================================
    # BLOCO 1 — Helpers locais
    # -----------------------------------------------------------
    # val_or / fmt_or_dash duplicam helpers já existentes em 11/14/16
    # (candidatos a mover para 01_utils_cc.R). fmt_dec1 é exclusivo
    # deste módulo (cards de RH com 1 casa decimal).
    # ===========================================================
    val_or <- function(x, default) {
      if (is.null(x) || length(x) == 0L || all(is.na(x))) default else x
    }
    fmt_or_dash <- function(x) {
      if (is.null(x) || length(x) == 0L || is.na(x)) "\u2013"
      else fmt_int(round(suppressWarnings(as.numeric(x))))
    }
    fmt_dec1 <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (!is.finite(x)) "\u2013" else sprintf("%.1f", x)
    }

    # ===========================================================
    # BLOCO 2 — Identificação do Brasil
    # -----------------------------------------------------------
    # Helper único cc_country_info() em 01_utils_cc.R devolve
    # list(code, label, is_brazil) a partir de input_global() e
    # dim_country. Substitui o trio replicado em 11/14/15/16/17.
    # ===========================================================
    br_code <- tryCatch({
      x <- dim_country[dim_country$population_name == "Brazil", "population_code"]
      if (length(x) == 0L || is.na(x[1])) NA_integer_ else as.integer(x[1])
    }, error = function(e) NA_integer_)

    ci <- reactive(cc_country_info(input_global(), dim_country, br_code))

    # ===========================================================
    # BLOCO 3 — Resolvedores de presets (HPV e Citologia)
    # -----------------------------------------------------------
    # Espelham `load_*_preset` de 10_mod_filters_cc.R, mas como
    # funções puras (não disparam updateNumericInput por si).
    # Usadas pelos observers do BLOCO 5.
    # ===========================================================
    # ---- preset helpers ----
    get_hpv_preset_params <- function(src) {
      if (is.null(src) || !exists("HPV_PRESETS", inherits = TRUE)) return(list())
      p <- tryCatch(get("HPV_PRESETS", inherits = TRUE)[[src]]$params, error = function(e) NULL)
      if (is.null(p)) list() else p
    }

    get_cito_preset_params <- function(src, uf = NULL) {
      if (is.null(src) || is.null(cito_presets)) return(list())
      if (!src %in% names(cito_presets)) return(list())
      fonte  <- cito_presets[[src]]
      meta   <- if (exists("CITO_PRESETS_META", inherits = TRUE))
                  get("CITO_PRESETS_META", inherits = TRUE) else list()
      por_uf <- isTRUE(meta[[src]]$por_uf)
      uf_key <- if (por_uf && !is.null(uf) && length(uf) == 1L) uf[1] else "brasil"
      p <- if (uf_key %in% names(fonte)) fonte[[uf_key]] else fonte[["brasil"]]
      if (is.null(p)) list() else as.list(p)
    }

    # ===========================================================
    # BLOCO 4 — Rótulo geográfico (geo_text)
    # -----------------------------------------------------------
    # Subtítulo "Compare": "Brazil - <Total/SUS> - <primeiro nível
    # com seleção>" (no Brasil) ou nome do país (fora). Helper único
    # cc_geo_label() em 01_utils_cc.R, modo "concat" com pop_tipo e
    # first_level_only — diferente do geo_desc dos outros módulos
    # porque aqui só o 1º nível com seleção entra na string.
    # ===========================================================
    geo_text <- reactive({
      cc_geo_label(input_global(), mode = "concat",
                   dim_country = dim_country, br_code = br_code,
                   pop_tipo = TRUE, first_level_only = TRUE)
    })

    # ===========================================================
    # BLOCO 5 — Sincronização Preset → inputs do Scenario B
    # -----------------------------------------------------------
    # Quando o usuário escolhe uma fonte (≠ "custom"), os 16 inputs
    # numéricos do painel Customize são pré-preenchidos com os
    # parâmetros da fonte. Essencial para que ao trocar para
    # "Customize" os valores partam do preset, não dos defaults.
    # ===========================================================
    # ── Sincroniza inputs numéricos com presets no Scenario B ────────────
    # 5a) HPV — observa input$hpv_param_source_b
    observeEvent(input$hpv_param_source_b, {
      if (!identical(input$screen_method_b, "hpv")) return()
      src <- input$hpv_param_source_b
      if (is.null(src) || identical(src, "custom")) return()
      if (!exists("HPV_PRESETS", inherits = TRUE)) return()
      presets <- get("HPV_PRESETS", inherits = TRUE)
      if (!(src %in% names(presets))) return()
      p <- presets[[src]]$params
      for (nm in names(p)) {
        updateNumericInput(session, paste0(nm, "_b"), value = round(p[[nm]], 2))
      }
    }, ignoreInit = TRUE)

    # 5b) Citologia — observa input$cito_param_source_b E filt_uf
    # (a fonte SISCAN é por_uf=TRUE → resolve a UF herdada de A;
    # 0 ou >1 UFs ⇒ fallback para "brasil")
    observe({
      src    <- input$cito_param_source_b
      g      <- input_global()
      uf_sel <- g$filt_uf
      if (!identical(input$screen_method_b, "cytology")) return()
      if (is.null(src) || identical(src, "custom")) return()
      if (is.null(cito_presets) || !(src %in% names(cito_presets))) return()

      meta_obj <- if (exists("CITO_PRESETS_META", inherits = TRUE))
                    get("CITO_PRESETS_META", inherits = TRUE) else list()
      por_uf   <- isTRUE(meta_obj[[src]]$por_uf)
      uf_key   <- if (por_uf && length(uf_sel) == 1L) uf_sel[1] else "brasil"

      fonte <- cito_presets[[src]]
      p     <- if (uf_key %in% names(fonte)) fonte[[uf_key]] else fonte[["brasil"]]
      if (is.null(p)) return()

      for (nm in names(p)) {
        updateNumericInput(session, paste0(nm, "_b"), value = round(p[[nm]], 3))
      }
    })

    # ===========================================================
    # BLOCO 6 — Botões "Reset" do Scenario B
    # -----------------------------------------------------------
    # Restauram HPV_DEFAULTS / CITO_DEFAULTS nos 16 inputs do
    # painel Customize do método ativo. Não tocam na fonte (`*_b`).
    # ===========================================================
    # ── Reset dos parâmetros do Scenario B ───────────────────────────────
    observeEvent(input$reset_params_b, {
      if (!identical(input$screen_method_b, "hpv")) return()
      for (nm in names(HPV_DEFAULTS)) {
        updateNumericInput(session, paste0(nm, "_b"), value = round(HPV_DEFAULTS[[nm]], 2))
      }
    }, ignoreInit = TRUE)

    observeEvent(input$reset_params_cito_b, {
      if (!identical(input$screen_method_b, "cytology")) return()
      for (nm in names(CITO_DEFAULTS)) {
        updateNumericInput(session, paste0(nm, "_b"), value = CITO_DEFAULTS[[nm]])
      }
    }, ignoreInit = TRUE)

    # ===========================================================
    # BLOCO 7 — Auto-balance "Lock proportions" (Scenario B)
    # -----------------------------------------------------------
    # Espelha lock_pair / lock_triplet de 10_mod_filters_cc.R, com
    # estado próprio (rv_lock_b) e nomes sufixados "_b". Mantém
    # somas = 100% identificando qual input mudou (via prev).
    # `which_method` evita o observer disparar quando o método
    # ativo do cenário B não corresponde (HPV vs Citologia).
    # ===========================================================
    # ── Auto-balance de proporções (Scenario B) ─────────────────────────
    rv_lock_b <- reactiveValues(updating = FALSE, prev = list())

    clamp100_b <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (length(x) == 0L || is.na(x)) x <- 0
      max(0, min(100, x))
    }
    approx_eq_b <- function(a, b, tol = 0.01) {
      a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
      if (is.na(a) && is.na(b)) return(TRUE)
      if (is.na(a) || is.na(b)) return(FALSE)
      abs(a - b) <= tol
    }
    safe_update_b <- function(id, val) updateNumericInput(session, id, value = round(val, 2))

    lock_pair_b <- function(idA, idB, which_method, tol = 0.01) {
      observeEvent(list(input[[idA]], input[[idB]], input$lock_prop_b, input$screen_method_b), {
        if (!identical(input$screen_method_b, which_method)) return()
        if (!isTRUE(input$lock_prop_b)) return()
        if (isTRUE(rv_lock_b$updating)) return()
        a <- clamp100_b(input[[idA]]); b <- clamp100_b(input[[idB]])
        prevA <- rv_lock_b$prev[[idA]]; if (is.null(prevA)) prevA <- a
        prevB <- rv_lock_b$prev[[idB]]; if (is.null(prevB)) prevB <- b
        if (abs((a + b) - 100) <= tol && approx_eq_b(a, prevA, tol) && approx_eq_b(b, prevB, tol)) return()
        changedA <- !approx_eq_b(a, prevA, tol); changedB <- !approx_eq_b(b, prevB, tol)
        primary <- if (changedA && !changedB) idA else if (changedB && !changedA) idB else idA
        rv_lock_b$updating <- TRUE
        if (identical(primary, idA)) { newA <- a; newB <- 100 - newA; safe_update_b(idB, newB) }
        else                         { newB <- b; newA <- 100 - newB; safe_update_b(idA, newA) }
        rv_lock_b$prev[[idA]] <- if (identical(primary, idA)) newA else 100 - newB
        rv_lock_b$prev[[idB]] <- if (identical(primary, idA)) newB else newB
        rv_lock_b$updating <- FALSE
      }, ignoreInit = TRUE)
    }

    lock_triplet_b <- function(idA, idB, idC, remainder_id, adjust_id, which_method,
                               lock_input = "lock_prop_b", tol = 0.01) {
      ids <- c(idA, idB, idC)
      observeEvent(c(lapply(ids, function(i) input[[i]]),
                     list(input[[lock_input]], input$screen_method_b)), {
        if (!identical(input$screen_method_b, which_method)) return()
        if (!isTRUE(input[[lock_input]])) return()
        if (isTRUE(rv_lock_b$updating)) return()
        v <- lapply(ids, function(i) clamp100_b(input[[i]])); names(v) <- ids
        prev <- lapply(ids, function(i) rv_lock_b$prev[[i]]); names(prev) <- ids
        for (i in ids) if (is.null(prev[[i]])) prev[[i]] <- v[[i]]
        changed <- vapply(ids, function(i) !approx_eq_b(v[[i]], prev[[i]], tol), logical(1))
        primary <- if (sum(changed) == 1) ids[which(changed)] else remainder_id
        other_ids <- setdiff(ids, remainder_id)
        rv_lock_b$updating <- TRUE
        if (!identical(primary, remainder_id)) {
          s <- v[[other_ids[1]]] + v[[other_ids[2]]]
          if (s <= 100) { new_rem <- 100 - s }
          else {
            new_rem <- 0
            fixed_id <- setdiff(other_ids, adjust_id)
            v[[adjust_id]] <- max(0, 100 - v[[fixed_id]])
          }
          v[[remainder_id]] <- new_rem
          safe_update_b(remainder_id, v[[remainder_id]])
          if (s > 100) safe_update_b(adjust_id, v[[adjust_id]])
        } else {
          fixed_id <- setdiff(other_ids, adjust_id)
          v[[adjust_id]] <- max(0, 100 - v[[fixed_id]] - v[[remainder_id]])
          safe_update_b(adjust_id, v[[adjust_id]])
        }
        for (i in ids) rv_lock_b$prev[[i]] <- v[[i]]
        rv_lock_b$updating <- FALSE
      }, ignoreInit = TRUE)
    }

    # HPV — pares
    lock_pair_b("cito_out_pos_b", "cito_out_neg_b", "hpv")
    lock_pair_b("colpo16_pos_b",  "colpo16_neg_b",  "hpv")
    lock_pair_b("colpoout_pos_b", "colpoout_neg_b", "hpv")

    # HPV — trios
    lock_triplet_b("p16_18_b", "poutros_b", "pneg_b",
                   remainder_id = "pneg_b", adjust_id = "poutros_b", which_method = "hpv")
    lock_triplet_b("b16_neg_nic1_b", "b16_nic23_b", "b16_cancer_b",
                   remainder_id = "b16_neg_nic1_b", adjust_id = "b16_nic23_b", which_method = "hpv")
    lock_triplet_b("bo_neg_nic1_b", "bo_nic23_b", "bo_cancer_b",
                   remainder_id = "bo_neg_nic1_b", adjust_id = "bo_nic23_b", which_method = "hpv")

    # Citologia — trios
    lock_triplet_b("res_asch_pct_b", "res_other_pct_b", "res_neg_pct_b",
                   remainder_id = "res_neg_pct_b", adjust_id = "res_other_pct_b",
                   which_method = "cytology", lock_input = "lock_prop_cito_b")
    lock_triplet_b("b_asch_neg_nic1_pct_b", "b_asch_nic23_pct_b", "b_asch_cancer_pct_b",
                   remainder_id = "b_asch_neg_nic1_pct_b", adjust_id = "b_asch_nic23_pct_b",
                   which_method = "cytology", lock_input = "lock_prop_cito_b")
    lock_triplet_b("b_other_neg_nic1_pct_b", "b_other_nic23_pct_b", "b_other_cancer_pct_b",
                   remainder_id = "b_other_neg_nic1_pct_b", adjust_id = "b_other_nic23_pct_b",
                   which_method = "cytology", lock_input = "lock_prop_cito_b")

    # ===========================================================
    # BLOCO 8 — Validação de soma = 100% (Scenario B)
    # -----------------------------------------------------------
    # Mostra alertas em params_alert_b / params_alert_cito_b
    # quando o usuário desativa "Lock proportions" e os grupos
    # de proporções deixam de somar 100 (com tolerância 0.1).
    # ===========================================================
    # ── Validações de soma = 100% (Scenario B) ───────────────────────────
    sum_check_b <- function(x, tol = 0.1) {
      x <- suppressWarnings(as.numeric(x)); x[is.na(x)] <- 0
      abs(sum(x) - 100) <= tol
    }

    output$params_alert_b <- renderUI({
      if (!identical(input$screen_method_b, "hpv")) return(NULL)
      if (!identical(input$hpv_param_source_b, "custom")) return(NULL)
      errs <- character(0)
      if (!sum_check_b(c(input$p16_18_b, input$poutros_b, input$pneg_b)))               errs <- c(errs, "HPV prevalence must sum to 100%.")
      if (!sum_check_b(c(input$cito_out_pos_b, input$cito_out_neg_b)))                   errs <- c(errs, "Reflex cytology must sum to 100%.")
      if (!sum_check_b(c(input$colpo16_pos_b, input$colpo16_neg_b)))                     errs <- c(errs, "Colposcopy (HPV 16/18) must sum to 100%.")
      if (!sum_check_b(c(input$colpoout_pos_b, input$colpoout_neg_b)))                   errs <- c(errs, "Colposcopy (HPV other) must sum to 100%.")
      if (!sum_check_b(c(input$b16_neg_nic1_b, input$b16_nic23_b, input$b16_cancer_b))) errs <- c(errs, "Biopsy (HPV 16/18) must sum to 100%.")
      if (!sum_check_b(c(input$bo_neg_nic1_b,  input$bo_nic23_b,  input$bo_cancer_b)))  errs <- c(errs, "Biopsy (HPV other) must sum to 100%.")
      if (!length(errs)) return(NULL)
      div(style = "background:var(--cc-danger-bg);border:1px solid var(--cc-danger);color:var(--cc-danger);padding:8px;border-radius:6px;margin-top:6px;font-size:var(--t-xs);",
          strong("Please check parameters:"), tags$ul(lapply(errs, tags$li)))
    })

    output$params_alert_cito_b <- renderUI({
      if (!identical(input$screen_method_b, "cytology")) return(NULL)
      if (!identical(input$cito_param_source_b, "custom")) return(NULL)
      errs <- character(0)
      if (!sum_check_b(c(input$res_asch_pct_b, input$res_other_pct_b, input$res_neg_pct_b)))
        errs <- c(errs, "Cytology result must sum to 100%.")
      if (!sum_check_b(c(input$b_asch_neg_nic1_pct_b, input$b_asch_nic23_pct_b, input$b_asch_cancer_pct_b)))
        errs <- c(errs, "Biopsy result (HSIL / ASC-H arm) must sum to 100%.")
      if (!sum_check_b(c(input$b_other_neg_nic1_pct_b, input$b_other_nic23_pct_b, input$b_other_cancer_pct_b)))
        errs <- c(errs, "Biopsy result (Other arm) must sum to 100%.")
      if (!length(errs)) return(NULL)
      div(style = "background:var(--cc-danger-bg);border:1px solid var(--cc-danger);color:var(--cc-danger);padding:8px;border-radius:6px;margin-top:6px;font-size:var(--t-xs);",
          strong("Please check parameters:"), tags$ul(lapply(errs, tags$li)))
    })

    # ===========================================================
    # BLOCO 9 — cfg_a: configuração do Scenario A
    # -----------------------------------------------------------
    # Wrapper único: cc_cfg_from_input() em 02_engine_capacity_cc.R
    # encapsula coerção/clamp/retrocompat e é compartilhado entre
    # Summary/Equipment/Pathway/Capacity/Compare.
    # ===========================================================
    cfg_a <- reactive({
      cc_cfg_from_input(input_global(), br_code)
    })

    # ===========================================================
    # BLOCO 10 — cfg_b: configuração do Scenario B
    # -----------------------------------------------------------
    # Constrói um pseudo-`g_b` que herda de input_global() apenas país,
    # pop_mode, custom_pop, br_pop_tipo e filtros geográficos; os demais
    # campos vêm de `input$*_b` (método, cobertura, faixa etária, params
    # HPV/Cito, capacidades). As capacidades são anualizadas via cap_mult_b
    # (Day×240, Week×48, Month×12, Year×1) — mesma tabela usada em
    # 10_mod_filters_cc.R. Em seguida, delega a tradução para
    # cc_cfg_from_input() — mesma função usada por cfg_a (e pelos demais
    # módulos).
    # ===========================================================
    cfg_b <- reactive({
      g <- input_global()

      cap_mult_b <- switch(
        val_or(input$cap_unidade_b, "ano"),
        dia = 240, semana = 48, mes = 12, ano = 1, 1
      )

      method_b <- as.character(val_or(input$screen_method_b, "hpv"))
      if (!method_b %in% c("hpv", "cytology")) method_b <- "hpv"

      g_b <- list(
        # Herdados do cenário A (sidebar global)
        pais_sel    = g$pais_sel,
        pop_mode    = g$pop_mode,
        custom_pop  = g$custom_pop,
        br_pop_tipo = g$br_pop_tipo,
        filt_uf     = g$filt_uf,
        filt_macro  = g$filt_macro,
        filt_reg    = g$filt_reg,
        filt_mun    = g$filt_mun,

        # Específicos do Scenario B
        screen_method  = method_b,
        coverage       = val_or(input$coverage_b,       70),
        target_age_min = val_or(input$target_age_min_b, 25),
        target_age_max = val_or(input$target_age_max_b, 64),

        # HPV (NA → engine cai em HPV_DEFAULTS se método = HPV;
        # ignorado se método = cytology)
        p16_18       = val_or(input$p16_18_b,       NA_real_),
        poutros      = val_or(input$poutros_b,      NA_real_),
        pneg         = val_or(input$pneg_b,         NA_real_),
        cito_out_pos = val_or(input$cito_out_pos_b, NA_real_),
        cito_out_neg = val_or(input$cito_out_neg_b, NA_real_),
        colpo16_pos  = val_or(input$colpo16_pos_b,  NA_real_),
        colpo16_neg  = val_or(input$colpo16_neg_b,  NA_real_),
        colpoout_pos = val_or(input$colpoout_pos_b, NA_real_),
        colpoout_neg = val_or(input$colpoout_neg_b, NA_real_),
        b16_neg_nic1 = val_or(input$b16_neg_nic1_b, NA_real_),
        b16_nic23    = val_or(input$b16_nic23_b,    NA_real_),
        b16_cancer   = val_or(input$b16_cancer_b,   NA_real_),
        bo_neg_nic1  = val_or(input$bo_neg_nic1_b,  NA_real_),
        bo_nic23     = val_or(input$bo_nic23_b,     NA_real_),
        bo_cancer    = val_or(input$bo_cancer_b,    NA_real_),

        # Citologia (NA → engine cai em CITO_DEFAULTS se método = cytology)
        first_time_pct         = val_or(input$first_time_pct_b,         NA_real_),
        unsatisfactory_pct     = val_or(input$unsatisfactory_pct_b,     NA_real_),
        res_asch_pct           = val_or(input$res_asch_pct_b,           NA_real_),
        res_other_pct          = val_or(input$res_other_pct_b,          NA_real_),
        res_neg_pct            = val_or(input$res_neg_pct_b,            NA_real_),
        colpo_asch_pct         = val_or(input$colpo_asch_pct_b,         NA_real_),
        colpo_other_follow_pct = val_or(input$colpo_other_follow_pct_b, NA_real_),
        biopsy_pos_asch_pct    = val_or(input$biopsy_pos_asch_pct_b,    NA_real_),
        biopsy_pos_other_pct   = val_or(input$biopsy_pos_other_pct_b,   NA_real_),
        b_asch_nic23_pct       = val_or(input$b_asch_nic23_pct_b,       NA_real_),
        b_asch_cancer_pct      = val_or(input$b_asch_cancer_pct_b,      NA_real_),
        b_asch_neg_nic1_pct    = val_or(input$b_asch_neg_nic1_pct_b,    NA_real_),
        b_other_nic23_pct      = val_or(input$b_other_nic23_pct_b,      NA_real_),
        b_other_cancer_pct     = val_or(input$b_other_cancer_pct_b,     NA_real_),
        b_other_neg_nic1_pct   = val_or(input$b_other_neg_nic1_pct_b,   NA_real_),

        # Capacidades anualizadas
        cap_colpo_device = cap_mult_b * val_or(input$cap_colposcopio_b,   5760),
        cap_colpo_med    = cap_mult_b * val_or(input$cap_colposcopista_b, 2880),
        cap_citopato     = cap_mult_b * val_or(input$cap_citopato_b,      14400),
        cap_patol_med    = cap_mult_b * val_or(input$cap_patologista_b,   7200)
      )

      cc_cfg_from_input(g_b, br_code)
    })

    # ===========================================================
    # BLOCO 11 — Execução dos engines + flatten
    # -----------------------------------------------------------
    # res_a / res_b: rodam cc_engine_run sobre cada cfg, com tryCatch
    #                para que erros virem NULL (UI mostra "Results
    #                not available" em vez de crashar a sessão).
    # dt_a / dt_b: 1 linha cada, via cc_engine_summary_dt — facilita
    #              o lookup de colunas na tabela comparativa.
    # ===========================================================
    # ── Run engines ─────────────────────────────────────────────────────
    res_a <- reactive({
      tryCatch(
        cc_engine_run(df_completo, cfg_a(), pop_mun_regional = pop_mun_regional),
        error = function(e) NULL
      )
    })

    res_b <- reactive({
      tryCatch(
        cc_engine_run(df_completo, cfg_b(), pop_mun_regional = pop_mun_regional),
        error = function(e) NULL
      )
    })

    dt_a <- reactive({
      r <- res_a(); if (is.null(r)) NULL else cc_engine_summary_dt(r)
    })

    dt_b <- reactive({
      r <- res_b(); if (is.null(r)) NULL else cc_engine_summary_dt(r)
    })

    # ===========================================================
    # BLOCO 12 — UIs dinâmicas auxiliares (geo + população + header)
    # -----------------------------------------------------------
    # geo_locked  — exibe a geografia herdada (read-only) no painel B.
    # pop_b_ui    — render condicional: se pop_mode = "other" mostra
    #               o custom_pop herdado de A; senão, só uma nota
    #               "GLOBOCAN (inherited)".
    # result_desc — subtítulo do header do "Compare".
    # ===========================================================
    # ── Dynamic UIs ─────────────────────────────────────────────────────

    output$geo_locked <- renderUI({
      div(class = "geo-locked-b", geo_text())
    })

    output$pop_b_ui <- renderUI({
      g <- input_global()
      pop_mode <- as.character(val_or(g$pop_mode, "globocan"))
      if (identical(pop_mode, "other")) {
        div(
          div(class = "scen-inherit-note",  "Custom population (inherited from Scenario A)"),
          div(class = "scen-inherit-value", fmt_or_dash(g$custom_pop))
        )
      } else {
        div(class = "scen-inherit-note", "Population source: GLOBOCAN (inherited from Scenario A)")
      }
    })

    output$result_desc <- renderUI({
      geo_text()
    })

    # ===========================================================
    # BLOCO 13 — Scenario badges (resumo visual A vs B)
    # -----------------------------------------------------------
    # Dois cartões lado a lado com:
    #   • label   ("Scenario A"/"Scenario B")
    #   • method  (HPV test / Cytology)
    #   • ages    (faixa) + coverage (%)
    # Helpers internos `method_label`, `age_label`, `cov_label`
    # tratam fallback quando dt_a/dt_b ainda são NULL (1ª render).
    # ===========================================================
    # ── Scenario badges ──────────────────────────────────────────────────
    output$scenario_badges <- renderUI({
      g  <- input_global()
      sa <- dt_a()
      sb <- dt_b()

      method_a <- if (!is.null(sa)) as.character(sa$screen_method[1]) else as.character(val_or(g$screen_method, "hpv"))
      method_b <- as.character(val_or(input$screen_method_b, "hpv"))

      method_label <- function(m) if (identical(m, "hpv")) "HPV test" else "Cytology"
      age_label    <- function(dt, min_default, max_default) {
        mn <- if (!is.null(dt)) val_or(dt$target_age_min[1], min_default) else min_default
        mx <- if (!is.null(dt)) val_or(dt$target_age_max[1], max_default) else max_default
        sprintf("Ages %s\u2013%s", mn, mx)
      }
      cov_label <- function(dt, default) {
        cv <- if (!is.null(dt)) val_or(dt$coverage_percent[1], default) else default
        sprintf("Coverage %s%%", cv)
      }

      div(
        class = "scen-badges",
        div(
          class = "scen-badge scen-badge-a",
          div(class = "scen-badge-label", "Scenario A"),
          div(class = "scen-badge-desc",  method_label(method_a)),
          div(class = "scen-badge-sub",
              age_label(sa, val_or(g$target_age_min, 25), val_or(g$target_age_max, 64)),
              HTML(" &nbsp;&middot;&nbsp; "),
              cov_label(sa, val_or(g$coverage, 70)))
        ),
        div(
          class = "scen-badge scen-badge-b",
          div(class = "scen-badge-label", "Scenario B"),
          div(class = "scen-badge-desc",  method_label(method_b)),
          div(class = "scen-badge-sub",
              age_label(sb, val_or(input$target_age_min_b, 25), val_or(input$target_age_max_b, 64)),
              HTML(" &nbsp;&middot;&nbsp; "),
              cov_label(sb, val_or(input$coverage_b, 70)))
        )
      )
    })

    # ===========================================================
    # BLOCO 14 — Tabela comparativa A | B | Δ B vs A
    # -----------------------------------------------------------
    # Estrutura por grupos (group_row → row1, row2, ...):
    #   1) Target population         — pop_selected, screened_per_year
    #   2) Work-up                    — citologia (reflex/diag), colpo, biópsia
    #   3) Treatment and follow-up    — EZT + 2 follow-ups (HPV: HPV+colpo;
    #                                    Cito: cytologies+colposcopies)
    #   4) Resources needed (annual)  — colposcópios, colposcopistas,
    #                                    citopatologistas, patologistas
    #
    # Helpers internos:
    #   • get_num(dt, col)  / get_hr(res, col) — leitura defensiva
    #   • delta_pill(va, vb)                    — pílula com Δ% (▲/▼/—)
    #   • row(label, col, ...)                  — monta uma <tr> da tabela
    #   • group_row(label)                      — header de seção (colspan=4)
    #
    # Caso especial — métodos diferentes em A e B (ex.: A=HPV, B=Cito):
    #   • a linha de "cytology" usa o rótulo do método de A;
    #   • follow-up vira "Follow-up test" (escolhe o que está disponível)
    #     + "Follow-up colposcopy" (genérico).
    # ===========================================================
    # ── Comparison table ─────────────────────────────────────────────────
    output$compare_table <- renderUI({
      sa <- dt_a()
      sb <- dt_b()
      ra <- res_a()
      rb <- res_b()

      if (is.null(sa) || is.null(sb)) {
        return(div(class = "compare-no-data", "Results not available. Check your settings."))
      }

      method_a <- as.character(sa$screen_method[1])
      method_b <- as.character(sb$screen_method[1])

      get_num <- function(dt, col) {
        if (!is.null(dt) && col %in% names(dt)) suppressWarnings(as.numeric(dt[[col]][1])) else NA_real_
      }
      get_hr <- function(res, col) {
        if (!is.null(res) && !is.null(res$hr) && col %in% names(res$hr))
          suppressWarnings(as.numeric(res$hr[[col]][1]))
        else NA_real_
      }

      # delta pill: B vs A
      delta_pill <- function(va, vb) {
        if (is.na(va) || is.na(vb) || va == 0) return(HTML('<span class="delta-pill neu">\u2014</span>'))
        pct <- round(100 * (vb - va) / abs(va))
        if (abs(pct) < 1) return(HTML('<span class="delta-pill neu">\u2014</span>'))
        if (pct > 0) HTML(sprintf('<span class="delta-pill up">+%d%%</span>', pct))
        else         HTML(sprintf('<span class="delta-pill down">%d%%</span>', pct))
      }

      # helper to build a data row
      row <- function(label, col, ra_obj = NULL, rb_obj = NULL, hr_col = NULL) {
        if (!is.null(hr_col)) {
          va <- get_hr(ra_obj, hr_col)
          vb <- get_hr(rb_obj, hr_col)
        } else {
          va <- get_num(sa, col)
          vb <- get_num(sb, col)
        }
        tags$tr(
          tags$td(class = "td-metric", label),
          tags$td(class = "td-val",    fmt_or_dash(va)),
          tags$td(class = "td-val",    fmt_or_dash(vb)),
          tags$td(class = "td-delta",  delta_pill(va, vb))
        )
      }

      group_row <- function(label) {
        tags$tr(
          class = "row-group",
          tags$td(colspan = "4", label)
        )
      }

      # Work-up rows depend on method
      workup_rows_a <- if (identical(method_a, "hpv")) {
        list(row("Reflex cytology", "cito_reflexa"))
      } else {
        list(row("Diagnostic cytology", "cit_diagnostica"))
      }
      workup_rows_b <- if (identical(method_b, "hpv")) {
        list(row("Reflex cytology (B)", "cito_reflexa"))
      } else {
        list(row("Diagnostic cytology (B)", "cit_diagnostica"))
      }

      # Because A and B may use different methods, show a combined colposcopy/biopsy row
      # and two separate cytology rows when methods differ
      methods_same <- identical(method_a, method_b)

      followup_rows <- if (identical(method_a, "hpv")) {
        list(
          row("Follow-up HPV test", "retorno_1ano"),
          row("Follow-up colposcopy", "followup_colposcopy")
        )
      } else {
        list(
          row("Follow-up cytologies", "followup_cytologies"),
          row("Follow-up colposcopies", "followup_colposcopies")
        )
      }

      if (!methods_same) {
        # Mixed methods: show follow-up as whichever is non-NA
        va_hpv_fu  <- get_num(sa, "retorno_1ano")
        va_cito_fu <- get_num(sa, "followup_cytologies")
        vb_hpv_fu  <- get_num(sb, "retorno_1ano")
        vb_cito_fu <- get_num(sb, "followup_cytologies")

        fu_a <- if (is.finite(va_hpv_fu)) va_hpv_fu else va_cito_fu
        fu_b <- if (is.finite(vb_hpv_fu)) vb_hpv_fu else vb_cito_fu

        colpo_fu_a <- get_num(sa, if (identical(method_a, "hpv")) "followup_colposcopy" else "followup_colposcopies")
        colpo_fu_b <- get_num(sb, if (identical(method_b, "hpv")) "followup_colposcopy" else "followup_colposcopies")

        followup_rows <- list(
          tags$tr(
            tags$td(class = "td-metric", "Follow-up test"),
            tags$td(class = "td-val", fmt_or_dash(fu_a)),
            tags$td(class = "td-val", fmt_or_dash(fu_b)),
            tags$td(class = "td-delta", delta_pill(fu_a, fu_b))
          ),
          tags$tr(
            tags$td(class = "td-metric", "Follow-up colposcopy"),
            tags$td(class = "td-val", fmt_or_dash(colpo_fu_a)),
            tags$td(class = "td-val", fmt_or_dash(colpo_fu_b)),
            tags$td(class = "td-delta", delta_pill(colpo_fu_a, colpo_fu_b))
          )
        )
      }

      # cytology row: use whichever is available
      cito_a <- get_num(sa, if (identical(method_a, "hpv")) "cito_reflexa" else "cit_diagnostica")
      cito_b <- get_num(sb, if (identical(method_b, "hpv")) "cito_reflexa" else "cit_diagnostica")
      cito_label_a <- if (identical(method_a, "hpv")) "Reflex cytology" else "Diagnostic cytology"

      div(
        class = "compare-table-wrap",
        tags$table(
          tags$thead(
            tags$tr(
              tags$th("Metric"),
              tags$th(class = "col-a", "Scenario A"),
              tags$th(class = "col-b", "Scenario B"),
              tags$th("\u0394 B vs A")
            )
          ),
          tags$tbody(

            group_row("Target population"),
            row("Selected population", "pop_selected"),
            row("Screened per year",   "screened_per_year"),

            group_row("Work-up"),
            tags$tr(
              tags$td(class = "td-metric", cito_label_a),
              tags$td(class = "td-val",    fmt_or_dash(cito_a)),
              tags$td(class = "td-val",    fmt_or_dash(cito_b)),
              tags$td(class = "td-delta",  delta_pill(cito_a, cito_b))
            ),
            row("Colposcopy indicated",  "colpo_indicada"),
            row("Biopsy indicated",       "biopsia_indicada"),

            group_row("Treatment and follow-up"),
            row("Excision (EZT)", "ezt"),
            followup_rows,

            group_row("Resources needed (annual)"),
            tags$tr(
              tags$td(class = "td-metric", "Colposcopes"),
              tags$td(class = "td-val",    fmt_dec1(get_hr(ra, "colpo_devices_needed"))),
              tags$td(class = "td-val",    fmt_dec1(get_hr(rb, "colpo_devices_needed"))),
              tags$td(class = "td-delta",  delta_pill(get_hr(ra, "colpo_devices_needed"),
                                                       get_hr(rb, "colpo_devices_needed")))
            ),
            tags$tr(
              tags$td(class = "td-metric", "Colposcopists"),
              tags$td(class = "td-val",    fmt_dec1(get_hr(ra, "colpo_med_needed"))),
              tags$td(class = "td-val",    fmt_dec1(get_hr(rb, "colpo_med_needed"))),
              tags$td(class = "td-delta",  delta_pill(get_hr(ra, "colpo_med_needed"),
                                                       get_hr(rb, "colpo_med_needed")))
            ),
            tags$tr(
              tags$td(class = "td-metric", "Cytopathologists"),
              tags$td(class = "td-val",    fmt_dec1(get_hr(ra, "citopato_needed"))),
              tags$td(class = "td-val",    fmt_dec1(get_hr(rb, "citopato_needed"))),
              tags$td(class = "td-delta",  delta_pill(get_hr(ra, "citopato_needed"),
                                                       get_hr(rb, "citopato_needed")))
            ),
            tags$tr(
              tags$td(class = "td-metric", "Pathologists"),
              tags$td(class = "td-val",    fmt_dec1(get_hr(ra, "patol_med_needed"))),
              tags$td(class = "td-val",    fmt_dec1(get_hr(rb, "patol_med_needed"))),
              tags$td(class = "td-delta",  delta_pill(get_hr(ra, "patol_med_needed"),
                                                       get_hr(rb, "patol_med_needed")))
            )
          )
        )
      )
    })

  })
}
