# ===========================================================
# 17_mod_compare.R
# Módulo Compare — comparação lado a lado de dois cenários
#   Scenario A: configuração atual (input_global)
#   Scenario B: configuração alternativa (inputs locais do módulo)
# ===========================================================

mod_compare_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "compare-layout",

    # ── Painel Scenario B ──────────────────────────────────────────────
    div(
      class = "scen-b-panel",
      div(class = "scen-b-header", "Scenario B"),
      div(class = "scen-b-subheader", "Geography is shared with Scenario A."),
      uiOutput(ns("geo_locked")),

      # Population
      tags$details(
        class = "scen-b-acc", open = NA,
        tags$summary("Population"),
        div(
          class = "scen-b-acc-body",
          uiOutput(ns("pop_b_ui"))
        )
      ),

      # ── Screening protocol (com Customize) ──────────────────────────────
      tags$details(
        class = "scen-b-acc", open = NA,
        tags$summary("Screening protocol"),
        div(
          class = "scen-b-acc-body",

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

          # ── HPV: seletor de fonte + painel Customize ──────────────────────
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

          # ── Citologia: seletor de fonte + painel Customize ────────────────
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

      # Screening coverage
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

      # Resources
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
    ),

    # ── Área de resultados ─────────────────────────────────────────────
    div(
      class = "compare-results",
      div(
        class = "cc-page-header",
        div(class = "cc-page-title", "Compare"),
        div(class = "cc-page-subtitle", uiOutput(ns("result_desc")))
      ),
      uiOutput(ns("scenario_badges")),
      uiOutput(ns("compare_table"))
    )
  )
}


# ===========================================================
# SERVER
# ===========================================================

mod_compare_server <- function(id, input_global, df_completo, dim_country, pop_mun_regional,
                               cito_presets = NULL) {
  moduleServer(id, function(input, output, session) {

    # ---- helpers ----
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

    # ---- Brazil code ----
    br_code <- tryCatch({
      x <- dim_country[dim_country$population_name == "Brazil", "population_code"]
      if (length(x) == 0L || is.na(x[1])) NA_integer_ else as.integer(x[1])
    }, error = function(e) NA_integer_)

    is_brazil <- reactive({
      g <- input_global()
      !is.na(br_code) && isTRUE(as.integer(val_or(g$pais_sel, NA)) == br_code)
    })

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

    # ---- geo label ----
    country_label <- function(code) {
      tryCatch({
        z <- dim_country[dim_country$population_code == code, "population_name"]
        if (length(z) == 0L || is.na(z[1])) "Selected country" else as.character(z[1])
      }, error = function(e) "Selected country")
    }

    geo_text <- reactive({
      g <- input_global()
      if (!isTRUE(is_brazil())) {
        return(country_label(as.integer(val_or(g$pais_sel, NA))))
      }
      parts <- "Brazil"
      pop_label <- if (isTRUE(g$br_pop_tipo == "sus")) "SUS-dependent" else "Total population"
      parts <- paste(parts, pop_label, sep = " - ")
      add_geo <- function(x) {
        x <- as.character(x); x <- x[!is.na(x) & nzchar(x)]
        if (!length(x)) return(parts)
        lab <- if (length(x) == 1L) x[1] else paste0(x[1], " (n=", length(x), ")")
        paste(parts, lab, sep = " - ")
      }
      for (filt in list(g$filt_uf, g$filt_macro, g$filt_reg, g$filt_mun)) {
        if (!is.null(filt) && length(filt) > 0) {
          parts <- add_geo(filt); break
        }
      }
      parts
    })

    # ── Sincroniza inputs numéricos com presets no Scenario B ────────────
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
      div(style = "background:#ffe6e6;border:1px solid #cc0000;color:#990000;padding:8px;border-radius:6px;margin-top:6px;font-size:11px;",
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
      div(style = "background:#ffe6e6;border:1px solid #cc0000;color:#990000;padding:8px;border-radius:6px;margin-top:6px;font-size:11px;",
          strong("Please check parameters:"), tags$ul(lapply(errs, tags$li)))
    })

    # ── cfg_a: mirrors 11_mod_resumo_geral ──────────────────────────────
    cfg_a <- reactive({
      g <- input_global()

      country_code <- suppressWarnings(as.integer(val_or(g$pais_sel, br_code)))
      if (is.na(country_code)) country_code <- br_code

      pop_mode <- as.character(val_or(g$pop_mode, "globocan"))
      if (!pop_mode %in% c("globocan", "other")) pop_mode <- "globocan"

      coverage <- suppressWarnings(as.numeric(val_or(g$coverage, 70)))
      if (!is.finite(coverage)) coverage <- 70

      screen_method <- as.character(val_or(g$screen_method, "hpv"))
      if (!screen_method %in% c("hpv", "cytology")) screen_method <- "hpv"

      target_age_min <- suppressWarnings(as.numeric(val_or(g$target_age_min, 25)))
      target_age_max <- suppressWarnings(as.numeric(val_or(g$target_age_max, 64)))
      if (!is.finite(target_age_min)) target_age_min <- 25
      if (!is.finite(target_age_max)) target_age_max <- 64

      custom_pop <- NA_real_
      if (identical(pop_mode, "other")) {
        custom_pop <- suppressWarnings(as.numeric(val_or(g$custom_pop, NA_real_)))
      }

      is_br       <- isTRUE(!is.na(country_code) && country_code == br_code)
      br_pop_tipo <- as.character(val_or(g$br_pop_tipo, "total"))
      if (!br_pop_tipo %in% c("total", "sus")) br_pop_tipo <- "total"

      cc_engine_settings(
        country_code      = country_code,
        pop_mode          = pop_mode,
        coverage          = coverage,
        screen_method     = screen_method,
        target_age_min    = target_age_min,
        target_age_max    = target_age_max,
        custom_pop        = custom_pop,

        p16_18            = val_or(g$p16_18, NA_real_),
        poutros           = val_or(g$poutros, NA_real_),
        pneg              = val_or(g$pneg, NA_real_),
        cito_out_pos      = val_or(g$cito_out_pos, NA_real_),
        cito_out_neg      = val_or(g$cito_out_neg, NA_real_),
        colpo16_pos       = val_or(g$colpo16_pos, NA_real_),
        colpo16_neg       = val_or(g$colpo16_neg, NA_real_),
        colpoout_pos      = val_or(g$colpoout_pos, NA_real_),
        colpoout_neg      = val_or(g$colpoout_neg, NA_real_),
        b16_neg_nic1      = val_or(g$b16_neg_nic1, NA_real_),
        b16_nic23         = val_or(g$b16_nic23, NA_real_),
        b16_cancer        = val_or(g$b16_cancer, NA_real_),
        bo_neg_nic1       = val_or(g$bo_neg_nic1, NA_real_),
        bo_nic23          = val_or(g$bo_nic23, NA_real_),
        bo_cancer         = val_or(g$bo_cancer, NA_real_),

        first_time_pct         = val_or(g$first_time_pct, NA_real_),
        unsatisfactory_pct     = val_or(g$unsatisfactory_pct, NA_real_),
        res_asch_pct           = val_or(g$res_asch_pct, NA_real_),
        res_other_pct          = val_or(g$res_other_pct, NA_real_),
        res_neg_pct            = val_or(g$res_neg_pct, NA_real_),
        colpo_asch_pct         = val_or(g$colpo_asch_pct, NA_real_),
        colpo_other_follow_pct = val_or(g$colpo_other_follow_pct, NA_real_),
        biopsy_pos_asch_pct    = val_or(g$biopsy_pos_asch_pct, NA_real_),
        biopsy_pos_other_pct   = val_or(g$biopsy_pos_other_pct, NA_real_),
        b_asch_nic23_pct       = val_or(g$b_asch_nic23_pct, NA_real_),
        b_asch_cancer_pct      = val_or(g$b_asch_cancer_pct, NA_real_),
        b_asch_neg_nic1_pct    = val_or(g$b_asch_neg_nic1_pct, NA_real_),
        b_other_nic23_pct      = val_or(g$b_other_nic23_pct, NA_real_),
        b_other_cancer_pct     = val_or(g$b_other_cancer_pct, NA_real_),
        b_other_neg_nic1_pct   = val_or(g$b_other_neg_nic1_pct, NA_real_),

        # input_global already exposes annualized capacity values
        cap_colpo_device  = val_or(g$cap_colpo_device, 5760),
        cap_colpo_med     = val_or(g$cap_colpo_med,    2880),
        cap_citopato      = val_or(g$cap_citopato,     14400),
        cap_patol_med     = val_or(g$cap_patol_med,    7200),

        is_brazil         = is_br,
        br_pop_tipo       = br_pop_tipo,
        filt_uf           = val_or(g$filt_uf, NULL),
        filt_macro        = val_or(g$filt_macro, NULL),
        filt_reg          = val_or(g$filt_reg, NULL),
        filt_mun          = val_or(g$filt_mun, NULL)
      )
    })

    # ── cfg_b: Scenario B inputs + inherited geo ─────────────────────────
    cfg_b <- reactive({
      g <- input_global()

      country_code <- suppressWarnings(as.integer(val_or(g$pais_sel, br_code)))
      if (is.na(country_code)) country_code <- br_code

      pop_mode <- as.character(val_or(g$pop_mode, "globocan"))
      if (!pop_mode %in% c("globocan", "other")) pop_mode <- "globocan"

      custom_pop <- NA_real_
      if (identical(pop_mode, "other"))
        custom_pop <- suppressWarnings(as.numeric(val_or(g$custom_pop, NA_real_)))

      is_br       <- isTRUE(!is.na(country_code) && country_code == br_code)
      br_pop_tipo <- as.character(val_or(g$br_pop_tipo, "total"))
      if (!br_pop_tipo %in% c("total", "sus")) br_pop_tipo <- "total"

      method_b <- as.character(val_or(input$screen_method_b, "hpv"))
      if (!method_b %in% c("hpv", "cytology")) method_b <- "hpv"

      coverage_b <- suppressWarnings(as.numeric(val_or(input$coverage_b, 70)))
      if (!is.finite(coverage_b)) coverage_b <- 70

      age_min_b <- suppressWarnings(as.numeric(val_or(input$target_age_min_b, 25)))
      age_max_b <- suppressWarnings(as.numeric(val_or(input$target_age_max_b, 64)))
      if (!is.finite(age_min_b)) age_min_b <- 25
      if (!is.finite(age_max_b)) age_max_b <- 64

      cap_mult_b <- switch(val_or(input$cap_unidade_b, "ano"), dia = 240, semana = 48, mes = 12, ano = 1, 1)
      cap_colpo_device_b <- cap_mult_b * val_or(input$cap_colposcopio_b,   5760)
      cap_colpo_med_b    <- cap_mult_b * val_or(input$cap_colposcopista_b, 2880)
      cap_citopato_b     <- cap_mult_b * val_or(input$cap_citopato_b,      14400)
      cap_patol_med_b    <- cap_mult_b * val_or(input$cap_patologista_b,   7200)

      if (identical(method_b, "hpv")) {
        cc_engine_settings(
          country_code      = country_code,
          pop_mode          = pop_mode,
          coverage          = coverage_b,
          screen_method     = "hpv",
          target_age_min    = age_min_b,
          target_age_max    = age_max_b,
          custom_pop        = custom_pop,

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

          cap_colpo_device = cap_colpo_device_b,
          cap_colpo_med    = cap_colpo_med_b,
          cap_citopato     = cap_citopato_b,
          cap_patol_med    = cap_patol_med_b,

          is_brazil   = is_br,
          br_pop_tipo = br_pop_tipo,
          filt_uf     = val_or(g$filt_uf, NULL),
          filt_macro  = val_or(g$filt_macro, NULL),
          filt_reg    = val_or(g$filt_reg, NULL),
          filt_mun    = val_or(g$filt_mun, NULL)
        )
      } else {
        cc_engine_settings(
          country_code      = country_code,
          pop_mode          = pop_mode,
          coverage          = coverage_b,
          screen_method     = "cytology",
          target_age_min    = age_min_b,
          target_age_max    = age_max_b,
          custom_pop        = custom_pop,

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

          cap_colpo_device = cap_colpo_device_b,
          cap_colpo_med    = cap_colpo_med_b,
          cap_citopato     = cap_citopato_b,
          cap_patol_med    = cap_patol_med_b,

          is_brazil   = is_br,
          br_pop_tipo = br_pop_tipo,
          filt_uf     = val_or(g$filt_uf, NULL),
          filt_macro  = val_or(g$filt_macro, NULL),
          filt_reg    = val_or(g$filt_reg, NULL),
          filt_mun    = val_or(g$filt_mun, NULL)
        )
      }
    })

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
