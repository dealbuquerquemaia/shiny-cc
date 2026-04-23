# ===========================================================
# 10_mod_filters_cc.R
# Módulo de filtros globais — Cervical Cancer Screening
# ===========================================================

mod_filters_cc_ui <- function(id, dim_country, br_code = NULL) {
  ns <- NS(id)
  
  # código de Brazil (se não vier de fora, tenta descobrir na tabela)
  if (is.null(br_code)) {
    br_code <- tryCatch(
      {
        x <- dim_country[dim_country$population_name == "Brazil", "population_code"]
        if (length(x) == 0L || is.na(x[1])) 1001L else as.integer(x[1])
      },
      error = function(e) 1001L
    )
  }
  
  tagList(
    h3("Settings"),
    
    # ================= POPULAÇÃO =================
    div(
      class = "cc-section cc-open",   # Population começa ABERTO
      div(class = "cc-section-header",
        tags$svg(xmlns="http://www.w3.org/2000/svg", viewBox="0 0 24 24",
          width="14", height="14", fill="none", stroke="currentColor",
          `stroke-width`="1.8", `stroke-linecap`="round", `stroke-linejoin`="round",
          tags$circle(cx="9", cy="7", r="3"),
          tags$path(d="M3 21v-2a4 4 0 0 1 4-4h4a4 4 0 0 1 4 4v2"),
          tags$circle(cx="18", cy="7", r="2.5"),
          tags$path(d="M21 21v-1.5a3.5 3.5 0 0 0-3-3.5")
        ),
        "Population",
        tags$span(class="cc-sec-chevron", "▼")
      ),
      div(
        class = "cc-section-body",
        
        selectInput(
          ns("pais_sel"), "Country / population",
          choices  = setNames(dim_country$population_code,
                              dim_country$population_name),
          selected = br_code
        ),
        radioButtons(
          ns("pop_mode"), "Source of population",
          choices = c(
            "World Population Prospects 2025"        = "globocan",
            "Other population (manual)"  = "other"
          ),
          selected = "globocan"
        ),
        
        # tipo de população Brasil (Total x SUS) ----------------------
        conditionalPanel(
          condition = sprintf("parseInt(input['%s']) == %d", ns("pais_sel"), br_code),
          tags$hr(),
          radioButtons(
            ns("br_pop_tipo"), "Brazilian population source",
            choices = c(
              "Total population (IBGE)"    = "total",
              "SUS-dependent (IBGE − ANS)" = "sus"
            ),
            selected = "total"
          )
        ),
        
        # população manual --------------------------------------------
        conditionalPanel(
          condition = sprintf("input['%s'] == 'other'", ns("pop_mode")),
          numericInput(
            ns("custom_pop_main"),
            "Target population",
            value = 100000, min = 0, step = 1000
          )
        ),
        
        
        
        # ============ FILTROS GEOGRÁFICOS BRASIL =====================
        # Dentro de Population, visível só se Brasil selecionado
        conditionalPanel(
          condition = sprintf("parseInt(input['%s']) == %d", ns("pais_sel"), br_code),
          tags$hr(),
          h5("Brazil – Geographic filters"),
          radioButtons( ns("sia_geo_ref"), "SIA geographic reference (capacity module only)",
                        choices = c( "Place of care" = "care", "Place of residence" = "res"),
                        selected = "care"),
          tags$hr(),
          selectizeInput(ns("filt_uf"),    "State (UF):",         choices = NULL, multiple = TRUE),
          selectizeInput(ns("filt_macro"), "Health macroregion:", choices = NULL, multiple = TRUE),
          selectizeInput(ns("filt_reg"),   "Health region:",      choices = NULL, multiple = TRUE),
          selectizeInput(ns("filt_mun"),   "Municipality:",       choices = NULL, multiple = TRUE),
          br(),
          actionButton(ns("limpar_filtros"), "🧹 Clear filters", class = "btn-secondary")
          
        )
      )
    ),
    tags$hr(),
    
    # ================= PROTOCOLO =================
    div(
      class = "cc-section",   # começa FECHADO
      div(class = "cc-section-header",
        tags$svg(xmlns="http://www.w3.org/2000/svg", viewBox="0 0 24 24",
          width="14", height="14", fill="none", stroke="currentColor",
          `stroke-width`="1.8", `stroke-linecap`="round", `stroke-linejoin`="round",
          tags$path(d="M9 3h6v7l4 9H5l4-9V3"),
          tags$line(x1="9", y1="9", x2="15", y2="9")
        ),
        "Screening protocol",
        tags$span(class="cc-sec-chevron", "▼")
      ),
      div(
        class = "cc-section-body",
        
        radioButtons(
          ns("screen_method"),
          "Screening test",
          choices = c(
            "HPV test" = "hpv",
            "Cytology" = "cytology"
          ),
          selected = "hpv"
        ),
        
        h5("Target age"),
        fluidRow(
          column(
            6,
            selectInput(
              ns("target_age_min"), "From",
              choices  = c(20,25,30,35,40,45,50,55,60,65,70),
              selected = 25
            )
          ),
          column(
            6,
            selectInput(
              ns("target_age_max"), "To",
              choices  = c(24,29,34,39,44,49,54,59,64,69,74),
              selected = 64
            )
          )
        ),
        
        
        tags$hr(),
        
        # -------- Parâmetros (HPV) --------
        conditionalPanel(
          condition = sprintf("input['%s'] == 'hpv'", ns("screen_method")),

          {
            preset_choices <- c(
              setNames(names(HPV_PRESETS), sapply(HPV_PRESETS, `[[`, "label")),
              "Customize" = "custom"
            )
            radioButtons(
              ns("hpv_param_source"),
              "Parameter source",
              choices  = preset_choices,
              selected = names(HPV_PRESETS)[1]
            )
          },

          conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", ns("hpv_param_source")),
            div(
              class = "cc-param-area",
              div(
                class = "cc-param-area-toggle",
                span(class = "cc-param-area-label",
                  "HPV pathway parameters",
                  tags$span(class = "cc-badge-customize", "customize")
                ),
                tags$span(class = "cc-param-area-chevron", "▼")
              ),
              div(
                class = "cc-param-area-body",
                fluidRow(
                  column(6, checkboxInput(ns("lock_prop"), "Lock proportions while editing", value = TRUE)),
                  column(6, actionButton(ns("reset_params"), "Reset parameters", class = "btn-secondary"))
                ),
                div(class = "cc-param-group",
                  div(class = "cc-param-group-title", "HPV prevalence"),
                  fluidRow(
                    column(4, numericInput(ns("p16_18"),  "HPV 16/18 (%)", value = round(HPV_DEFAULTS$p16_18, 2),  min = 0, max = 100, step = 0.01)),
                    column(4, numericInput(ns("poutros"), "HPV other (%)", value = round(HPV_DEFAULTS$poutros, 2), min = 0, max = 100, step = 0.01)),
                    column(4, numericInput(ns("pneg"),    "Negative (%)",  value = round(HPV_DEFAULTS$pneg, 2),    min = 0, max = 100, step = 0.01))
                  )
                ),
                div(class = "cc-param-group",
                  div(class = "cc-param-group-title", "Cytology (HPV other)"),
                  fluidRow(
                    column(6, numericInput(ns("cito_out_pos"), "Positive (%)", value = round(HPV_DEFAULTS$cito_out_pos, 2), min = 0, max = 100, step = 0.01)),
                    column(6, numericInput(ns("cito_out_neg"), "Negative (%)", value = round(HPV_DEFAULTS$cito_out_neg, 2), min = 0, max = 100, step = 0.01))
                  )
                ),
                div(class = "cc-param-group",
                  div(class = "cc-param-group-title", "Colposcopy (HPV 16/18)"),
                  fluidRow(
                    column(6, numericInput(ns("colpo16_pos"), "Positive (%)", value = round(HPV_DEFAULTS$colpo16_pos, 2), min = 0, max = 100, step = 0.01)),
                    column(6, numericInput(ns("colpo16_neg"), "Negative (%)", value = round(HPV_DEFAULTS$colpo16_neg, 2), min = 0, max = 100, step = 0.01))
                  )
                ),
                div(class = "cc-param-group",
                  div(class = "cc-param-group-title", "Colposcopy (HPV other)"),
                  fluidRow(
                    column(6, numericInput(ns("colpoout_pos"), "Positive (%)", value = round(HPV_DEFAULTS$colpoout_pos, 2), min = 0, max = 100, step = 0.01)),
                    column(6, numericInput(ns("colpoout_neg"), "Negative (%)", value = round(HPV_DEFAULTS$colpoout_neg, 2), min = 0, max = 100, step = 0.01))
                  )
                ),
                div(class = "cc-param-group",
                  div(class = "cc-param-group-title", "Biopsy (HPV 16/18)"),
                  fluidRow(
                    column(4, numericInput(ns("b16_neg_nic1"), "Negative / CIN1 (%)", value = round(HPV_DEFAULTS$b16_neg_nic1, 2), min = 0, max = 100, step = 0.01)),
                    column(4, numericInput(ns("b16_nic23"),    "CIN2 / CIN3 (%)",     value = round(HPV_DEFAULTS$b16_nic23, 2),    min = 0, max = 100, step = 0.01)),
                    column(4, numericInput(ns("b16_cancer"),   "Cancer (%)",           value = round(HPV_DEFAULTS$b16_cancer, 2),   min = 0, max = 100, step = 0.01))
                  )
                ),
                div(class = "cc-param-group",
                  div(class = "cc-param-group-title", "Biopsy (HPV other)"),
                  fluidRow(
                    column(4, numericInput(ns("bo_neg_nic1"), "Negative / CIN1 (%)", value = round(HPV_DEFAULTS$bo_neg_nic1, 2), min = 0, max = 100, step = 0.01)),
                    column(4, numericInput(ns("bo_nic23"),    "CIN2 / CIN3 (%)",     value = round(HPV_DEFAULTS$bo_nic23, 2),    min = 0, max = 100, step = 0.01)),
                    column(4, numericInput(ns("bo_cancer"),   "Cancer (%)",           value = round(HPV_DEFAULTS$bo_cancer, 2),   min = 0, max = 100, step = 0.01))
                  )
                ),
                div(class = "cc-param-group",
                  div(class = "cc-param-group-title", "Follow-up HPV"),
                  numericInput(ns("hpv_followup_pos_pct"), "HPV positivity at follow-up (%)",
                    value = round(HPV_DEFAULTS$hpv_followup_pos_pct, 2), min = 0, max = 100, step = 0.01)
                ),
                uiOutput(ns("params_alert"))
              )
            )
          )     # fecha conditionalPanel(hpv_param_source == custom)
        ),      # fecha conditionalPanel(screen_method == hpv)

        # -------- Parâmetros (Citologia) --------
        conditionalPanel(
          condition = sprintf("input['%s'] == 'cytology'", ns("screen_method")),

          {
            cito_choices <- c(
              setNames(names(CITO_PRESETS_META),
                       sapply(CITO_PRESETS_META, `[[`, "label")),
              "Customize" = "custom"
            )
            radioButtons(
              ns("cito_param_source"),
              "Parameter source",
              choices  = cito_choices,
              selected = names(CITO_PRESETS_META)[1]
            )
          },

          conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", ns("cito_param_source")),
            div(
              class = "cc-param-area",
              div(
                class = "cc-param-area-toggle",
                span(class = "cc-param-area-label",
                  "Cytology pathway parameters",
                  tags$span(class = "cc-badge-customize", "customize")
                ),
                tags$span(class = "cc-param-area-chevron", "▼")
              ),
              div(
                class = "cc-param-area-body",
                fluidRow(
                  column(6, checkboxInput(ns("lock_prop_cito"), "Lock proportions while editing", value = TRUE)),
                  column(6, actionButton(ns("reset_params_cito"), "Reset parameters", class = "btn-secondary"))
                ),
                div(class = "cc-param-group",
                  div(class = "cc-param-group-title", "Exam volume"),
                  fluidRow(
                    column(6, numericInput(ns("first_time_pct"),     "First-time exams (%)",   value = CITO_DEFAULTS$first_time_pct,     min = 0, max = 100, step = 0.01)),
                    column(6, numericInput(ns("unsatisfactory_pct"), "Unsatisfactory (%)",     value = CITO_DEFAULTS$unsatisfactory_pct, min = 0, max = 100, step = 0.01))
                  )
                ),
                div(class = "cc-param-group",
                  div(class = "cc-param-group-title", "Cytology result"),
                  fluidRow(
                    column(4, numericInput(ns("res_asch_pct"),  "HSIL / ASC-H / AIS / Ca (%)", value = CITO_DEFAULTS$res_asch_pct,  min = 0, max = 100, step = 0.01)),
                    column(4, numericInput(ns("res_other_pct"), "Other abnormal (%)",          value = CITO_DEFAULTS$res_other_pct, min = 0, max = 100, step = 0.01)),
                    column(4, numericInput(ns("res_neg_pct"),   "Negative (%)",                value = CITO_DEFAULTS$res_neg_pct,   min = 0, max = 100, step = 0.01))
                  )
                ),
                div(class = "cc-param-group",
                  div(class = "cc-param-group-title", "Colposcopy referral"),
                  fluidRow(
                    column(6, numericInput(ns("colpo_asch_pct"),         "After HSIL arm (%)",        value = CITO_DEFAULTS$colpo_asch_pct,         min = 0, max = 100, step = 0.01)),
                    column(6, numericInput(ns("colpo_other_follow_pct"), "After other abnormal (%)",  value = CITO_DEFAULTS$colpo_other_follow_pct, min = 0, max = 100, step = 0.01))
                  )
                ),
                div(class = "cc-param-group",
                  div(class = "cc-param-group-title", "Colposcopy positivity"),
                  fluidRow(
                    column(6, numericInput(ns("biopsy_pos_asch_pct"),  "HSIL arm (%)",        value = CITO_DEFAULTS$biopsy_pos_asch_pct,  min = 0, max = 100, step = 0.01)),
                    column(6, numericInput(ns("biopsy_pos_other_pct"), "Other abnormal (%)",  value = CITO_DEFAULTS$biopsy_pos_other_pct, min = 0, max = 100, step = 0.01))
                  )
                ),
                div(class = "cc-param-group",
                  div(class = "cc-param-group-title", "Biopsy (HSIL arm)"),
                  fluidRow(
                    column(4, numericInput(ns("b_asch_neg_nic1_pct"), "Negative / CIN1 (%)", value = CITO_DEFAULTS$b_asch_neg_nic1_pct, min = 0, max = 100, step = 0.01)),
                    column(4, numericInput(ns("b_asch_nic23_pct"),    "CIN2 / CIN3 (%)",     value = CITO_DEFAULTS$b_asch_nic23_pct,    min = 0, max = 100, step = 0.01)),
                    column(4, numericInput(ns("b_asch_cancer_pct"),   "Cancer (%)",          value = CITO_DEFAULTS$b_asch_cancer_pct,   min = 0, max = 100, step = 0.01))
                  )
                ),
                div(class = "cc-param-group",
                  div(class = "cc-param-group-title", "Biopsy (other arm)"),
                  fluidRow(
                    column(4, numericInput(ns("b_other_neg_nic1_pct"), "Negative / CIN1 (%)", value = CITO_DEFAULTS$b_other_neg_nic1_pct, min = 0, max = 100, step = 0.01)),
                    column(4, numericInput(ns("b_other_nic23_pct"),    "CIN2 / CIN3 (%)",     value = CITO_DEFAULTS$b_other_nic23_pct,    min = 0, max = 100, step = 0.01)),
                    column(4, numericInput(ns("b_other_cancer_pct"),   "Cancer (%)",          value = CITO_DEFAULTS$b_other_cancer_pct,   min = 0, max = 100, step = 0.01))
                  )
                ),
                uiOutput(ns("params_alert_cito"))
              )
            )
          )     # fecha conditionalPanel(cito_param_source == custom)
        )       # fecha conditionalPanel(screen_method == cytology)

      )
    ),
    
    tags$hr(),
    
   
   
    # ================= COBERTURA =================
    div(
      class = "cc-section",
      div(class = "cc-section-header",
        tags$svg(xmlns="http://www.w3.org/2000/svg", viewBox="0 0 24 24",
          width="14", height="14", fill="none", stroke="currentColor",
          `stroke-width`="1.8", `stroke-linecap`="round", `stroke-linejoin`="round",
          tags$path(d="M22 11.08V12a10 10 0 1 1-5.93-9.14"),
          tags$polyline(points="22 4 12 14.01 9 11.01")
        ),
        "Screening coverage",
        tags$span(class="cc-sec-chevron", "▼")
      ),
      div(
        class = "cc-section-body",
        sliderInput(
          ns("coverage"), NULL,
          min   = 0, max = 100,
          value = 70, step = 5, post = "%"
        )
      )
    ),
    
    tags$hr(),
    
    # ================= RECURSOS / CAPACIDADES ==============
    div(
      class = "cc-section",
      div(class = "cc-section-header",
        tags$svg(xmlns="http://www.w3.org/2000/svg", viewBox="0 0 24 24",
          width="14", height="14", fill="none", stroke="currentColor",
          `stroke-width`="1.8", `stroke-linecap`="round", `stroke-linejoin`="round",
          tags$rect(x="2", y="7", width="20", height="14", rx="2", ry="2"),
          tags$path(d="M16 21V5a2 2 0 0 0-2-2h-4a2 2 0 0 0-2 2v16")
        ),
        "Resources",
        tags$span(class="cc-sec-chevron", "▼")
      ),
      div(
        class = "cc-section-body",
        
        

          radioButtons(
            ns("cap_unidade"),
            "Capacity unit:",
            choices  = c("Day" = "dia", "Week" = "semana", "Month" = "mes", "Year" = "ano"),
            selected = "ano",
            inline   = TRUE
          ),
          helpText("Conversions: 1 week = 5 days; 1 month = 4 weeks; 1 year = 12 months."),

          div(class = "cc-param-group",
            div(class = "cc-param-group-title", "Colposcopy"),
            numericInput(ns("cap_colposcopio"),   "Colposcope (procedures/unit)",          value = 5760, min = 1, step = 1),
            numericInput(ns("cap_colposcopista"), "Colposcopist 20h/wk (procedures/unit)", value = 2880, min = 1, step = 1)
          ),
          div(class = "cc-param-group",
            div(class = "cc-param-group-title", "Pathology"),
            numericInput(ns("cap_citopato"),    "Cytopathologist (slides/unit)",     value = 14400, min = 1, step = 1),
            numericInput(ns("cap_patologista"), "Pathologist 20h/wk (slides/unit)",  value = 7200,  min = 1, step = 1)
          )
        )
      
    ))
    
}


# ===========================================================
# SERVER
# ===========================================================

mod_filters_cc_server <- function(id,
                                  pop_municipio_regional = NULL,
                                  cito_presets           = NULL,
                                  br_code = 1001L) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # ---------- HPV: validação + auto-balance ----------
    if (!exists("HPV_DEFAULTS", inherits = TRUE)) {
      stop("mod_filters_cc_server(): objeto 'HPV_DEFAULTS' não encontrado.", call. = FALSE)
    }
    hpv_defaults <- get("HPV_DEFAULTS", inherits = TRUE)

    # ---------- Citologia: defaults ----------
    if (!exists("CITO_DEFAULTS", inherits = TRUE)) {
      stop("mod_filters_cc_server(): objeto 'CITO_DEFAULTS' não encontrado.", call. = FALSE)
    }
    cito_defaults <- get("CITO_DEFAULTS", inherits = TRUE)
    
    
    sum_check <- function(x, tol = 0.1) {
      x <- suppressWarnings(as.numeric(x))
      x[is.na(x)] <- 0
      abs(sum(x) - 100) <= tol
    }
    sum_ok <- function(...) sum_check(c(...))
    
    param_errors <- reactive({
      if (!identical(input$screen_method, "hpv")) return(character(0))
      
      errs <- character(0)
      if (!sum_ok(input$p16_18, input$poutros, input$pneg)) errs <- c(errs, "HPV prevalence must sum to 100%.")
      if (!sum_ok(input$cito_out_pos, input$cito_out_neg))  errs <- c(errs, "Cytology (HPV other) must sum to 100%.")
      if (!sum_ok(input$colpo16_pos, input$colpo16_neg))    errs <- c(errs, "Colposcopy (HPV 16/18) must sum to 100%.")
      if (!sum_ok(input$colpoout_pos, input$colpoout_neg))  errs <- c(errs, "Colposcopy (HPV other) must sum to 100%.")
      if (!sum_ok(input$b16_neg_nic1, input$b16_nic23, input$b16_cancer)) errs <- c(errs, "Biopsy (HPV 16/18) must sum to 100%.")
      if (!sum_ok(input$bo_neg_nic1,  input$bo_nic23,  input$bo_cancer))  errs <- c(errs, "Biopsy (HPV other) must sum to 100%.")
      errs
    })
    
    output$params_alert <- renderUI({
      errs <- param_errors()
      if (length(errs) == 0) return(NULL)
      div(
        style="background:#ffe6e6;border:1px solid #cc0000;color:#990000;padding:10px;border-radius:8px;",
        strong("Please check parameters:"),
        tags$ul(lapply(errs, tags$li))
      )
    })
    
    # ---------- AUTO-BALANCE (estável) ----------
    rv_lock <- reactiveValues(updating = FALSE, prev = list())
    
    clamp100 <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (length(x) == 0L || is.na(x)) x <- 0
      max(0, min(100, x))
    }
    
    approx_eq <- function(a, b, tol = 0.01) {
      a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
      if (is.na(a) && is.na(b)) return(TRUE)
      if (is.na(a) || is.na(b)) return(FALSE)
      abs(a - b) <= tol
    }
    
    safe_update_num <- function(id, val) {
      updateNumericInput(session, id, value = round(val, 2))
    }
    
    lock_pair <- function(idA, idB, tol = 0.01) {
      observeEvent(list(input[[idA]], input[[idB]], input$lock_prop, input$screen_method), {
        if (!identical(input$screen_method, "hpv")) return()
        if (!isTRUE(input$lock_prop)) return()
        if (isTRUE(rv_lock$updating)) return()
        
        a <- clamp100(input[[idA]])
        b <- clamp100(input[[idB]])
        
        prevA <- rv_lock$prev[[idA]]; if (is.null(prevA)) prevA <- a
        prevB <- rv_lock$prev[[idB]]; if (is.null(prevB)) prevB <- b
        
        # já está consistente
        if (abs((a + b) - 100) <= tol && approx_eq(a, prevA, tol) && approx_eq(b, prevB, tol)) return()
        
        changedA <- !approx_eq(a, prevA, tol)
        changedB <- !approx_eq(b, prevB, tol)
        
        # decide “quem foi editado”
        primary <- if (changedA && !changedB) idA else if (changedB && !changedA) idB else idA
        
        rv_lock$updating <- TRUE
        if (identical(primary, idA)) {
          newA <- a
          newB <- 100 - newA
          safe_update_num(idB, newB)
        } else {
          newB <- b
          newA <- 100 - newB
          safe_update_num(idA, newA)
        }
        rv_lock$prev[[idA]] <- newA
        rv_lock$prev[[idB]] <- newB
        rv_lock$updating <- FALSE
      }, ignoreInit = TRUE)
    }
    
    lock_triplet <- function(idA, idB, idC, remainder_id, adjust_id, tol = 0.01) {
      ids <- c(idA, idB, idC)
      
      observeEvent(c(lapply(ids, function(i) input[[i]]), list(input$lock_prop, input$screen_method)), {
        if (!identical(input$screen_method, "hpv")) return()
        if (!isTRUE(input$lock_prop)) return()
        if (isTRUE(rv_lock$updating)) return()
        
        v <- lapply(ids, function(i) clamp100(input[[i]]))
        names(v) <- ids
        
        prev <- lapply(ids, function(i) rv_lock$prev[[i]])
        names(prev) <- ids
        for (i in ids) if (is.null(prev[[i]])) prev[[i]] <- v[[i]]
        
        # detecta quem mudou
        changed <- vapply(ids, function(i) !approx_eq(v[[i]], prev[[i]], tol), logical(1))
        primary <- if (sum(changed) == 1) ids[which(changed)] else remainder_id
        
        other_ids <- setdiff(ids, remainder_id)
        
        rv_lock$updating <- TRUE
        
        if (!identical(primary, remainder_id)) {
          # regra: ajusta SEMPRE o remainder
          s <- v[[other_ids[1]]] + v[[other_ids[2]]]
          if (s <= 100) {
            new_rem <- 100 - s
          } else {
            # se estourou 100, zera remainder e reduz "adjust_id"
            new_rem <- 0
            fixed_id <- setdiff(other_ids, adjust_id)
            new_adj <- max(0, 100 - v[[fixed_id]])
            v[[adjust_id]] <- new_adj
          }
          v[[remainder_id]] <- new_rem
          safe_update_num(remainder_id, v[[remainder_id]])
          if (s > 100) safe_update_num(adjust_id, v[[adjust_id]])
        } else {
          # usuário mexeu no remainder: ajusta "adjust_id" para fechar 100 mantendo o outro fixo
          fixed_id <- setdiff(other_ids, adjust_id)
          new_adj <- max(0, 100 - v[[fixed_id]] - v[[remainder_id]])
          v[[adjust_id]] <- new_adj
          safe_update_num(adjust_id, v[[adjust_id]])
        }
        
        for (i in ids) rv_lock$prev[[i]] <- v[[i]]
        rv_lock$updating <- FALSE
      }, ignoreInit = TRUE)
    }
    
    # pares (2 categorias)
    lock_pair("cito_out_pos", "cito_out_neg")
    lock_pair("colpo16_pos", "colpo16_neg")
    lock_pair("colpoout_pos", "colpoout_neg")
    
    # trios (3 categorias) — ajusta só “Negative” / “Negative/CIN1” por padrão
    lock_triplet("p16_18", "poutros", "pneg", remainder_id = "pneg", adjust_id = "poutros")
    lock_triplet("b16_neg_nic1", "b16_nic23", "b16_cancer", remainder_id = "b16_neg_nic1", adjust_id = "b16_nic23")
    lock_triplet("bo_neg_nic1",  "bo_nic23",  "bo_cancer",  remainder_id = "bo_neg_nic1",  adjust_id = "bo_nic23")
    
    observeEvent(input$reset_params, {
      if (!identical(input$screen_method, "hpv")) return()
      for (nm in names(hpv_defaults)) {
        updateNumericInput(session, nm, value = hpv_defaults[[nm]])
      }
    }, ignoreInit = TRUE)

    # ---------- Preset HPV: carrega parâmetros ao selecionar fonte ----------
    observeEvent(input$hpv_param_source, {
      src <- input$hpv_param_source
      if (is.null(src) || identical(src, "custom")) return()

      if (!exists("HPV_PRESETS", inherits = TRUE)) return()
      presets <- get("HPV_PRESETS", inherits = TRUE)
      if (!src %in% names(presets)) return()

      p <- presets[[src]]$params
      for (nm in names(p)) {
        updateNumericInput(session, nm, value = round(p[[nm]], 2))
      }
    }, ignoreInit = TRUE)

    # ---------- Preset Citologia: carrega parâmetros ao mudar fonte ou UF ----------
    # Lógica:
    #   inca2019          → parâmetros nacionais INCA, sempre
    #   siscan + 0 UFs    → parâmetros SISCAN Brasil
    #   siscan + 1 UF     → parâmetros SISCAN daquela UF
    #   siscan + N UFs    → parâmetros SISCAN Brasil (fallback)
    #   custom            → usuário edita livremente, não carrega nada

    load_cito_preset <- function() {
      src <- input$cito_param_source
      if (is.null(src) || identical(src, "custom")) return()
      if (is.null(cito_presets) || !src %in% names(cito_presets)) return()
      
      meta    <- if (exists("CITO_PRESETS_META", inherits = TRUE))
        get("CITO_PRESETS_META", inherits = TRUE) else list()
      por_uf  <- isTRUE(meta[[src]]$por_uf)
      uf_sel  <- input$filt_uf
      uf_key  <- if (por_uf && length(uf_sel) == 1L) uf_sel else "brasil"
      
      fonte <- cito_presets[[src]]
      p     <- if (uf_key %in% names(fonte)) fonte[[uf_key]] else fonte[["brasil"]]
      
      for (nm in names(p)) {
        updateNumericInput(session, nm, value = round(p[[nm]], 3))
      }
    }

    observeEvent(input$cito_param_source, {
      load_cito_preset()
    }, ignoreInit = TRUE)

    observeEvent(input$filt_uf, {
      if (!identical(input$screen_method, "cytology")) return()
      src <- input$cito_param_source
      if (is.null(src) || identical(src, "custom")) return()
      meta <- if (exists("CITO_PRESETS_META", inherits = TRUE))
        get("CITO_PRESETS_META", inherits = TRUE) else list()
      if (!isTRUE(meta[[src]]$por_uf)) return()
      load_cito_preset()
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # ---------- Cytology: validação + auto-balance ----------
    param_errors_cito <- reactive({
      if (!identical(input$screen_method, "cytology")) return(character(0))
      
      errs <- character(0)
      if (!sum_ok(input$res_asch_pct, input$res_other_pct, input$res_neg_pct)) {
        errs <- c(errs, "Cytology result must sum to 100%.")
      }
      if (!sum_ok(input$b_asch_neg_nic1_pct, input$b_asch_nic23_pct, input$b_asch_cancer_pct)) {
        errs <- c(errs, "Biopsy result (HSIL / ASC-H / AOI / AIS / Carcinoma arm) must sum to 100%.")
      }
      if (!sum_ok(input$b_other_neg_nic1_pct, input$b_other_nic23_pct, input$b_other_cancer_pct)) {
        errs <- c(errs, "Biopsy result (Other abnormalities arm) must sum to 100%.")
      }
      errs
    })
    
    output$params_alert_cito <- renderUI({
      errs <- param_errors_cito()
      if (length(errs) == 0) return(NULL)
      div(
        style="background:#ffe6e6;border:1px solid #cc0000;color:#990000;padding:10px;border-radius:8px;",
        strong("Please check parameters:"),
        tags$ul(lapply(errs, tags$li))
      )
    })
    
    rv_lock_cito <- reactiveValues(updating = FALSE, prev = list())
    
    safe_update_num_cito <- function(id, val) {
      updateNumericInput(session, id, value = round(val, 2))
    }
    
    lock_triplet_cito <- function(idA, idB, idC, remainder_id, adjust_id, tol = 0.01) {
      ids <- c(idA, idB, idC)
      
      observeEvent(c(lapply(ids, function(i) input[[i]]), list(input$lock_prop_cito, input$screen_method)), {
        if (!identical(input$screen_method, "cytology")) return()
        if (!isTRUE(input$lock_prop_cito)) return()
        if (isTRUE(rv_lock_cito$updating)) return()
        
        v <- lapply(ids, function(i) clamp100(input[[i]]))
        names(v) <- ids
        
        prev <- lapply(ids, function(i) rv_lock_cito$prev[[i]])
        names(prev) <- ids
        for (i in ids) if (is.null(prev[[i]])) prev[[i]] <- v[[i]]
        
        changed <- vapply(ids, function(i) !approx_eq(v[[i]], prev[[i]], tol), logical(1))
        primary <- if (sum(changed) == 1) ids[which(changed)] else remainder_id
        
        other_ids <- setdiff(ids, remainder_id)
        
        rv_lock_cito$updating <- TRUE
        
        if (!identical(primary, remainder_id)) {
          s <- v[[other_ids[1]]] + v[[other_ids[2]]]
          if (s <= 100) {
            new_rem <- 100 - s
          } else {
            new_rem <- 0
            fixed_id <- setdiff(other_ids, adjust_id)
            new_adj <- max(0, 100 - v[[fixed_id]])
            v[[adjust_id]] <- new_adj
          }
          v[[remainder_id]] <- new_rem
          safe_update_num_cito(remainder_id, v[[remainder_id]])
          if (s > 100) safe_update_num_cito(adjust_id, v[[adjust_id]])
        } else {
          fixed_id <- setdiff(other_ids, adjust_id)
          new_adj <- max(0, 100 - v[[fixed_id]] - v[[remainder_id]])
          v[[adjust_id]] <- new_adj
          safe_update_num_cito(adjust_id, v[[adjust_id]])
        }
        
        for (i in ids) rv_lock_cito$prev[[i]] <- v[[i]]
        rv_lock_cito$updating <- FALSE
      }, ignoreInit = TRUE)
    }
    
    lock_triplet_cito("res_asch_pct", "res_other_pct", "res_neg_pct",
                      remainder_id = "res_neg_pct", adjust_id = "res_other_pct")
    
    lock_triplet_cito("b_asch_neg_nic1_pct", "b_asch_nic23_pct", "b_asch_cancer_pct",
                      remainder_id = "b_asch_neg_nic1_pct", adjust_id = "b_asch_nic23_pct")
    
    lock_triplet_cito("b_other_neg_nic1_pct", "b_other_nic23_pct", "b_other_cancer_pct",
                      remainder_id = "b_other_neg_nic1_pct", adjust_id = "b_other_nic23_pct")
    
    observeEvent(input$reset_params_cito, {
      if (!identical(input$screen_method, "cytology")) return()
      for (nm in names(cito_defaults)) {
        updateNumericInput(session, nm, value = cito_defaults[[nm]])
      }
    }, ignoreInit = TRUE)
    
    
    
    # ---------- Conversão automática de unidade de capacidade ----------
    unit_mult <- function(u) switch(u, dia = 240, semana = 48, mes = 12, ano = 1, 1)

    prev_cap_unit <- reactiveVal("ano")

    observeEvent(input$cap_unidade, {
      new_unit <- input$cap_unidade
      old_unit <- prev_cap_unit()
      if (identical(new_unit, old_unit)) return()

      factor <- unit_mult(old_unit) / unit_mult(new_unit)

      updateNumericInput(session, "cap_colposcopio",   value = round((input$cap_colposcopio   %||% 5760)  * factor, 2))
      updateNumericInput(session, "cap_colposcopista", value = round((input$cap_colposcopista %||% 2880)  * factor, 2))
      updateNumericInput(session, "cap_citopato",      value = round((input$cap_citopato      %||% 14400) * factor, 2))
      updateNumericInput(session, "cap_patologista",   value = round((input$cap_patologista   %||% 7200)  * factor, 2))

      prev_cap_unit(new_unit)
    }, ignoreInit = TRUE)

    # ---------- BASE BRASIL PARA FILTROS GEOGRÁFICOS ----------
    dt_br <- NULL
    if (!is.null(pop_municipio_regional)) {
      dt_br <- data.table::as.data.table(pop_municipio_regional)[
        ,
        .(
          UF,
          macro = `Macrorregiao de Saude`,
          reg   = `Regiao de Saude`,
          Municipio
        )
      ]
      data.table::setkey(dt_br, UF, macro, reg, Municipio)
      dt_br <- unique(dt_br)
    }
    
    # Quando muda o país: inicializa ou limpa filtros geográficos
    observeEvent(input$pais_sel, {
      req(!is.null(dt_br))
      
      # se não é Brasil -> limpa tudo e sai
      if (is.null(input$pais_sel) || as.integer(input$pais_sel) != br_code) {
        updateSelectizeInput(session, "filt_uf",
                             choices  = character(0),
                             selected = character(0))
        updateSelectizeInput(session, "filt_macro",
                             choices  = character(0),
                             selected = character(0))
        updateSelectizeInput(session, "filt_reg",
                             choices  = character(0),
                             selected = character(0))
        updateSelectizeInput(session, "filt_mun",
                             choices  = character(0),
                             selected = character(0))
        return()
      }
      
      # Brasil selecionado: carrega opções iniciais
      dt <- dt_br
      
      updateSelectizeInput(
        session, "filt_uf",
        choices  = sort(unique(dt$UF)),
        selected = intersect(input$filt_uf, unique(dt$UF)),
        server   = TRUE
      )
      updateSelectizeInput(
        session, "filt_macro",
        choices  = sort(unique(dt$macro)),
        selected = intersect(input$filt_macro, unique(dt$macro)),
        server   = TRUE
      )
      updateSelectizeInput(
        session, "filt_reg",
        choices  = sort(unique(dt$reg)),
        selected = intersect(input$filt_reg, unique(dt$reg)),
        server   = TRUE
      )
      updateSelectizeInput(
        session, "filt_mun",
        choices  = sort(unique(dt$Municipio)),
        selected = intersect(input$filt_mun, unique(dt$Municipio)),
        server   = TRUE
      )
    }, ignoreInit = FALSE)
    
    # Botão "Clear filters"
    observeEvent(input$limpar_filtros, {
      if (is.null(dt_br)) return()
      
      # zera seleções
      updateSelectizeInput(session, "filt_uf",    selected = character(0))
      updateSelectizeInput(session, "filt_macro", selected = character(0))
      updateSelectizeInput(session, "filt_reg",   selected = character(0))
      updateSelectizeInput(session, "filt_mun",   selected = character(0))
      
      # se Brasil estiver selecionado, volta opções completas
      if (!is.null(input$pais_sel) && as.integer(input$pais_sel) == br_code) {
        dt <- dt_br
        updateSelectizeInput(
          session, "filt_uf",
          choices  = sort(unique(dt$UF)),
          selected = character(0),
          server   = TRUE
        )
        updateSelectizeInput(
          session, "filt_macro",
          choices  = sort(unique(dt$macro)),
          selected = character(0),
          server   = TRUE
        )
        updateSelectizeInput(
          session, "filt_reg",
          choices  = sort(unique(dt$reg)),
          selected = character(0),
          server   = TRUE
        )
        updateSelectizeInput(
          session, "filt_mun",
          choices  = sort(unique(dt$Municipio)),
          selected = character(0),
          server   = TRUE
        )
      }
    }, ignoreInit = TRUE)
    
    # Cascata: UF -> macro -> reg -> mun
    observeEvent(input$filt_uf, {
      req(!is.null(dt_br))
      dt <- dt_br
      if (length(input$filt_uf)) dt <- dt[UF %in% input$filt_uf]
      
      updateSelectizeInput(
        session, "filt_macro",
        choices  = sort(unique(dt$macro)),
        selected = intersect(input$filt_macro, unique(dt$macro)),
        server   = TRUE
      )
      updateSelectizeInput(
        session, "filt_reg",
        choices  = sort(unique(dt$reg)),
        selected = intersect(input$filt_reg, unique(dt$reg)),
        server   = TRUE
      )
      updateSelectizeInput(
        session, "filt_mun",
        choices  = sort(unique(dt$Municipio)),
        selected = intersect(input$filt_mun, unique(dt$Municipio)),
        server   = TRUE
      )
    }, ignoreNULL = FALSE)
    
    observeEvent(input$filt_macro, {
      req(!is.null(dt_br))
      dt <- dt_br
      if (length(input$filt_uf))    dt <- dt[UF %in% input$filt_uf]
      if (length(input$filt_macro)) dt <- dt[macro %in% input$filt_macro]
      
      updateSelectizeInput(
        session, "filt_reg",
        choices  = sort(unique(dt$reg)),
        selected = intersect(input$filt_reg, unique(dt$reg)),
        server   = TRUE
      )
      updateSelectizeInput(
        session, "filt_mun",
        choices  = sort(unique(dt$Municipio)),
        selected = intersect(input$filt_mun, unique(dt$Municipio)),
        server   = TRUE
      )
    }, ignoreNULL = FALSE)
    
    observeEvent(input$filt_reg, {
      req(!is.null(dt_br))
      dt <- dt_br
      if (length(input$filt_uf))    dt <- dt[UF %in% input$filt_uf]
      if (length(input$filt_macro)) dt <- dt[macro %in% input$filt_macro]
      if (length(input$filt_reg))   dt <- dt[reg   %in% input$filt_reg]
      
      updateSelectizeInput(
        session, "filt_mun",
        choices  = sort(unique(dt$Municipio)),
        selected = intersect(input$filt_mun, unique(dt$Municipio)),
        server   = TRUE
      )
    }, ignoreNULL = FALSE)
    
    # ---------- Saída: lista reativa com todos os filtros ----------
    filters <- reactive({
      cap_unit <- input$cap_unidade %||% "ano"
      cap_mult <- switch(
        cap_unit,
        dia    = 240,
        semana = 48,
        mes    = 12,
        ano    = 1,
        1
      )
      
      # ---------- HPV / Cytology: defaults ----------
      if (!exists("HPV_DEFAULTS", inherits = TRUE)) {
        stop("mod_filters_cc_server(): objeto 'HPV_DEFAULTS' não encontrado.", call. = FALSE)
      }
      hpv_defaults <- get("HPV_DEFAULTS", inherits = TRUE)
      
      if (!exists("CITO_DEFAULTS", inherits = TRUE)) {
        stop("mod_filters_cc_server(): objeto 'CITO_DEFAULTS' não encontrado.", call. = FALSE)
      }
      cito_defaults <- get("CITO_DEFAULTS", inherits = TRUE)
      
      
      list(
        # aliases úteis
        pais_sel     = input$pais_sel,
        country_code = as.integer(input$pais_sel),
        
        # população (engine_capacity_cc)
        pop_mode   = input$pop_mode %||% "globocan",
        custom_pop = input$custom_pop_main %||% NA_real_,
        
        # protocolo (CCU)
        screen_method  = input$screen_method %||% "hpv",
        target_age_min = input$target_age_min %||% 25,
        target_age_max = input$target_age_max %||% 64,
        
        # cobertura
        coverage = input$coverage %||% 70,
        
        # HPV params (defaults: HPV_DEFAULTS)
        p16_18       = input$p16_18 %||% hpv_defaults$p16_18,
        poutros      = input$poutros %||% hpv_defaults$poutros,
        pneg         = input$pneg %||% hpv_defaults$pneg,
        cito_out_pos = input$cito_out_pos %||% hpv_defaults$cito_out_pos,
        cito_out_neg = input$cito_out_neg %||% hpv_defaults$cito_out_neg,
        colpo16_pos  = input$colpo16_pos %||% hpv_defaults$colpo16_pos,
        colpo16_neg  = input$colpo16_neg %||% hpv_defaults$colpo16_neg,
        colpoout_pos = input$colpoout_pos %||% hpv_defaults$colpoout_pos,
        colpoout_neg = input$colpoout_neg %||% hpv_defaults$colpoout_neg,
        b16_neg_nic1 = input$b16_neg_nic1 %||% hpv_defaults$b16_neg_nic1,
        b16_nic23    = input$b16_nic23 %||% hpv_defaults$b16_nic23,
        b16_cancer   = input$b16_cancer %||% hpv_defaults$b16_cancer,
        bo_neg_nic1  = input$bo_neg_nic1 %||% hpv_defaults$bo_neg_nic1,
        bo_nic23     = input$bo_nic23 %||% hpv_defaults$bo_nic23,
        bo_cancer    = input$bo_cancer %||% hpv_defaults$bo_cancer,
        hpv_followup_pos_pct = input$hpv_followup_pos_pct %||% hpv_defaults$hpv_followup_pos_pct,
        
        
        # citologia (novo modelo)
        first_time_pct         = input$first_time_pct %||% cito_defaults$first_time_pct,
        unsatisfactory_pct     = input$unsatisfactory_pct %||% cito_defaults$unsatisfactory_pct,
        
        res_asch_pct           = input$res_asch_pct %||% cito_defaults$res_asch_pct,
        res_other_pct          = input$res_other_pct %||% cito_defaults$res_other_pct,
        res_neg_pct            = input$res_neg_pct %||% cito_defaults$res_neg_pct,
        
        colpo_asch_pct         = input$colpo_asch_pct %||% cito_defaults$colpo_asch_pct,
        colpo_other_follow_pct = input$colpo_other_follow_pct %||% cito_defaults$colpo_other_follow_pct,
        
        biopsy_pos_asch_pct    = input$biopsy_pos_asch_pct %||% cito_defaults$biopsy_pos_asch_pct,
        biopsy_pos_other_pct   = input$biopsy_pos_other_pct %||% cito_defaults$biopsy_pos_other_pct,
        
        b_asch_nic23_pct        = input$b_asch_nic23_pct %||% cito_defaults$b_asch_nic23_pct,
        b_asch_cancer_pct       = input$b_asch_cancer_pct %||% cito_defaults$b_asch_cancer_pct,
        b_asch_neg_nic1_pct     = input$b_asch_neg_nic1_pct %||% cito_defaults$b_asch_neg_nic1_pct,
        
        b_other_nic23_pct       = input$b_other_nic23_pct %||% cito_defaults$b_other_nic23_pct,
        b_other_cancer_pct      = input$b_other_cancer_pct %||% cito_defaults$b_other_cancer_pct,
        b_other_neg_nic1_pct    = input$b_other_neg_nic1_pct %||% cito_defaults$b_other_neg_nic1_pct,
        
        
        
        # capacidades (engine_capacity_cc) — sempre anualizadas
        cap_colpo_device = cap_mult * (input$cap_colposcopio %||% 5760),
        cap_colpo_med    = cap_mult * (input$cap_colposcopista %||% 2880),
        cap_citopato     = cap_mult * (input$cap_citopato %||% 14400),
        cap_patol_med    = cap_mult * (input$cap_patologista %||% 7200),
        
        # Brasil (subnacional)
        is_brazil   = !is.null(input$pais_sel) && as.integer(input$pais_sel) == as.integer(br_code),
        br_pop_tipo = input$br_pop_tipo %||% "total",
        filt_uf     = input$filt_uf,
        filt_macro  = input$filt_macro,
        filt_reg    = input$filt_reg,
        filt_mun    = input$filt_mun,
        
        # (mantém para módulos SUS)
        sia_geo_ref = input$sia_geo_ref,

        # fonte de parâmetros (HPV e citologia)
        hpv_param_source  = input$hpv_param_source  %||% names(HPV_PRESETS)[1],
        cito_param_source = input$cito_param_source %||% names(CITO_PRESETS_META)[1]
      )
    })
    
    
    return(filters)
  })
}
