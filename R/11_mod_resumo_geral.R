# ===========================================================
#11_mod_resumo_geral.R
# ===========================================================

mod_resumo_geral_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Summary"),
    h3(textOutput(ns("geo_desc"))),
    uiOutput(ns("cards_resumo"))
  )
}


#####################SERVER###########################
mod_resumo_geral_server <- function(
    id,
    df_completo,
    dim_age,
    dim_country,
    input_global,
    pop_mun_regional
) {
  moduleServer(id, function(input, output, session) {
    
    # ---- helpers ----
    val_or <- function(x, default) {
      if (is.null(x) || length(x) == 0L || all(is.na(x))) default else x
    }
    fmt_or_dash <- function(x) {
      if (is.null(x) || length(x) == 0L || is.na(x)) "–" else fmt_int(round(x))
    }
    
    # ---- país selecionado -------------------------------------------
    country_code <- reactive({
      req(input_global()$pais_sel)
      as.integer(input_global()$pais_sel)
    })
    
    br_code <- tryCatch(
      {
        x <- dim_country[dim_country$population_name == "Brazil", "population_code"]
        if (length(x) == 0L || is.na(x[1])) NA_integer_ else as.integer(x[1])
      },
      error = function(e) NA_integer_
    )
    
    is_brazil <- reactive({
      !is.na(br_code) && isTRUE(country_code() == br_code)
    })
    
    pick_all <- function(x) {
      if (is.null(x) || !length(x)) return("–")
      x <- as.character(x)
      x <- x[!is.na(x) & nzchar(x)]
      if (!length(x)) return("–")
      if (length(x) == 1L) return(x[1])
      paste0(x[1], " (n=", length(x), ")")
    }
    
    country_label <- function(code) {
      if (is.null(code) || is.na(code)) return("World")
      nm <- tryCatch(
        {
          z <- dim_country[dim_country$population_code == code, "population_name"]
          if (length(z) == 0L || is.na(z[1])) "Selected country" else as.character(z[1])
        },
        error = function(e) "Selected country"
      )
      nm
    }
    
    output$geo_desc <- renderText({
      g <- input_global()
      
      # Mundo / País
      if (!isTRUE(is_brazil())) {
        return(country_label(country_code()))
      }
      
      parts <- c("Brazil")
      
      add_if <- function(x, prefix = NULL) {
        x <- if (is.null(x) || !length(x)) character(0) else as.character(x)
        x <- x[!is.na(x) & nzchar(x)]
        if (!length(x)) return(invisible(NULL))
        
        lab <- if (length(x) == 1L) x[1] else paste0(x[1], " (n=", length(x), ")")
        if (!is.null(prefix)) lab <- paste0(prefix, ": ", lab)
        parts <<- c(parts, lab)
      }
      
      add_if(g$filt_uf)     # UF
      add_if(g$filt_macro)  # Macro
      add_if(g$filt_reg)    # Região
      add_if(g$filt_mun)    # Município
      
      paste(parts, collapse = " - ")
    })
    
    
    # ---- cfg for engine_capacity_cc ----
    cfg <- reactive({
      g <- input_global()
      
      country_code <- suppressWarnings(as.integer(val_or(g$pais_sel, br_code)))
      if (is.na(country_code)) country_code <- br_code
      
      pop_mode <- as.character(val_or(g$pop_mode, "globocan"))
      if (!pop_mode %in% c("globocan", "other")) pop_mode <- "globocan"
      
      coverage <- suppressWarnings(as.numeric(val_or(g$coverage, 70)))
      if (!is.finite(coverage)) coverage <- 70
      
      # screen_method: prefer novo nome; fallback para "programa" (HPV/Citologia)
      screen_method <- val_or(g$screen_method, NULL)
      if (is.null(screen_method)) {
        prog <- val_or(g$programa, "HPV")
        screen_method <- if (identical(prog, "Citologia")) "cytology" else "hpv"
      }
      screen_method <- as.character(screen_method)
      if (!screen_method %in% c("hpv", "cytology")) screen_method <- "hpv"
      
      # idades/intervalo: prefer novo; fallback para proto1_* (se ainda existir)
      target_age_min <- suppressWarnings(as.numeric(val_or(g$target_age_min, val_or(g$proto1_age_min, 25))))
      target_age_max <- suppressWarnings(as.numeric(val_or(g$target_age_max, val_or(g$proto1_age_max, 64))))
      if (!is.finite(target_age_min)) target_age_min <- 25
      if (!is.finite(target_age_max)) target_age_max <- 64
      
      
      custom_pop <- NA_real_
      if (identical(pop_mode, "other")) {
        custom_pop <- suppressWarnings(as.numeric(val_or(g$custom_pop_main, NA_real_)))
      }
      
      is_br <- isTRUE(!is.na(country_code) && country_code == br_code)
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
        
        # HPV params (se não vierem, engine usa defaults)
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
        
        # citologia (novo modelo)
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
        
        # capacidades (se existirem no filters; se NA, engine cai no BASE_ANO)
        cap_colpo_device  = val_or(g$cap_colpo_device, NA_real_),
        cap_colpo_med     = val_or(g$cap_colpo_med, NA_real_),
        cap_citopato      = val_or(g$cap_citopato, NA_real_),
        cap_patol_med     = val_or(g$cap_patol_med, NA_real_),
        
        # BR subnacional
        is_brazil         = is_br,
        br_pop_tipo       = br_pop_tipo,
        filt_uf           = val_or(g$filt_uf, NULL),
        filt_macro        = val_or(g$filt_macro, NULL),
        filt_reg          = val_or(g$filt_reg, NULL),
        filt_mun          = val_or(g$filt_mun, NULL)
      )
    })
    
    # ---- run engine ----
    res_engine <- reactive({
      req(df_completo)
      tryCatch(
        cc_engine_run(df_completo, cfg(), pop_mun_regional = pop_mun_regional),
        error = function(e) e
      )
    })
    
    dt_sum <- reactive({
      r <- res_engine()
      if (inherits(r, "error")) return(NULL)
      cc_engine_summary_dt(r)
    })
    
    # ---- cards ----
    output$cards_resumo <- renderUI({
      dt <- dt_sum()
      tt <- cc_TOOLTIPS$resumo_geral_ccu
      
      if (is.null(dt) || !nrow(dt)) {
        return(tags$div(
          div(style = "margin:6px; opacity:0.8;", "No data for the current selection.")
        ))
      }
      
      s <- dt[1]
      method <- as.character(s$screen_method)
      if (!method %in% c("hpv", "cytology")) method <- "hpv"
      
      g <- input_global()
      
      num_or_na <- function(x) {
        x <- suppressWarnings(as.numeric(x))
        if (!is.finite(x) || is.na(x)) NA_real_ else x
      }
      pct2 <- function(x) {
        x <- num_or_na(x)
        if (is.na(x)) "NA" else sprintf("%.2f%%", x)
      }
      
      card <- function(title, value, tooltip = NULL, flex = "flex: 1 1 260px;") {
        div(
          class = "card-ccu",
          title = tooltip,
          style = flex,
          div(style = "font-size:12px; opacity:0.9;", title),
          div(style = "font-size:22px; font-weight:bold;", fmt_or_dash(value))
        )
      }
      
      
      # ---------------------------
      # Notes (English)
      # ---------------------------
      age_txt <- sprintf("Ages %s–%s", fmt_or_dash(s$target_age_min), fmt_or_dash(s$target_age_max))
      cov_txt <- sprintf("Coverage %s%%", fmt_or_dash(s$coverage_percent))
      test_txt <- if (identical(method, "hpv")) "HPV every 5 years." else "Cytology every 3 years."
      note_target <- paste(age_txt, cov_txt, test_txt, sep = "; ")
      
      if (identical(method, "hpv")) {
        note_workup <- paste0(
          "HPV test parameters: HPV 16/18+: ", pct2(g$p16_18),
          "; Other HR-HPV+: ", pct2(g$poutros),
          "; negative: ", pct2(g$pneg),
          "; Other HR-HPV reflex cytology positivity: ", pct2(g$cito_out_pos), ".",
          "<br/>",
          "Colposcopy parameters: HPV 16/18+: ", pct2(g$colpo16_pos),
          "; after other HR-HPV+: ", pct2(g$colpoout_pos), "."
        )
        
        cin2p_16  <- if (is.na(num_or_na(g$b16_nic23))) "NA" else sprintf("%.2f%%", num_or_na(g$b16_nic23))
        cin2p_out <- if (is.na(num_or_na(g$bo_nic23)))  "NA" else sprintf("%.2f%%", num_or_na(g$bo_nic23))
        can_16    <- if (is.na(num_or_na(g$b16_cancer))) "NA" else sprintf("%.2f%%", num_or_na(g$b16_cancer))
        can_out   <- if (is.na(num_or_na(g$bo_cancer)))  "NA" else sprintf("%.2f%%", num_or_na(g$bo_cancer))
        
        note_trt <- paste0(
          "Biopsy positivity (CIN2+): HPV 16/18+ = ", cin2p_16,
          "; after other HR-HPV+ = ", cin2p_out, ".",
          "<br/>",
          "Cancer: HPV 16/18+ = ", can_16,
          "; after other HR-HPV+ = ", can_out, "."
        )
        
      } else {
        note_workup <- paste0(
          "Cytology volume parameters: first-time exams = ", pct2(g$first_time_pct),
          "; unsatisfactory exams = ", pct2(g$unsatisfactory_pct), ".",
          "<br/>",
          "Cytology results: ASC-H+ = ", pct2(g$res_asch_pct),
          "; other abnormalities = ", pct2(g$res_other_pct),
          "; negative = ", pct2(g$res_neg_pct), ".",
          "<br/>",
          "Colposcopy referral: after ASC-H+ = ", pct2(g$colpo_asch_pct),
          "; after other abnormalities = ", pct2(g$colpo_other_follow_pct), ".",
          "<br/>",
          "Colposcopy positivity (biopsy indication): ASC-H+ arm = ", pct2(g$biopsy_pos_asch_pct),
          "; other abnormalities arm = ", pct2(g$biopsy_pos_other_pct), "."
        )
        
        note_trt <- paste0(
          "Biopsy outcomes (ASC-H+ arm): CIN2/3 = ", pct2(g$b_asch_nic23_pct),
          "; cancer = ", pct2(g$b_asch_cancer_pct),
          "; negative/CIN1 = ", pct2(g$b_asch_neg_nic1_pct), ".",
          "<br/>",
          "Biopsy outcomes (Other abnormalities arm): CIN2/3 = ", pct2(g$b_other_nic23_pct),
          "; cancer = ", pct2(g$b_other_cancer_pct),
          "; negative/CIN1 = ", pct2(g$b_other_neg_nic1_pct), ".",
          "<br/>",
          "Follow-up assumptions: EZT follow-up = 6 cytologies and 2 colposcopies per EZT."
        )
      }
      
      # =========================
      # 1) Target population
      # =========================
      sec_target <- tags$div(
        tags$h4(class = "ccu-section-title", "Target population"),
        div(
          class = "cards-ccu-wrap",
          card("Selected population", s$pop_selected, tt$common$pop_selected, flex = "flex: 1 1 360px;"),
          card("Screened per year",   s$screened_per_year, tt$common$screened_year, flex = "flex: 1 1 360px;")
        ),
        div(class = "ccu-note", note_target)
      )
      
      # =========================
      # 2) Work-up
      # =========================
      sec_workup <- if (identical(method, "hpv")) {
        tags$div(
          tags$h4(class = "ccu-section-title", "Work-up"),
          div(
            class = "cards-ccu-wrap",
            card("Reflex cytology",      s$cito_reflexa,     tt$hpv$cito_reflexa,     flex = "flex: 1 1 320px;"),
            card("Colposcopy indicated", s$colpo_indicada,   tt$hpv$colpo_indicada,   flex = "flex: 1 1 320px;"),
            card("Biopsy indicated",     s$biopsia_indicada, tt$hpv$biopsia_indicada, flex = "flex: 1 1 320px;")
          ),
          div(class = "ccu-note", HTML(note_workup))
        )
      } else {
        tags$div(
          tags$h4(class = "ccu-section-title", "Work-up"),
          div(
            class = "cards-ccu-wrap",
            card("Diagnostic cytology", s$cit_diagnostica,   tt$cytology$cit_diagnostica,  flex = "flex: 1 1 320px;"),
            card("Colposcopy",          s$colpo_indicada,    tt$cytology$colpo_indicada,   flex = "flex: 1 1 320px;"),
            card("Biopsy",              s$biopsia_indicada,  tt$cytology$biopsia_indicada, flex = "flex: 1 1 320px;")
          ),
          div(class = "ccu-note", HTML(note_workup))
        )
      }
      
      # =========================
      # 3) Treatment and follow up
      # =========================
      sec_trt <- if (identical(method, "hpv")) {
        tags$div(
          tags$h4(class = "ccu-section-title", "Treatment and follow up"),
          div(
            class = "cards-ccu-wrap",
            card("Excision indicated (EZT)", s$ezt,              tt$hpv$ezt,               flex = "flex: 1 1 320px;"),
            card("Invasive Carcinoma",         s$alta_complexidade, tt$hpv$alta_complexidade, flex = "flex: 1 1 320px;"),
            card("Return in 1 year",        s$retorno_1ano,      tt$hpv$retorno_1ano,       flex = "flex: 1 1 320px;")
          ),
          div(class = "ccu-note", HTML(note_trt))
        )
      } else {
        tags$div(
          tags$h4(class = "ccu-section-title", "Treatment and follow up"),
          div(
            class = "cards-ccu-wrap",
            card("Excision indicated (EZT)",  s$ezt,               tt$cytology$ezt,                flex = "flex: 1 1 320px;"),
            card("Follow-up cytologies",  s$followup_cytologies,  (tt$cytology$followup_cytologies %||% NULL),  flex = "flex: 1 1 320px;"),
            card("Follow-up colposcopies", s$followup_colposcopies, (tt$cytology$followup_colposcopies %||% NULL), flex = "flex: 1 1 320px;"),
            card("Invasive Carcinoma",        s$alta_complexidade, tt$cytology$alta_complexidade,  flex = "flex: 1 1 320px;")
          ),
        
          div(class = "ccu-note", HTML(note_trt))
        )
      }
      
      tags$div(
        sec_target,
        sec_workup,
        sec_trt
      )
    })
    
    
    
  })
}
