# ===========================================================
# Shiny-cc — 14_mod_equipamentos.R
# Equipment and human resources needs (CCU)
#   - Cards (Demand | Capacity | Required)
# ===========================================================

mod_equipment_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "cc-page-header",
      div(class = "cc-page-title", "Equipment & HR needs"),
      div(class = "cc-page-subtitle", textOutput(ns("geo_desc")))
    ),
    uiOutput(ns("ccu_cards"))
  )
}

mod_equipment_server <- function(id,
                                 df_completo,
                                 dim_age,
                                 dim_country,
                                 input_global,
                                 pop_mun_regional) {
  
  moduleServer(id, function(input, output, session) {
    
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
    
    # ---- config (CCU engine) ----------------------------------------
    cfg_reactive <- reactive({
      g <- input_global()
      
      cc_engine_settings(
        country_code   = country_code(),
        pop_mode       = g$pop_mode %||% "globocan",
        coverage       = g$coverage %||% 70,
        screen_method  = g$screen_method %||% "hpv",
        
        target_age_min = g$target_age_min %||% 25,
        target_age_max = g$target_age_max %||% 64,
        custom_pop     = g$custom_pop_main %||% NA_real_,
        
        # HPV params
        p16_18       = g$p16_18,
        poutros      = g$poutros,
        pneg         = g$pneg,
        cito_out_pos = g$cito_out_pos,
        cito_out_neg = g$cito_out_neg,
        colpo16_pos  = g$colpo16_pos,
        colpo16_neg  = g$colpo16_neg,
        colpoout_pos = g$colpoout_pos,
        colpoout_neg = g$colpoout_neg,
        b16_neg_nic1 = g$b16_neg_nic1,
        b16_nic23    = g$b16_nic23,
        b16_cancer   = g$b16_cancer,
        bo_neg_nic1  = g$bo_neg_nic1,
        bo_nic23     = g$bo_nic23,
        bo_cancer    = g$bo_cancer,
        
        # citologia (novo modelo)
        first_time_pct         = g$first_time_pct,
        unsatisfactory_pct     = g$unsatisfactory_pct,
        
        res_asch_pct           = g$res_asch_pct,
        res_other_pct          = g$res_other_pct,
        res_neg_pct            = g$res_neg_pct,
        
        colpo_asch_pct         = g$colpo_asch_pct,
        colpo_other_follow_pct = g$colpo_other_follow_pct,
        
        biopsy_pos_asch_pct    = g$biopsy_pos_asch_pct,
        biopsy_pos_other_pct   = g$biopsy_pos_other_pct,
        
        b_asch_nic23_pct       = g$b_asch_nic23_pct,
        b_asch_cancer_pct      = g$b_asch_cancer_pct,
        b_asch_neg_nic1_pct    = g$b_asch_neg_nic1_pct,
        
        b_other_nic23_pct      = g$b_other_nic23_pct,
        b_other_cancer_pct     = g$b_other_cancer_pct,
        b_other_neg_nic1_pct   = g$b_other_neg_nic1_pct,
        
        
        # capacities (Equipment/HR)
        cap_colpo_device = g$cap_colpo_device,
        cap_colpo_med    = g$cap_colpo_med,
        cap_citopato     = g$cap_citopato,
        cap_patol_med    = g$cap_patol_med,
        
        # Brasil subnacional
        is_brazil   = g$is_brazil,
        br_pop_tipo = g$br_pop_tipo,
        filt_uf     = g$filt_uf,
        filt_macro  = g$filt_macro,
        filt_reg    = g$filt_reg,
        filt_mun    = g$filt_mun
      )
    })
    
    engine_res <- reactive({
      cfg <- cfg_reactive()
      cc_engine_run(
        df_completo,
        cfg,
        pop_mun_regional = pop_mun_regional
      )
    })
    
    # ---- helpers ----------------------------------------------------
    num1 <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (!is.finite(x) || is.na(x) || x < 0) 0 else x
    }
    
    cap1 <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (!is.finite(x) || is.na(x) || x <= 0) NA_real_ else x
    }
    
    required_n <- function(demand, cap_year) {
      if (!is.finite(cap_year) || is.na(cap_year) || cap_year <= 0) return(NA_real_)
      ceiling(demand / cap_year)
    }
    
    demand_cytology <- function(m) {
      if (is.null(m) || !nrow(m)) return(0)
      if (all(c("cit_rastreamento", "cit_diagnostica") %chin% names(m))) {
        return(num1(m$cit_rastreamento[1]) + num1(m$cit_diagnostica[1]))
      }
      if ("cito_reflexa" %chin% names(m)) return(num1(m$cito_reflexa[1]))
      if ("rastreada" %chin% names(m)) return(num1(m$rastreada[1]))
      0
    }
    
    demand_pathology <- function(m) {
      if (is.null(m) || !nrow(m)) return(0)
      if (("ap_biopsia" %chin% names(m)) || ("ap_peca_cir" %chin% names(m))) {
        return(num1(m$ap_biopsia[1]) + num1(m$ap_peca_cir[1]))
      }
      if ("biopsia_indicada" %chin% names(m)) return(num1(m$biopsia_indicada[1]))
      0
    }
    
    # SVG icons (mesmo estilo do Summary)
    ico_scope  <- '<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><circle cx="11" cy="11" r="7"/><line x1="16.5" y1="16.5" x2="21" y2="21"/><line x1="8" y1="11" x2="14" y2="11"/><line x1="11" y1="8" x2="11" y2="14"/></svg>'
    ico_doctor <- '<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="7" r="3"/><path d="M5 21v-2a4 4 0 0 1 4-4h6a4 4 0 0 1 4 4v2"/><path d="M12 10v5"/><path d="M10 12h4"/></svg>'
    ico_micro  <- '<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><circle cx="10" cy="8" r="3"/><path d="M10 11v4"/><path d="M7 15h6"/><path d="M6 18h13"/><path d="M13 8h3a3 3 0 0 1 3 3v0"/></svg>'
    ico_lab    <- '<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><rect x="4" y="3" width="16" height="5" rx="1"/><path d="M6 8v11a2 2 0 0 0 2 2h8a2 2 0 0 0 2-2V8"/><path d="M10 12h4"/><path d="M10 16h4"/></svg>'

    card_ui <- function(title, demand, cap_year, required, icon_svg = NULL, cap_label = "Capacity") {
      req_txt    <- if (is.na(required)) "\u2013" else fmt_int(required)
      demand_txt <- paste0("Demand: ", fmt_int(round(demand)))
      cap_txt    <- if (is.na(cap_year))
        paste0(cap_label, ": \u2013 / year")
      else
        paste0(cap_label, ": ", fmt_int(round(cap_year)), " / year")

      div(
        class = "card-ccu",
        if (!is.null(icon_svg)) HTML(icon_svg),
        div(class = "card-ccu-label", title),
        div(class = "card-ccu-value", req_txt),
        div(class = "card-ccu-sub",
            HTML(paste0(demand_txt, " &nbsp;&middot;&nbsp; ", cap_txt, " &nbsp;&middot;&nbsp; Required")))
      )
    }

    # ---- notes (texto técnico dos parâmetros) ---------------------
    build_notes <- function() {
      g <- input_global()

      fmt_or_dash <- function(x) {
        if (is.null(x) || length(x) == 0L || is.na(x)) "\u2013" else fmt_int(round(as.numeric(x)))
      }
      num_or_na <- function(x) {
        x <- suppressWarnings(as.numeric(x))
        if (!is.finite(x) || is.na(x)) NA_real_ else x
      }
      pct2 <- function(x) {
        x <- num_or_na(x)
        if (is.na(x)) "NA" else sprintf("%.2f%%", x)
      }

      age_txt <- sprintf("Ages %s\u2013%s", fmt_or_dash(g$target_age_min), fmt_or_dash(g$target_age_max))
      cov_txt <- sprintf("Coverage %s%%", fmt_or_dash(g$coverage))

      method <- as.character(cfg_reactive()$screen_method %||% g$screen_method %||% "hpv")
      test_txt <- if (identical(method, "hpv")) "HPV every 5 years" else "Cytology every 3 years"
      note_target <- paste(age_txt, cov_txt, test_txt, sep = " \u00b7 ")

      if (identical(method, "hpv")) {
        note_params <- paste0(
          "HPV 16/18+: ", pct2(g$p16_18),
          " &middot; Other HR-HPV+: ", pct2(g$poutros),
          " &middot; Negative: ", pct2(g$pneg),
          " &middot; Reflex cytology positivity: ", pct2(g$cito_out_pos),
          "<br/>Colposcopy positivity \u2013 HPV 16/18+: ", pct2(g$colpo16_pos),
          " &middot; Other HR-HPV+: ", pct2(g$colpoout_pos),
          "<br/>Biopsy (CIN2+) \u2013 HPV 16/18+: ", pct2(g$b16_nic23),
          " &middot; Other HR-HPV+: ", pct2(g$bo_nic23),
          " &middot; Cancer (16/18+): ", pct2(g$b16_cancer),
          " &middot; Cancer (other): ", pct2(g$bo_cancer)
        )
      } else {
        note_params <- paste0(
          "First-time exams: ", pct2(g$first_time_pct),
          " &middot; Unsatisfactory: ", pct2(g$unsatisfactory_pct),
          "<br/>Results \u2013 HSIL/ASC-H/AOI/AIS/Ca: ", pct2(g$res_asch_pct),
          " &middot; Other abnorm.: ", pct2(g$res_other_pct),
          " &middot; Negative: ", pct2(g$res_neg_pct),
          "<br/>Colposcopy referral \u2013 HSIL arm: ", pct2(g$colpo_asch_pct),
          " &middot; Other arm: ", pct2(g$colpo_other_follow_pct),
          "<br/>Colposcopy positivity \u2013 HSIL arm: ", pct2(g$biopsy_pos_asch_pct),
          " &middot; Other arm: ", pct2(g$biopsy_pos_other_pct),
          "<br/>Biopsy (HSIL arm) \u2013 CIN2/3: ", pct2(g$b_asch_nic23_pct),
          " &middot; Cancer: ", pct2(g$b_asch_cancer_pct),
          "<br/>Biopsy (Other arm) \u2013 CIN2/3: ", pct2(g$b_other_nic23_pct),
          " &middot; Cancer: ", pct2(g$b_other_cancer_pct),
          "<br/>EZT follow-up: 6 cytologies and 2 colposcopies per EZT"
        )
      }

      HTML(paste0(note_target, "<br/>", note_params))
    }

    # ---- UI ---------------------------------------------------------
    output$ccu_cards <- renderUI({
      res <- engine_res()
      if (is.null(res)) return(NULL)

      g   <- input_global()
      cfg <- cfg_reactive()
      m   <- res$metrics
      data.table::setDT(m)

      d_colpo <- if ("colpo_indicada" %chin% names(m)) num1(m$colpo_indicada[1]) else 0
      d_cito  <- demand_cytology(m)
      d_ap    <- demand_pathology(m)

      cap_colpo_device <- cap1(cfg$cap_colpo_device)
      cap_colpo_med    <- cap1(cfg$cap_colpo_med)
      cap_citopato     <- cap1(cfg$cap_citopato)
      cap_patol_med    <- cap1(cfg$cap_patol_med)

      r_colpo_device <- required_n(d_colpo, cap_colpo_device)
      r_colpo_med    <- required_n(d_colpo, cap_colpo_med)
      r_citopato     <- required_n(d_cito,  cap_citopato)
      r_patol_med    <- required_n(d_ap,    cap_patol_med)

      div(
        class = "ccu-flow",
        div(
          class = "ccu-section ccu-section-2",
          div(class = "ccu-section-title", "Equipment and human resources"),
          div(
            class = "cards-ccu-wrap",
            cc_with_tt(
              card_ui("Colposcope", d_colpo, cap_colpo_device, r_colpo_device, ico_scope),
              cc_TOOLTIPS$equipamentos_ccu$colposcope
            ),
            cc_with_tt(
              card_ui("Colposcopist (20h/week)", d_colpo, cap_colpo_med, r_colpo_med, ico_doctor),
              cc_TOOLTIPS$equipamentos_ccu$colposcopist
            ),
            cc_with_tt(
              card_ui("Cytopathologist", d_cito, cap_citopato, r_citopato, ico_micro),
              cc_TOOLTIPS$equipamentos_ccu$cytopathologist
            ),
            cc_with_tt(
              card_ui("Pathologist (20h/week)", d_ap, cap_patol_med, r_patol_med, ico_lab),
              cc_TOOLTIPS$equipamentos_ccu$pathologist
            )
          ),
          div(class = "ccu-note", build_notes())
        )
      )
    })

  })
}
