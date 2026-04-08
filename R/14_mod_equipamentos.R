# ===========================================================
# Shiny-cc — 14_mod_equipamentos.R
# Equipment and human resources needs (CCU)
#   - Cards (Demand | Capacity | Required)
# ===========================================================

mod_equipment_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Equipment and human resources needs"),
    h3(textOutput(ns("geo_desc"))),
    uiOutput(ns("ccu_cards")),
    uiOutput(ns("cap_comp_note"))
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
    
    card_ui <- function(title, demand, cap_year, required, cap_label = "Capacity") {
      demand_txt <- paste0("Demand: ", fmt_int(round(demand)))
      cap_txt    <- if (is.na(cap_year)) paste0(cap_label, ": – / year") else paste0(cap_label, ": ", fmt_int(round(cap_year)), " / year")
      req_txt    <- paste0("Required: ", if (is.na(required)) "–" else fmt_int(required))
      
      div(
        class = "card-ccu",
        tags$div(class = "cc-card-title", title),
        tags$div(class = "cc-card-sub",  paste0(demand_txt, "  |  ", cap_txt)),
        tags$div(class = "cc-card-kpi",  req_txt)
      )
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
        class = "cards-ccu-wrap",
        cc_with_tt(
          card_ui("Colposcope", d_colpo, cap_colpo_device, r_colpo_device),
          cc_TOOLTIPS$equipamentos_ccu$colposcope
        ),
        cc_with_tt(
          card_ui("Colposcopist (20h/week)", d_colpo, cap_colpo_med, r_colpo_med),
          cc_TOOLTIPS$equipamentos_ccu$colposcopist
        ),
        cc_with_tt(
          card_ui("Cytopathologist", d_cito, cap_citopato, r_citopato),
          cc_TOOLTIPS$equipamentos_ccu$cytopathologist
        ),
        cc_with_tt(
          card_ui("Pathologist (20h/week)", d_ap, cap_patol_med, r_patol_med),
          cc_TOOLTIPS$equipamentos_ccu$pathologist
        )
      )
    })
    output$cap_comp_note <- renderUI({
      g <- input_global()
      
      fmt_or_dash <- function(x) {
        if (is.null(x) || length(x) == 0L || is.na(x)) "–" else fmt_int(round(as.numeric(x)))
      }
      num_or_na <- function(x) {
        x <- suppressWarnings(as.numeric(x))
        if (!is.finite(x) || is.na(x)) NA_real_ else x
      }
      pct2 <- function(x) {
        x <- num_or_na(x)
        if (is.na(x)) "NA" else sprintf("%.2f%%", x)
      }
      
      # target + coverage + interval (texto)
      age_txt <- sprintf("Ages %s–%s", fmt_or_dash(g$target_age_min), fmt_or_dash(g$target_age_max))
      cov_txt <- sprintf("Coverage %s%%", fmt_or_dash(g$coverage))
      
      method <- as.character(cfg_reactive()$screen_method %||% g$screen_method %||% "hpv")
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
          "Cytology results: HSIL / ASC-H / AOI / AIS / Carcinoma = ", pct2(g$res_asch_pct),
          "; other abnormalities = ", pct2(g$res_other_pct),
          "; negative = ", pct2(g$res_neg_pct), ".",
          "<br/>",
          "Colposcopy referral: after HSIL / ASC-H / AOI / AIS / Carcinoma = ", pct2(g$colpo_asch_pct),
          "; after other abnormalities = ", pct2(g$colpo_other_follow_pct), ".",
          "<br/>",
          "Colposcopy positivity (biopsy indication): HSIL / ASC-H / AOI / AIS / Carcinoma arm = ", pct2(g$biopsy_pos_asch_pct),
          "; other abnormalities arm = ", pct2(g$biopsy_pos_other_pct), "."
        )
        
        note_trt <- paste0(
          "Biopsy outcomes (HSIL / ASC-H / AOI / AIS / Carcinoma arm): CIN2/3 = ", pct2(g$b_asch_nic23_pct),
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
      
      div(
        class = "ccu-note",
        HTML(paste0(note_target, "<br/>", note_workup, "<br/>", note_trt))
      )
    })
  })
}
