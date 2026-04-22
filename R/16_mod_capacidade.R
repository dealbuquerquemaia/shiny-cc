# ===========================================================
# 16_mod_capacidade.R
# Módulo Capacidade (CCU)
#   - Totais realizados (SUS/SIA, 2024): citologia, colposcopia, biópsia, EZT
#   - Comparativo: realizado vs necessidade estimada (mesmos exames)
# ===========================================================

mod_capacity_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "cc-page-header",
      div(class = "cc-page-title", "Capacity"),
      div(class = "cc-page-subtitle", textOutput(ns("geo_desc")))
    ),

    # ── Section 1: Procedure delivery ──────────────────────────────────
    div(
      class = "ccu-section ccu-section-1",
      uiOutput(ns("delivery_title")),
      div(
        class = "cards-ccu-wrap",

        div(
          class = "card-ccu",
          title = cc_TOOLTIPS$capacidade_ccu$citologia_total,
          HTML('<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><path d="M9 3h6"/><path d="M10 3v9l-4 6a1 1 0 0 0 .85 1.5h10.3A1 1 0 0 0 18 18l-4-6V3"/><line x1="8.5" y1="15" x2="15.5" y2="15"/></svg>'),
          div(class = "card-ccu-label", "Cytology"),
          div(class = "card-ccu-value", textOutput(ns("citologia_value"))),
          div(class = "card-ccu-sub",   textOutput(ns("citologia_sub")))
        ),

        div(
          class = "card-ccu",
          title = cc_TOOLTIPS$capacidade_ccu$colposcopia_total,
          HTML('<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><circle cx="11" cy="11" r="7"/><line x1="16.5" y1="16.5" x2="21" y2="21"/><line x1="8" y1="11" x2="14" y2="11"/><line x1="11" y1="8" x2="11" y2="14"/></svg>'),
          div(class = "card-ccu-label", "Colposcopy"),
          div(class = "card-ccu-value", textOutput(ns("colpo_value"))),
          div(class = "card-ccu-sub",   textOutput(ns("colpo_sub")))
        ),

        div(
          class = "card-ccu",
          title = cc_TOOLTIPS$capacidade_ccu$biopsia_total,
          HTML('<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><rect x="10" y="5" width="4" height="3" rx="0.8"/><line x1="12" y1="5" x2="12" y2="8"/><path d="M11.2 8 L8 17.8"/><path d="M12.8 8 L16 17.8"/><circle cx="8" cy="20" r="2.2"/><circle cx="16" cy="20" r="2.2"/></svg>'),
          div(class = "card-ccu-label", "Biopsies"),
          div(class = "card-ccu-value", textOutput(ns("biopsia_value"))),
          div(class = "card-ccu-sub",   textOutput(ns("biopsia_sub")))
        ),

        div(
          class = "card-ccu",
          title = cc_TOOLTIPS$capacidade_ccu$ezt_total,
          HTML('<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="4" x2="4" y2="18"/><line x1="12" y1="4" x2="20" y2="18"/><ellipse cx="12" cy="18" rx="8" ry="2.5"/><line x1="12" y1="6" x2="12" y2="15.5" stroke-dasharray="1.8 2"/></svg>'),
          div(class = "card-ccu-label", "EZT"),
          div(class = "card-ccu-value", textOutput(ns("ezt_value"))),
          div(class = "card-ccu-sub",   textOutput(ns("ezt_sub")))
        )
      )
    ),

    # ── Section 2: Comparison ──────────────────────────────────────────
    uiOutput(ns("comp_title")),
    uiOutput(ns("cap_comp_cards")),
    uiOutput(ns("cap_comp_note"))
  )
}

# ===========================================================
# Módulo Capacidade — server (integra com input_global)
# ===========================================================

mod_capacity_server <- function(id,
                                df_completo,
                                dim_country,
                                input_global,
                                pop_mun_regional,
                                sia_cc_resumo,
                                regional_sus_map) {
  
  moduleServer(id, function(input, output, session) {
    
    # ---- código de Brazil na tabela de dimensões --------------------
    br_code <- tryCatch(
      {
        x <- dim_country[dim_country$population_name == "Brazil", "population_code"]
        if (length(x) == 0L || is.na(x[1])) NA_integer_ else as.integer(x[1])
      },
      error = function(e) NA_integer_
    )
    
    # ---- país selecionado -------------------------------------------
    country_code <- reactive({
      req(input_global()$pais_sel)
      as.integer(input_global()$pais_sel)
    })

    is_brazil <- reactive({
      !is.na(br_code) && isTRUE(country_code() == br_code)
    })

    cap_geo_label <- function(g) {
      if (!isTRUE(is_brazil())) return("selected geography")

      pick <- function(x, what) {
        if (is.null(x) || !length(x)) return(NULL)
        x <- as.character(x)
        x <- x[!is.na(x) & nzchar(x)]
        if (!length(x)) return(NULL)
        if (length(x) == 1L) return(paste0(what, ": ", x[1]))
        paste0(what, " (n=", length(x), "): ", x[1])
      }

      mun   <- pick(g$filt_mun,   "Municipality")
      reg   <- pick(g$filt_reg,   "Health region")
      macro <- pick(g$filt_macro, "Macro-region")
      uf    <- pick(g$filt_uf,    "State")

      if (!is.null(mun))   return(mun)
      if (!is.null(reg))   return(reg)
      if (!is.null(macro)) return(macro)
      if (!is.null(uf))    return(uf)

      "Brazil"
    }
    
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
    
    # ---- Engine (necessidade) --------------------------------------
    cfg_reactive <- reactive({
      g <- input_global()
      
      cc_engine_settings(
        country_code     = country_code(),
        pop_mode         = g$pop_mode %||% "globocan",
        coverage         = g$coverage %||% 70,
        screen_method    = g$screen_method %||% "hpv",
        target_age_min   = g$target_age_min %||% 25,
        target_age_max   = g$target_age_max %||% 64,
        custom_pop       = g$custom_pop %||% NA_real_,
        
        # HPV
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
        
        
        # capacidades
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
    
    # ---- SUS resumo (produção real) — já regionalizado e leve --------
    sia_filtered <- reactive({
      if (!is_brazil()) return(NULL)
      
      g  <- input_global()
      dt <- data.table::as.data.table(sia_cc_resumo)
      
      # Espera-se o novo dataset "sus_proc_resumo":
      # geo_ref, categoria, total_all, total_25_64, e geografia já mergeada (UF/macro/reg/mun)
      needed <- c(
        "geo_ref", "categoria", "total_all", "total_25_64",
        "UF", "Macrorregiao de Saude", "Regiao de Saude", "Municipio"
      )
      miss <- setdiff(needed, names(dt))
      if (length(miss)) {
        stop(
          "Capacidade: dataset SUS resumo não tem colunas esperadas: ",
          paste(miss, collapse = ", "),
          call. = FALSE
        )
      }
      
      geo_ref_sel <- g$sia_geo_ref
      if (is.null(geo_ref_sel) || !geo_ref_sel %in% c("care", "res")) geo_ref_sel <- "care"
      
      dt <- dt[geo_ref == geo_ref_sel]
      
      if (!is.null(g$filt_uf) && length(g$filt_uf))
        dt <- dt[UF %in% g$filt_uf]
      if (!is.null(g$filt_macro) && length(g$filt_macro))
        dt <- dt[`Macrorregiao de Saude` %in% g$filt_macro]
      if (!is.null(g$filt_reg) && length(g$filt_reg))
        dt <- dt[`Regiao de Saude` %in% g$filt_reg]
      if (!is.null(g$filt_mun) && length(g$filt_mun))
        dt <- dt[Municipio %in% g$filt_mun]
      
      if (!nrow(dt)) return(NULL)
      dt
    })
    
    
    
    # ---- Totais (SIA 2024) ------------------------------------------
    realized_cap <- reactive({
      dt <- sia_filtered()
      if (is.null(dt)) return(NULL)
      cc_capacity_from_sia(dt, ano_cmp_ref = 2024L)
    })
    
    # ---- Comparativo (realizado vs necessidade) ----------------------
    comp_dt <- reactive({
      if (!is_brazil()) return(data.table::data.table())
      res_engine <- engine_res()
      cap        <- realized_cap()
      if (is.null(res_engine) || is.null(cap)) return(data.table::data.table())
      
      d <- mod_capacity_compare(res_engine, cap)
      if (!nrow(d)) return(d)
      
      d[item %chin% c("citologia_total", "colposcopia_total", "biopsia_total", "tratamento_total")]
    })
    
    # ---- KPI helpers -------------------------------------------------
    txt_na_val <- function() "–"
    txt_na_sub <- function() "Data not available – only Brazil (SUS, 2024)"

    cap_sub <- function() {
      g <- input_global()
      paste0("SUS/SIA 2024 \u2014 ", cap_geo_label(g))
    }

    # Most specific geographic name without "Type: " prefix
    geo_name_label <- function(g) {
      pick_name <- function(x) {
        if (is.null(x) || !length(x)) return(NULL)
        x <- as.character(x[!is.na(x) & nzchar(x)])
        if (!length(x)) return(NULL)
        if (length(x) == 1L) x[1L] else paste0(x[1L], " (+", length(x) - 1L, ")")
      }
      pick_name(g$filt_mun)   %||%
        pick_name(g$filt_reg)   %||%
        pick_name(g$filt_macro) %||%
        pick_name(g$filt_uf)
    }

    # Section 1 title (reactive)
    output$delivery_title <- renderUI({
      g   <- input_global()
      geo <- geo_name_label(g)

      suffix <- if (!isTRUE(is_brazil())) {
        "SUS/SIA 2024"
      } else if (!is.null(geo)) {
        geo
      } else {
        pop_lbl <- if (isTRUE(g$br_pop_tipo == "sus")) "SUS-dependent" else "Total population"
        paste0("Brazil \u00B7 ", pop_lbl)
      }

      div(class = "ccu-section-title",
          paste0("Procedure delivery \u2014 ", suffix))
    })

    # Section 2 title (reactive)
    output$comp_title <- renderUI({
      g       <- input_global()
      sia_ref <- g$sia_geo_ref %||% "care"
      sia_lbl <- if (identical(sia_ref, "res")) "SIA by place of residence" else "SIA by place of care"

      div(class = "cap-comp-section-title",
          paste0("Actual production vs. estimated need \u2014 ", sia_lbl))
    })

    output$citologia_value <- renderText({
      if (!is_brazil()) return(txt_na_val())
      c <- realized_cap()
      if (is.null(c) || is.na(c$citologia_total)) return(txt_na_val())
      fmt_int(c$citologia_total)
    })
    output$citologia_sub <- renderText({
      if (!is_brazil()) return(txt_na_sub())
      cap_sub()
    })
    
    output$colpo_value <- renderText({
      if (!is_brazil()) return(txt_na_val())
      c <- realized_cap()
      if (is.null(c) || is.na(c$colposcopia_total)) return(txt_na_val())
      fmt_int(c$colposcopia_total)
    })
    output$colpo_sub <- renderText({
      if (!is_brazil()) return(txt_na_sub())
      cap_sub()
    })
    
    output$biopsia_value <- renderText({
      if (!is_brazil()) return(txt_na_val())
      c <- realized_cap()
      if (is.null(c) || is.na(c$biopsia_total)) return(txt_na_val())
      fmt_int(c$biopsia_total)
    })
    output$biopsia_sub <- renderText({
      if (!is_brazil()) return(txt_na_sub())
      cap_sub()
    })
    
    output$ezt_value <- renderText({
      if (!is_brazil()) return(txt_na_val())
      c <- realized_cap()
      if (is.null(c) || is.na(c$tratamento_total)) return(txt_na_val())
      fmt_int(c$tratamento_total)
    })
    output$ezt_sub <- renderText({
      if (!is_brazil()) return(txt_na_sub())
      cap_sub()
    })
    
    ######################Cards Comparativos###################################

    # Returns list(pct, pct_num, sub) from comp_dt row
    ratio_vals <- function(d_all, item_name) {
      na_result <- list(
        pct     = "\u2013",
        pct_num = NA_real_,
        sub     = "Data not available \u2013 only Brazil (SUS, 2024)"
      )
      if (is.null(d_all) || !nrow(d_all)) return(na_result)
      d <- d_all[item == item_name]
      if (!nrow(d)) return(na_result)
      d <- d[1]

      pct_num <- suppressWarnings(as.numeric(d$coverage_percent))
      if (!is.finite(pct_num)) pct_num <- NA_real_

      pct <- if (is.na(pct_num)) "\u2013" else paste0(fmt_rate(pct_num, 1), "%")

      actual_str   <- if (is.na(d$realized))         "\u2013" else fmt_int(d$realized)
      estimated_str <- if (is.na(d$needed))           "\u2013" else fmt_int(d$needed)
      sub <- paste0("Actual: ", actual_str, " \u00B7 Estimated: ", estimated_str)

      list(pct = pct, pct_num = pct_num, sub = sub)
    }

    # Renders the bar indicator div given a numeric percentage
    bar_indicator <- function(pct_num) {
      if (is.na(pct_num)) {
        return(tagList(
          div(class = "bar-outer",
              div(class = "bar-fill", style = "width:0%;")),
          div(class = "bar-labels", span("0%"), span("100%"))
        ))
      }
      is_over   <- pct_num >= 100
      fill_w    <- min(pct_num, 100)
      fill_cls  <- if (pct_num < 50) "bar-fill-low" else if (pct_num < 100) "bar-fill-mid" else "bar-fill-ok"
      br_r      <- if (is_over) "5px 0 0 5px" else "5px"

      overflow_el <- if (is_over) {
        div(class = "bar-overflow",
          div(class = "bar-overflow-arrow"),
          div(class = "bar-overflow-val", paste0(round(pct_num), "%"))
        )
      } else NULL

      tagList(
        div(
          class = "bar-outer",
          div(class = paste("bar-fill", fill_cls),
              style = paste0("width:", fill_w, "%; border-radius:", br_r, ";")),
          overflow_el
        ),
        div(class = "bar-labels", span("0%"), span("100%"))
      )
    }

    # Builds one white comparison card
    comp_card_ui <- function(title, rv, tooltip = NULL) {
      div(
        class = "comp-card",
        title = tooltip,
        div(class = "comp-title",  title),
        div(class = "comp-pct",    rv$pct),
        div(class = "comp-detail", HTML(rv$sub)),
        bar_indicator(rv$pct_num)
      )
    }

    output$cap_comp_cards <- renderUI({
      na_rv <- list(
        pct     = "\u2013",
        pct_num = NA_real_,
        sub     = "Data not available \u2013 only Brazil (SUS, 2024)"
      )

      if (!isTRUE(is_brazil())) {
        return(div(
          class = "comp-cards-row",
          comp_card_ui("Cytology",   na_rv, cc_TOOLTIPS$capacidade_ccu$comp_citologia),
          comp_card_ui("Colposcopy", na_rv, cc_TOOLTIPS$capacidade_ccu$comp_colposcopia),
          comp_card_ui("Biopsies",   na_rv, cc_TOOLTIPS$capacidade_ccu$comp_biopsia),
          comp_card_ui("EZT",        na_rv, cc_TOOLTIPS$capacidade_ccu$comp_ezt)
        ))
      }

      d_all <- comp_dt()
      cy <- ratio_vals(d_all, "citologia_total")
      co <- ratio_vals(d_all, "colposcopia_total")
      bi <- ratio_vals(d_all, "biopsia_total")
      ez <- ratio_vals(d_all, "tratamento_total")

      div(
        class = "comp-cards-row",
        comp_card_ui("Cytology",   cy, cc_TOOLTIPS$capacidade_ccu$comp_citologia),
        comp_card_ui("Colposcopy", co, cc_TOOLTIPS$capacidade_ccu$comp_colposcopia),
        comp_card_ui("Biopsies",   bi, cc_TOOLTIPS$capacidade_ccu$comp_biopsia),
        comp_card_ui("EZT",        ez, cc_TOOLTIPS$capacidade_ccu$comp_ezt)
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
        class = "cap-note",
        HTML(paste0(note_target, "<br/>", note_workup, "<br/>", note_trt))
      )
    })
  })
}