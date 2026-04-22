# ===========================================================
# 20_mod_tabela_detalhada.R
# Módulo "Detailed table" — necessidade estimada vs produção SIA 2024
# Disponível apenas para Brasil (pop_mode != "other")
# ===========================================================

# ── UI ──────────────────────────────────────────────────────────────────────

mod_detailed_table_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Header
    div(
      class = "cc-page-header",
      div(class = "cc-page-title", "Detailed table"),
      div(class = "cc-page-subtitle", textOutput(ns("geo_desc")))
    ),

    uiOutput(ns("body"))
  )
}

# ── Server ───────────────────────────────────────────────────────────────────

mod_detailed_table_server <- function(id,
                                      pop_mun_regional,
                                      sia_cc_resumo,
                                      regional_sus_map,
                                      dim_country,
                                      input_global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Brazil code ─────────────────────────────────────────────────────────
    br_code <- tryCatch({
      x <- dim_country[dim_country$population_name == "Brazil", "population_code"]
      if (length(x) == 0L || is.na(x[1])) NA_integer_ else as.integer(x[1])
    }, error = function(e) NA_integer_)

    # ── Availability checks ──────────────────────────────────────────────────
    is_brazil <- reactive({
      g <- input_global()
      !is.na(br_code) && isTRUE(as.integer(g$pais_sel) == br_code)
    })

    pop_is_manual <- reactive({
      g <- input_global()
      isTRUE(g$pop_mode == "other")
    })

    available <- reactive({
      is_brazil() && !pop_is_manual()
    })

    # ── geo_desc (header) ────────────────────────────────────────────────────
    output$geo_desc <- renderText({
      g <- input_global()
      if (!is_brazil()) {
        nm <- tryCatch({
          z <- dim_country[dim_country$population_code == as.integer(g$pais_sel), "population_name"]
          if (length(z) == 0L || is.na(z[1])) "Selected country" else as.character(z[1])
        }, error = function(e) "Selected country")
        return(nm)
      }
      parts <- "Brazil"
      add_if <- function(x) {
        x <- as.character(x[!is.na(x) & nzchar(x)])
        if (!length(x)) return(invisible(NULL))
        lab <- if (length(x) == 1L) x[1] else paste0(x[1], " (n=", length(x), ")")
        parts <<- paste(parts, lab, sep = " - ")
      }
      add_if(g$filt_uf)
      add_if(g$filt_macro)
      add_if(g$filt_reg)
      add_if(g$filt_mun)
      parts
    })

    # ── Effective granularity ────────────────────────────────────────────────
    # Returns one of: "uf", "macro", "reg", "mun"
    effective_gran <- reactive({
      if (!available()) return("mun")
      toggle <- input$granularity %||% "mun"
      if (toggle == "mun") return("mun")
      # auto: level below deepest active filter
      g <- input_global()
      has_mun   <- !is.null(g$filt_mun)   && length(g$filt_mun)   > 0
      has_reg   <- !is.null(g$filt_reg)   && length(g$filt_reg)   > 0
      has_macro <- !is.null(g$filt_macro) && length(g$filt_macro) > 0
      has_uf    <- !is.null(g$filt_uf)    && length(g$filt_uf)    > 0
      if (has_mun)                       return("mun")
      if (has_uf && has_macro && has_reg) return("mun")
      if (has_uf && has_macro)           return("reg")
      if (has_uf)                        return("macro")
      "uf"
    })

    # ── Per-capita from engine ───────────────────────────────────────────────
    per_capita <- reactive({
      if (!available()) return(NULL)
      g <- input_global()

      cfg_pc <- tryCatch(
        cc_engine_settings(
          country_code     = br_code,
          pop_mode         = "other",   # custom_pop = 1
          coverage         = g$coverage %||% 70,
          screen_method    = g$screen_method %||% "hpv",
          target_age_min   = g$target_age_min %||% 25,
          target_age_max   = g$target_age_max %||% 64,
          custom_pop       = 1,
          # HPV
          p16_18           = g$p16_18,
          poutros          = g$poutros,
          pneg             = g$pneg,
          cito_out_pos     = g$cito_out_pos,
          cito_out_neg     = g$cito_out_neg,
          colpo16_pos      = g$colpo16_pos,
          colpo16_neg      = g$colpo16_neg,
          colpoout_pos     = g$colpoout_pos,
          colpoout_neg     = g$colpoout_neg,
          b16_neg_nic1     = g$b16_neg_nic1,
          b16_nic23        = g$b16_nic23,
          b16_cancer       = g$b16_cancer,
          bo_neg_nic1      = g$bo_neg_nic1,
          bo_nic23         = g$bo_nic23,
          bo_cancer        = g$bo_cancer,
          # citologia
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
          is_brazil = FALSE
        ),
        error = function(e) NULL
      )
      if (is.null(cfg_pc)) return(NULL)

      res_pc <- tryCatch(
        cc_engine_run(NULL, cfg_pc, pop_mun_regional = NULL),
        error = function(e) NULL
      )
      if (is.null(res_pc)) return(NULL)

      m <- res_pc$metrics
      method <- res_pc$screen_method

      # primary tests per woman per year
      tests_pc <- if (identical(method, "hpv")) {
        as.numeric(m$rastreada[1])
      } else {
        r <- as.numeric(m$cit_rastreamento[1])
        d <- as.numeric(m$cit_diagnostica[1])
        (if (is.finite(r)) r else 0) + (if (is.finite(d)) d else 0)
      }

      list(
        method   = method,
        tests    = tests_pc,
        colpo    = as.numeric(m$colpo_indicada[1]),
        biopsia  = as.numeric(m$biopsia_indicada[1]),
        ezt      = as.numeric(m$ezt[1])
      )
    })

    # ── Pop aggregated ────────────────────────────────────────────────────────
    pop_agg <- reactive({
      if (!available()) return(NULL)
      g   <- input_global()
      gran <- effective_gran()

      age_min <- as.numeric(g$target_age_min %||% 25)
      age_max <- as.numeric(g$target_age_max %||% 64)

      dt <- data.table::as.data.table(pop_mun_regional)
      dt <- dt[from >= age_min & to <= age_max]

      # geographic filters
      if (!is.null(g$filt_uf)    && length(g$filt_uf))    dt <- dt[UF %in% g$filt_uf]
      if (!is.null(g$filt_macro) && length(g$filt_macro)) dt <- dt[`Macrorregiao de Saude` %in% g$filt_macro]
      if (!is.null(g$filt_reg)   && length(g$filt_reg))   dt <- dt[`Regiao de Saude` %in% g$filt_reg]
      if (!is.null(g$filt_mun)   && length(g$filt_mun))   dt <- dt[Municipio %in% g$filt_mun]

      if (!nrow(dt)) return(data.table::data.table())

      by_cols <- switch(gran,
        uf    = c("UF"),
        macro = c("UF", "Macrorregiao de Saude"),
        reg   = c("UF", "Macrorregiao de Saude", "Regiao de Saude"),
        mun   = c("UF", "Macrorregiao de Saude", "Regiao de Saude", "Municipio")
      )

      dt[, .(pop_total = sum(pop_total, na.rm = TRUE),
             pop_sus   = sum(pop_sus,   na.rm = TRUE)),
         by = by_cols]
    })

    # ── SIA aggregated ────────────────────────────────────────────────────────
    sia_agg <- reactive({
      if (!available()) return(NULL)
      g    <- input_global()
      gran <- effective_gran()

      geo_ref_sel <- g$sia_geo_ref %||% "care"
      if (!geo_ref_sel %in% c("care", "res")) geo_ref_sel <- "care"

      dt <- data.table::as.data.table(sia_cc_resumo)
      dt <- dt[geo_ref == geo_ref_sel]

      if (!is.null(g$filt_uf)    && length(g$filt_uf))    dt <- dt[UF %in% g$filt_uf]
      if (!is.null(g$filt_macro) && length(g$filt_macro)) dt <- dt[`Macrorregiao de Saude` %in% g$filt_macro]
      if (!is.null(g$filt_reg)   && length(g$filt_reg))   dt <- dt[`Regiao de Saude` %in% g$filt_reg]
      if (!is.null(g$filt_mun)   && length(g$filt_mun))   dt <- dt[Municipio %in% g$filt_mun]

      if (!nrow(dt)) return(data.table::data.table())

      by_cols <- switch(gran,
        uf    = c("UF"),
        macro = c("UF", "Macrorregiao de Saude"),
        reg   = c("UF", "Macrorregiao de Saude", "Regiao de Saude"),
        mun   = c("UF", "Macrorregiao de Saude", "Regiao de Saude", "Municipio")
      )

      cats <- c("citologia", "colposcopia", "biopsia", "tratamento")
      dt_filt <- dt[categoria %in% cats]

      # sum by geo group + categoria
      dt_long <- dt_filt[, .(total = sum(total_all, na.rm = TRUE)),
                         by = c(by_cols, "categoria")]

      # pivot wide (backtick-quote cols with spaces so as.formula() parses correctly)
      lhs <- paste(
        sapply(by_cols, function(x) if (grepl(" ", x, fixed = TRUE)) paste0("`", x, "`") else x),
        collapse = "+"
      )
      dt_wide <- data.table::dcast(dt_long,
                                   formula   = as.formula(paste(lhs, "~ categoria")),
                                   value.var = "total",
                                   fill      = 0)

      # ensure all category columns exist
      for (cat in cats) {
        if (!cat %in% names(dt_wide)) dt_wide[, (cat) := 0L]
      }

      dt_wide[]
    })

    # ── Join and compute table ────────────────────────────────────────────────
    tabela_base <- reactive({
      if (!available()) return(NULL)
      g   <- input_global()
      gran <- effective_gran()
      pc  <- per_capita()
      if (is.null(pc)) return(NULL)

      dt_pop <- pop_agg()
      dt_sia <- sia_agg()
      if (is.null(dt_pop) || !nrow(dt_pop)) return(data.table::data.table())

      by_cols <- switch(gran,
        uf    = c("UF"),
        macro = c("UF", "Macrorregiao de Saude"),
        reg   = c("UF", "Macrorregiao de Saude", "Regiao de Saude"),
        mun   = c("UF", "Macrorregiao de Saude", "Regiao de Saude", "Municipio")
      )

      # population column to use for needs
      pop_alvo_col <- if (isTRUE(g$br_pop_tipo == "sus")) "pop_sus" else "pop_total"

      # join SIA into pop
      if (!is.null(dt_sia) && nrow(dt_sia)) {
        dt <- merge(dt_pop, dt_sia, by = by_cols, all.x = TRUE)
      } else {
        dt <- data.table::copy(dt_pop)
        dt[, `:=`(citologia = NA_real_, colposcopia = NA_real_,
                  biopsia = NA_real_, tratamento = NA_real_)]
      }

      # fill NAs from SIA with 0
      for (col in c("citologia", "colposcopia", "biopsia", "tratamento")) {
        if (col %in% names(dt)) {
          dt[is.na(get(col)), (col) := 0]
        } else {
          dt[, (col) := 0]
        }
      }

      # needs (per-capita × pop_alvo)
      dt[, `:=`(
        tests_needed   = round(get(pop_alvo_col) * pc$tests),
        colpo_needed   = round(get(pop_alvo_col) * pc$colpo),
        biopsia_needed = round(get(pop_alvo_col) * pc$biopsia),
        ezt_needed     = round(get(pop_alvo_col) * pc$ezt)
      )]

      # gaps (%)
      safe_pct <- function(num, den) {
        data.table::fifelse(
          !is.na(den) & den > 0,
          100 * num / den,
          NA_real_
        )
      }

      dt[, `:=`(
        cov_cito  = safe_pct(citologia,   tests_needed),
        cov_colpo = safe_pct(colposcopia, colpo_needed),
        cov_biop  = safe_pct(biopsia,     biopsia_needed),
        cov_ezt   = safe_pct(tratamento,  ezt_needed)
      )]

      dt[]
    })

    # ── Params line text ─────────────────────────────────────────────────────
    params_line <- reactive({
      g  <- input_global()
      pc <- per_capita()
      method_lbl <- if (!is.null(pc) && pc$method == "cytology") "Cytology" else "HPV"
      preset_lbl <- as.character(g$hpv_param_source %||% "Default")
      if (exists("HPV_PRESETS") && preset_lbl %in% names(HPV_PRESETS))
        preset_lbl <- HPV_PRESETS[[preset_lbl]]$label %||% preset_lbl

      age_min <- g$target_age_min %||% 25
      age_max <- g$target_age_max %||% 64
      cov     <- g$coverage %||% 70

      paste0(
        "Method: ", method_lbl,
        "  \u00B7  Ages ", age_min, "\u2013", age_max,
        "  \u00B7  Coverage ", cov, "%",
        "  \u00B7  Preset: ", preset_lbl
      )
    })

    # ── Column names for display ──────────────────────────────────────────────
    col_labels <- function(gran, method) {
      test_lbl <- if (identical(method, "hpv")) "HPV tests needed" else "Pap smears needed"
      id_cols <- switch(gran,
        uf    = c("UF"),
        macro = c("Macro-region", "UF"),
        reg   = c("Health region", "Macro-region", "UF"),
        mun   = c("Municipality", "Health region", "Macro-region", "UF")
      )
      c(id_cols,
        "Female pop (total)", "Female pop (SUS-dep.)",
        test_lbl, "Colposcopies needed", "Biopsies needed", "EZTs needed",
        "Cytologies produced", "Colposcopies produced", "Biopsies produced", "EZTs produced",
        "Cov. — cytology/primary (%)", "Cov. — colposcopy (%)",
        "Cov. — biopsy (%)", "Cov. — EZT (%)")
    }

    # Maps data.table column names to display order/renaming
    build_display_dt <- function(dt, gran, method) {
      if (is.null(dt) || !nrow(dt)) return(dt)

      geo_cols <- switch(gran,
        uf    = c("UF"),
        macro = c("Macrorregiao de Saude", "UF"),
        reg   = c("Regiao de Saude", "Macrorregiao de Saude", "UF"),
        mun   = c("Municipio", "Regiao de Saude", "Macrorregiao de Saude", "UF")
      )

      data_cols <- c("pop_total", "pop_sus",
                     "tests_needed", "colpo_needed", "biopsia_needed", "ezt_needed",
                     "citologia", "colposcopia", "biopsia", "tratamento",
                     "cov_cito", "cov_colpo", "cov_biop", "cov_ezt")

      all_cols <- c(geo_cols, data_cols)
      # keep only existing columns
      all_cols <- intersect(all_cols, names(dt))
      dt_out <- dt[, ..all_cols]

      # rename
      test_lbl <- if (identical(method, "hpv")) "HPV tests needed" else "Pap smears needed"
      rename_map <- c(
        "UF"                        = "UF",
        "Macrorregiao de Saude"     = "Macro-region",
        "Regiao de Saude"           = "Health region",
        "Municipio"                 = "Municipality",
        "pop_total"                 = "Female pop (total)",
        "pop_sus"                   = "Female pop (SUS-dep.)",
        "tests_needed"              = test_lbl,
        "colpo_needed"              = "Colposcopies needed",
        "biopsia_needed"            = "Biopsies needed",
        "ezt_needed"                = "EZTs needed",
        "citologia"                 = "Cytologies produced",
        "colposcopia"               = "Colposcopies produced",
        "biopsia"                   = "Biopsies produced",
        "tratamento"                = "EZTs produced",
        "cov_cito"                  = "Cov. cytology/primary (%)",
        "cov_colpo"                 = "Cov. colposcopy (%)",
        "cov_biop"                  = "Cov. biopsy (%)",
        "cov_ezt"                   = "Cov. EZT (%)"
      )
      new_names <- rename_map[names(dt_out)]
      new_names[is.na(new_names)] <- names(dt_out)[is.na(new_names)]
      data.table::setnames(dt_out, old = names(dt_out), new = as.character(new_names))

      dt_out
    }

    # ── Body UI (conditional availability) ───────────────────────────────────
    output$body <- renderUI({
      if (!is_brazil()) {
        return(div(
          class = "cc-kpi-card",
          style = "color:#666; font-size:14px; padding:20px;",
          "This view is only available for Brazil."
        ))
      }
      if (pop_is_manual()) {
        return(div(
          class = "cc-kpi-card",
          style = "color:#666; font-size:14px; padding:20px;",
          "Detailed table is only available with IBGE population (total or SUS-dependent).",
          " Manual population cannot be distributed across municipalities."
        ))
      }

      tagList(
        # Granularity toggle
        div(
          style = "margin-bottom:12px;",
          radioButtons(
            ns("granularity"),
            label    = NULL,
            choices  = c("By municipality"             = "mun",
                         "By smallest selected level"  = "auto"),
            selected = "mun",
            inline   = TRUE
          )
        ),

        # Parameters line + Download button
        div(
          style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:10px;",
          div(
            style = "font-size:12px; color:#888; font-style:italic;",
            textOutput(ns("params_text"), inline = TRUE)
          ),
          downloadButton(
            ns("download_csv"),
            label = "Download CSV",
            style = "font-size:12px; padding:4px 12px; background:#0B7285; color:#fff; border:none; border-radius:5px; cursor:pointer;"
          )
        ),

        # Table
        DT::DTOutput(ns("table")),

        # Footer note
        div(
          class = "cap-note",
          style = "margin-top:12px;",
          HTML(paste0(
            "SIA production shows actual SUS procedures (mostly cytology). ",
            "When HPV is selected, the gap reflects the shift from cytology-based ",
            "to HPV-based screening."
          ))
        )
      )
    })

    output$params_text <- renderText({
      req(available())
      params_line()
    })

    # ── DT table ─────────────────────────────────────────────────────────────
    output$table <- DT::renderDT({
      req(available())
      dt <- tabela_base()
      pc <- per_capita()
      if (is.null(dt) || !nrow(dt)) {
        return(DT::datatable(data.frame(Message = "No municipalities match the current filters."),
                             rownames = FALSE, options = list(dom = "t")))
      }

      gran   <- effective_gran()
      method <- if (!is.null(pc)) pc$method else "hpv"
      dt_out <- build_display_dt(dt, gran, method)

      # find gap column indices (1-based) in dt_out
      gap_cols <- grep("Cov\\.", names(dt_out))
      # round gap columns
      for (j in gap_cols) {
        set(dt_out, j = j, value = round(dt_out[[j]], 1))
      }

      # sort order column: first gap (cytology/primary)
      sort_col_idx <- if (length(gap_cols) > 0L) gap_cols[1] - 1L else 0L  # 0-based for DT

      tbl <- DT::datatable(
        dt_out,
        rownames = FALSE,
        filter   = "top",
        options  = list(
          pageLength = 25,
          dom        = "frtip",
          scrollX    = TRUE,
          order      = list(list(sort_col_idx, "asc"))
        )
      )

      # format integer columns (population + needs + production)
      int_cols <- grep("Female pop|needed|produced", names(dt_out), value = TRUE)
      if (length(int_cols)) {
        tbl <- DT::formatCurrency(tbl, columns = int_cols,
                                  currency = "", digits = 0,
                                  mark = ",", before = FALSE)
      }

      # color formatting for gap columns
      if (length(gap_cols)) {
        gap_col_names <- names(dt_out)[gap_cols]
        tbl <- DT::formatStyle(
          tbl,
          columns         = gap_col_names,
          backgroundColor = DT::styleInterval(
            cuts   = c(50, 100),
            values = c("#D9534F", "#E8A838", "#2E7D52")
          )
        )
      }

      tbl
    })

    # ── Download CSV ─────────────────────────────────────────────────────────
    output$download_csv <- downloadHandler(
      filename = function() {
        g    <- input_global()
        meth <- g$screen_method %||% "hpv"
        gran <- effective_gran()
        paste0("detailed_table_", meth, "_", gran, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        dt <- tabela_base()
        pc <- per_capita()
        if (is.null(dt) || !nrow(dt)) {
          data.table::fwrite(data.table::data.table(Message = "No data"),
                             file, sep = ";", dec = ",", bom = TRUE)
          return(invisible(NULL))
        }
        gran   <- effective_gran()
        method <- if (!is.null(pc)) pc$method else "hpv"
        dt_out <- build_display_dt(dt, gran, method)
        data.table::fwrite(dt_out, file, sep = ";", dec = ",", bom = TRUE)
      }
    )
  })
}
