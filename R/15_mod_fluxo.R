# ===========================================================
# Shiny-cc — 15_mod_fluxo.R
# Screening & diagnostic pathway diagram with counts (%)
# ===========================================================

mod_fluxo_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "pathway.css"),
      tags$script(HTML("
(function() {
  function scalePathway() {
    document.querySelectorAll('.pathway-scalable').forEach(function(el) {
      var w = el.parentElement ? el.parentElement.clientWidth : 0;
      if (w <= 0) return;
      var scale = Math.min(1, w / 1200);
      el.style.transform = 'scale(' + scale + ')';
      el.parentElement.style.height = Math.ceil(780 * scale) + 'px';
    });
  }
  window.addEventListener('resize', scalePathway);
  var obs = new MutationObserver(scalePathway);
  document.addEventListener('DOMContentLoaded', function() {
    obs.observe(document.body, { childList: true, subtree: true });
    scalePathway();
  });
})();
      "))
    ),
    div(
      class = "cc-page-header",
      div(class = "cc-page-title", "Pathway")
    ),
    uiOutput(ns("pathway_ui"))
  )
}

mod_fluxo_server <- function(id,
                             df_completo,
                             dim_age,
                             dim_country,
                             input_global,
                             pop_mun_regional) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    geo_label <- reactive({
      g <- input_global()

      if (!isTRUE(is_brazil())) {
        return(country_label(country_code()))
      }

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

    # ---------- Números do fluxo (engine) ----------------------------
    fluxo <- reactive({
      res <- engine_res()
      if (is.null(res)) return(NULL)

      method <- as.character(cfg_reactive()$screen_method %||% input_global()$screen_method %||% "hpv")

      m <- res$metrics
      data.table::setDT(m)

      p2 <- function(x, def = NA_real_) {
        x <- suppressWarnings(as.numeric(x))
        if (!is.finite(x) || is.na(x)) x <- def
        if (!is.finite(x) || is.na(x)) return(NA_real_)
        x / 100
      }

      g <- input_global()

      # ── Cytology pathway ──────────────────────────────────────────
      if (identical(method, "cytology")) {
        N <- suppressWarnings(as.numeric(m$cit_rastreamento[1]))
        if (!is.finite(N) || is.na(N) || N <= 0) {
          return(list(method = method, women_screened = NA_real_))
        }

        p_asch  <- p2(g$res_asch_pct)
        p_other <- p2(g$res_other_pct)

        n_asch  <- N * p_asch
        n_other <- N * p_other
        n_neg   <- pmax(N - n_asch - n_other, 0)

        n_diag_tot <- n_other
        p_diag_pos <- p2(g$colpo_other_follow_pct)
        n_diag_pos <- n_diag_tot * p_diag_pos
        n_diag_neg <- pmax(n_diag_tot - n_diag_pos, 0)

        p_col_asch <- p2(g$colpo_asch_pct)
        n_col_asch <- n_asch * p_col_asch

        n_col_other <- n_diag_pos

        p_bpos_asch  <- p2(g$biopsy_pos_asch_pct)
        p_bpos_other <- p2(g$biopsy_pos_other_pct)

        n_b_asch  <- n_col_asch  * p_bpos_asch
        n_b_other <- n_col_other * p_bpos_other

        n_colneg_asch  <- pmax(n_col_asch  - n_b_asch,  0)
        n_colneg_other <- pmax(n_col_other - n_b_other, 0)

        p_asch_neg1  <- p2(g$b_asch_neg_nic1_pct)
        p_asch_nic23 <- p2(g$b_asch_nic23_pct)
        p_asch_can   <- p2(g$b_asch_cancer_pct)

        p_oth_neg1   <- p2(g$b_other_neg_nic1_pct)
        p_oth_nic23  <- p2(g$b_other_nic23_pct)
        p_oth_can    <- p2(g$b_other_cancer_pct)

        return(list(
          method = method,
          women_screened = N,

          negative = n_neg,
          asch     = n_asch,
          other    = n_other,

          diag_tot = n_diag_tot,
          diag_pos = n_diag_pos,
          diag_neg = n_diag_neg,

          colpo_asch_pos = n_b_asch,
          colpo_asch_neg = n_colneg_asch,

          colpo_other_pos = n_b_other,
          colpo_other_neg = n_colneg_other,

          biopsy_neg_cin1_asch = n_b_asch  * p_asch_neg1,
          cin2_3_asch          = n_b_asch  * p_asch_nic23,
          cancer_asch          = n_b_asch  * p_asch_can,

          biopsy_neg_cin1_other = n_b_other * p_oth_neg1,
          cin2_3_other          = n_b_other * p_oth_nic23,
          cancer_other          = n_b_other * p_oth_can
        ))
      }

      # ── HPV pathway ───────────────────────────────────────────────
      if (!identical(method, "hpv")) {
        return(list(method = method))
      }

      N <- suppressWarnings(as.numeric(m$rastreada[1]))
      if (!is.finite(N) || is.na(N) || N <= 0) {
        return(list(method = method, women_screened = NA_real_))
      }

      p16  <- p2(g$p16_18)
      pout <- p2(g$poutros)
      pneg <- p2(g$pneg)

      tot <- p16 + pout + pneg
      if (!is.finite(tot) || is.na(tot) || tot <= 0) tot <- 1
      w16  <- p16 / tot
      wout <- pout / tot
      wneg <- pneg / tot

      n_neg <- N * wneg
      n16   <- N * w16
      nout  <- N * wout

      cito_out_pos <- p2(g$cito_out_pos)
      cito_out_neg <- p2(g$cito_out_neg)

      n_cito_pos <- nout * cito_out_pos
      n_cito_neg <- nout * cito_out_neg

      colpo16_pos  <- p2(g$colpo16_pos)
      colpoout_pos <- p2(g$colpoout_pos)

      n_colpo_pos_16  <- n16        * colpo16_pos
      n_colpo_pos_out <- n_cito_pos * colpoout_pos

      n_colpo_neg_16  <- pmax(n16        - n_colpo_pos_16,  0)
      n_colpo_neg_out <- pmax(n_cito_pos - n_colpo_pos_out, 0)

      b16_neg_nic1 <- p2(g$b16_neg_nic1)
      b16_nic23    <- p2(g$b16_nic23)
      b16_cancer   <- p2(g$b16_cancer)

      bo_neg_nic1 <- p2(g$bo_neg_nic1)
      bo_nic23    <- p2(g$bo_nic23)
      bo_cancer   <- p2(g$bo_cancer)

      list(
        method = method,

        women_screened = N,
        negative       = n_neg,
        hpv16_18       = n16,
        other_hrhpv    = nout,

        cytology_pos = n_cito_pos,
        cytology_neg = n_cito_neg,

        colpo_pos_16  = n_colpo_pos_16,
        colpo_neg_16  = n_colpo_neg_16,
        colpo_pos_out = n_colpo_pos_out,
        colpo_neg_out = n_colpo_neg_out,

        biopsy_neg_cin1_16  = n_colpo_pos_16  * b16_neg_nic1,
        cin2_3_16           = n_colpo_pos_16  * b16_nic23,
        cancer_16           = n_colpo_pos_16  * b16_cancer,

        biopsy_neg_cin1_out = n_colpo_pos_out * bo_neg_nic1,
        cin2_3_out          = n_colpo_pos_out * bo_nic23,
        cancer_out          = n_colpo_pos_out * bo_cancer
      )
    })

    # ---------- Caption text -----------------------------------------
    caption_txt <- reactive({
      g   <- input_global()
      cfg <- cfg_reactive()

      method_lbl <- if (identical(cfg$screen_method, "cytology"))
        "Cytology screening"
      else
        "HPV screening"

      interval_lbl <- if (identical(cfg$screen_method, "cytology"))
        "every 3 years"
      else
        "every 5 years"

      preset_lbl <- if (!is.null(g$hpv_param_source) && nzchar(g$hpv_param_source))
        paste0(" HPV parameters: ", g$hpv_param_source, ".")
      else
        ""

      age_min <- cfg$target_age_min %||% 25
      age_max <- cfg$target_age_max %||% 64
      cov     <- cfg$coverage %||% 70

      paste0(
        "Method: ", method_lbl, " ", interval_lbl, ".",
        " Age range: ", age_min, "\u2013", age_max, " years.",
        " Coverage: ", cov, "%.",
        " Population source: World Population Prospects 2025.",
        preset_lbl
      )
    })

    # ---------- Render -----------------------------------------------
    output$pathway_ui <- renderUI({
      f <- fluxo()

      if (is.null(f) || is.na(f$women_screened) || f$women_screened <= 0) {
        return(tags$p(
          style = "color:#6b8885; margin-top:40px; text-align:center;",
          "Flow not available for current settings."
        ))
      }

      if (!f$method %in% c("hpv", "cytology")) {
        return(tags$p(
          style = "color:#6b8885; margin-top:40px; text-align:center;",
          "Flow not available for current settings."
        ))
      }

      geo  <- geo_label()
      cap  <- caption_txt()

      if (identical(f$method, "hpv")) {
        render_pathway_hpv(f, geo, cap)
      } else {
        render_pathway_cytology(f, geo, cap)
      }
    })

  })
}
