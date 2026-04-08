# ===========================================================
# Shiny-cc — 15_mod_fluxo.R
# Screening & diagnostic pathway diagram with counts (%)
# ===========================================================

mod_fluxo_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Screening and diagnostic pathway"),
    h4(textOutput(ns("geo_desc"))),
    tags$div(
      style = "height: calc(100vh - 260px); min-height: 520px;",
      DiagrammeR::grVizOutput(ns("plot_flow"), width = "100%", height = "100%")
    )
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
    
    # ---------- Números do fluxo (OMS/WHO apenas) ----------------
    fluxo <- reactive({
      res <- engine_res()
      if (is.null(res)) return(NULL)
      
      # método
      method <- as.character(cfg_reactive()$screen_method %||% input_global()$screen_method %||% "hpv")
      
      m <- res$metrics
      data.table::setDT(m)
      
      # helpers (percent -> proportion)
      p2 <- function(x, def = NA_real_) {
        x <- suppressWarnings(as.numeric(x))
        if (!is.finite(x) || is.na(x)) x <- def
        if (!is.finite(x) || is.na(x)) return(NA_real_)
        x / 100
      }
      
      g <- input_global()
      
      # -------------------------
      # Cytology (simple pathway)
      # -------------------------
      if (identical(method, "cytology")) {
        N <- suppressWarnings(as.numeric(m$cit_rastreamento[1]))
        if (!is.finite(N) || is.na(N) || N <= 0) {
          return(list(method = method, women_screened = NA_real_))
        }
        
        p_asch  <- p2(g$res_asch_pct)
        p_other <- p2(g$res_other_pct)
        p_neg   <- p2(g$res_neg_pct)
        
        n_asch  <- N * p_asch
        n_other <- N * p_other
        n_neg   <- pmax(N - n_asch - n_other, 0)
        
        # Diagnostic cytology after "Other abnormalities"
        # (positive proportion = colposcopy referral after other abnormalities)
        n_diag_tot <- n_other
        p_diag_pos <- p2(g$colpo_other_follow_pct)
        n_diag_pos <- n_diag_tot * p_diag_pos
        n_diag_neg <- pmax(n_diag_tot - n_diag_pos, 0)
        
        # Colposcopy
        p_col_asch <- p2(g$colpo_asch_pct)
        n_col_asch <- n_asch * p_col_asch
        
        n_col_other <- n_diag_pos
        
        # Colposcopy positivity (biopsy indication)
        p_bpos_asch  <- p2(g$biopsy_pos_asch_pct)
        p_bpos_other <- p2(g$biopsy_pos_other_pct)
        
        n_b_asch  <- n_col_asch  * p_bpos_asch
        n_b_other <- n_col_other * p_bpos_other
        
        n_colneg_asch  <- pmax(n_col_asch  - n_b_asch,  0)
        n_colneg_other <- pmax(n_col_other - n_b_other, 0)
        
        # Biopsy outcomes (shares among biopsy indicated)
        p_asch_neg1  <- p2(g$b_asch_neg_nic1_pct)
        p_asch_nic23 <- p2(g$b_asch_nic23_pct)
        p_asch_can   <- p2(g$b_asch_cancer_pct)
        
        p_oth_neg1   <- p2(g$b_other_neg_nic1_pct)
        p_oth_nic23  <- p2(g$b_other_nic23_pct)
        p_oth_can    <- p2(g$b_other_cancer_pct)
        
        bneg_asch <- n_b_asch  * p_asch_neg1
        nic_asch  <- n_b_asch  * p_asch_nic23
        can_asch  <- n_b_asch  * p_asch_can
        
        bneg_oth  <- n_b_other * p_oth_neg1
        nic_oth   <- n_b_other * p_oth_nic23
        can_oth   <- n_b_other * p_oth_can
        
        return(list(
          method = method,
          women_screened = N,
          
          negative = n_neg,
          asch = n_asch,
          other = n_other,
          
          diag_tot = n_diag_tot,
          diag_pos = n_diag_pos,
          diag_neg = n_diag_neg,
          
          colpo_asch_pos = n_b_asch,
          colpo_asch_neg = n_colneg_asch,
          
          colpo_other_pos = n_b_other,
          colpo_other_neg = n_colneg_other,
          
          biopsy_neg_cin1_asch = bneg_asch,
          cin2_3_asch          = nic_asch,
          cancer_asch          = can_asch,
          
          biopsy_neg_cin1_other = bneg_oth,
          cin2_3_other          = nic_oth,
          cancer_other          = can_oth
        ))
      }
      
      # ------------
      # HPV pathway
      # ------------
      if (!identical(method, "hpv")) {
        return(list(method = method))
      }
      
      N <- suppressWarnings(as.numeric(m$rastreada[1]))
      if (!is.finite(N) || is.na(N) || N <= 0) {
        return(list(method = method, women_screened = NA_real_))
      }
      
      
      # helpers (percent -> proportion)
      p2 <- function(x, def = NA_real_) {
        x <- suppressWarnings(as.numeric(x))
        if (!is.finite(x) || is.na(x)) x <- def
        if (!is.finite(x) || is.na(x)) return(NA_real_)
        x / 100
      }
      
      g <- input_global()
      
      # HPV distribution
      p16   <- p2(g$p16_18)
      pout  <- p2(g$poutros)
      pneg  <- p2(g$pneg)
      
      tot <- p16 + pout + pneg
      if (!is.finite(tot) || is.na(tot) || tot <= 0) tot <- 1
      w16   <- p16 / tot
      wout  <- pout / tot
      wneg  <- pneg / tot
      
      n_neg <- N * wneg
      n16   <- N * w16
      nout  <- N * wout
      
      # Reflex cytology (other HR-HPV only)
      cito_out_pos <- p2(g$cito_out_pos)
      cito_out_neg <- p2(g$cito_out_neg)
      
      n_cito_pos <- nout * cito_out_pos
      n_cito_neg <- nout * cito_out_neg
      
      # Colposcopy positivity
      colpo16_pos  <- p2(g$colpo16_pos)
      colpoout_pos <- p2(g$colpoout_pos)
      
      n_colpo_pos_16   <- n16   * colpo16_pos
      n_colpo_pos_out  <- n_cito_pos * colpoout_pos
      
      n_colpo_ind_16   <- n16
      n_colpo_ind_out  <- n_cito_pos
      
      n_colpo_neg_16   <- pmax(n_colpo_ind_16  - n_colpo_pos_16,  0)
      n_colpo_neg_out  <- pmax(n_colpo_ind_out - n_colpo_pos_out, 0)
      
      n_colpo_pos_total <- n_colpo_pos_16 + n_colpo_pos_out
      n_colpo_neg_total <- n_colpo_neg_16 + n_colpo_neg_out
      
      # Biopsy outcomes (shares among colpo+)
      b16_neg_nic1 <- p2(g$b16_neg_nic1)
      b16_nic23    <- p2(g$b16_nic23)
      b16_cancer   <- p2(g$b16_cancer)
      
      bo_neg_nic1  <- p2(g$bo_neg_nic1)
      bo_nic23     <- p2(g$bo_nic23)
      bo_cancer    <- p2(g$bo_cancer)
      
      # Branch 16/18 (biopsy = colpo+ in 16/18 arm)
      biop_16 <- n_colpo_pos_16
      bneg_16 <- biop_16 * b16_neg_nic1
      nic23_16 <- biop_16 * b16_nic23
      can_16  <- biop_16 * b16_cancer
      
      # Branch other HR-HPV (biopsy = colpo+ in other arm)
      biop_out <- n_colpo_pos_out
      bneg_out <- biop_out * bo_neg_nic1
      nic23_out <- biop_out * bo_nic23
      can_out  <- biop_out * bo_cancer
      
      list(
        method = method,
        
        women_screened = N,
        negative = n_neg,
        
        hpv16_18 = n16,
        other_hrhpv = nout,
        
        cytology_pos = n_cito_pos,
        cytology_neg = n_cito_neg,
        
        colpo_pos_16 = n_colpo_pos_16,
        colpo_neg_16 = n_colpo_neg_16,
        colpo_pos_out = n_colpo_pos_out,
        colpo_neg_out = n_colpo_neg_out,
        
        colpo_pos_total = n_colpo_pos_total,
        colpo_neg_total = n_colpo_neg_total,
        
        biopsy_neg_cin1_16 = bneg_16,
        cin2_3_16 = nic23_16,
        cancer_16 = can_16,
        
        biopsy_neg_cin1_out = bneg_out,
        cin2_3_out = nic23_out,
        cancer_out = can_out
      )
    })
    
      
    
    # ---------- Diagrama (DiagrammeR / Graphviz) ----------------------
    output$plot_flow <- DiagrammeR::renderGrViz({
      f <- fluxo()
      if (is.null(f) || is.na(f$women_screened) || f$women_screened <= 0) {
        return(DiagrammeR::grViz("
digraph x {
graph [layout = dot, rankdir = LR]
node [shape=box]
a [label='Flow not available for current settings.']
}
"))
      }
      
      if (!identical(f$method, "hpv") && !identical(f$method, "cytology")) {
        return(DiagrammeR::grViz("
digraph x {
graph [layout = dot, rankdir = LR]
node [shape=box]
a [label='Flow not available for current settings.']
}
"))
      }
    
    if (identical(f$method, "cytology")) {
      N <- as.numeric(f$women_screened)
      
      pct <- function(x, denom) {
        x <- suppressWarnings(as.numeric(x)); denom <- suppressWarnings(as.numeric(denom))
        if (!is.finite(x) || is.na(x) || !is.finite(denom) || is.na(denom) || denom <= 0) return(NA_real_)
        round(100 * x / denom, 1)
      }
      fmtN <- function(x) {
        x <- suppressWarnings(as.numeric(x))
        if (!is.finite(x) || is.na(x)) return("NA")
        fmt_int(round(x))
      }
      lbl_pct <- function(title, n, p) {
        ptxt <- if (is.na(p)) "(–)" else paste0("(", p, "%)")
        paste0(title, "\\n", fmtN(n), "\\n", ptxt)
      }
      lbl <- function(title, n) paste0(title, "\\n", fmtN(n))
      
      # --- cores (paleta CCU) ---
      bg         <- CC_COLORS$primary
      edge_col   <- CC_COLORS$white
      node_fill  <- CC_COLORS$white
      text_col   <- CC_COLORS$text
      
      # --- percentuais ---
      p_neg   <- pct(f$negative, N)
      p_asch  <- pct(f$asch, N)
      p_oth   <- pct(f$other, N)
      
      p_dpos  <- pct(f$diag_pos, f$diag_tot)
      p_dneg  <- pct(f$diag_neg, f$diag_tot)
      
      p_col_asch_pos <- pct(f$colpo_asch_pos, f$asch)
      p_col_asch_neg <- pct(f$colpo_asch_neg, f$asch)
      
      p_col_oth_pos  <- pct(f$colpo_other_pos, f$diag_pos)
      p_col_oth_neg  <- pct(f$colpo_other_neg, f$diag_pos)
      
      p_bneg_asch <- pct(f$biopsy_neg_cin1_asch, f$colpo_asch_pos)
      p_cin_asch  <- pct(f$cin2_3_asch,          f$colpo_asch_pos)
      p_can_asch  <- pct(f$cancer_asch,          f$colpo_asch_pos)
      
      p_bneg_oth  <- pct(f$biopsy_neg_cin1_other, f$colpo_other_pos)
      p_cin_oth   <- pct(f$cin2_3_other,          f$colpo_other_pos)
      p_can_oth   <- pct(f$cancer_other,          f$colpo_other_pos)
      
      dot <- paste0(
        "digraph flow {",
        "graph [layout=neato, overlap=true, splines=ortho, bgcolor='", bg, "', pad='0.30', nodesep='0.45', ranksep='0.60'];",
        "node  [shape=box, style='rounded,filled', penwidth=1.6, fontname='Helvetica', fontsize=11, margin='0.18,0.12', pin=true, fixedsize=true, width=2.15, height=0.95, fillcolor='", node_fill, "', color='", edge_col, "', fontcolor='", text_col, "'];",
        "edge  [color='", edge_col, "', penwidth=1.5, arrowsize=0.8, fontname='Helvetica', fontsize=10, fontcolor='", edge_col, "'];",
        
        # --- NÓS ---
        "scr [label='", lbl("Screening cytologies", N), "', pos='0,0!', width=2.55];",
        
        "neg  [label='", lbl_pct("Negative", f$negative, p_neg), "', pos='0,-2.3!'];",
        "asch [label='", lbl_pct("HSIL / ASC-H / AOI / AIS / CarcinomaHSIL / ASC-H / AOI / AIS / Carcinoma", f$asch, p_asch), "', pos='3.8,1.9!'];",
        "oth  [label='", lbl_pct("Other abnormalities", f$other, p_oth), "', pos='3.8,-1.9!', width=2.75];",
        
        "dpos [label='", lbl_pct("Diagnostic cytology +", f$diag_pos, p_dpos), "', pos='7.9,-1.2!', width=2.85];",
        "dneg [label='", lbl_pct("Diagnostic cytology -", f$diag_neg, p_dneg), "', pos='7.9,-2.7!', width=2.85];",
        
        "cap  [label='", lbl_pct("Colposcopy +", f$colpo_asch_pos, p_col_asch_pos), "', pos='12.2,1.2!'];",
        "can  [label='", lbl_pct("Colposcopy -", f$colpo_asch_neg, p_col_asch_neg), "', pos='12.2,2.7!'];",
        
        "cop  [label='", lbl_pct("Colposcopy +", f$colpo_other_pos, p_col_oth_pos), "', pos='12.2,-1.2!'];",
        "con  [label='", lbl_pct("Colposcopy -", f$colpo_other_neg, p_col_oth_neg), "', pos='12.2,-2.7!'];",
        
        "bna [label='", lbl_pct("NIC1/negative", f$biopsy_neg_cin1_asch, p_bneg_asch), "', pos='16.4,2.9!'];",
        "cna [label='", lbl_pct("CIN2/3", f$cin2_3_asch, p_cin_asch), "', pos='16.4,1.7!'];",
        "kna [label='", lbl_pct("Cancer", f$cancer_asch, p_can_asch), "', pos='16.4,0.5!'];",
        
        "bno [label='", lbl_pct("NIC1/negative", f$biopsy_neg_cin1_other, p_bneg_oth), "', pos='16.4,-0.5!'];",
        "cno [label='", lbl_pct("CIN2/3", f$cin2_3_other, p_cin_oth), "', pos='16.4,-1.7!'];",
        "kno [label='", lbl_pct("Cancer", f$cancer_other, p_can_oth), "', pos='16.4,-2.9!'];",
        
        # --- ARESTAS ---
        "scr:d -> neg:u;",
        "scr:e -> asch:w;",
        "scr:e -> oth:w;",
        
        "oth:e -> dpos:w;",
        "oth:e -> dneg:w;",
        
        "asch:e -> cap:w;",
        "asch:e -> can:w;",
        
        "dpos:e -> cop:w;",
        "dpos:e -> con:w;",
        
        "cap:e -> bna:w;",
        "cap:e -> cna:w;",
        "cap:e -> kna:w;",
        
        "cop:e -> bno:w;",
        "cop:e -> cno:w;",
        "cop:e -> kno:w;",
        
        "}"
      )
      
      return(DiagrammeR::grViz(dot))
    }
    

N <- as.numeric(f$women_screened)

pct <- function(x, denom) {
  x <- suppressWarnings(as.numeric(x)); denom <- suppressWarnings(as.numeric(denom))
  if (!is.finite(x) || is.na(x) || !is.finite(denom) || is.na(denom) || denom <= 0) return(NA_real_)
  round(100 * x / denom, 1)
}

fmtN <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!is.finite(x) || is.na(x)) return("NA")
  fmt_int(round(x))
}

lbl_pct <- function(title, n, p) {
  ptxt <- if (is.na(p)) "(–)" else paste0("(", p, "%)")
  paste0(title, "\\n", fmtN(n), "\\n", ptxt)
}

lbl <- function(title, n) paste0(title, "\\n", fmtN(n))

# --- cores (paleta CCU) ---
bg         <- CC_COLORS$primary
border     <- CC_COLORS$primary_darker
edge_col   <- CC_COLORS$white
node_fill  <- CC_COLORS$white
text_col   <- CC_COLORS$text

# --- percentuais (coerentes com o diagrama) ---
p_neg   <- pct(f$negative, N)
p_16    <- pct(f$hpv16_18, N)
p_out   <- pct(f$other_hrhpv, N)

p_cpos  <- pct(f$cytology_pos, f$other_hrhpv)
p_cneg  <- pct(f$cytology_neg, f$other_hrhpv)

p_colpo16_pos <- pct(f$colpo_pos_16, f$hpv16_18)
p_colpo16_neg <- pct(f$colpo_neg_16, f$hpv16_18)

p_colpoout_pos <- pct(f$colpo_pos_out, f$cytology_pos)
p_colpoout_neg <- pct(f$colpo_neg_out, f$cytology_pos)

p_bneg_16  <- pct(f$biopsy_neg_cin1_16, f$colpo_pos_16)
p_cin_16   <- pct(f$cin2_3_16,           f$colpo_pos_16)
p_can_16   <- pct(f$cancer_16,           f$colpo_pos_16)

p_bneg_out <- pct(f$biopsy_neg_cin1_out, f$colpo_pos_out)
p_cin_out  <- pct(f$cin2_3_out,          f$colpo_pos_out)
p_can_out  <- pct(f$cancer_out,          f$colpo_pos_out)

dot <- paste0(
  "digraph flow {",
  "graph [layout=neato, overlap=true, splines=ortho, bgcolor='", bg, "', pad='0.30', nodesep='0.45', ranksep='0.60'];",
  "node  [shape=box, style='rounded,filled', penwidth=1.6, fontname='Helvetica', fontsize=11, margin='0.18,0.12', pin=true, fixedsize=true, width=2.10, height=0.95, fillcolor='", node_fill, "', color='", edge_col, "', fontcolor='", text_col, "'];",
  "edge  [color='", edge_col, "', penwidth=1.5, arrowsize=0.8, fontname='Helvetica', fontsize=10, fontcolor='", edge_col, "'];",
  
  # --- NÓS (pos fixas) ---
  "scr [label='", lbl("Women screened", N), "', pos='0,0!', width=2.3];",
  
  "neg [label='", lbl_pct("Negative", f$negative, p_neg), "', pos='0,-2.2!'];",
  "hpv16 [label='", lbl_pct("HPV 16/18", f$hpv16_18, p_16), "', pos='3.6,2.2!'];",
  "hr   [label='", lbl_pct("Other high-risk HPV", f$other_hrhpv, p_out), "', pos='3.6,-2.2!', width=2.6];",
  
  "cp16 [label='", lbl_pct("Colposcopy +", f$colpo_pos_16, p_colpo16_pos), "', pos='11.4,1.4!'];",
  "cn16 [label='", lbl_pct("Colposcopy -", f$colpo_neg_16, p_colpo16_neg), "', pos='11.4,2.8!'];",
  
  "cpos [label='", lbl_pct("Cytology +", f$cytology_pos, p_cpos), "', pos='7.4,-1.4!'];",
  "cneg [label='", lbl_pct("Cytology -", f$cytology_neg, p_cneg), "', pos='7.4,-2.8!'];",
  
  "bneg16 [label='", lbl_pct("Biopsy - / CIN1", f$biopsy_neg_cin1_16, p_bneg_16), "', pos='15.6,3.0!'];",
  "cin16  [label='", lbl_pct("CIN2 / CIN3", f$cin2_3_16, p_cin_16), "', pos='15.6,1.8!'];",
  "can16  [label='", lbl_pct("Cancer", f$cancer_16, p_can_16), "', pos='15.6,0.6!'];",
  
  "cpout [label='", lbl_pct("Colposcopy +", f$colpo_pos_out, p_colpoout_pos), "', pos='11.4,-1.4!'];",
  "cnout [label='", lbl_pct("Colposcopy -", f$colpo_neg_out, p_colpoout_neg), "', pos='11.4,-2.8!'];",
  
  "bnegout [label='", lbl_pct("Biopsy - / CIN1", f$biopsy_neg_cin1_out, p_bneg_out), "', pos='15.6,-0.6!'];",
  "cinout  [label='", lbl_pct("CIN2 / CIN3", f$cin2_3_out, p_cin_out), "', pos='15.6,-1.8!'];",
  "canout  [label='", lbl_pct("Cancer", f$cancer_out, p_can_out), "', pos='15.6,-3.0!'];",
  
  # --- ARESTAS ---
  "scr:d -> neg:u;",
  "scr:e -> hpv16:w;",
  "scr:e -> hr:w;",
  
  "hr:e -> cpos:w;",
  "hr:e -> cneg:w;",
  
  "hpv16:e -> cp16:w;",
  "hpv16:e -> cn16:w;",
  
  "cpos:e -> cpout:w;",
  "cpos:e -> cnout:w;",
  
  "cp16:e -> bneg16:w;",
  "cp16:e -> cin16:w;",
  "cp16:e -> can16:w;",
  
  "cpout:e -> bnegout:w;",
  "cpout:e -> cinout:w;",
  "cpout:e -> canout:w;",
  
  "}"
)

DiagrammeR::grViz(dot)
    })
    
    
  })
}
