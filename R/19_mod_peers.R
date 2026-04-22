# ===========================================================
# R/19_mod_peers.R
# Módulo Peer Analysis — UI + Server
# ===========================================================

peers_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "peers-layout",
    style = "display:flex; margin:-15px; min-height:calc(100vh - 90px); overflow:hidden;",

    # ── Sidebar interna (apenas filtros específicos do peer analysis) ──
    div(
      style = paste(
        "width:230px; flex-shrink:0;",
        "background:#f4fbfb; border-right:1px solid #c8e6e8;",
        "overflow-y:auto; padding:16px 14px;"
      ),

      div(
        style = "font-size:10px; font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:#0B7285; margin-bottom:4px;",
        "PEER ANALYSIS"
      ),
      div(
        style = "font-size:11px; color:#888; margin-bottom:14px; line-height:1.4;",
        HTML(paste0(
          "Geographic scope is controlled by the <b>global filters</b> in the main sidebar ",
          "(State, Macro-region, Health region, Municipality). Use the controls below to ",
          "refine the peer comparison."
        ))
      ),

      selectInput(ns("grupo_sel"), "Peer group", choices = NULL, width = "100%"),

      selectInput(
        ns("cor_sel"),
        label   = "Coverage level",
        choices = c(
          "All"                     = "",
          "Low — score < 0.50"      = "red",
          "Moderate — 0.50 to 0.79" = "yellow",
          "Good — score ≥ 0.80" = "green"
        ),
        selected = "",
        width    = "100%"
      ),

      hr(style = "border-color:#d0e8ea; margin:12px 0;"),

      div(
        style = "font-size:10px; color:#aaa; line-height:1.5;",
        HTML(paste0(
          "Source: IBGE/DATASUS (population) and SIA/SUS (production).<br>",
          "Reference parameters: INCA 2019, 70% target coverage, age 25–64, 3-year interval."
        ))
      )
    ),

    # ── Painel principal ─────────────────────────────────────
    div(
      style = "flex:1; overflow-y:auto; padding:20px 24px 40px; min-width:0;",

      # Header dinâmico
      div(
        class = "cc-page-header",
        div(class = "cc-page-title", "Peer Analysis"),
        div(class = "cc-page-subtitle", uiOutput(ns("header_desc")))
      ),

      # Texto explicativo (metodologia)
      div(
        class = "cc-kpi-card",
        style = "background:#f9fcfc; border-left:3px solid #0B7285;",
        uiOutput(ns("method_explain"))
      ),

      # Cards de detalhe — ACIMA da tabela quando município selecionado
      uiOutput(ns("detail_section")),

      # Card Priority Municipalities — sempre visível
      div(
        class = "cc-kpi-card",

        div(
          style = paste(
            "display:flex; justify-content:space-between; align-items:center;",
            "font-size:11px; font-weight:700; letter-spacing:0.08em;",
            "text-transform:uppercase; color:#0B7285; margin-bottom:10px;"
          ),
          div("PRIORITY MUNICIPALITIES"),
          div(
            style = "font-size:10px; font-weight:500; color:#888; text-transform:none; letter-spacing:0;",
            "→ click a row to see that municipality's peer profile"
          )
        ),

        DT::dataTableOutput(ns("tabela_prioritarios"))
      )
    )
  )
}


peers_server <- function(id, peers_data, input_global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dt <- peers_data

    # Compatibilidade com versões antigas do .rds
    if (!"gap_top25" %in% names(dt) && "gap_p25" %in% names(dt)) {
      dt[, gap_top25 := gap_p25]
    }

    # Helper %||% (string-aware)
    `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && nzchar(as.character(a)[1])) a else b

    # ── Filtros globais como reactive ────────────────────────
    g <- reactive(input_global())

    # ── Popular dropdown de peer groups conforme escopo atual
    observe({
      gl <- g()
      d <- dt
      if (length(gl$filt_uf))    d <- d[uf            %in% gl$filt_uf]
      if (length(gl$filt_macro)) d <- d[nome_macro    %in% gl$filt_macro]
      if (length(gl$filt_reg))   d <- d[nome_regiao   %in% gl$filt_reg]
      if (length(gl$filt_mun))   d <- d[nome_municipio %in% gl$filt_mun]

      grupos <- sort(unique(d$grupo_label))
      updateSelectInput(session, "grupo_sel",
        choices  = c("All groups" = "", setNames(grupos, grupos)),
        selected = isolate(input$grupo_sel) %||% ""
      )
    })

    # ── Seleção local via clique na tabela ───────────────────
    mun_click <- reactiveVal(NULL)

    # Sempre que o escopo geográfico mudar, limpa a seleção local
    observeEvent(
      list(g()$filt_uf, g()$filt_macro, g()$filt_reg,
           g()$filt_mun, input$grupo_sel, input$cor_sel),
      {
        mun_click(NULL)
      },
      ignoreInit = TRUE
    )

    # Dados da tabela (respeitando filtros globais + locais)
    tabela_prio_data <- reactive({
      gl <- g()
      d  <- dt

      if (length(gl$filt_uf))    d <- d[uf             %in% gl$filt_uf]
      if (length(gl$filt_macro)) d <- d[nome_macro     %in% gl$filt_macro]
      if (length(gl$filt_reg))   d <- d[nome_regiao    %in% gl$filt_reg]
      if (length(gl$filt_mun))   d <- d[nome_municipio %in% gl$filt_mun]

      if (nchar(input$grupo_sel %||% "") > 0)
        d <- d[grupo_label == input$grupo_sel]

      if (!is.null(input$cor_sel) && nchar(input$cor_sel) > 0) {
        d <- switch(input$cor_sel,
          "red"    = d[score_geral <  0.5],
          "yellow" = d[score_geral >= 0.5 & score_geral <  0.8],
          "green"  = d[score_geral >= 0.8],
          d
        )
      }

      d[order(-gap_mediana)]
    })

    # Observer: clique na linha da tabela atualiza mun_click
    observeEvent(input$tabela_prioritarios_rows_selected, {
      idx <- input$tabela_prioritarios_rows_selected
      if (is.null(idx) || length(idx) == 0L) {
        mun_click(NULL)
        return()
      }
      d <- tabela_prio_data()
      if (idx > nrow(d)) {
        mun_click(NULL)
        return()
      }
      row <- d[idx]
      mun_click(list(nome = row$nome_municipio, uf = row$uf))
    }, ignoreNULL = FALSE)

    # Município selecionado: prioridade clique > filtro global filt_mun
    municipio_sel <- reactive({
      click <- mun_click()
      gl    <- g()

      if (!is.null(click)) {
        res <- dt[nome_municipio == click$nome & uf == click$uf]
        if (nrow(res) > 0L) return(res[1L])
      }

      # fallback: se filt_mun global tem exatamente 1 município, usar ele
      if (length(gl$filt_mun) == 1L) {
        res <- dt[nome_municipio == gl$filt_mun]
        if (length(gl$filt_uf) >= 1L) res <- res[uf %in% gl$filt_uf]
        if (nrow(res) > 0L) return(res[1L])
      }

      NULL
    })

    # ── Header dinâmico ──────────────────────────────────────
    output$header_desc <- renderUI({
      gl <- g()
      m  <- municipio_sel()

      scope <- if (length(gl$filt_uf)) {
        if (length(gl$filt_uf) == 1L) gl$filt_uf else paste0(length(gl$filt_uf), " states")
      } else {
        "Brazil (all states)"
      }

      if (is.null(m)) {
        n_prio <- nrow(tabela_prio_data())
        span(style = "color:#666;",
             sprintf("Showing %s priority municipalities — scope: %s",
                     format(n_prio, big.mark = ","), scope))
      } else {
        span(style = "color:#666;",
             m$nome_municipio, " — ", m$uf, " | ", m$grupo_label)
      }
    })

    # ── Texto explicativo (metodologia) ──────────────────────
    output$method_explain <- renderUI({
      gl <- g()
      d  <- dt
      if (length(gl$filt_uf))    d <- d[uf %in% gl$filt_uf]
      n_mun    <- nrow(d)
      n_grupos <- length(unique(d$grupo_label))

      HTML(sprintf(
        paste0(
          "<div style='font-size:11px; font-weight:700; letter-spacing:0.08em; ",
          "text-transform:uppercase; color:#0B7285; margin-bottom:8px;'>",
          "HOW TO READ THIS TABLE</div>",
          "<div style='font-size:12px; color:#444; line-height:1.55;'>",
          "<p style='margin:0 0 6px 0;'><b>Score</b> (0–1) measures each municipality's screening production ",
          "relative to the estimated need, averaged across four procedures (cytology, colposcopy, biopsy, EZT). ",
          "For each procedure, <code>score = min(real production / estimated need, 1)</code>, where the need assumes ",
          "70%% coverage of the SUS-dependent population aged 25–64, with cytology every 3 years and INCA 2019 ",
          "positivity rates for downstream procedures.</p>",
          "<p style='margin:0 0 6px 0;'><b>Gap</b> is the distance between a municipality's score and the group median ",
          "(or the group's top-25%% threshold). A positive gap means the municipality is <i>below</i> the benchmark ",
          "and would need to improve by that many points to reach it. A negative gap means it already exceeds the benchmark.</p>",
          "<p style='margin:0 0 6px 0;'><b>Rank</b> is the municipality's position within its peer group ",
          "(1 = worst performer; higher = better). Only municipalities in the <i>same</i> peer group are comparable ",
          "— groups with very different population size or SUS dependence face different structural conditions.</p>",
          "<p style='margin:0;'><b>Peer groups</b> were built by an unsupervised clustering algorithm ",
          "(PAM — Partitioning Around Medoids), which allocated the %s municipalities in the current scope into ",
          "<b>%d groups</b> based on target population size, SUS-dependent share, and the age structure of the ",
          "25–64 band (shares of ages 25–34, 35–49, 50–64). The optimal number of groups was chosen by ",
          "maximizing the average silhouette width.</p>",
          "</div>"
        ),
        format(n_mun, big.mark = ","),
        n_grupos
      ))
    })

    # ── Tabela de prioritários ────────────────────────────────
    output$tabela_prioritarios <- DT::renderDataTable({
      d <- tabela_prio_data()

      dt_show <- d[, .(
        Municipality    = nome_municipio,
        State           = uf,
        `Peer Group`    = grupo_label,
        `Health Region` = nome_regiao,
        Rank            = rank_grupo,
        Score           = round(score_geral, 2),
        Gap             = round(gap_mediana, 2)
      )]

      DT::datatable(
        dt_show,
        rownames  = FALSE,
        selection = list(mode = "single", target = "row"),
        options   = list(
          pageLength = 15,
          dom        = "tp",
          order      = list(list(6L, "desc")),
          columnDefs = list(list(className = "dt-center", targets = c(4L, 5L, 6L)))
        )
      ) |>
        DT::formatRound(columns = "Score", digits = 2) |>
        DT::formatRound(columns = "Gap",   digits = 2) |>
        DT::formatStyle(
          "Score",
          backgroundColor = DT::styleInterval(
            c(0.5, 0.8),
            c("#ffd5d5", "#fff3cd", "#d4edda")
          )
        )
    })

    # ── Cards de detalhe (acima da tabela, condicional) ───────
    output$detail_section <- renderUI({
      m <- municipio_sel()
      if (is.null(m)) return(NULL)

      # Profile
      prop_sus_pct <- if (is.finite(m$prop_sus)) sprintf("%.1f%%", m$prop_sus * 100) else "—"
      pop_fmt      <- format(round(m$pop_alvo_total), big.mark = ".", scientific = FALSE)

      # Ranking
      score_val   <- if (!is.na(m$score_geral)) m$score_geral else 0
      score_txt   <- sprintf("%.2f", score_val)
      posicao_txt <- if (!is.na(m$rank_grupo)) paste0(m$rank_grupo, " of ", m$n_grupo) else "—"
      pct_barra   <- if (!is.na(m$rank_grupo) && m$n_grupo > 1L)
                       round((m$rank_grupo - 1) / (m$n_grupo - 1) * 100) else 0L
      cor_score   <- if (score_val < 0.5) "#e74c3c" else if (score_val < 0.8) "#f39c12" else "#27ae60"

      # Gap helper
      .gap_linha <- function(label, val) {
        if (is.na(val)) return(.peers_kv(label, "—"))
        cor   <- if (val > 0) "#e74c3c" else "#27ae60"
        sinal <- if (val > 0) paste0("+", sprintf("%.2f", val)) else sprintf("%.2f", val)
        div(
          style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:6px;",
          div(style = "font-size:12px; color:#666;", label),
          div(style = paste0("font-size:14px; font-weight:700; color:", cor, ";"), sinal)
        )
      }

      frase_gap <- if (!is.na(m$gap_mediana) && m$gap_mediana <= 0) {
        paste0(m$nome_municipio, " is above its group median (gap: ",
               sprintf("%.2f", abs(m$gap_mediana)), ").")
      } else if (!is.na(m$gap_mediana)) {
        paste0("To reach the group median, ", m$nome_municipio,
               " would need to improve its score by ", sprintf("%.2f", m$gap_mediana), " points.")
      } else {
        "Insufficient data to calculate gaps."
      }

      gap_top25_val <- m$gap_top25

      tagList(
        div(style = "height:14px;"),

        # Row 1: Municipality Profile + Peer Ranking
        fluidRow(
          column(5,
            div(class = "cc-kpi-card", style = "height:100%;",
              div(style = "font-size:11px; font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:#0B7285; margin-bottom:10px;",
                  "Municipality Profile"),
              .peers_kv("Municipality",            m$nome_municipio),
              .peers_kv("State",                   m$uf),
              .peers_kv("Peer group",              m$grupo_label),
              .peers_kv("Municipalities in group", paste0(m$n_grupo, " municipalities")),
              .peers_kv("Target pop. (25–64)", pop_fmt),
              .peers_kv("SUS-dependent",           prop_sus_pct),
              .peers_kv("Macro-region",            m$nome_macro),
              .peers_kv("Health region",           m$nome_regiao)
            )
          ),
          column(7,
            div(class = "cc-kpi-card", style = "height:100%;",
              div(style = "font-size:11px; font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:#0B7285; margin-bottom:10px;",
                  "Peer Ranking"),
              fluidRow(
                column(6,
                  div(style = "margin-bottom:10px;",
                    div(style = "font-size:11px; color:#888;", "Rank in group"),
                    div(style = "font-size:26px; font-weight:700; color:#111; line-height:1.1;",
                        posicao_txt)
                  ),
                  div(
                    div(style = "font-size:11px; color:#888;", "Overall score (0–1)"),
                    div(style = paste0("font-size:28px; font-weight:700; line-height:1.1; color:", cor_score, ";"),
                        score_txt)
                  )
                ),
                column(6,
                  div(style = "font-size:11px; color:#888; margin-bottom:6px;",
                      "Relative position in group"),
                  div(class = "bar-outer",
                    div(class = "bar-fill",
                        style = paste0("width:", pct_barra, "%; background:", cor_score, ";"))
                  ),
                  div(style = "font-size:10px; color:#bbb; margin-top:4px;",
                      "← worse  |  better →")
                )
              ),
              div(style = "margin-top:10px; border-top:1px solid #eee; padding-top:10px;",
                div(style = "font-size:11px; font-weight:600; color:#555; margin-bottom:6px;",
                    "Scores by procedure"),
                .peers_score_row("Cytology",   m$score_cito),
                .peers_score_row("Colposcopy", m$score_colpo),
                .peers_score_row("Biopsy",     m$score_biopsia),
                .peers_score_row("EZT",        m$score_ezt)
              )
            )
          )
        ),

        div(style = "height:14px;"),

        # Row 2: Gap to Best Performers
        fluidRow(
          column(4,
            div(class = "cc-kpi-card",
              div(style = "font-size:11px; font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:#0B7285; margin-bottom:10px;",
                  "Gap to Best Performers"),
              .gap_linha("Gap to group median",   m$gap_mediana),
              .gap_linha("Gap to group top 25%",  gap_top25_val),
              div(
                style = "margin-top:12px; padding:8px 10px; border-radius:6px; background:#f0f9f8; font-size:11px; color:#444; line-height:1.5;",
                frase_gap
              ),
              if (isTRUE(m$prioritario))
                div(
                  style = "margin-top:10px; padding:6px 10px; border-radius:6px; background:#fff0f0; font-size:11px; color:#c0392b; font-weight:600;",
                  "⚠ Priority municipality: bottom 10% (or 5) of worst performers in group."
                )
            )
          )
        ),

        div(style = "height:14px;")
      )
    })

  })
}

# ── Helpers internos ─────────────────────────────────────────

.peers_kv <- function(label, valor) {
  div(
    style = "display:flex; justify-content:space-between; margin-bottom:5px;",
    div(style = "font-size:12px; color:#888;",                label),
    div(style = "font-size:12px; font-weight:600; color:#222;", valor)
  )
}

.peers_score_row <- function(label, val) {
  if (is.null(val) || is.na(val)) {
    txt <- "0.00"; cor <- "#e74c3c"
  } else {
    txt <- sprintf("%.2f", val)
    cor <- if (val < 0.5) "#e74c3c" else if (val < 0.8) "#f39c12" else "#27ae60"
  }
  div(
    style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:4px;",
    div(style = "font-size:11px; color:#666;", label),
    div(style = paste0("font-size:12px; font-weight:700; color:", cor, ";"), txt)
  )
}
