# ===========================================================
# R/19_mod_peers.R
# Módulo Peer Analysis — UI + Server
# -----------------------------------------------------------
# Aba "Peer Analysis": comparação dos municípios brasileiros
# em grupos formados por clusterização não-supervisionada
# (PAM — Partitioning Around Medoids) sobre população-alvo,
# % SUS-dependente e estrutura etária 25–64. Para cada
# município o dataset `peers_data` (gerado em
# `data-raw/09_prepare_peers.R`) traz:
#   - score_geral / score_cito / score_colpo / score_biopsia /
#     score_ezt   (produção SUS/SIA / necessidade estimada,
#                  saturado em 1)
#   - rank_grupo (1 = pior do grupo) e n_grupo
#   - gap_mediana e gap_top25 (distância p/ benchmarks)
#   - prioritario (flag bottom-10% ou 5 piores)
#
# Particularidades arquiteturais:
#  - **Aba só-Brasil de fato** (peers_data é de municípios BR);
#    a UI não bloqueia explicitamente fora do BR, mas o
#    dataset não traz nada útil — geografia herdada de
#    `input_global()` filtra para o escopo desejado.
#  - **NÃO usa o engine** (`cc_engine_run` não é chamado).
#    Os scores foram pré-calculados no ETL com cenário fixo
#    (INCA 2019, 70% cobertura, 25–64, intervalo 3 anos),
#    independente dos parâmetros da sidebar.
#  - **Sidebar interna própria** (2 controles: `grupo_sel` e
#    `cor_sel` para faixa de score). Geografia continua sendo
#    governada pela sidebar global.
#  - **Drill-down por clique na tabela DT** (similar ao Maps,
#    mas único uso de `*_rows_selected`): clique em linha →
#    cards de detalhe acima da tabela.
# ===========================================================

# ── UI ─────────────────────────────────────────────────────
# Layout com sidebar interna à esquerda (filtros do módulo)
# e painel principal à direita (header + texto metodológico
# + cards de detalhe condicionais + tabela de prioritários).
peers_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "peers-layout",
    style = "display:flex; margin:-15px; min-height:calc(100vh - 90px); overflow:hidden;",

    # ── Sidebar interna (apenas filtros específicos do peer analysis) ──
    div(
      style = paste(
        "width:230px; flex-shrink:0;",
        # [T1] era #f4fbfb / #c8e6e8 hardcoded → tokens
        "background:var(--cc-teal-surface); border-right:1px solid var(--cc-teal-border);",
        "overflow-y:auto; padding:16px 14px;"
      ),

      div(
        # [T3] era #0B7285 hardcoded → var(--cc-dark)
        # [T9] era font-size:10px → var(--t-xs)
        style = "font-size:var(--t-xs); font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:var(--cc-dark); margin-bottom:4px;",
        "PEER ANALYSIS"
      ),
      div(
        # [C1] era #888 (3.5:1) → var(--cc-gray2) (#6B7280, 4.8:1)
        style = "font-size:var(--t-sm); color:var(--cc-gray2); margin-bottom:14px; line-height:1.4;",
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

      # [T1] era #d0e8ea → var(--cc-teal-border)
      hr(style = "border-color:var(--cc-teal-border); margin:12px 0;"),

      div(
        # [C1] era #aaa (2.3:1) → var(--cc-gray2)
        style = "font-size:var(--t-xs); color:var(--cc-gray2); line-height:1.5;",
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
        # [T1] era #f9fcfc → var(--cc-teal-surface); [T3] era #0B7285 → var(--cc-dark)
        style = "background:var(--cc-teal-surface); border-left:3px solid var(--cc-dark);",
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
            # [T9] era font-size:11px → var(--t-xs); [T3] era #0B7285 → var(--cc-dark)
            "font-size:var(--t-xs); font-weight:700; letter-spacing:0.08em;",
            "text-transform:uppercase; color:var(--cc-dark); margin-bottom:10px;"
          ),
          div("PRIORITY MUNICIPALITIES"),
          div(
            # [C1] era #888 → var(--cc-gray2)
            style = "font-size:var(--t-xs); font-weight:500; color:var(--cc-gray2); text-transform:none; letter-spacing:0;",
            "\u2192 click a row to see that municipality's peer profile"
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

    # ── Bloco 1 ─ Setup do dataset ───────────────────────────
    # Retrocompatibilidade: versões antigas do .rds traziam a
    # coluna `gap_p25`; renomeada para `gap_top25` no ETL atual.
    # Se faltar a nova mas existir a antiga, copia.
    if (!"gap_top25" %in% names(dt) && "gap_p25" %in% names(dt)) {
      dt[, gap_top25 := gap_p25]
    }

    # ── Bloco 2 ─ Helper %||% (string-aware) ─────────────────
    # Variante local do operador null-coalescing: trata também
    # string vazia como "ausente". `%||%` global de utils só
    # checa NULL/length; aqui precisamos cobrir `""` também
    # porque os inputs `selectInput` devolvem "" quando "All".
    `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && nzchar(as.character(a)[1])) a else b

    # ── Bloco 3 ─ Filtros globais como reactive ──────────────
    # Encapsula `input_global()` para reuso. A geografia
    # (filt_uf/macro/reg/mun) é a única parte consumida —
    # o módulo não roda o engine, então cfg/método/cobertura
    # não são lidos aqui.
    g <- reactive(input_global())

    # ── Bloco 4 ─ Popular dropdown de peer groups ────────────
    # Observer que dinamicamente preenche `grupo_sel` (peer
    # group) com os grupos PAM presentes no recorte geográfico
    # atual. Reativo a qualquer mudança nos 4 filtros geo.
    # Mantém a seleção do usuário se ainda for válida.
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

    # ── Bloco 5 ─ Seleção local via clique na tabela ─────────
    # `mun_click` é o estado de drill-down DESTE módulo (paralelo
    # ao `drill` em mod_maps). Guarda a referência (nome+UF) do
    # município escolhido pela linha clicada na DataTable.
    # O observer abaixo zera o clique sempre que o usuário muda
    # o escopo (filtros geo, peer group ou cor) — caso contrário
    # o drill ficaria preso a um município fora do recorte atual.
    mun_click <- reactiveVal(NULL)

    observeEvent(
      list(g()$filt_uf, g()$filt_macro, g()$filt_reg,
           g()$filt_mun, input$grupo_sel, input$cor_sel),
      {
        mun_click(NULL)
      },
      ignoreInit = TRUE
    )

    # ── Bloco 6 ─ Tabela de prioritários (data) ──────────────
    # Reactive central da aba: parte do `dt` completo, aplica
    # filtros geo da sidebar global, filtros do módulo (peer
    # group + cor por faixa de score) e ordena por gap_mediana
    # decrescente (maior gap → mais prioritário).
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

    # ── Bloco 7 ─ Clique na linha → drill-down ───────────────
    # Observer do `_rows_selected` (DT). Pega o índice e
    # resolve a linha correspondente no `tabela_prio_data()`
    # corrente, salvando (nome, uf) em `mun_click`. Defesas
    # contra NULL/length-0 (deselect) e índice fora do range
    # (caso a tabela tenha sido refiltrada após o clique).
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

    # ── Bloco 8 ─ Município selecionado (resolução) ──────────
    # Resolve o "município ativo" para os cards de detalhe,
    # com prioridade: (1) clique na tabela; (2) sidebar global
    # com `filt_mun` único. Se nenhum dos dois → NULL e os
    # cards de detalhe ficam ocultos.
    municipio_sel <- reactive({
      click <- mun_click()
      gl    <- g()

      if (!is.null(click)) {
        res <- dt[nome_municipio == click$nome & uf == click$uf]
        if (nrow(res) > 0L) return(res[1L])
      }

      if (length(gl$filt_mun) == 1L) {
        res <- dt[nome_municipio == gl$filt_mun]
        if (length(gl$filt_uf) >= 1L) res <- res[uf %in% gl$filt_uf]
        if (nrow(res) > 0L) return(res[1L])
      }

      NULL
    })

    # ── Bloco 9 ─ Header dinâmico ─────────────────────────────
    # Subtítulo do `cc-page-header`. 2 modos:
    #   - sem município ativo: contagem de prioritários no
    #     escopo + descrição do escopo (UF única, N UFs ou
    #     "Brazil");
    #   - município ativo: nome + UF + grupo PAM.
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
        # [C1] era #666 → var(--cc-gray2)
        span(style = "color:var(--cc-gray2);",
             sprintf("Showing %s priority municipalities — scope: %s",
                     format(n_prio, big.mark = ","), scope))
      } else {
        span(style = "color:var(--cc-gray2);",
             m$nome_municipio, " — ", m$uf, " | ", m$grupo_label)
      }
    })

    # ── Bloco 10 ─ Texto explicativo (metodologia) ────────────
    # Card com explicação inline de Score / Gap / Rank / peer
    # groups (PAM). Os números #municipalities (n_mun) e
    # #grupos (n_grupos) são calculados dinamicamente sobre
    # o recorte atual (apenas filtro UF — propositadamente NÃO
    # aplica macro/região/município, porque o texto descreve
    # o universo de comparação, não o foco).
    output$method_explain <- renderUI({
      gl <- g()
      d  <- dt
      if (length(gl$filt_uf))    d <- d[uf %in% gl$filt_uf]
      n_mun    <- nrow(d)
      n_grupos <- length(unique(d$grupo_label))

      HTML(sprintf(
        paste0(
          # [T3] era #0B7285 → var(--cc-dark); [T7] era #444 → var(--cc-gray1)
          "<div style='font-size:var(--t-xs); font-weight:700; letter-spacing:0.08em; ",
          "text-transform:uppercase; color:var(--cc-dark); margin-bottom:8px;'>",
          "HOW TO READ THIS TABLE</div>",
          "<div style='font-size:var(--t-xs); color:var(--cc-gray1); line-height:1.55;'>",
          "<p style='margin:0 0 6px 0;'><b>Score</b> (0\u20131) measures each municipality's screening production ",
          "relative to the estimated need, averaged across four procedures (cytology, colposcopy, biopsy, EZT). ",
          "For each procedure, <code>score = min(real production / estimated need, 1)</code>, where the need assumes ",
          "70%% coverage of the SUS-dependent population aged 25\u201364, with cytology every 3 years and INCA 2019 ",
          "positivity rates for downstream procedures.</p>",
          "<p style='margin:0 0 6px 0;'><b>Gap</b> is the distance between a municipality's score and the group median ",
          "(or the group's top-25%% threshold). A positive gap means the municipality is <i>below</i> the benchmark ",
          "and would need to improve by that many points to reach it. A negative gap means it already exceeds the benchmark.</p>",
          "<p style='margin:0 0 6px 0;'><b>Rank</b> is the municipality's position within its peer group ",
          "(1 = worst performer; higher = better). Only municipalities in the <i>same</i> peer group are comparable ",
          "\u2014 groups with very different population size or SUS dependence face different structural conditions.</p>",
          "<p style='margin:0;'><b>Peer groups</b> were built by an unsupervised clustering algorithm ",
          "(PAM \u2014 Partitioning Around Medoids), which allocated the %s municipalities in the current scope into ",
          "<b>%d groups</b> based on target population size, SUS-dependent share, and the age structure of the ",
          "25\u201364 band (shares of ages 25\u201334, 35\u201349, 50\u201364). The optimal number of groups was chosen by ",
          "maximizing the average silhouette width.</p>",
          "</div>"
        ),
        format(n_mun, big.mark = ","),
        n_grupos
      ))
    })

    # ── Bloco 11 ─ Tabela de prioritários (render) ────────────
    # `DT::datatable` com seleção de linha única (clique →
    # Bloco 7 escuta). 7 colunas: Município / UF / Peer Group
    # / Health Region / Rank / Score / Gap. Score recebe
    # `formatStyle` com cores semânticas:
    #   < 0.50 → vermelho-claro (cc-danger-bg)
    #   0.50–0.79 → laranja-claro (cc-warning-bg)
    #   ≥ 0.80 → verde-claro (cc-success-bg)
    # Hex literais necessários porque o DT não consome CSS vars.
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
          # DT não suporta CSS vars — usar hex dos tokens semânticos
          # --cc-danger-bg #FDECEA / --cc-warning-bg #FFF3E0 / --cc-success-bg #E6F5EE
          backgroundColor = DT::styleInterval(
            c(0.5, 0.8),
            c("#FDECEA", "#FFF3E0", "#E6F5EE")
          )
        )
    })

    # ── Bloco 12 ─ Cards de detalhe (condicionais) ────────────
    # Renderizado apenas quando há município ativo
    # (`municipio_sel()` não-NULL). 2 linhas de cards:
    #   Row 1: "Municipality Profile" (5 col) + "Peer Ranking"
    #          (7 col, com 4 scores por procedimento).
    #   Row 2: "Gap to Best Performers" (4 col) — gap → mediana
    #          + gap → top 25% + frase descritiva + alerta de
    #          prioritário (bottom 10% / 5 piores) se aplicável.
    # Helpers locais .gap_linha (pílula colorida com sinal +/–)
    # e cor_score por faixa (danger/warning/success).
    output$detail_section <- renderUI({
      m <- municipio_sel()
      if (is.null(m)) return(NULL)

      prop_sus_pct <- if (is.finite(m$prop_sus)) sprintf("%.1f%%", m$prop_sus * 100) else "\u2014"
      pop_fmt      <- format(round(m$pop_alvo_total), big.mark = ".", scientific = FALSE)

      score_val   <- if (!is.na(m$score_geral)) m$score_geral else 0
      score_txt   <- sprintf("%.2f", score_val)
      posicao_txt <- if (!is.na(m$rank_grupo)) paste0(m$rank_grupo, " of ", m$n_grupo) else "\u2014"
      pct_barra   <- if (!is.na(m$rank_grupo) && m$n_grupo > 1L)
                       round((m$rank_grupo - 1) / (m$n_grupo - 1) * 100) else 0L

      # [T8] era "#e74c3c"/"#f39c12"/"#27ae60" → tokens CSS (funcionam em inline styles)
      cor_score <- if (score_val < 0.5) "var(--cc-danger)" else if (score_val < 0.8) "var(--cc-warning)" else "var(--cc-success)"

      .gap_linha <- function(label, val) {
        if (is.na(val)) return(.peers_kv(label, "\u2014"))
        # [T8] cores dinâmicas → tokens CSS
        cor   <- if (val > 0) "var(--cc-danger)" else "var(--cc-success)"
        sinal <- if (val > 0) paste0("+", sprintf("%.2f", val)) else sprintf("%.2f", val)
        div(
          style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:6px;",
          # [C1] era #666 → var(--cc-gray2)
          div(style = "font-size:var(--t-xs); color:var(--cc-gray2);", label),
          div(style = paste0("font-size:var(--t-base); font-weight:700; color:", cor, ";"), sinal)
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
              # [T3] era #0B7285 → var(--cc-dark); [T9] font-size:11px → var(--t-xs)
              div(style = "font-size:var(--t-xs); font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:var(--cc-dark); margin-bottom:10px;",
                  "Municipality Profile"),
              .peers_kv("Municipality",            m$nome_municipio),
              .peers_kv("State",                   m$uf),
              .peers_kv("Peer group",              m$grupo_label),
              .peers_kv("Municipalities in group", paste0(m$n_grupo, " municipalities")),
              .peers_kv("Target pop. (25\u201364)", pop_fmt),
              .peers_kv("SUS-dependent",           prop_sus_pct),
              .peers_kv("Macro-region",            m$nome_macro),
              .peers_kv("Health region",           m$nome_regiao)
            )
          ),
          column(7,
            div(class = "cc-kpi-card", style = "height:100%;",
              div(style = "font-size:var(--t-xs); font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:var(--cc-dark); margin-bottom:10px;",
                  "Peer Ranking"),
              fluidRow(
                column(6,
                  div(style = "margin-bottom:10px;",
                    # [C1] era #888 → var(--cc-gray2)
                    div(style = "font-size:var(--t-xs); color:var(--cc-gray2);", "Rank in group"),
                    # [T7] era #111 → var(--cc-text)
                    div(style = "font-size:26px; font-weight:700; color:var(--cc-text); line-height:1.1;",
                        posicao_txt)
                  ),
                  div(
                    div(style = "font-size:var(--t-xs); color:var(--cc-gray2);", "Overall score (0\u20131)"),
                    div(style = paste0("font-size:28px; font-weight:700; line-height:1.1; color:", cor_score, ";"),
                        score_txt)
                  )
                ),
                column(6,
                  div(style = "font-size:var(--t-xs); color:var(--cc-gray2); margin-bottom:6px;",
                      "Relative position in group"),
                  div(class = "bar-outer",
                    div(class = "bar-fill",
                        style = paste0("width:", pct_barra, "%; background:", cor_score, ";"))
                  ),
                  # decorativo — usando token com opacidade reduzida
                  div(style = "font-size:var(--t-xs); color:var(--cc-gray2); opacity:0.55; margin-top:4px;",
                      "\u2190 worse  |  better \u2192")
                )
              ),
              div(style = "margin-top:10px; border-top:1px solid var(--cc-border); padding-top:10px;",
                # [T7] era #555 → var(--cc-gray1)
                div(style = "font-size:var(--t-xs); font-weight:600; color:var(--cc-gray1); margin-bottom:6px;",
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
              div(style = "font-size:var(--t-xs); font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:var(--cc-dark); margin-bottom:10px;",
                  "Gap to Best Performers"),
              .gap_linha("Gap to group median",   m$gap_mediana),
              .gap_linha("Gap to group top 25%",  gap_top25_val),
              div(
                # [T1] era #f0f9f8 → var(--cc-teal-surface); [T7] era #444 → var(--cc-gray1)
                style = "margin-top:12px; padding:8px 10px; border-radius:6px; background:var(--cc-teal-surface); font-size:var(--t-xs); color:var(--cc-gray1); line-height:1.5;",
                frase_gap
              ),
              if (isTRUE(m$prioritario))
                div(
                  # [T8] era #fff0f0/#c0392b → tokens semânticos
                  style = "margin-top:10px; padding:6px 10px; border-radius:6px; background:var(--cc-danger-bg); font-size:var(--t-xs); color:var(--cc-danger); font-weight:600;",
                  "\u26A0 Priority municipality: bottom 10% (or 5) of worst performers in group."
                )
            )
          )
        ),

        div(style = "height:14px;")
      )
    })

  })
}

# ── Helpers internos (escopo de arquivo) ─────────────────────
# Definidos FORA do moduleServer (sem `.` final reservado a
# CRAN, mas com prefixo `.` para indicar uso restrito a este
# arquivo). Usados pelos cards do Bloco 12.

# `.peers_kv(label, valor)` — linha key-value horizontal usada
# nos cards "Municipality Profile". Estilo idêntico em todas
# as linhas (label cinza à esquerda, valor escuro à direita).
.peers_kv <- function(label, valor) {
  div(
    style = "display:flex; justify-content:space-between; margin-bottom:5px;",
    # [C1] era #888 → var(--cc-gray2); [T7] era #222 → var(--cc-text)
    div(style = "font-size:var(--t-xs); color:var(--cc-gray2);",                 label),
    div(style = "font-size:var(--t-xs); font-weight:600; color:var(--cc-text);", valor)
  )
}

# `.peers_score_row(label, val)` — linha de score por
# procedimento usada no card "Peer Ranking" (Cytology /
# Colposcopy / Biopsy / EZT). Cor do número segue mesma
# convenção do score geral: <0.5 danger, <0.8 warning,
# ≥0.8 success. NA / NULL exibem 0.00 vermelho.
.peers_score_row <- function(label, val) {
  if (is.null(val) || is.na(val)) {
    txt <- "0.00"; cor <- "var(--cc-danger)"
  } else {
    txt <- sprintf("%.2f", val)
    # [T8] era "#e74c3c"/"#f39c12"/"#27ae60" → tokens CSS
    cor <- if (val < 0.5) "var(--cc-danger)" else if (val < 0.8) "var(--cc-warning)" else "var(--cc-success)"
  }
  div(
    style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:4px;",
    # [C1] era #666 → var(--cc-gray2)
    div(style = "font-size:var(--t-xs); color:var(--cc-gray2);", label),
    div(style = paste0("font-size:var(--t-xs); font-weight:700; color:", cor, ";"), txt)
  )
}
