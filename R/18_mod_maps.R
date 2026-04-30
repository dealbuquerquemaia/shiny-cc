# ===========================================================
# 18_mod_maps.R
# Módulo Maps — coroplético (leaflet) da produção SUS/SIA 2024
#   em 4 níveis geográficos: Município → Região de saúde →
#   Macrorregião → Estado (UF).
#
# Particularidades:
# - Aba **só-Brasil**: fora do Brasil renderiza basemap em branco
#   com mensagem (validate em map_data()).
# - Único módulo que usa pacote {sf} + {leaflet}.
# - Não consome o engine (não roda cc_engine_run): trabalha
#   direto sobre `sia_cc_resumo` agregando ao nível desejado.
# - Granularidade é decidida em duas camadas:
#     (a) DRILL-DOWN local (clique num polígono) tem prioridade
#         máxima e gera estado em reactiveValues `drill`;
#     (b) Filtro geográfico da sidebar manda o "auto_geo" para
#         o nível imediatamente abaixo do filtrado;
#     (c) Sem (a) nem (b), usa o seletor manual `input$granularity`.
# - Clique num polígono dispara drill-down: UF → Macro → Região
#   de saúde → Município (folha, sem drill adicional).
# - Botão "Reset zoom" só aparece com drill ativo.
# ===========================================================

# ── UI ─────────────────────────────────────────────────────
# Cabeçalho compacto com 3 controles (metric / granularity /
# per100k) + área de mensagem dinâmica e o leafletOutput
# preenchendo o restante da viewport (altura calc 100vh-210px).
mod_maps_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "maps-header",
      div(class = "maps-header-title", "Maps"),
      div(
        class = "maps-header-controls",
        # Procedimento exibido no choropleth (cores)
        div(
          class = "maps-header-field",
          tags$label("Metric", `for` = ns("metric")),
          selectInput(ns("metric"), NULL,
            choices = c(
              "Cytology"         = "citologia",

              "Colposcopy"       = "colposcopia",
              "Biopsy"           = "biopsia",
              "Anatomopathology" = "anatomo",
              "EZT / treatment"  = "tratamento"
            ),
            selected = "citologia",
            width = "180px"
          )
        ),
        # Nível geográfico (manual) — sobrescrito pela sidebar
        # ou pelo drill quando algum estiver ativo (ver auto_geo)
        div(
          class = "maps-header-field",
          tags$label("Geographic level", `for` = ns("granularity")),
          selectInput(ns("granularity"), NULL,
            choices = c(
              "State (UF)"    = "estado",
              "Macroregion"   = "macro",
              "Health region" = "regiao",
              "Municipality"  = "municipio"
            ),
            selected = "estado",
            width = "180px"
          )
        ),
        # Toggle entre valor absoluto e taxa por 100k mulheres 25–64
        div(
          class = "maps-header-check",
          checkboxInput(ns("per100k"), "Per 100k women (25–64)", value = FALSE)
        ),
        # Linha de status + botão reset (visível só com drill ativo)
        div(
          class = "maps-header-note",
          textOutput(ns("map_note"), inline = TRUE),
          uiOutput(ns("reset_btn_ui"), inline = TRUE)
        )
      )
    ),
    leaflet::leafletOutput(ns("map"), height = "calc(100vh - 210px)")
  )
}


# ===========================================================
# SERVER
# ===========================================================
#
# Args:
# - sia_cc_resumo            (data.table): produção SUS/SIA 2024
#                            (já em formato `sus_proc_resumo` —
#                            colunas `geo_ref`, `categoria`,
#                            `total_25_64`, `total_all`, codes geo).
# - pop_municipio_regional   (data.table): pop feminina por
#                            município × faixa etária (denom 25–64).
# - geo_municipios/_regioes_saude/_macrorregioes/_estados (sf):
#                            polígonos para os 4 níveis. Devem
#                            ter atributos compatíveis com SIA
#                            (`mun_code6`, `regiao_codigo`,
#                            `macro_codigo`, `uf_codigo`).
# - input_global             (reactive): produzido por
#                            mod_filters_cc (sidebar global).
# - br_code                  (integer): código GLOBOCAN do Brasil.
#
# Saídas (renderizadas):
# - output$map        : leaflet choropleth (ou basemap vazio)
# - output$map_note   : string de status (ex.: "click to zoom")
# - output$reset_btn_ui: botão "Reset zoom" (só com drill ativo)

mod_maps_server <- function(id,
                              sia_cc_resumo,
                              pop_municipio_regional,
                              geo_municipios,
                              geo_regioes_saude,
                              geo_macrorregioes,
                              geo_estados,
                              input_global,
                              br_code) {

  moduleServer(id, function(input, output, session) {

    # ── 1. SETUP ÚNICO (executa 1× ao abrir a sessão) ──────────────────
    # Transforma todos os shapefiles para WGS-84 (EPSG:4326) — leaflet
    # exige lon/lat. Cache na inicialização do módulo evita refazer a
    # cada renderização do mapa.
    geo_mun_wgs   <- sf::st_transform(geo_municipios,    4326)
    geo_reg_wgs   <- sf::st_transform(geo_regioes_saude, 4326)
    geo_macro_wgs <- sf::st_transform(geo_macrorregioes, 4326)
    geo_est_wgs   <- sf::st_transform(geo_estados,       4326)

    # ── 2. HIERARQUIA & DENOMINADOR (lookups derivados) ─────────────────
    # mun_hier: tabela única que liga município → região de saúde →
    # macrorregião → UF (uf_codigo extraído dos 2 primeiros dígitos
    # do mun_code6 — convenção IBGE).
    mun_hier <- as.data.table(sf::st_drop_geometry(geo_municipios))[,
      .(
        mun_code6     = as.character(mun_code6),
        regiao_codigo = as.integer(regiao_codigo),
        macro_codigo  = as.integer(macro_codigo),
        uf_codigo     = as.integer(substr(mun_code6, 1L, 2L))
      )
    ]

    # pop_denom_mun: denominador para taxas por 100k — soma de mulheres
    # 25–64 por município. Faz merge com mun_hier para que possa ser
    # reagregado em qualquer dos 4 níveis sem refazer a soma da pop.
    pop_denom_mun <- pop_municipio_regional[from >= 25L & to <= 64L,
      .(pop_denom = sum(pop_total, na.rm = TRUE)),
      by = .(mun_code6 = as.character(geo_id))
    ]
    pop_denom_mun <- merge(pop_denom_mun, mun_hier, by = "mun_code6", all.x = TRUE)

    # ── 3. LOOKUPS PARA RESOLVER FILTRO DA SIDEBAR ──────────────────────
    # A sidebar entrega NOMES (ex.: "MG", "Macro Centro-Sul") e o sf
    # carrega CÓDIGOS — estas tabelas convertem entre eles para que o
    # auto_geo() consiga filtrar o sf correto.

    # macro_nome → macro_codigo (resolve filt_macro → códigos numéricos)
    mac_nome_to_codigo <- as.data.table(sf::st_drop_geometry(geo_macrorregioes))[,
      .(macro_codigo = as.integer(macro_codigo), macro_nome)
    ]
    # macro_codigo → conjunto de regiao_codigos (deriva do mun_hier)
    mac_to_reg <- unique(mun_hier[, .(macro_codigo, regiao_codigo)])

    # uf_codigo → uf_sigla (nome usado em geo_macro / filt_uf da sidebar).
    # Obs: em geo_municipios, `uf_sigla` é o NOME completo do estado;
    # o código numérico vem dos 2 primeiros dígitos do mun_code6.
    uf_codigo_to_nome <- unique(
      as.data.table(sf::st_drop_geometry(geo_municipios))[,
        .(uf_codigo = as.integer(substr(mun_code6, 1L, 2L)), uf_sigla)
      ]
    )

    # ── 4. is_brazil reactive ────────────────────────────────────────
    # Gating principal: o mapa só renderiza para Brasil (validate em
    # map_data() abaixo). Padrão replicado em 11/14/15/16/17.
    is_brazil <- reactive({
      g <- input_global()
      isTRUE(!is.null(g$pais_sel) && as.integer(g$pais_sel) == as.integer(br_code))
    })

    # ── 5. ESTADO DE DRILL-DOWN (clique no mapa) ───────────────────────
    # `drill` mantém estado LOCAL ao módulo, independente da sidebar.
    # Ao clicar num polígono, este estado é populado e o auto_geo()
    # passa a usá-lo (prioridade máxima). Quando a sidebar muda, drill
    # é zerado (a sidebar reancorou a geografia).
    drill <- reactiveValues(
      use        = FALSE,    # flag — TRUE quando drill está ativo
      gran       = NULL,     # "macro"/"regiao"/"municipio"
      filter_col = NULL,     # nome da coluna do sf para filtrar
      filter_vals = NULL     # valores a manter
    )

    # Mudança nos filtros geográficos da sidebar → reseta drill local.
    # Justificativa: a sidebar acabou de definir uma nova "âncora",
    # então o drill anterior perde sentido.
    observeEvent(
      list(
        input_global()$filt_uf,
        input_global()$filt_macro,
        input_global()$filt_reg
      ),
      {
        drill$use         <- FALSE
        drill$gran        <- NULL
        drill$filter_col  <- NULL
        drill$filter_vals <- NULL
      },
      ignoreInit = TRUE
    )

    # ── 6. AUTO_GEO — decide granularidade + filtro do sf ───────────────
    # Resolve em UMA passada quem dita a visualização atual:
    #   1) drill local (clique no polígono) → prioridade máxima;
    #   2) filtro da sidebar, do mais específico para o mais geral —
    #      ao filtrar uma região de saúde, mostra os municípios dela;
    #      ao filtrar uma macro, mostra as regiões; ao filtrar uma UF,
    #      mostra as macros;
    #   3) sem filtro nenhum → respeita o seletor manual.
    # Devolve list(gran, filter_col, filter_vals) — usado por map_data().
    auto_geo <- reactive({
      g       <- input_global()
      uf_sel  <- g$filt_uf    %||% character(0)
      mac_sel <- g$filt_macro %||% character(0)
      reg_sel <- g$filt_reg   %||% character(0)

      # (1) Drill-down (clique no mapa) tem prioridade máxima
      if (isTRUE(drill$use)) {
        return(list(
          gran        = drill$gran,
          filter_col  = drill$filter_col,
          filter_vals = drill$filter_vals
        ))
      }

      # (2) Filtro da sidebar (do mais específico para o mais geral)
      if (length(reg_sel) > 0L) {
        # Região(ões) de saúde selecionada(s) → mostra municípios delas
        list(gran = "municipio", filter_col = "regiao_nome", filter_vals = reg_sel)

      } else if (length(mac_sel) > 0L) {
        # Macro selecionada → resolve códigos e mostra regiões da macro
        mac_codes <- mac_nome_to_codigo[macro_nome %in% mac_sel, macro_codigo]
        reg_codes <- mac_to_reg[macro_codigo %in% mac_codes, unique(regiao_codigo)]
        list(gran = "regiao", filter_col = "regiao_codigo", filter_vals = reg_codes)

      } else if (length(uf_sel) > 0L) {
        # UF selecionada → mostra macrorregiões da UF
        list(gran = "macro", filter_col = "uf_sigla", filter_vals = uf_sel)

      } else {
        # (3) Sem filtro algum → seletor manual `input$granularity`
        list(gran = input$granularity %||% "estado", filter_col = NULL, filter_vals = NULL)
      }
    })

    # ── 7. MAP_DATA — pipeline central de dados do mapa ─────────────────
    # Reativo principal. Ordem dos passos:
    #   (a) gating: só prossegue se Brasil; senão `validate(need(...))`
    #       imprime mensagem amigável no leaflet (vide bloco 11).
    #   (b) filtra `sia_cc_resumo` pelo geo_ref (care/res) + categoria
    #       (procedimento) e agrega por município (sempre por mun
    #       primeiro — granularidade fina permite reagregar a qualquer
    #       nível abaixo).
    #   (c) reagrega para o nível alvo (`gran_sel`) e seleciona o sf
    #       correspondente.
    #   (d) aplica o filtro vindo de auto_geo (mantém só os polígonos
    #       que correspondem ao recorte da sidebar/drill).
    #   (e) se `per100k` ativo, divide pela população 25–64 do mesmo
    #       nível (`pop_denom_mun` reagregado) × 100 000.
    #   (f) anexa colunas `map_key`/`map_value`/`map_label` ao sf;
    #       `map_key` vira `layerId` no leaflet (chave do clique).
    map_data <- reactive({
      validate(need(is_brazil(),
        "Maps are available for Brazil only.\nSelect Brazil in the Population filter to view geographic distribution of SIA procedures."))

      g           <- input_global()
      geo_ref_sel <- as.character(g$sia_geo_ref %||% "care")
      metric_sel  <- as.character(input$metric  %||% "citologia")
      ag          <- auto_geo()
      gran_sel    <- ag$gran
      per100k     <- isTRUE(input$per100k)

      # (b) filtra SIA e agrega ao nível município (granularidade base)
      sia_mun <- sia_cc_resumo[
        geo_ref == geo_ref_sel & categoria == metric_sel,
        .(value = sum(total_25_64, na.rm = TRUE)),
        by = .(mun_code6 = as.character(geo_id))
      ]

      # Anexa códigos hierárquicos para permitir reagregação
      sia_mun <- merge(sia_mun, mun_hier, by = "mun_code6", all.x = TRUE)

      # (c) reagrega ao nível alvo + seleciona o sf
      if (gran_sel == "municipio") {
        agg <- sia_mun[, .(join_key = mun_code6, value)]
        geo_sf   <- geo_mun_wgs
        join_col <- "mun_code6"
        lbl_col  <- "mun_nome"
      } else if (gran_sel == "regiao") {
        agg <- sia_mun[!is.na(regiao_codigo),
          .(value = sum(value, na.rm = TRUE)),
          by = .(join_key = as.character(regiao_codigo))
        ]
        geo_sf   <- geo_reg_wgs
        join_col <- "regiao_codigo"
        lbl_col  <- "regiao_nome"
      } else if (gran_sel == "macro") {
        agg <- sia_mun[!is.na(macro_codigo),
          .(value = sum(value, na.rm = TRUE)),
          by = .(join_key = as.character(macro_codigo))
        ]
        geo_sf   <- geo_macro_wgs
        join_col <- "macro_codigo"
        lbl_col  <- "macro_nome"
      } else {  # estado (default)
        agg <- sia_mun[!is.na(uf_codigo),
          .(value = sum(value, na.rm = TRUE)),
          by = .(join_key = as.character(uf_codigo))
        ]
        geo_sf   <- geo_est_wgs
        join_col <- "uf_codigo"
        lbl_col  <- "uf_nome"
      }

      # (d) aplica filtro geográfico vindo da sidebar/drill (ag)
      if (!is.null(ag$filter_col) && !is.null(ag$filter_vals) &&
          ag$filter_col %in% names(geo_sf)) {
        keep_idx <- geo_sf[[ag$filter_col]] %in% ag$filter_vals
        geo_sf   <- geo_sf[keep_idx, , drop = FALSE]
      }

      # (e) se per-100k, calcula taxa por mulheres 25–64 (×100000)
      if (per100k) {
        if (gran_sel == "municipio") {
          pop_agg <- pop_denom_mun[, .(join_key = mun_code6, pop_denom)]
        } else if (gran_sel == "regiao") {
          pop_agg <- pop_denom_mun[!is.na(regiao_codigo),
            .(pop_denom = sum(pop_denom, na.rm = TRUE)),
            by = .(join_key = as.character(regiao_codigo))
          ]
        } else if (gran_sel == "macro") {
          pop_agg <- pop_denom_mun[!is.na(macro_codigo),
            .(pop_denom = sum(pop_denom, na.rm = TRUE)),
            by = .(join_key = as.character(macro_codigo))
          ]
        } else {
          pop_agg <- pop_denom_mun[!is.na(uf_codigo),
            .(pop_denom = sum(pop_denom, na.rm = TRUE)),
            by = .(join_key = as.character(uf_codigo))
          ]
        }
        agg <- merge(agg, pop_agg, by = "join_key", all.x = TRUE)
        # Taxa por 100k — NA quando denominador é 0/NA (evita Inf)
        agg[, value := fifelse(
          !is.na(pop_denom) & pop_denom > 0,
          value / pop_denom * 1e5,
          NA_real_
        )]
        agg[, pop_denom := NULL]
      }

      # (f) anexa valores ao sf — preserva ordem original das linhas
      # (`leaflet` é sensível a ordem; merge() de data.table reordena).
      geo_attr <- as.data.table(sf::st_drop_geometry(geo_sf))
      geo_attr[, join_key := as.character(get(join_col))]
      geo_attr <- merge(geo_attr, agg, by = "join_key", all.x = TRUE)

      match_idx <- match(
        as.character(geo_sf[[join_col]]),
        geo_attr$join_key
      )
      geo_sf$map_key   <- as.character(geo_sf[[join_col]])  # vira layerId no leaflet
      geo_sf$map_value <- geo_attr$value[match_idx]
      geo_sf$map_label <- geo_sf[[lbl_col]]

      list(
        sf       = geo_sf,
        per100k  = per100k,
        metric   = metric_sel,
        geo_ref  = geo_ref_sel,
        gran     = gran_sel,
        join_col = join_col
      )
    })

    # ── 8. CLIQUE NO MAPA → DRILL-DOWN ─────────────────────────────────
    # `input$map_shape_click` é populado pelo leaflet quando o usuário
    # clica num polígono que tenha `layerId` (definido como `map_key`
    # no bloco 7). O `id` recebido = `map_key` clicado.
    # Cada nível desce para o seguinte:
    #   estado   → macro
    #   macro    → regiao (de saúde)
    #   regiao   → municipio
    #   municipio → folha (sem drill adicional, no-op)
    # `isolate()` evita rerender prematura do map_data — o estado é
    # atualizado e a invalidation reativa do drill cuida do resto.
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      if (is.null(click) || is.null(click$id)) return()

      current_gran <- isolate(auto_geo()$gran)
      clicked_key  <- click$id

      if (current_gran == "estado") {
        # Clique num estado → mostrar macrorregiões dessa UF.
        # Resolve uf_codigo (numérico, layerId) → uf_sigla (nome cheio,
        # como armazenado em geo_macrorregioes$uf_sigla).
        uf_nm <- uf_codigo_to_nome[uf_codigo == as.integer(clicked_key), uf_sigla][1L]
        if (is.na(uf_nm)) return()
        drill$gran        <- "macro"
        drill$filter_col  <- "uf_sigla"
        drill$filter_vals <- uf_nm
        drill$use         <- TRUE

      } else if (current_gran == "macro") {
        # Clique numa macro → mostrar regiões de saúde da macro.
        mac_code  <- as.integer(clicked_key)
        reg_codes <- mac_to_reg[macro_codigo == mac_code, unique(regiao_codigo)]
        if (!length(reg_codes)) return()
        drill$gran        <- "regiao"
        drill$filter_col  <- "regiao_codigo"
        drill$filter_vals <- reg_codes
        drill$use         <- TRUE

      } else if (current_gran == "regiao") {
        # Clique numa região de saúde → mostrar municípios dela.
        drill$gran        <- "municipio"
        drill$filter_col  <- "regiao_codigo"
        drill$filter_vals <- as.integer(clicked_key)
        drill$use         <- TRUE
      }
      # municipio: nível folha — sem drill adicional (no-op)
    })

    # ── 9. RESET DRILL-DOWN ────────────────────────────────────────────
    # Disparado pelo botão "↑ Reset zoom" (renderizado em 10).
    observeEvent(input$reset_drill, {
      drill$use         <- FALSE
      drill$gran        <- NULL
      drill$filter_col  <- NULL
      drill$filter_vals <- NULL
    })

    # ── 10. BOTÃO RESET (UI condicional) ───────────────────────────────
    # Aparece só quando há drill ativo. O actionButton é namespaceado
    # via `session$ns()` para que o id "reset_drill" case com o
    # observeEvent do bloco 9.
    output$reset_btn_ui <- renderUI({
      if (!drill$use) return(NULL)
      actionButton(
        session$ns("reset_drill"), "↑ Reset zoom",
        style = paste(
          "font-size:11px; padding:2px 8px; margin-top:4px;",
          "background:#f0f0f0; border:1px solid #ccc; border-radius:4px;",
          "cursor:pointer;"
        )
      )
    })

    # ── 11. NOTA DE STATUS (header) ────────────────────────────────────
    # Texto acima do mapa que descreve o estado atual:
    # - Sempre menciona "SIA 2024" e o tipo de geo_ref ativo
    #   (place of care vs place of residence).
    # - Sufixo varia conforme drill ativo / sidebar filtrando / livre.
    output$map_note <- renderText({
      g       <- input_global()
      ref     <- g$sia_geo_ref %||% "care"
      ref_lbl <- if (ref == "care") "place of care" else "place of residence"

      has_sidebar <- length(g$filt_uf    %||% character(0)) > 0L ||
                     length(g$filt_macro %||% character(0)) > 0L ||
                     length(g$filt_reg   %||% character(0)) > 0L

      suffix <- if (drill$use) {
        " · drill-down active · click to zoom further"
      } else if (has_sidebar) {
        " · level auto-set by sidebar · click to zoom"
      } else {
        " · click a polygon to drill down"
      }
      paste0("SIA 2024 — ", ref_lbl, suffix)
    })

    # ── 12. CHOROPLETH (renderLeaflet) ─────────────────────────────────
    # Render final do mapa. Etapas:
    #   (a) extrai o sf de map_data() — `validate()` no bloco 7 cuida
    #       do caso "não-Brasil" propagando msg amigável.
    #   (b) curto-circuito de "no data": se nenhum polígono tem valor
    #       finito (raro — pode acontecer se o filtro remove tudo),
    #       renderiza só o basemap centrado no Brasil.
    #   (c) paleta institucional teal em 7 bins (ramp claro→escuro).
    #   (d) formatter do tooltip — 1 casa decimal para taxas, inteiro
    #       com vírgula para totais.
    #   (e) labels HTML (por polígono): "Nome\nMétrica: valor\n[hint]".
    #       Hint "Click to zoom in" só nos níveis acima de município.
    #   (f) addPolygons com layerId = `map_key` (necessário para que
    #       input$map_shape_click receba o id do polígono clicado).
    #   (g) fitBounds com bbox do sf filtrado (zoom automático ao
    #       drill-down ou ao mudar nível).
    output$map <- leaflet::renderLeaflet({
      d <- map_data()   # validate() interno propaga mensagem em erro

      sf_data     <- d$sf
      vals        <- sf_data$map_value
      vals_finite <- vals[is.finite(vals) & !is.na(vals)]

      # (c) paleta teal institucional (alinhada ao CSS / CC_COLORS)
      pal_colors <- c("#EAF7F7", "#B8E5E6", "#7CCDCE", "#4ABDAC",
                      "#138898", "#0B7285", "#0F6B7A")

      # (b) sem dados → basemap puro centrado no Brasil (lon -52, lat -14)
      if (length(vals_finite) == 0L) {
        return(
          leaflet::leaflet() %>%
            leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
            leaflet::setView(lng = -52, lat = -14, zoom = 4)
        )
      }

      pal <- leaflet::colorBin(
        palette  = pal_colors,
        domain   = vals_finite,
        bins     = 7L,
        na.color = "#d0d0d0"
      )

      # (d) formatter do valor no tooltip — depende do modo per100k
      fmt_val <- if (d$per100k) {
        function(x) if (is.na(x) || !is.finite(x)) "–" else sprintf("%.1f", x)
      } else {
        function(x) if (is.na(x) || !is.finite(x)) "–" else
          formatC(as.integer(round(x)), format = "d", big.mark = ",")
      }

      # Rótulo legível da métrica (en) — fallback ao próprio código
      metric_label <- switch(d$metric,
        citologia   = "Cytology",

        colposcopia = "Colposcopy",
        biopsia     = "Biopsy",
        anatomo     = "Anatomopathology",
        tratamento  = "EZT / treatment",
        d$metric
      )
      value_suffix <- if (d$per100k) " per 100k" else ""

      # Hint de drill-down só faz sentido acima do nível folha
      drill_hint <- if (d$gran != "municipio") {
        "<br/><span style='color:#aaa;font-size:10px;'>Click to zoom in</span>"
      } else ""

      # (e) labels HTML por polígono (htmltools::HTML escapa o nome)
      labels <- lapply(seq_len(nrow(sf_data)), function(i) {
        htmltools::HTML(sprintf(
          "<div style='font-size:12px;'><strong>%s</strong><br/>%s: <b>%s</b>%s%s</div>",
          htmltools::htmlEscape(sf_data$map_label[i]),
          metric_label,
          fmt_val(sf_data$map_value[i]),
          value_suffix,
          drill_hint
        ))
      })

      # Título da legenda muda conforme o modo (taxa vs total)
      legend_title <- if (d$per100k)
        paste(metric_label, "<br/>per 100k women (25–64)")
      else
        paste(metric_label, "<br/>total (2024)")

      # (f, g) montagem do leaflet final — basemap + polígonos +
      # legenda + zoom automático no bbox do sf filtrado.
      leaflet::leaflet(sf_data) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::addPolygons(
          fillColor    = ~pal(map_value),
          fillOpacity  = 0.8,
          color        = "#ffffff",
          weight       = 0.6,
          opacity      = 1,
          smoothFactor = 0.5,
          layerId      = ~map_key,   # CHAVE — habilita input$map_shape_click
          highlightOptions = leaflet::highlightOptions(
            weight       = 2.5,
            color        = "#0F6B7A",
            fillOpacity  = 0.92,
            bringToFront = TRUE
          ),
          label        = labels,
          labelOptions = leaflet::labelOptions(
            style     = list("font-family" = "inherit", "padding" = "4px 8px"),
            direction = "auto",
            textsize  = "12px"
          )
        ) %>%
        leaflet::addLegend(
          pal       = pal,
          values    = ~map_value,
          opacity   = 0.9,
          title     = legend_title,
          position  = "bottomright",
          labFormat = leaflet::labelFormat(
            big.mark = ",",
            digits   = if (d$per100k) 1L else 0L
          )
        ) %>%
        {
          # fitBounds automático no bbox do sf após filtro/drill
          bbox <- sf::st_bbox(sf_data)
          leaflet::fitBounds(.,
            lng1 = as.numeric(bbox["xmin"]),
            lat1 = as.numeric(bbox["ymin"]),
            lng2 = as.numeric(bbox["xmax"]),
            lat2 = as.numeric(bbox["ymax"])
          )
        }
    })

  })
}
