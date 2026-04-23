# ===========================================================
# 18_mod_maps.R
# Módulo Maps — choropleth leaflet dos procedimentos SIA/SUS
#   por município / região de saúde / macrorregião / estado
# Clique num polígono faz drill-down para o nível seguinte.
# ===========================================================

mod_maps_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "maps-header",
      div(class = "maps-header-title", "Maps"),
      div(
        class = "maps-header-controls",
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
        div(
          class = "maps-header-check",
          checkboxInput(ns("per100k"), "Per 100k women (25–64)", value = FALSE)
        ),
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

    # ── One-time setup (outside reactives) ─────────────────────────────

    # Transform all geometries to WGS-84 (required by leaflet)
    geo_mun_wgs   <- sf::st_transform(geo_municipios,    4326)
    geo_reg_wgs   <- sf::st_transform(geo_regioes_saude, 4326)
    geo_macro_wgs <- sf::st_transform(geo_macrorregioes, 4326)
    geo_est_wgs   <- sf::st_transform(geo_estados,       4326)

    # Hierarchy lookup: mun_code6 → regiao / macro / uf_codigo
    mun_hier <- as.data.table(sf::st_drop_geometry(geo_municipios))[,
      .(
        mun_code6     = as.character(mun_code6),
        regiao_codigo = as.integer(regiao_codigo),
        macro_codigo  = as.integer(macro_codigo),
        uf_codigo     = as.integer(substr(mun_code6, 1L, 2L))
      )
    ]

    # Population denominator: women 25–64 per mun_code6
    pop_denom_mun <- pop_municipio_regional[from >= 25L & to <= 64L,
      .(pop_denom = sum(pop_total, na.rm = TRUE)),
      by = .(mun_code6 = as.character(geo_id))
    ]
    pop_denom_mun <- merge(pop_denom_mun, mun_hier, by = "mun_code6", all.x = TRUE)

    # ── Lookup tables for indirect geographic filtering ─────────────────
    # macro_nome → macro_codigo  (to resolve mac_sel → regiao_codigos)
    mac_nome_to_codigo <- as.data.table(sf::st_drop_geometry(geo_macrorregioes))[,
      .(macro_codigo = as.integer(macro_codigo), macro_nome)
    ]
    # macro_codigo → regiao_codigos  (derived from mun_hier)
    mac_to_reg <- unique(mun_hier[, .(macro_codigo, regiao_codigo)])

    # uf_codigo → uf_sigla (full name, as used in geo_macro / filt_uf)
    # geo_municipios$uf_sigla is the full state name; substr gives uf_codigo
    uf_codigo_to_nome <- unique(
      as.data.table(sf::st_drop_geometry(geo_municipios))[,
        .(uf_codigo = as.integer(substr(mun_code6, 1L, 2L)), uf_sigla)
      ]
    )

    # ── Is Brazil selected? ─────────────────────────────────────────────
    is_brazil <- reactive({
      g <- input_global()
      isTRUE(!is.null(g$pais_sel) && as.integer(g$pais_sel) == as.integer(br_code))
    })

    # ── Local drill-down state (map click, independent of sidebar) ──────
    drill <- reactiveValues(
      use        = FALSE,
      gran       = NULL,
      filter_col = NULL,
      filter_vals = NULL
    )

    # Sidebar change → reset drill (nova "âncora" geográfica)
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

    # ── Auto granularity from sidebar geographic filter ─────────────────────
    auto_geo <- reactive({
      g       <- input_global()
      uf_sel  <- g$filt_uf    %||% character(0)
      mac_sel <- g$filt_macro %||% character(0)
      reg_sel <- g$filt_reg   %||% character(0)

      # Drill-down (clique no mapa) tem prioridade máxima
      if (isTRUE(drill$use)) {
        return(list(
          gran        = drill$gran,
          filter_col  = drill$filter_col,
          filter_vals = drill$filter_vals
        ))
      }

      # Filtro da sidebar (do mais específico para o mais geral)
      if (length(reg_sel) > 0L) {
        list(gran = "municipio", filter_col = "regiao_nome", filter_vals = reg_sel)

      } else if (length(mac_sel) > 0L) {
        mac_codes <- mac_nome_to_codigo[macro_nome %in% mac_sel, macro_codigo]
        reg_codes <- mac_to_reg[macro_codigo %in% mac_codes, unique(regiao_codigo)]
        list(gran = "regiao", filter_col = "regiao_codigo", filter_vals = reg_codes)

      } else if (length(uf_sel) > 0L) {
        list(gran = "macro", filter_col = "uf_sigla", filter_vals = uf_sel)

      } else {
        # Sem filtro algum → seletor manual de granularidade
        list(gran = input$granularity %||% "estado", filter_col = NULL, filter_vals = NULL)
      }
    })

    # ── Aggregated map data ─────────────────────────────────────────────
    map_data <- reactive({
      validate(need(is_brazil(),
        "Maps are available for Brazil only.\nSelect Brazil in the Population filter to view geographic distribution of SIA procedures."))

      g           <- input_global()
      geo_ref_sel <- as.character(g$sia_geo_ref %||% "care")
      metric_sel  <- as.character(input$metric  %||% "citologia")
      ag          <- auto_geo()
      gran_sel    <- ag$gran
      per100k     <- isTRUE(input$per100k)

      # --- Filter SIA and aggregate to municipality level ---------------
      sia_mun <- sia_cc_resumo[
        geo_ref == geo_ref_sel & categoria == metric_sel,
        .(value = sum(total_25_64, na.rm = TRUE)),
        by = .(mun_code6 = as.character(geo_id))
      ]

      # Attach hierarchy codes
      sia_mun <- merge(sia_mun, mun_hier, by = "mun_code6", all.x = TRUE)

      # --- Aggregate to selected granularity ----------------------------
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
      } else {  # estado
        agg <- sia_mun[!is.na(uf_codigo),
          .(value = sum(value, na.rm = TRUE)),
          by = .(join_key = as.character(uf_codigo))
        ]
        geo_sf   <- geo_est_wgs
        join_col <- "uf_codigo"
        lbl_col  <- "uf_nome"
      }

      # --- Apply sidebar geographic filter to sf ---
      if (!is.null(ag$filter_col) && !is.null(ag$filter_vals) &&
          ag$filter_col %in% names(geo_sf)) {
        keep_idx <- geo_sf[[ag$filter_col]] %in% ag$filter_vals
        geo_sf   <- geo_sf[keep_idx, , drop = FALSE]
      }

      # --- Per-100k denominator -----------------------------------------
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
        agg[, value := fifelse(
          !is.na(pop_denom) & pop_denom > 0,
          value / pop_denom * 1e5,
          NA_real_
        )]
        agg[, pop_denom := NULL]
      }

      # --- Attach values to sf ------------------------------------------
      geo_attr <- as.data.table(sf::st_drop_geometry(geo_sf))
      geo_attr[, join_key := as.character(get(join_col))]
      geo_attr <- merge(geo_attr, agg, by = "join_key", all.x = TRUE)

      # Re-order to match original sf row order
      match_idx <- match(
        as.character(geo_sf[[join_col]]),
        geo_attr$join_key
      )
      geo_sf$map_key   <- as.character(geo_sf[[join_col]])  # used as layerId
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

    # ── Map click → drill-down ──────────────────────────────────────────
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      if (is.null(click) || is.null(click$id)) return()

      # Read current level WITHOUT triggering map_data re-render
      current_gran <- isolate(auto_geo()$gran)
      clicked_key  <- click$id

      if (current_gran == "estado") {
        # Clicked a state (join_key = uf_codigo) → zoom into its macroregions
        uf_nm <- uf_codigo_to_nome[uf_codigo == as.integer(clicked_key), uf_sigla][1L]
        if (is.na(uf_nm)) return()
        drill$gran        <- "macro"
        drill$filter_col  <- "uf_sigla"
        drill$filter_vals <- uf_nm
        drill$use         <- TRUE

      } else if (current_gran == "macro") {
        # Clicked a macro (join_key = macro_codigo) → zoom into its health regions
        mac_code  <- as.integer(clicked_key)
        reg_codes <- mac_to_reg[macro_codigo == mac_code, unique(regiao_codigo)]
        if (!length(reg_codes)) return()
        drill$gran        <- "regiao"
        drill$filter_col  <- "regiao_codigo"
        drill$filter_vals <- reg_codes
        drill$use         <- TRUE

      } else if (current_gran == "regiao") {
        # Clicked a health region → zoom into its municipalities
        drill$gran        <- "municipio"
        drill$filter_col  <- "regiao_codigo"
        drill$filter_vals <- as.integer(clicked_key)
        drill$use         <- TRUE
      }
      # municipio: leaf level, no further drill
    })

    # ── Reset drill-down ────────────────────────────────────────────────
    observeEvent(input$reset_drill, {
      drill$use         <- FALSE
      drill$gran        <- NULL
      drill$filter_col  <- NULL
      drill$filter_vals <- NULL
    })

    # ── Reset button (only visible when drill is active) ────────────────
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

    # ── Map note ────────────────────────────────────────────────────────
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

    # ── Choropleth ─────────────────────────────────────────────────────
    output$map <- leaflet::renderLeaflet({
      d <- map_data()   # validate() inside propagates error message

      sf_data     <- d$sf
      vals        <- sf_data$map_value
      vals_finite <- vals[is.finite(vals) & !is.na(vals)]

      # Colour palette — app teal ramp
      pal_colors <- c("#EAF7F7", "#B8E5E6", "#7CCDCE", "#4ABDAC",
                      "#138898", "#0B7285", "#0F6B7A")

      if (length(vals_finite) == 0L) {
        # No data: plain basemap centred on Brazil
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

      # Tooltip formatter
      fmt_val <- if (d$per100k) {
        function(x) if (is.na(x) || !is.finite(x)) "–" else sprintf("%.1f", x)
      } else {
        function(x) if (is.na(x) || !is.finite(x)) "–" else
          formatC(as.integer(round(x)), format = "d", big.mark = ",")
      }

      metric_label <- switch(d$metric,
        citologia   = "Cytology",
        
        colposcopia = "Colposcopy",
        biopsia     = "Biopsy",
        anatomo     = "Anatomopathology",
        tratamento  = "EZT / treatment",
        d$metric
      )
      value_suffix <- if (d$per100k) " per 100k" else ""

      # Drill-down hint in tooltip (not at municipality level)
      drill_hint <- if (d$gran != "municipio") {
        "<br/><span style='color:#aaa;font-size:10px;'>Click to zoom in</span>"
      } else ""

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

      legend_title <- if (d$per100k)
        paste(metric_label, "<br/>per 100k women (25–64)")
      else
        paste(metric_label, "<br/>total (2024)")

      leaflet::leaflet(sf_data) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::addPolygons(
          fillColor    = ~pal(map_value),
          fillOpacity  = 0.8,
          color        = "#ffffff",
          weight       = 0.6,
          opacity      = 1,
          smoothFactor = 0.5,
          layerId      = ~map_key,   # enables input$map_shape_click
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
