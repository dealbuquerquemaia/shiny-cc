# ===========================================================
# Shiny-cc — módulo: 12_mod_resumo_pais.R
#   - Cards de resumo
#   - Gráficos por idade
#   - País vem dos filtros globais (input_global()$pais_sel)
# ===========================================================
mod_resumo_pais_ui <- function(id) {
  ns <- NS(id)
  
  # helper: card horizontal compacto para blocos escuros
  epi_card <- function(label, value_out, foot_out,
                       ico,
                       asr_out = NULL, asr_label = NULL) {
    div(
      class = "card-ccu",
      style = "flex-direction:row; align-items:flex-start; gap:10px; padding:10px 14px;",
      
      # lado esquerdo: label + valor + foot
      div(style = "flex:1; min-width:0;",
          div(class = "card-ccu-label", label),
          div(class = "card-ccu-value", style = "font-size:20px;", value_out),
          if (!is.null(foot_out))
            div(class = "card-ccu-sub", foot_out)
      ),
      
      # lado direito: ícone (+ ASR se fornecido)
      div(style = "flex-shrink:0; text-align:right; display:flex; flex-direction:column; align-items:flex-end; gap:2px;",
          div(style = "color:rgba(255,255,255,0.7); font-size:18px;", ico),
          if (!is.null(asr_out)) tagList(
            div(style = "color:#fff; font-weight:700; font-size:18px; line-height:1.1;", asr_out),
            div(style = "font-size:10px; color:rgba(255,255,255,0.5); line-height:1.3;", asr_label)
          )
      )
    )
  }
  
  tagList(
    div(
      class = "cc-page-header",
      div(class = "cc-page-title", "Epidemiology"),
      div(class = "cc-page-subtitle", "World Population Prospects 2025 · GLOBOCAN 2022")
    ),
    
    # --- caso população padrão (GLOBOCAN) ----------------------------
    conditionalPanel(
      condition = "input['filters-pop_mode'] != 'other'",
      
      # ── Linha superior: 2 blocos escuros de mesma altura ────────────
      fluidRow(class = "cc-row-eq",
               
               # COLUNA ESQUERDA — Population
               column(6,
                      div(class = "ccu-section ccu-section-1",
                          style = "flex:1; margin-bottom:12px;",
                          
                          div(class = "ccu-section-title", "World Population Prospects 2025"),
                          div(class = "cards-ccu-wrap", style = "flex-direction:column; gap:8px;",
                              
                              epi_card(
                                label    = "Total female population",
                                value_out = textOutput(ns("pop_total_txt")),
                                foot_out  = NULL,
                                ico       = icon("venus")
                              ),
                              
                              epi_card(
                                label    = "Female population, 25\u201364 years",
                                value_out = textOutput(ns("pop_2564_txt")),
                                foot_out  = NULL,
                                ico       = icon("users")
                              )
                          )
                      )
               ),
               
               # COLUNA DIREITA — Incidence & Mortality
               column(6,
                      div(class = "ccu-section ccu-section-2",
                          style = "flex:1; margin-bottom:12px;",
                          
                          div(class = "ccu-section-title", "Incidence & Mortality · GLOBOCAN 2022"),
                          div(class = "cards-ccu-wrap", style = "flex-direction:column; gap:8px;",
                              
                              epi_card(
                                label    = "Cervical cancer cases",
                                value_out = textOutput(ns("inc_n_txt")),
                                foot_out  = NULL,
                                ico       = icon("chart-line"),
                                asr_out   = textOutput(ns("inc_asr_txt")),
                                asr_label = "ASR per 100k women"
                              ),
                              
                              epi_card(
                                label    = "Cervical cancer deaths",
                                value_out = textOutput(ns("mort_n_txt")),
                                foot_out  = NULL,
                                ico       = icon("ribbon"),
                                asr_out   = textOutput(ns("mort_asr_txt")),
                                asr_label = "ASR per 100k women"
                              )
                          )
                      )
               )
      ), # /fluidRow blocos
      
      # ── Linha inferior: gráficos alinhados com os blocos acima ──────
      fluidRow(
        
        column(6,
               div(class = "cc-kpi-card", style = "margin-bottom:0;",
                   p(style = "font-size:var(--t-sm); font-weight:600; color:var(--cc-gray1); margin-bottom:8px;",
                     "Female population by age group (2025)"),
                   plotOutput(ns("plot_pop"), height = "280px", width = "100%")
               )
        ),
        
        column(6,
               div(class = "cc-kpi-card", style = "margin-bottom:0;",
                   p(style = "font-size:var(--t-sm); font-weight:600; color:var(--cc-gray1); margin-bottom:8px;",
                     "Incidence and mortality rates per 100 000 women"),
                   plotOutput(ns("plot_rates"), height = "280px", width = "100%")
               )
        )
      ) # /fluidRow gráficos
      
    ), # /conditionalPanel
    
    # --- caso Other population ---------------------------------------
    conditionalPanel(
      condition = "input['filters-pop_mode'] == 'other'",
      fluidRow(
        column(12,
               br(), br(),
               h3("Epidemiology"),
               p("Epidemiology indicators are not available when using 'Other population'.")
        )
      )
    )
  )
}


mod_resumo_pais_server <- function(id,
                                   df_completo,
                                   df_taxas,
                                   dim_country,
                                   input_global) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    sel_pais <- reactive({
      req(input_global()$pais_sel)
      as.integer(input_global()$pais_sel)
    })

    nome_pais <- reactive({
      dim_country[population_code == sel_pais(), population_name][1]
    })

    df_pop <- reactive({
      df_completo[
        population_code == sel_pais() &
          type == "Incidence",
        .(
          pop_2022 = {
            x <- suppressWarnings(as.numeric(pop_2022))
            x <- x[!is.na(x)]
            if (length(x)) x[1] else NA_real_
          },
          pop_2025 = {
            x <- suppressWarnings(as.numeric(pop_2025))
            x <- x[!is.na(x)]
            if (length(x)) x[1] else NA_real_
          }
        ),
        by = .(age_code, age)
      ][
        , age := factor(age, levels = AGE_ORDER)
      ][order(age_code)]
    })


    df_tax <- reactive({
      df_taxas[population_code == sel_pais()]
    })

    # ---- Cards -------------------------------------------------------

    output$pop_total_txt <- renderText({
      df <- df_pop()
      x  <- suppressWarnings(as.numeric(df$pop_2025))
      tot <- sum(x, na.rm = TRUE)
      if (is.na(tot) || !is.finite(tot) || tot <= 0) return("–")
      fmt_rate(tot, 0)
    })


    output$pop_total_foot <- renderText({
      paste0(nome_pais(), ", 2025")
    })

    # Faixa alvo de rastreamento: 25–64 anos
    output$pop_2564_txt <- renderText({
      df <- df_pop()[age %in% c("25-29","30-34","35-39","40-44","45-49",
                                "50-54","55-59","60-64")]
      x  <- suppressWarnings(as.numeric(df$pop_2025))
      tot <- sum(x, na.rm = TRUE)
      if (is.na(tot) || !is.finite(tot) || tot <= 0) return("–")
      fmt_rate(tot, 0)
    })


    output$pop_2564_foot <- renderText({
      paste0(nome_pais(), ", 2025")
    })

    output$inc_n_txt <- renderText({
      df <- df_tax()
      if (nrow(df) == 0) return("–")
      fmt_int(df$number_incidence[1])
    })
    output$inc_asr_txt <- renderText({
      df <- df_tax()
      if (nrow(df) == 0) return("")
      fmt_rate(df$incidence_asr_world[1], 1)
    })
    output$inc_foot <- renderText({
      paste0(nome_pais(), ", 2022")
    })

    output$mort_n_txt <- renderText({
      df <- df_tax()
      if (nrow(df) == 0) return("–")
      fmt_int(df$number_mortality[1])
    })
    output$mort_asr_txt <- renderText({
      df <- df_tax()
      if (nrow(df) == 0) return("")
      fmt_rate(df$mortality_asr_world[1], 1)
    })
    output$mort_foot <- renderText({
      paste0(nome_pais(), ", 2022")
    })

    # ---- Gráfico população 2025 -------------------------------------

    output$plot_pop <- renderPlot({
      df <- df_pop()
      if (nrow(df) == 0) return(NULL)

      pop <- suppressWarnings(as.numeric(df$pop_2025))
      tot <- sum(pop, na.rm = TRUE)
      if (is.na(tot) || !is.finite(tot) || tot <= 0) return(NULL)

      share <- pop / tot * 100
      plot_df <- data.frame(
        age   = factor(df$age, levels = AGE_ORDER),
        share = share
      )

      ggplot(plot_df, aes(x = share, y = age)) +
        geom_col(fill = CC_COLORS$primary, width = 0.7) +
        geom_text(
          aes(label = paste0(round(share, 1), "%")),
          hjust = -0.15, size = 3, color = "gray30"
        ) +
        scale_x_continuous(
          expand = expansion(mult = c(0, 0.15)),
          labels = function(x) paste0(x, "%")
        ) +
        labs(x = "Percent of total female population", y = NULL) +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text.y        = element_text(size = 10, color = "gray30"),
          axis.text.x        = element_text(size = 10, color = "gray30"),
          axis.title.x       = element_text(size = 11, color = "gray40", margin = margin(t = 8)),
          plot.margin        = margin(4, 12, 4, 4)
        )
    }, res = 110)

    # ---- Gráfico taxas ----------------------------------------------

    output$plot_rates <- renderPlot({
      inc <- df_completo[
        population_code == sel_pais() & type == "Incidence",
        .(cases_2022 = first(cases_2022), pop_2022 = first(pop_2022)),
        by = .(age_code, age)
      ][, age := factor(age, levels = AGE_ORDER)][order(age_code)]

      if (nrow(inc) == 0) return(NULL)

      mort <- df_completo[
        population_code == sel_pais() & type == "Mortality",
        .(cases_2022 = first(cases_2022), pop_2022 = first(pop_2022)),
        by = .(age_code, age)
      ][, age := factor(age, levels = AGE_ORDER)][order(age_code)]

      rate_inc  <- inc$cases_2022  / inc$pop_2022  * 1e5
      rate_mort <- if (nrow(mort) > 0) mort$cases_2022 / mort$pop_2022 * 1e5 else rep(NA_real_, nrow(inc))

      plot_df <- rbind(
        data.frame(age = inc$age,  rate = rate_inc,  type = "Incidence rate"),
        data.frame(age = inc$age,  rate = rate_mort, type = "Mortality rate")
      )
      plot_df$age  <- factor(plot_df$age,  levels = AGE_ORDER)
      plot_df$type <- factor(plot_df$type, levels = c("Incidence rate", "Mortality rate"))

      ggplot(plot_df, aes(x = age, y = rate, fill = type)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.65, na.rm = TRUE) +
        geom_text(
          aes(label = ifelse(!is.na(rate) & rate > 0, round(rate, 1), "")),
          position = position_dodge(width = 0.75),
          vjust = -0.4, size = 2.6, color = "gray30"
        ) +
        scale_fill_manual(
          values = c("Incidence rate" = CC_COLORS$primary,
                     "Mortality rate" = CC_COLORS$forest),
          name = NULL
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(x = NULL, y = "Rate per 100 000 women") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text.x        = element_text(size = 9, angle = 45, hjust = 1, color = "gray30"),
          axis.text.y        = element_text(size = 10, color = "gray30"),
          axis.title.y       = element_text(size = 11, color = "gray40", margin = margin(r = 8)),
          legend.position    = "top",
          legend.text        = element_text(size = 11, color = "gray30"),
          plot.margin        = margin(4, 12, 4, 4)
        )
    }, res = 110)
  })
}
