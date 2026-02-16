# ===========================================================
# Shiny-cc — módulo: 12_mod_resumo_pais.R
#   - Cards de resumo
#   - Gráficos por idade
#   - País vem dos filtros globais (input_global()$pais_sel)
# ===========================================================

mod_resumo_pais_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # --- caso população padrão (GLOBOCAN) ----------------------------
    conditionalPanel(
      condition = "input['filters-pop_mode'] != 'other'",
      div(
        class = "cc-row-eq",
        fluidRow(
          column(
            width = 6,
            div(class = "cc-kpi-card",
                fluidRow(
                  column(
                    9,
                    div(class = "cc-kpi-title", "Total female population,"),
                    div(class = "cc-kpi-title",  textOutput(ns("pop_total_txt"))),
                    div(class = "cc-kpi-foot",  textOutput(ns("pop_total_foot")))
                  ),
                  column(
                    3,
                    div(class = "cc-kpi-icon", HTML("&#128105;"))
                  )
                ))
          ),
          column(
            width = 6,
            div(class = "cc-kpi-card",
                fluidRow(
                  column(
                    9,
                    div(class = "cc-kpi-title",  "Incidence"),
                    div(class = "cc-kpi-title",  textOutput(ns("inc_n_txt"))),
                    div(class = "cc-kpi-subtitle", "Cervical cancer cases"),
                    div(class = "cc-kpi-foot",  textOutput(ns("inc_foot")))
                  ),
                  column(
                    3,
                    div(class = "cc-kpi-icon", HTML("&#127892;")),
                    div(class = "cc-kpi-asr",       textOutput(ns("inc_asr_txt"))),
                    div(class = "cc-kpi-asr-label", "ASR")
                  )
                ))
          )
        )
      ),
      
      div(
        class = "cc-row-eq",
        fluidRow(
          column(
            width = 6,
            div(class = "cc-kpi-card",
                fluidRow(
                  column(
                    9,
                    div(class = "cc-kpi-title",  "Female population aged 50–69 years"),
                    div(class = "cc-kpi-title",  textOutput(ns("pop_5069_txt"))),
                    div(class = "cc-kpi-foot",  textOutput(ns("pop_5069_foot")))
                  ),
                  column(
                    3,
                    div(class = "cc-kpi-icon", HTML("&#128105;"))
                  )
                ))
          ),
          column(
            width = 6,
            div(class = "cc-kpi-card",
                fluidRow(
                  column(
                    9,
                    div(class = "cc-kpi-title",  "Mortality"),
                    div(class = "cc-kpi-title",  textOutput(ns("mort_n_txt"))),
                    div(class = "cc-kpi-subtitle", "Cervical cancer deaths"),
                    div(class = "cc-kpi-foot",  textOutput(ns("mort_foot")))
                  ),
                  column(
                    3,
                    div(class = "cc-kpi-icon", HTML("&#128719;")),
                    div(class = "cc-kpi-asr",       textOutput(ns("mort_asr_txt"))),
                    div(class = "cc-kpi-asr-label", "ASR")
                  )
                ))
          )
        )
      ),
      
      hr(),
      fluidRow(
        column(
          width = 6,
          h4("Female population (2025)"),
          plotOutput(ns("plot_pop"), height = 380)
        ),
        column(
          width = 6,
          h4("Cervical cancer incidence and mortality rates per 100 000 women"),
          plotOutput(ns("plot_rates"), height = 380)
        )
      )
    ),
    
    # --- caso Other population ---------------------------------------
    conditionalPanel(
      condition = "input['filters-pop_mode'] == 'other'",
      fluidRow(
        column(
          12,
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
    
    output$pop_5069_txt <- renderText({
      df <- df_pop()[age %in% c("50-54","55-59","60-64","65-69")]
      x  <- suppressWarnings(as.numeric(df$pop_2025))
      tot <- sum(x, na.rm = TRUE)
      if (is.na(tot) || !is.finite(tot) || tot <= 0) return("–")
      fmt_rate(tot, 0)
    })
    
    
    output$pop_5069_foot <- renderText({
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
      ages  <- df$age
      
      par(mar = c(5, 6, 4, 2))
      bp <- barplot(
        share,
        horiz = TRUE,
        names.arg = ages,
        las = 1,
        xlab = "Percent of total female population",
        col  = CC_COLORS$primary,
        xlim = c(0, max(share, na.rm = TRUE) * 1.15)
      )
      
      text(
        x = share,
        y = bp,
        labels = paste0(round(share, 1), "%"),
        pos = 4, cex = 0.7
      )
    })
    
    # ---- Gráfico taxas ----------------------------------------------
    
    output$plot_rates <- renderPlot({
      inc <- df_completo[
        population_code == sel_pais() &
          type == "Incidence",
        .(
          cases_2022 = first(cases_2022),
          pop_2022   = first(pop_2022)
        ),
        by = .(age_code, age)
      ][
        , age := factor(age, levels = AGE_ORDER)
      ][order(age_code)]
      
      if (nrow(inc) == 0) return(NULL)
      
      mort <- df_completo[
        population_code == sel_pais() &
          type == "Mortality",
        .(
          cases_2022 = first(cases_2022),
          pop_2022   = first(pop_2022)
        ),
        by = .(age_code, age)
      ][
        , age := factor(age, levels = AGE_ORDER)
      ][order(age_code)]
      
      rate_inc <- inc$cases_2022 / inc$pop_2022 * 1e5
      if (nrow(mort) > 0) {
        rate_mort <- mort$cases_2022 / mort$pop_2022 * 1e5
      } else {
        rate_mort <- rep(NA_real_, length(rate_inc))
      }
      
      mat <- rbind(rate_inc, rate_mort)
      rownames(mat) <- c("Incidence", "Mortality")
      
      par(mar = c(8, 4, 4, 2))
      bp <- barplot(
        mat,
        beside = TRUE,
        names.arg = inc$age,
        las = 2,
        cex.names = 0.8,
        ylab = "Rate per 100 000 women",
        col = c(CC_COLORS$primary, CC_COLORS$forest),
        ylim = c(0, max(mat, na.rm = TRUE) * 1.15)
      )
      
      vals <- as.vector(mat)
      good <- !is.na(vals)
      text(
        x = bp[good],
        y = vals[good],
        labels = round(vals[good], 1),
        pos = 3, cex = 0.7
      )
      
      legend(
        "topleft",
        legend = c("Incidence rate", "Mortality rate"),
        fill   = c(CC_COLORS$primary, CC_COLORS$forest),
        bty    = "n"
      )
    })
  })
}
