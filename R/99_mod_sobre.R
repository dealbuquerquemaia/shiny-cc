# ===========================================================
# Shiny-cc — 99_mod_sobre.R
# ===========================================================

mod_sobre_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    div(
      style = "max-width:1100px; margin:0 auto; padding-top:8px;",
      
      h2(
        "About NEEDS-BR",
        style = "color:var(--cc-primary-dark); font-weight:700; margin-bottom:6px;"
      ),
      p(
        style = "font-size:16px; color:var(--cc-gray1); margin-bottom:18px;",
        "An interactive decision-support tool for evidence-informed planning of cervical cancer screening."
      ),
      hr(),
      
      fluidRow(
        column(
          width = 8,
          
          h4("Overview", style = "color:var(--cc-primary-dark); font-weight:600;"),
          p(
            "NEEDS-BR is an interactive web-based platform developed to support evidence-informed planning of cervical cancer screening. ",
            "The tool adapts the IARC Workforce and Equipment Needs Estimator (NEEDS) framework to the Brazilian context, ",
            "translating screening policies and pathway assumptions into projected service volumes, capacity requirements, and resource implications."
          ),
          p(
            "The platform integrates demographic, epidemiological, and administrative health data and allows users to explore scenarios by geography, ",
            "target population, screening strategy, follow-up algorithm, coverage, positivity parameters, and productivity assumptions."
          ),
          p(
            "Its purpose is to help programme managers, researchers, and decision-makers assess expected needs for screening, triage, colposcopy, biopsy, ",
            "and treatment-related procedures, while comparing projected needs with observed service delivery."
          ),
          
          h4("Main features", style = "color:var(--cc-primary-dark); font-weight:600; margin-top:18px;"),
          tags$ul(
            tags$li("Interactive scenario analysis for cytology-based and HPV-based screening strategies."),
            tags$li("Estimation of target populations and annual screening volumes."),
            tags$li("Projection of downstream diagnostic and treatment-related procedures."),
            tags$li("Benchmarking of projected needs against observed service delivery."),
            tags$li("Translation of expected volumes into workforce and equipment requirements."),
            tags$li("Support for national and subnational planning across Brazilian SUS-relevant geographies.")
          ),
          
          h4("Data and assumptions", style = "color:var(--cc-primary-dark); font-weight:600; margin-top:18px;"),
          p(
            "The model combines Brazilian demographic, epidemiological, and administrative data sources. ",
            "Default parameters were informed by literature, official technical materials, and analyses of Brazilian data. ",
            "All results should be interpreted as planning estimates designed to support decision-making, not as direct measures of programme performance."
          )
        ),
        
        column(
          width = 4,
          
          div(
            class = "cc-kpi-card",
            style = "padding:18px;",
            h4("Credits", style = "margin-top:0; color:var(--cc-primary-dark); 
               font-weight:600;"),
            p(tags$strong("Authors:"), br(),
              "Fernando Henrique de Albuquerque Maia", br(),
              "Andre Carvalho", br(),
              "Beatriz Jardim"
            ),
            p(tags$strong("Affiliations:"), br(),
              "International Agency for Research on Cancer (IARC)", br(),
              "Faculty of Public Health, University of São Paulo"
            ),
            p(tags$strong("Version:"), "1.0"),
            p(tags$strong("Last update:"), format(Sys.Date(), "%d/%m/%Y")),
            p(tags$strong("Funding:"), "This research was supported by the São 
              Paulo Research Foundation, FAPESP. Grant numbers: 2025/08308-1; 
              2025/00444-3; 2021/11794-4. "), 
            tags$img(src = "logo-iarc.svg", style = "max-height:45px; height:100%; width:auto"),
            
            tags$img(src = "logo-fsp.svg", style = "max-height:45px; height:100%; width:auto"),
            
            tags$img(src = "logo-ConeCta-SP.png", style = "max-height:45px; height:100%; width:auto")
                      
            
            
          )
        )
      )
    )
  )
}

mod_sobre_server <- function(input, output, session) {}