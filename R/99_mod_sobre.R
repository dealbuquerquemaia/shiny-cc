# ===========================================================
# Shiny-cc — 99_mod_sobre.R
# ===========================================================

mod_sobre_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("About Shiny-cc"),
      hr(),
    p(em("Authors:"), "Fernando Henrique de Albuquerque Maia, Andre Carvalho, Beatriz Jardim"),
    p(em("Last Update:"), format(Sys.Date(), "%d/%m/%Y"))
  )
}

mod_sobre_server <- function(input, output, session) {}
