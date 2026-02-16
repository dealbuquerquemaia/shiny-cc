# ===========================================================
# Shiny-CC — app.R (v1.0)
# ===========================================================

suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(grid)
  library(DiagrammeR)
})



# Utilitários e constantes
source("R/00_constants_cc.R")
source("R/01_utils_cc.R")
source("R/02_engine_capacity_cc.R")

# ===========================================================
# Dados (saída agora em .rds)
# ===========================================================

df_cc_completo <- readRDS("data/df_cc_completo.rds")
df_cc_taxas    <- readRDS("data/df_cc_taxas.rds")
df_dim_country             <- readRDS("data/df_dim_country.rds")
df_dim_age                 <- readRDS("data/df_dim_age.rds")
df_dim_type                <- readRDS("data/df_dim_type.rds")
df_dim_year                <- readRDS("data/df_dim_year.rds")

# --- Bases Brasil / SUS (saídas do 02_prepare_BR e 03_prepare_SUS) -----
pop_municipio_faixas           <- readRDS("data/pop_municipio_faixas.rds")
pop_municipio_faixas_total_sus <- readRDS("data/pop_municipio_faixas_total_sus.rds")
pop_municipio_regional         <- readRDS("data/pop_municipio_regional.rds")
regional_sus_map               <- readRDS("data/regional_sus_map.rds")
sia_cc_resumo                  <- readRDS("data/sus_proc_resumo.rds")   

# data.table
setDT(df_cc_completo)
setDT(df_cc_taxas)
setDT(df_dim_country)
setDT(df_dim_age)
setDT(df_dim_type)
setDT(df_dim_year)
setDT(sia_cc_resumo)
setDT(pop_municipio_faixas)
setDT(pop_municipio_faixas_total_sus)
setDT(pop_municipio_regional)
setDT(regional_sus_map)

br_code <- df_dim_country[population_name == "Brazil", population_code][1]
if (is.na(br_code)) br_code <- 1001L   # fallback coerente com o resto do app



# Módulos
source("R/10_mod_filters_cc.R") 
source("R/11_mod_resumo_geral.R")
source("R/12_mod_resumo_pais.R")
source("R/14_mod_equipamentos.R") 
source("R/15_mod_fluxo.R")
source("R/16_mod_capacidade.R")
source("R/99_mod_sobre.R")

# ===========================================================
# UI
# ===========================================================

ui <- tagList(
  tags$head(
    tags$style(HTML("
      :root{
        --cc-primary: #4ABDAC;
        --cc-primary-dark: #0B7285;
        --cc-petrol: #0F6B7A;
        --cc-forest: #2B7A5E;
        --cc-primary-darker: #094B59;
        --cc-primary-light: #D9F3F2;

        --cc-text: #111111;
        --cc-gray1: #343A40;
        --cc-gray2: #868E96;
        --cc-bg: #F1F3F5;
        --cc-white: #FFFFFF;
      }

      body { background-color: var(--cc-bg); color: var(--cc-text); }

      /* Navbar sempre visível (sem criar espaço extra) */
      .navbar {
        position: sticky;
        top: 0;
        z-index: 3000;
      }

      /* Botão hambúrguer na navbar */
      .cc-hamburger-btn {
        margin-right: 10px;
        background-color: var(--cc-primary) !important;
        border-color: var(--cc-primary) !important;
        color: var(--cc-white) !important;
      }
      .cc-hamburger-btn:hover {
        background-color: var(--cc-primary-dark) !important;
        border-color: var(--cc-primary-dark) !important;
      }

      /* Painel lateral (off-canvas) */
      #cc-settings-panel {
        position: fixed;
        top: 51px;
        left: -320px;
        width: 300px;
        height: calc(100% - 51px);
        background-color: var(--cc-white);
        box-shadow: 2px 0 6px rgba(0,0,0,0.15);
        padding: 15px;
        z-index: 2000;
        overflow-y: auto;
        transition: left 0.25s ease-in-out;
      }
      #cc-settings-panel.cc-open { left: 0; }

      #cc-settings-overlay {
        position: fixed;
        top: 51px;
        left: 0;
        width: 100%;
        height: calc(100% - 51px);
        background: rgba(0,0,0,0.35);
        z-index: 1500;
        display: none;
      }
      #cc-settings-overlay.cc-open { display: block; }

      /* Acordeão do menu lateral */
      .cc-section-header {
        font-weight: 700;
        color: var(--cc-primary-darker);
        cursor: pointer;
        margin-top: 10px;
        margin-bottom: 4px;
      }
      .cc-section-header::before {
        content: '▸';
        display: inline-block;
        margin-right: 5px;
      }
      .cc-section.cc-open .cc-section-header::before {
        content: '▾';
      }

      .cc-section-body {
        display: none;
        margin-left: 12px;
        margin-bottom: 8px;
      }
      .cc-section.cc-open .cc-section-body {
        display: block;
      }

      /* Sub-acordeão (Age >=50 / <50) dentro de .cc-section-body */
      .cc-subsection {
        margin-left: 0;
        margin-top: 6px;
        margin-bottom: 6px;
      }

      .cc-subsection > summary {
        list-style: none;
        cursor: pointer;
        font-weight: 700;
        color: var(--cc-primary-darker);
        margin: 6px 0 4px 0;
        outline: none;
      }

      .cc-subsection > summary::-webkit-details-marker { display: none; }

      .cc-subsection > summary::before {
        content: '▸';
        display: inline-block;
        margin-right: 5px;
      }
      .cc-subsection[open] > summary::before {
        content: '▾';
      }

      .cc-subsection > .cc-subsection-body {
        margin-left: 12px;
        margin-bottom: 8px;
      }

      /* Cards de epidemiologia */
      .cc-kpi-card {
        background-color: var(--cc-white);
        border-radius: 10px;
        padding: 12px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.06);
        margin-bottom: 12px;
        word-wrap: break-word;
        white-space: normal;
      }

      .cc-row-eq > .row {
        display: flex;
        flex-wrap: wrap;
      }

      .cc-row-eq > .row > [class*='col-'] {
        display: flex;
      }

      .cc-row-eq .cc-kpi-card {
        width: 100%;
      }

      .cc-kpi-title {
        color: var(--cc-primary-dark);
        font-size: 24px;
        font-weight: 700;
        margin-bottom: 4px;
      }
      .cc-kpi-subtitle {
        color: var(--cc-gray1);
        font-size: 13px;
      }
      .cc-kpi-foot {
        color: var(--cc-gray2);
        font-size: 11px;
        margin-top: 6px;
        font-style: italic;
      }
      .cc-kpi-icon {
        font-size: 34px;
        color: var(--cc-primary);
        text-align: right;
      }
      .cc-kpi-asr {
        font-size: 22px;
        font-weight: 700;
        color: var(--cc-primary-dark);
        text-align: right;
      }
      .cc-kpi-asr-label {
        font-size: 11px;
        color: var(--cc-gray2);
        text-align: right;
      }

      /* Summary (módulo) - centralizado aqui */
      .cards-ccu-wrap{
        display:flex;
        flex-wrap:wrap;
        gap:10px;
        align-items:stretch;
      }
      .card-ccu{
        background: var(--cc-primary);
        color: var(--cc-white);
        padding:12px 14px;
        border-radius:12px;
        min-width:220px;
        box-shadow:0 1px 2px rgba(0,0,0,0.08);
      }
      .ccu-scope{
        margin:6px;
        padding:12px;
        border-radius:10px;
        border:1px solid #ddd;
        background: var(--cc-primary-light);
      }
      .ccu-section-title{
        margin:18px 6px 10px 6px;
        font-weight:600;
        color: var(--cc-primary-darker);
      }
      .ccu-note{
        margin:8px 6px 0 6px;
        color: var(--cc-primary-dark);
        font-size:12px;
      }
      .cc-card-title{
  font-size:20px;
  font-weight:700;
  margin-bottom:6px;
}
.cc-card-sub{
  font-size:14px;
}
.cc-card-kpi{
  font-size:22px;
  font-weight:700;
  margin-top:6px;
}
      
    ")),
    
    # =================== CSS (tooltips) ===================
    tags$style(HTML("
      /* TOOLTIP GLOBAL (Bootstrap) */
      .tooltip .tooltip-inner{
        background-color: var(--cc-primary-dark);
        color: var(--cc-white);
        font-size: 12.5px;
        line-height: 1.25;
        padding: 8px 10px;
        border-radius: 8px;
        max-width: 340px;
      }

      /* seta (arrow) */
      .tooltip.top .tooltip-arrow{ border-top-color: var(--cc-primary-darker) !important; }
      .tooltip.right .tooltip-arrow{ border-right-color: var(--cc-primary-darker) !important; }
      .tooltip.bottom .tooltip-arrow{ border-bottom-color: var(--cc-primary-darker) !important; }
      .tooltip.left .tooltip-arrow{ border-left-color: var(--cc-primary-darker) !important; }
    ")),
    
    # =================== JS ===================
    tags$script(HTML("
      $(function(){

        // abre/fecha painel lateral
        $('#cc_hamburger').on('click', function(){
          $('#cc-settings-panel').toggleClass('cc-open');
          $('#cc-settings-overlay').toggleClass('cc-open');
        });

        $('#cc-settings-overlay').on('click', function(){
          $('#cc-settings-panel').removeClass('cc-open');
          $('#cc-settings-overlay').removeClass('cc-open');
        });

        // acordeão: abre/fecha seções internas
        $('#cc-settings-panel').on('click', '.cc-section-header', function(){
          var sec = $(this).closest('.cc-section');
          sec.toggleClass('cc-open');
        });

        // tooltips (bootstrap)
        function cc_init_tooltips(){
          $('[data-toggle=tooltip]').tooltip({ container: 'body' });
        }

        cc_init_tooltips();
        $(document).on('shiny:connected', cc_init_tooltips);
        $(document).on('shiny:value',     cc_init_tooltips);

      });
    "))
  ),

  
  
  # Navbar principal -----------------------------------------------------
  navbarPage(
    title = div(
      actionButton("cc_hamburger", label = "\u2630", class = "cc-hamburger-btn"),
      span(textOutput("app_title"),
           style = "display:inline-block; padding-left:8px;")
    ),
    id = "nav",
    inverse = FALSE,
    tabPanel("Summary",      mod_resumo_geral_ui("sum")),
    tabPanel("Capacity",      mod_capacity_ui("capacidade")),
    tabPanel("Epidemiology", mod_resumo_pais_ui("epi")),
    tabPanel("Equipment & HR needs",  mod_equipment_ui("equip")),
    tabPanel("Pathway",      mod_fluxo_ui("flow")),
    tabPanel("About",        mod_sobre_ui("sobre"))
  ),
  
  # Overlay para fechar clicando fora -----------------------------------
  div(id = "cc-settings-overlay"),
  
  # Painel lateral de configurações globais -----------------------------
  div(
    id = "cc-settings-panel",
    mod_filters_cc_ui("filters", df_dim_country, br_code = br_code)
    
  )
)
    
  


# ===========================================================
# SERVER
# ===========================================================

server <- function(input, output, session) {
  
  # ====== Filtros globais (módulo) ==========================
  input_global <- mod_filters_cc_server(
    id                     = "filters",
    pop_municipio_regional = pop_municipio_regional,
    br_code                = br_code
  )
  
  # título dinâmico na navbar
  output$app_title <- renderText({
    f <- input_global()
    
    code <- if (is.null(f$pais_sel)) 1001L else as.integer(f$pais_sel)
    
    nm <- df_dim_country[population_code == code, population_name][1]
    if (is.na(nm) || nm == "") nm <- "World"
    
    paste("Cervical Cancer Screening Dashboard -", nm)
  })

  
  # ===== Módulos principais ===========================================
  mod_resumo_geral_server(
    id               = "sum",
    df_completo      = df_cc_completo,
    dim_age          = df_dim_age,
    dim_country      = df_dim_country,
    input_global     = input_global,
    pop_mun_regional = pop_municipio_regional
  )
  
  mod_fluxo_server(
    id               = "flow",
    df_completo      = df_cc_completo,
    dim_age          = df_dim_age,
    dim_country      = df_dim_country,
    input_global     = input_global,
    pop_mun_regional = pop_municipio_regional
  )
  
  mod_resumo_pais_server(
    id           = "epi",
    df_completo  = df_cc_completo,
    df_taxas     = df_cc_taxas,
    dim_country  = df_dim_country,
    input_global = input_global
  )
  
  mod_equipment_server(
    id               = "equip",
    df_completo      = df_cc_completo,
    dim_age          = df_dim_age,
    dim_country      = df_dim_country,
    input_global     = input_global,
    pop_mun_regional = pop_municipio_regional
  )
  
  mod_sobre_server("sobre")
  

  mod_capacity_server(
    id               = "capacidade",
    df_completo      = df_cc_completo,
    dim_country      = df_dim_country,
    input_global     = input_global,
    pop_mun_regional = pop_municipio_regional,
    sia_cc_resumo    = sia_cc_resumo,
    regional_sus_map = regional_sus_map
  )
  
  
  
 
  
}




shinyApp(ui, server)
