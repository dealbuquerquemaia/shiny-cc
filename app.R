# ===========================================================
# Shiny-CC — app.R (v1.2)
# -----------------------------------------------------------
# Entry point do dashboard de rastreamento de câncer do colo
# do útero (CCU). Responsabilidades deste arquivo:
#   1. Carregar pacotes e scripts R/ (constantes, utils, engine, módulos)
#   2. Ler os .rds de data/ e converter para data.table quando aplicável
#   3. Montar a UI (navbarPage com 10 abas + sidebar de filtros globais)
#   4. No server: instanciar o módulo de filtros (produz input_global)
#      e propagá-lo para cada módulo de aba
#   5. Implementar o export de PDF (rerroda o engine e renderiza
#      www/report_template.Rmd via pagedown)
#
# Dados consumidos: ver seção "Dados" abaixo (todos vêm de data-raw/).
# Documentação: README.md (seção "Arquivos") e docs/INVENTORY.md
# ===========================================================

suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(ggplot2)
  library(grid)
  library(DiagrammeR)
  library(leaflet)
  library(sf)
  library(htmltools)
  library(DT)
})



# Utilitários e constantes
source("R/00_constants_cc.R")
source("R/01_utils_cc.R")
source("R/02_engine_capacity_cc.R")

# ===========================================================
# Dados (todos os .rds são produzidos pelos scripts de data-raw/)
# Carregados uma vez na inicialização do app e usados via closure.
# ===========================================================

# --- Bases globais GLOBOCAN (produzidas por data-raw/01_prepare_cc.R) ----
df_cc_completo <- readRDS("data/df_cc_completo.rds")   # incidência/mortalidade/população por país × faixa etária
df_cc_taxas    <- readRDS("data/df_cc_taxas.rds")      # taxas (incidência/mortalidade ASR-world) por país
df_dim_country <- readRDS("data/df_dim_country.rds")   # dimensão país: population_code ↔ population_name
df_dim_age     <- readRDS("data/df_dim_age.rds")       # dimensão faixas etárias
df_dim_type    <- readRDS("data/df_dim_type.rds")      # dimensão tipo de métrica (incidência, mortalidade)
df_dim_year    <- readRDS("data/df_dim_year.rds")      # dimensão ano

# --- Bases Brasil / SUS (produzidas por data-raw/02_prepare_BR e 03_prepare_SUS) ----
pop_municipio_faixas           <- readRDS("data/pop_municipio_faixas.rds")            # pop fem por município × faixa etária (IBGE)
pop_municipio_faixas_total_sus <- readRDS("data/pop_municipio_faixas_total_sus.rds")  # idem, versão SUS-dependente (IBGE − ANS)
pop_municipio_regional         <- readRDS("data/pop_municipio_regional.rds")          # pop + mapeamento município → região de saúde → macro → UF
regional_sus_map               <- readRDS("data/regional_sus_map.rds")                # mapeamento geográfico (sem população)
sia_cc_resumo                  <- readRDS("data/sus_proc_resumo.rds")                 # produção SIA 2024 (citologia, colposcopia, biópsia, EZT)
cito_presets                   <- readRDS("data/cito_presets.rds")                    # parâmetros de citologia (INCA 2019 + SISCAN por UF)
peers_data                     <- readRDS("data/peers.rds")                           # países peers (para aba Peer Analysis)

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
setDT(peers_data)

# --- Geometrias Brasil (sf, produzidas por data-raw/08_prepare_geometrias.R) ----
# São objetos `sf` já simplificados (rmapshaper) para performance no leaflet.
geo_municipios    <- readRDS("data/geo_municipios.rds")     # polígonos dos municípios
geo_regioes_saude <- readRDS("data/geo_regioes_saude.rds")  # polígonos das regiões de saúde
geo_macrorregioes <- readRDS("data/geo_macrorregioes.rds")  # polígonos das macrorregiões de saúde
geo_estados       <- readRDS("data/geo_estados.rds")        # polígonos das UFs

# Código GLOBOCAN do Brasil — usado como default quando o país não está selecionado
# e como flag (is_br) que ativa funcionalidades subnacionais nos módulos.
br_code <- df_dim_country[population_name == "Brazil", population_code][1]
if (is.na(br_code)) br_code <- 1001L   # fallback coerente com o resto do app



# Módulos
source("R/10_mod_filters_cc.R") 
source("R/11_mod_resumo_geral.R")
source("R/12_mod_resumo_pais.R")
source("R/14_mod_equipamentos.R") 
source("R/15_mod_fluxo.R")
source("R/16_mod_capacidade.R")
source("R/17_mod_compare.R")
source("R/18_mod_maps.R")
source("R/19_mod_peers.R")
source("R/20_mod_tabela_detalhada.R")
source("R/99_mod_sobre.R")

# ===========================================================
# UI
# ===========================================================

ui <- tagList(
  tags$head(
    tags$link(
      rel  = "preconnect",
      href = "https://fonts.googleapis.com"
    ),
    tags$link(
      rel         = "stylesheet",
      href        = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap",
      crossorigin = NA
    ),
    
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),

    # =================== JS ===================
    tags$script(HTML("
      $(function(){

        function cc_set_arrow(){
          if ($('body').hasClass('cc-collapsed')) {
            $('#cc_sidebar_toggle').text('❯');
          } else {
            $('#cc_sidebar_toggle').text('❮');
          }
        }

        // toggle pela seta
        $(document).on('click', '#cc_sidebar_toggle', function(){
          $('body').toggleClass('cc-collapsed');
          cc_set_arrow();
        });

        // opcional: hamburger faz a mesma função (pode manter)
        $('#cc_hamburger').on('click', function(){
          $('body').toggleClass('cc-collapsed');
          cc_set_arrow();
        });

        cc_set_arrow();

        // acordeão: abre/fecha seções internas
        $('#cc-settings-panel').on('click', '.cc-section-header', function(){
          var sec = $(this).closest('.cc-section');
          sec.toggleClass('cc-open');
        });

        // toggle area de parametros avancados
        $(document).on('click', '.cc-param-area-toggle', function(){
          $(this).closest('.cc-param-area').toggleClass('cc-open');
        });

        // ao selecionar Customize, abre automaticamente a area de parametros
        $(document).on('change', 'input[name$=hpv_param_source], input[name$=cito_param_source]', function(){
          var isCustom = $(this).val() === 'custom';
          var paramArea = $(this).closest('.cc-section-body').find('.cc-param-area');
          if (isCustom) {
            paramArea.addClass('cc-open');
          } else {
            paramArea.removeClass('cc-open');
          }
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
      style = "display:flex; align-items:center; width:100%;",
      
      # hambúrguer 
      actionButton("cc_hamburger", label = "\u2630", class = "cc-hamburger-btn"),
      
      # título dinâmico 
      span(
        textOutput("app_title"),
        style = "display:inline-block; padding-left:8px;"
      ),
      
      
      
    ),
    
    id      = "nav",
    inverse = FALSE,
    # REMOVER o argumento `header = ...` que existia aqui
    
    tabPanel("Summary",              mod_resumo_geral_ui("sum")),
    tabPanel("Compare",              mod_compare_ui("comp")),
    tabPanel("Pathway",              mod_fluxo_ui("flow")),
    tabPanel("Epidemiology",         mod_resumo_pais_ui("epi")),
    tabPanel("Capacity",             mod_capacity_ui("capacidade")),
    tabPanel("Equipment & HR needs", mod_equipment_ui("equip")),
    tabPanel("Peer Analysis",        peers_ui("peers")),
    tabPanel("Maps",                 mod_maps_ui("maps")),
    tabPanel("Detailed table",       mod_detailed_table_ui("detailed")),
    tabPanel("About",                mod_sobre_ui("sobre"))
  ),
  

  # Painel lateral de configurações globais -----------------------------
  div(
    id = "cc-settings-panel",
    
    div(
      class = "cc-sidebar-top",
      tags$button(id = "cc_sidebar_toggle", type = "button", class = "cc-sidebar-toggle", HTML("❮"))
    ),
    
    mod_filters_cc_ui("filters", df_dim_country, br_code = br_code),
    
    div(id = "cc-download-wrap",
        downloadButton("btn_download_pdf", label = tagList(
          tags$svg(xmlns="http://www.w3.org/2000/svg", viewBox="0 0 24 24",
                   width="12", height="12", fill="none", stroke="currentColor",
                   `stroke-width`="2", `stroke-linecap`="round", `stroke-linejoin`="round",
                   tags$path(d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"),
                   tags$polyline(points="7 10 12 15 17 10"),
                   tags$line(x1="12", y1="15", x2="12", y2="3")),
          " Download report"
        ))
    )

  )
)
    
  


# ===========================================================
# SERVER
# ===========================================================

server <- function(input, output, session) {

  # ====== Filtros globais (módulo) ==========================
  # O módulo de filtros produz um reactive `input_global()` que encapsula
  # TODAS as seleções do usuário (país, cobertura, método, faixa etária,
  # parâmetros HPV/citologia, filtros geográficos subnacionais e capacidades).
  # Esse reactive é passado para cada módulo de aba — ou seja, nenhum outro
  # módulo tem inputs próprios de seleção geográfica/protocolo.
  input_global <- mod_filters_cc_server(
    id                     = "filters",
    pop_municipio_regional = pop_municipio_regional,
    cito_presets           = cito_presets,
    br_code                = br_code
  )
  
  # rodapé de status da sidebar
  output$sidebar_status <- renderText({
    f <- input_global()
    country <- df_dim_country[population_code == as.integer(f$pais_sel), population_name][1]
    if (is.na(country)) country <- "World"
    method <- if (identical(f$screen_method, "cytology")) "Cytology" else "HPV"
    paste0(country, " · ", method, " · ", f$coverage, "%")
  })

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

  peers_server(
    id           = "peers",
    peers_data   = peers_data,
    input_global = input_global
  )

  mod_maps_server(
    id                    = "maps",
    sia_cc_resumo         = sia_cc_resumo,
    pop_municipio_regional = pop_municipio_regional,
    geo_municipios        = geo_municipios,
    geo_regioes_saude     = geo_regioes_saude,
    geo_macrorregioes     = geo_macrorregioes,
    geo_estados           = geo_estados,
    input_global          = input_global,
    br_code               = br_code
  )

  # ── PDF export ────────────────────────────────────────────────────────
  # O export de PDF NÃO reaproveita os outputs dos módulos — rerroda o engine
  # independentemente (ver `pdf_data` reactive abaixo) para garantir um snapshot
  # consistente em um único ponto no tempo. Depois renderiza `www/report_template.Rmd`
  # e converte para PDF via pagedown::chrome_print.
  #
  # NOTA: os helpers abaixo (.fmt_safe, .val_or, .num1, .cap1, .req_n, .demand_cito,
  # .demand_patol, .fmt_rate_safe) são cópias locais de helpers que também existem
  # em outros módulos. Candidatos a consolidação em R/01_utils_cc.R
  # (ver docs/INVENTORY.md → pendências).

  # Helpers (mirrors module helpers, used only in PDF export path)
  .fmt_safe <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (!is.finite(x) || is.na(x)) return("\u2013")
    fmt_int(round(x))
  }
  .fmt_rate_safe <- function(x, digits = 1) {
    x <- suppressWarnings(as.numeric(x))
    if (!is.finite(x) || is.na(x)) return("\u2013")
    fmt_rate(x, digits)
  }
  .val_or <- function(x, default) {
    if (is.null(x) || length(x) == 0L || all(is.na(x))) default else x
  }
  .num1 <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (!is.finite(x) || is.na(x) || x < 0) 0 else x
  }
  .cap1 <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (!is.finite(x) || is.na(x) || x <= 0) NA_real_ else x
  }
  .req_n <- function(demand, cap_year) {
    if (!is.finite(cap_year) || is.na(cap_year) || cap_year <= 0) return(NA_real_)
    ceiling(demand / cap_year)
  }
  .demand_cito <- function(m) {
    if (is.null(m) || !nrow(m)) return(0)
    if (all(c("cit_rastreamento", "cit_diagnostica") %chin% names(m)))
      return(.num1(m$cit_rastreamento[1]) + .num1(m$cit_diagnostica[1]))
    if ("cito_reflexa" %chin% names(m)) return(.num1(m$cito_reflexa[1]))
    0
  }
  .demand_patol <- function(m) {
    if (is.null(m) || !nrow(m)) return(0)
    if ("ap_biopsia" %chin% names(m) || "ap_peca_cir" %chin% names(m))
      return(.num1(m$ap_biopsia[1]) + .num1(m$ap_peca_cir[1]))
    if ("biopsia_indicada" %chin% names(m)) return(.num1(m$biopsia_indicada[1]))
    0
  }

  pdf_data <- reactive({
    g <- input_global()

    # ── 1. Engine (same logic as Summary module) ──────────────────────
    country_code_pdf <- suppressWarnings(as.integer(.val_or(g$pais_sel, br_code)))
    if (is.na(country_code_pdf)) country_code_pdf <- br_code

    pop_mode <- as.character(.val_or(g$pop_mode, "globocan"))
    if (!pop_mode %in% c("globocan", "other")) pop_mode <- "globocan"

    coverage <- suppressWarnings(as.numeric(.val_or(g$coverage, 70)))
    if (!is.finite(coverage)) coverage <- 70

    screen_method <- as.character(.val_or(g$screen_method, "hpv"))
    if (!screen_method %in% c("hpv", "cytology")) screen_method <- "hpv"

    age_min <- suppressWarnings(as.numeric(.val_or(g$target_age_min, 25)))
    age_max <- suppressWarnings(as.numeric(.val_or(g$target_age_max, 64)))
    if (!is.finite(age_min)) age_min <- 25
    if (!is.finite(age_max)) age_max <- 64

    custom_pop <- NA_real_
    if (identical(pop_mode, "other"))
      custom_pop <- suppressWarnings(as.numeric(.val_or(g$custom_pop, NA_real_)))

    is_br       <- isTRUE(!is.na(country_code_pdf) && country_code_pdf == br_code)
    br_pop_tipo <- as.character(.val_or(g$br_pop_tipo, "total"))
    if (!br_pop_tipo %in% c("total", "sus")) br_pop_tipo <- "total"

    cfg_pdf <- tryCatch(cc_engine_settings(
      country_code      = country_code_pdf,
      pop_mode          = pop_mode,
      coverage          = coverage,
      screen_method     = screen_method,
      target_age_min    = age_min,
      target_age_max    = age_max,
      custom_pop        = custom_pop,
      p16_18            = .val_or(g$p16_18, NA_real_),
      poutros           = .val_or(g$poutros, NA_real_),
      pneg              = .val_or(g$pneg, NA_real_),
      cito_out_pos      = .val_or(g$cito_out_pos, NA_real_),
      cito_out_neg      = .val_or(g$cito_out_neg, NA_real_),
      colpo16_pos       = .val_or(g$colpo16_pos, NA_real_),
      colpo16_neg       = .val_or(g$colpo16_neg, NA_real_),
      colpoout_pos      = .val_or(g$colpoout_pos, NA_real_),
      colpoout_neg      = .val_or(g$colpoout_neg, NA_real_),
      b16_neg_nic1      = .val_or(g$b16_neg_nic1, NA_real_),
      b16_nic23         = .val_or(g$b16_nic23, NA_real_),
      b16_cancer        = .val_or(g$b16_cancer, NA_real_),
      bo_neg_nic1       = .val_or(g$bo_neg_nic1, NA_real_),
      bo_nic23          = .val_or(g$bo_nic23, NA_real_),
      bo_cancer         = .val_or(g$bo_cancer, NA_real_),
      first_time_pct         = .val_or(g$first_time_pct, NA_real_),
      unsatisfactory_pct     = .val_or(g$unsatisfactory_pct, NA_real_),
      res_asch_pct           = .val_or(g$res_asch_pct, NA_real_),
      res_other_pct          = .val_or(g$res_other_pct, NA_real_),
      res_neg_pct            = .val_or(g$res_neg_pct, NA_real_),
      colpo_asch_pct         = .val_or(g$colpo_asch_pct, NA_real_),
      colpo_other_follow_pct = .val_or(g$colpo_other_follow_pct, NA_real_),
      biopsy_pos_asch_pct    = .val_or(g$biopsy_pos_asch_pct, NA_real_),
      biopsy_pos_other_pct   = .val_or(g$biopsy_pos_other_pct, NA_real_),
      b_asch_nic23_pct       = .val_or(g$b_asch_nic23_pct, NA_real_),
      b_asch_cancer_pct      = .val_or(g$b_asch_cancer_pct, NA_real_),
      b_asch_neg_nic1_pct    = .val_or(g$b_asch_neg_nic1_pct, NA_real_),
      b_other_nic23_pct      = .val_or(g$b_other_nic23_pct, NA_real_),
      b_other_cancer_pct     = .val_or(g$b_other_cancer_pct, NA_real_),
      b_other_neg_nic1_pct   = .val_or(g$b_other_neg_nic1_pct, NA_real_),
      cap_colpo_device  = .val_or(g$cap_colpo_device, 5760),
      cap_colpo_med     = .val_or(g$cap_colpo_med,    2880),
      cap_citopato      = .val_or(g$cap_citopato,     14400),
      cap_patol_med     = .val_or(g$cap_patol_med,    7200),
      is_brazil         = is_br,
      br_pop_tipo       = br_pop_tipo,
      filt_uf           = .val_or(g$filt_uf, NULL),
      filt_macro        = .val_or(g$filt_macro, NULL),
      filt_reg          = .val_or(g$filt_reg, NULL),
      filt_mun          = .val_or(g$filt_mun, NULL)
    ), error = function(e) NULL)

    if (is.null(cfg_pdf)) return(NULL)

    res_pdf <- tryCatch(
      cc_engine_run(df_cc_completo, cfg_pdf, pop_mun_regional = pop_municipio_regional),
      error = function(e) NULL
    )
    if (is.null(res_pdf)) return(NULL)

    dt_pdf <- tryCatch(cc_engine_summary_dt(res_pdf), error = function(e) NULL)
    if (is.null(dt_pdf)) return(NULL)

    m_pdf <- data.table::as.data.table(res_pdf$metrics)

    # ── 2. Summary values ─────────────────────────────────────────────
    method <- as.character(dt_pdf$screen_method[1])
    if (!method %in% c("hpv", "cytology")) method <- "hpv"

    is_hpv <- identical(method, "hpv")
    cito_label <- if (is_hpv) "Reflex cytology" else "Diagnostic cytology"
    cito_val   <- if (is_hpv) dt_pdf$cito_reflexa[1] else dt_pdf$cit_diagnostica[1]
    fu_test_label <- if (is_hpv) "Follow-up HPV test" else "Follow-up cytologies"
    fu_test_val   <- if (is_hpv) dt_pdf$retorno_1ano[1] else dt_pdf$followup_cytologies[1]
    fu_colpo_val  <- if (is_hpv) dt_pdf$followup_colposcopy[1] else dt_pdf$followup_colposcopies[1]

    # ── 3. Equipment rows ─────────────────────────────────────────────
    d_colpo <- if ("colpo_indicada" %chin% names(m_pdf)) .num1(m_pdf$colpo_indicada[1]) else 0
    d_cito  <- .demand_cito(m_pdf)
    d_ap    <- .demand_patol(m_pdf)

    cap_dev <- .cap1(cfg_pdf$cap_colpo_device)
    cap_med <- .cap1(cfg_pdf$cap_colpo_med)
    cap_cit <- .cap1(cfg_pdf$cap_citopato)
    cap_pat <- .cap1(cfg_pdf$cap_patol_med)

    equip_rows <- list(
      list(label = "Colposcope",              demand = .fmt_safe(d_colpo), cap = .fmt_safe(cap_dev), required = .fmt_safe(.req_n(d_colpo, cap_dev))),
      list(label = "Colposcopist (20h/week)", demand = .fmt_safe(d_colpo), cap = .fmt_safe(cap_med), required = .fmt_safe(.req_n(d_colpo, cap_med))),
      list(label = "Cytopathologist",         demand = .fmt_safe(d_cito),  cap = .fmt_safe(cap_cit), required = .fmt_safe(.req_n(d_cito,  cap_cit))),
      list(label = "Pathologist (20h/week)",  demand = .fmt_safe(d_ap),    cap = .fmt_safe(cap_pat), required = .fmt_safe(.req_n(d_ap,    cap_pat)))
    )

    # ── 4. Epidemiology ───────────────────────────────────────────────
    df_tax_pdf <- tryCatch(
      df_cc_taxas[population_code == country_code_pdf],
      error = function(e) data.table::data.table()
    )
    inc_n    <- if (nrow(df_tax_pdf)) df_tax_pdf$number_incidence[1]    else NA_real_
    inc_asr  <- if (nrow(df_tax_pdf)) df_tax_pdf$incidence_asr_world[1] else NA_real_
    mort_n   <- if (nrow(df_tax_pdf)) df_tax_pdf$number_mortality[1]    else NA_real_
    mort_asr <- if (nrow(df_tax_pdf)) df_tax_pdf$mortality_asr_world[1] else NA_real_

    # ── 5. Capacity (Brazil only) ─────────────────────────────────────
    cap_rows <- list()
    if (is_br) {
      geo_ref_sel <- .val_or(g$sia_geo_ref, "care")
      dt_sia <- tryCatch({
        dt_s <- data.table::as.data.table(sia_cc_resumo)
        dt_s <- dt_s[geo_ref == geo_ref_sel]
        if (!is.null(g$filt_uf)    && length(g$filt_uf))    dt_s <- dt_s[UF %in% g$filt_uf]
        if (!is.null(g$filt_macro) && length(g$filt_macro)) dt_s <- dt_s[`Macrorregiao de Saude` %in% g$filt_macro]
        if (!is.null(g$filt_reg)   && length(g$filt_reg))   dt_s <- dt_s[`Regiao de Saude` %in% g$filt_reg]
        if (!is.null(g$filt_mun)   && length(g$filt_mun))   dt_s <- dt_s[Municipio %in% g$filt_mun]
        dt_s
      }, error = function(e) NULL)

      sia_cap <- if (!is.null(dt_sia) && nrow(dt_sia))
        tryCatch(cc_capacity_from_sia(dt_sia, ano_cmp_ref = 2024L), error = function(e) NULL)
      else NULL

      if (!is.null(sia_cap)) {
        comp <- tryCatch(mod_capacity_compare(res_pdf, sia_cap), error = function(e) NULL)
        if (!is.null(comp) && nrow(comp)) {
          comp <- comp[item %chin% c("citologia_total", "colposcopia_total", "biopsia_total", "tratamento_total")]
          labels_map <- c(
            citologia_total   = "Cytology",
            colposcopia_total = "Colposcopy",
            biopsia_total     = "Biopsies",
            tratamento_total  = "EZT"
          )
          for (i in seq_len(nrow(comp))) {
            r <- comp[i]
            cap_rows[[i]] <- list(
              label   = labels_map[[r$item]],
              actual  = .fmt_safe(r$realized),
              needed  = .fmt_safe(r$needed),
              pct_num = suppressWarnings(as.numeric(r$coverage_percent))
            )
          }
        }
      }
    }

    # ── 6. Identity & assumptions ─────────────────────────────────────
    country_nm <- tryCatch({
      z <- df_dim_country[population_code == country_code_pdf, population_name][1]
      if (is.na(z)) "World" else as.character(z)
    }, error = function(e) "World")

    geo_parts <- country_nm
    if (is_br) {
      pop_lbl <- if (br_pop_tipo == "sus") "SUS-dependent" else "Total population"
      geo_parts <- paste(geo_parts, pop_lbl, sep = " \u00b7 ")
      for (filt in list(g$filt_uf, g$filt_macro, g$filt_reg, g$filt_mun)) {
        if (!is.null(filt) && length(filt)) {
          f <- as.character(filt[!is.na(filt) & nzchar(filt)])
          if (length(f)) {
            lbl <- if (length(f) == 1L) f[1] else paste0(f[1], " (n=", length(f), ")")
            geo_parts <- paste(geo_parts, lbl, sep = " \u2013 ")
            break
          }
        }
      }
    }

    preset_lbl <- as.character(.val_or(g$hpv_param_source, "Default"))
    if (exists("HPV_PRESETS") && preset_lbl %in% names(HPV_PRESETS))
      preset_lbl <- HPV_PRESETS[[preset_lbl]]$label %||% preset_lbl

    # ── 7. Logos (PNG paths resolved at render time) ──────────────────

    # ── 8. Detailed footer (assumptions + all params) ─────────────────
    pct2 <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (!is.finite(x) || is.na(x)) "NA" else sprintf("%.2f%%", x)
    }
    dot <- " \u00b7 "
    test_interval <- if (is_hpv) "HPV every 5 years" else "Cytology every 3 years"

    base_parts <- paste(
      sprintf("Ages %s\u2013%s", age_min, age_max),
      sprintf("Coverage %s%%", coverage),
      test_interval,
      sprintf("Parameters: %s", preset_lbl),
      sep = dot
    )

    param_parts <- if (is_hpv) {
      paste(
        paste0("HPV 16/18+: ",                 pct2(g$p16_18)),
        paste0("Other HR-HPV+: ",              pct2(g$poutros)),
        paste0("Negative: ",                   pct2(g$pneg)),
        paste0("Reflex cytology positivity: ", pct2(g$cito_out_pos)),
        paste0("Colposcopy HPV16/18+: ",       pct2(g$colpo16_pos)),
        paste0("Other HR-HPV+: ",              pct2(g$colpoout_pos)),
        paste0("Biopsy CIN2+ HPV16/18+: ",    pct2(g$b16_nic23)),
        paste0("Other HR-HPV+: ",              pct2(g$bo_nic23)),
        paste0("Cancer HPV16/18+: ",           pct2(g$b16_cancer)),
        paste0("Other HR-HPV+: ",              pct2(g$bo_cancer)),
        sep = dot
      )
    } else {
      paste(
        paste0("First-time: ",         pct2(g$first_time_pct)),
        paste0("Unsatisfactory: ",     pct2(g$unsatisfactory_pct)),
        paste0("HSIL/ASC-H/AIS/Ca: ", pct2(g$res_asch_pct)),
        paste0("Other abnorm.: ",      pct2(g$res_other_pct)),
        paste0("Negative: ",           pct2(g$res_neg_pct)),
        paste0("Colposcopy HSIL arm: ",pct2(g$colpo_asch_pct)),
        paste0("Colposcopy other arm: ",pct2(g$colpo_other_follow_pct)),
        paste0("Biopsy CIN2+ HSIL: ",  pct2(g$b_asch_nic23_pct)),
        paste0("Biopsy CIN2+ other: ", pct2(g$b_other_nic23_pct)),
        sep = dot
      )
    }

    assumptions <- paste(
      base_parts, param_parts,
      paste0("Generated: ", format(Sys.Date(), "%B %d, %Y")),
      sep = dot
    )

    # ── Return params list ────────────────────────────────────────────
    list(
      geo_label     = geo_parts,
      method        = method,
      preset_label  = preset_lbl,
      age_from      = age_min,
      age_to        = age_max,
      coverage      = coverage,
      date          = format(Sys.Date(), "%B %d, %Y"),
      # Summary
      pop_selected        = .fmt_safe(dt_pdf$pop_selected),
      screened_year       = .fmt_safe(dt_pdf$screened_per_year),
      cito_label          = cito_label,
      cito_val            = .fmt_safe(cito_val),
      colpo_val           = .fmt_safe(dt_pdf$colpo_indicada),
      biopsy_val          = .fmt_safe(dt_pdf$biopsia_indicada),
      ezt_val             = .fmt_safe(dt_pdf$ezt),
      followup_test_label = fu_test_label,
      followup_test_val   = .fmt_safe(fu_test_val),
      followup_colpo_val  = .fmt_safe(fu_colpo_val),
      # Equipment
      equip_rows = equip_rows,
      # Epidemiology
      inc_n    = .fmt_safe(inc_n),
      inc_asr  = .fmt_rate_safe(inc_asr),
      mort_n   = .fmt_safe(mort_n),
      mort_asr = .fmt_rate_safe(mort_asr),
      # Capacity
      is_brazil = is_br,
      cap_rows  = cap_rows,
      # Assumptions
      assumptions = assumptions
    )
  })

  output$btn_download_pdf <- downloadHandler(
    filename = function() {
      paste0("cc_screening_report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      if (!requireNamespace("pagedown",  quietly = TRUE)) stop("Package 'pagedown' is required. Install it with: install.packages('pagedown')")
      if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("Package 'rmarkdown' is required. Install it with: install.packages('rmarkdown')")
      if (!requireNamespace("rsvg",      quietly = TRUE)) stop("Package 'rsvg' is required. Install it with: install.packages('rsvg')")

      data <- pdf_data()
      if (is.null(data)) stop("Could not generate report data. Please check your filter settings.")

      # Convert SVG logos to PNG so <img> tags work in the HTML template
      tmp_fsp  <- tempfile(fileext = ".png")
      tmp_iarc <- tempfile(fileext = ".png")
      tmp_html <- tempfile(fileext = ".html")
      on.exit({ unlink(tmp_fsp); unlink(tmp_iarc); unlink(tmp_html) }, add = TRUE)

      tryCatch(rsvg::rsvg_png("logo-fsp.svg",  tmp_fsp,  height = 72L), error = function(e) file.copy(tmp_fsp, tmp_fsp))
      tryCatch(rsvg::rsvg_png("logo-iarc.svg", tmp_iarc, height = 72L), error = function(e) file.copy(tmp_iarc, tmp_iarc))

      data$logo_fsp_png  <- tmp_fsp
      data$logo_iarc_png <- tmp_iarc

      rmarkdown::render(
        input       = "www/report_template.Rmd",
        output_file = tmp_html,
        params      = data,
        envir       = new.env(parent = globalenv()),
        quiet       = TRUE
      )

      # wait = 0: skip pagedjs polyfill (not used); use Chrome's native PDF print
      pagedown::chrome_print(
        input   = tmp_html,
        output  = file,
        wait    = 0,
        timeout = 120
      )
    }
  )

  mod_compare_server(
    id               = "comp",
    input_global     = input_global,
    df_completo      = df_cc_completo,
    dim_country      = df_dim_country,
    pop_mun_regional = pop_municipio_regional,
    cito_presets     = cito_presets
  )
  

  mod_capacity_server(
    id               = "capacidade",
    df_completo      = df_cc_completo,
    dim_country      = df_dim_country,
    input_global     = input_global,
    pop_mun_regional = pop_municipio_regional,
    sia_cc_resumo    = sia_cc_resumo,
    regional_sus_map = regional_sus_map
  )

  mod_detailed_table_server(
    id               = "detailed",
    pop_mun_regional = pop_municipio_regional,
    sia_cc_resumo    = sia_cc_resumo,
    regional_sus_map = regional_sus_map,
    dim_country      = df_dim_country,
    input_global     = input_global
  )
  
  
  
 
  
}




shinyApp(ui, server)
