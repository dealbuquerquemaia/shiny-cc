# ===========================================================
# Shiny-cc — 14_mod_equipamentos.R
# Aba "Equipment & HR needs" — Necessidade de equipamentos e RH
#
# Camada fina sobre o engine: consome `input_global()`, roda
# `cc_engine_run()` e renderiza 4 cards no padrão "Demand |
# Capacity / year | Required":
#   - Colposcope (cap_colpo_device → colpo_devices_needed)
#   - Colposcopist 20h/week (cap_colpo_med → colpo_med_needed)
#   - Cytopathologist (cap_citopato → citopato_needed)
#   - Pathologist 20h/week (cap_patol_med → patol_med_needed)
#
# Todos os valores de "Required" são ⌈demanda / capacidade⌉
# (ver `cc_hr_metrics` no engine).
# ===========================================================

# ---- UI --------------------------------------------------------
# Layout simples: cabeçalho com subtítulo dinâmico (`geo_desc`)
# + contêiner único onde os cards são renderizados pelo server.
mod_equipment_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "cc-page-header",
      div(class = "cc-page-title", "Equipment & HR needs"),
      div(class = "cc-page-subtitle", textOutput(ns("geo_desc")))
    ),
    uiOutput(ns("ccu_cards"))
  )
}

mod_equipment_server <- function(id,
                                 df_completo,
                                 dim_age,
                                 dim_country,
                                 input_global,
                                 pop_mun_regional) {

  moduleServer(id, function(input, output, session) {

    # ---- Bloco 1: País selecionado ---------------------------------
    # Helper único cc_country_info() em 01_utils_cc.R devolve
    # list(code, label, is_brazil) a partir de input_global() e
    # dim_country. Substitui o trio replicado em 11/14/15/16/17.
    br_code <- tryCatch(
      {
        x <- dim_country[dim_country$population_name == "Brazil", "population_code"]
        if (length(x) == 0L || is.na(x[1])) NA_integer_ else as.integer(x[1])
      },
      error = function(e) NA_integer_
    )

    ci <- reactive(cc_country_info(input_global(), dim_country, br_code))

    # Helper definido mas NÃO USADO neste arquivo (código morto) — a
    # lógica de "primeiro item (n=K)" foi reimplementada via `add_if`
    # dentro de `output$geo_desc`. Mesma duplicação existe em
    # `11_mod_resumo_geral.R`.
    pick_all <- function(x) {
      if (is.null(x) || !length(x)) return("–")
      x <- as.character(x)
      x <- x[!is.na(x) & nzchar(x)]
      if (!length(x)) return("–")
      if (length(x) == 1L) return(x[1])
      paste0(x[1], " (n=", length(x), ")")
    }


    # ---- Bloco 3: Subtítulo geográfico ("geo_desc") ----------------
    # Fora do Brasil → só o nome do país.
    # Dentro do Brasil → "Brazil - UF - Macro - Região - Município",
    # incluindo apenas os níveis com seleção. Múltiplas seleções viram
    # "Primeiro (n=K)". Helper único cc_geo_label() em 01_utils_cc.R.
    output$geo_desc <- renderText({
      cc_geo_label(input_global(), mode = "concat",
                   dim_country = dim_country, br_code = br_code)
    })
    
    # ---- Bloco 4: cfg reactive (tradução sidebar → engine) ---------
    # Wrapper único: cc_cfg_from_input() em 02_engine_capacity_cc.R
    # encapsula a tradução input_global() → cc_engine_settings() e é
    # compartilhado entre Summary/Equipment/Pathway/Capacity/Compare.
    cfg_reactive <- reactive({
      cc_cfg_from_input(input_global(), br_code)
    })
    
    # ---- Bloco 5: Execução do engine -------------------------------
    # Roda o engine a cada mudança de cfg. Retorna a lista padrão
    # (`$metrics`, `$hr`, `$pop_selected`, etc.); aqui só `$hr` é
    # consumido pela UI.
    engine_res <- reactive({
      cfg <- cfg_reactive()
      cc_engine_run(
        df_completo,
        cfg,
        pop_mun_regional = pop_mun_regional
      )
    })

    # ---- Bloco 6: Ícones SVG ---------------------------------------
    # Ícones inline (sem dependência de fonte/biblioteca externa),
    # mesmo estilo/cor dos cards em Summary.
    #   ico_scope  = colposcópio (lupa+cruz)
    #   ico_doctor = colposcopista (pessoa+cruz médica)
    #   ico_micro  = citopatologista (microscópio)
    #   ico_lab    = patologista (lâmina/frasco de AP)
    ico_scope  <- '<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><circle cx="11" cy="11" r="7"/><line x1="16.5" y1="16.5" x2="21" y2="21"/><line x1="8" y1="11" x2="14" y2="11"/><line x1="11" y1="8" x2="11" y2="14"/></svg>'
    ico_doctor <- '<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="7" r="3"/><path d="M5 21v-2a4 4 0 0 1 4-4h6a4 4 0 0 1 4 4v2"/><path d="M12 10v5"/><path d="M10 12h4"/></svg>'
    ico_micro  <- '<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><circle cx="10" cy="8" r="3"/><path d="M10 11v4"/><path d="M7 15h6"/><path d="M6 18h13"/><path d="M13 8h3a3 3 0 0 1 3 3v0"/></svg>'
    ico_lab    <- '<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><rect x="4" y="3" width="16" height="5" rx="1"/><path d="M6 8v11a2 2 0 0 0 2 2h8a2 2 0 0 0 2-2V8"/><path d="M10 12h4"/><path d="M10 16h4"/></svg>'

    # ---- Bloco 7: Fábrica de card ----------------------------------
    # Produz 1 card no padrão:
    #   título + ícone +  <valor grande = "Required">
    #                  + "Demand: X · Capacity: Y / year · Required"
    # `required` é NA_real_ quando não há capacidade → exibe en-dash.
    card_ui <- function(title, demand, cap_year, required, icon_svg = NULL, cap_label = "Capacity") {
      req_txt    <- if (is.na(required)) "\u2013" else fmt_int(required)
      demand_txt <- paste0("Demand: ", fmt_int(round(demand)))
      cap_txt    <- if (is.na(cap_year))
        paste0(cap_label, ": \u2013 / year")
      else
        paste0(cap_label, ": ", fmt_int(round(cap_year)), " / year")

      div(
        class = "card-ccu",
        if (!is.null(icon_svg)) HTML(icon_svg),
        div(class = "card-ccu-label", title),
        div(class = "card-ccu-value", req_txt),
        div(class = "card-ccu-sub",
            HTML(paste0(demand_txt, " &nbsp;&middot;&nbsp; ", cap_txt, " &nbsp;&middot;&nbsp; Required")))
      )
    }

    # ---- Bloco 8: Notas técnicas (parâmetros do cenário) -----------
    # Constrói a string HTML exibida em `.ccu-note` abaixo dos cards,
    # descrevendo cenário (ages/coverage/método) + todos os parâmetros
    # numéricos do modelo (% por braço, HPV ou Citologia).
    #
    # Helpers locais — `fmt_or_dash` é wrapper de `fmt_int` (duplica
    # `fmt_int` de utils); `num_or_na` e `pct2` são idênticos aos de
    # `11_mod_resumo_geral.R` (ver pendências). Candidatos a mover
    # para `01_utils_cc.R`.
    build_notes <- function() {
      g <- input_global()

      fmt_or_dash <- function(x) {
        if (is.null(x) || length(x) == 0L || is.na(x)) "\u2013" else fmt_int(round(as.numeric(x)))
      }
      num_or_na <- function(x) {
        x <- suppressWarnings(as.numeric(x))
        if (!is.finite(x) || is.na(x)) NA_real_ else x
      }
      pct2 <- function(x) {
        x <- num_or_na(x)
        if (is.na(x)) "NA" else sprintf("%.2f%%", x)
      }

      # Linha 1: resumo do cenário ("Ages 25–64 · Coverage 70% · HPV every 5 years")
      age_txt <- sprintf("Ages %s\u2013%s", fmt_or_dash(g$target_age_min), fmt_or_dash(g$target_age_max))
      cov_txt <- sprintf("Coverage %s%%", fmt_or_dash(g$coverage))

      method <- as.character(cfg_reactive()$screen_method %||% g$screen_method %||% "hpv")
      test_txt <- if (identical(method, "hpv")) "HPV every 5 years" else "Cytology every 3 years"
      note_target <- paste(age_txt, cov_txt, test_txt, sep = " \u00b7 ")

      # Linha 2+: parâmetros numéricos específicos do método escolhido
      if (identical(method, "hpv")) {
        note_params <- paste0(
          "HPV 16/18+: ", pct2(g$p16_18),
          " &middot; Other HR-HPV+: ", pct2(g$poutros),
          " &middot; Negative: ", pct2(g$pneg),
          " &middot; Reflex cytology positivity: ", pct2(g$cito_out_pos),
          "<br/>Colposcopy positivity \u2013 HPV 16/18+: ", pct2(g$colpo16_pos),
          " &middot; Other HR-HPV+: ", pct2(g$colpoout_pos),
          "<br/>Biopsy (CIN2+) \u2013 HPV 16/18+: ", pct2(g$b16_nic23),
          " &middot; Other HR-HPV+: ", pct2(g$bo_nic23),
          " &middot; Cancer (16/18+): ", pct2(g$b16_cancer),
          " &middot; Cancer (other): ", pct2(g$bo_cancer)
        )
      } else {
        note_params <- paste0(
          "First-time exams: ", pct2(g$first_time_pct),
          " &middot; Unsatisfactory: ", pct2(g$unsatisfactory_pct),
          "<br/>Results ASC-H+: ", pct2(g$res_asch_pct),
          " &middot; Other abnormal: ", pct2(g$res_other_pct),
          " &middot; Negative: ", pct2(g$res_neg_pct),
          "<br/>Colposcopy referral (ASC-H+): ", pct2(g$colpo_asch_pct),
          " &middot; Other abnormal: ", pct2(g$colpo_other_follow_pct),
          "<br/>Colposcopy positivity (ASC-H+): ", pct2(g$biopsy_pos_asch_pct),
          " &middot; Other abnormal: ", pct2(g$biopsy_pos_other_pct),
          "<br/>Biopsy (ASC-H+) \u2013 CIN2/3: ", pct2(g$b_asch_nic23_pct),
          " &middot; Cancer: ", pct2(g$b_asch_cancer_pct),
          "<br/>Biopsy (Other abnormal) \u2013 CIN2/3: ", pct2(g$b_other_nic23_pct),
          " &middot; Cancer: ", pct2(g$b_other_cancer_pct),
          "<br/>EZT follow-up: 6 cytologies and 2 colposcopies per EZT"
        )
      }

      HTML(paste0(note_target, "<br/>", note_params))
    }

    # ---- Bloco 9: Renderização dos cards ---------------------------
    # Extrai métricas de RH (`res$hr`) — resultado de `cc_hr_metrics()`
    # — e monta as 4 cards com `card_ui()` + tooltip (`cc_with_tt`).
    #
    # Helpers locais:
    #   get_hr(col)  — lookup defensivo: retorna NA_real_ se a coluna
    #                  não existe (fallback quando engine foi rodado
    #                  com cenário inválido).
    #   safe_n(x)    — normaliza demanda: NA/Inf/negativo → 0. Evita
    #                  valores estranhos nos cards.
    output$ccu_cards <- renderUI({
      res <- engine_res()
      if (is.null(res)) return(NULL)

      hr <- res$hr
      get_hr <- function(col) {
        if (!is.null(hr) && col %in% names(hr)) suppressWarnings(as.numeric(hr[[col]][1L])) else NA_real_
      }
      safe_n <- function(x) { if (!is.finite(x) || is.na(x) || x < 0) 0 else x }

      # Demandas anuais (procedimentos): colposcopia, citologia (total
      # all-in: rastreamento + reflexa/diagnóstica + follow-up),
      # anatomia patológica (biópsia + peça cirúrgica).
      d_colpo <- safe_n(get_hr("colpo_demand"))
      d_cito  <- safe_n(get_hr("cito_demand"))
      d_ap    <- safe_n(get_hr("ap_demand"))

      # Capacidades anuais (já anualizadas pela sidebar).
      cap_colpo_device <- get_hr("cap_colpo_device")
      cap_colpo_med    <- get_hr("cap_colpo_med")
      cap_citopato     <- get_hr("cap_citopato")
      cap_patol_med    <- get_hr("cap_patol_med")

      # Nº de recursos necessários = ⌈demanda / capacidade⌉
      # (o engine já devolve o quociente; aqui só arredonda pra cima).
      r_colpo_device <- { n <- get_hr("colpo_devices_needed"); if (is.finite(n) && !is.na(n)) ceiling(n) else NA_real_ }
      r_colpo_med    <- { n <- get_hr("colpo_med_needed");     if (is.finite(n) && !is.na(n)) ceiling(n) else NA_real_ }
      r_citopato     <- { n <- get_hr("citopato_needed");      if (is.finite(n) && !is.na(n)) ceiling(n) else NA_real_ }
      r_patol_med    <- { n <- get_hr("patol_med_needed");     if (is.finite(n) && !is.na(n)) ceiling(n) else NA_real_ }

      # Layout final: 1 seção com 4 cards + nota técnica abaixo.
      # Cada card é envolto por `cc_with_tt` com texto vindo de
      # `cc_TOOLTIPS$equipamentos_ccu` (definidos em 00_constants_cc.R).
      div(
        class = "ccu-flow",
        div(
          class = "ccu-section ccu-section-2",
          div(class = "ccu-section-title", "Equipment and human resources"),
          div(
            class = "cards-ccu-wrap",
            cc_with_tt(
              card_ui("Colposcope", d_colpo, cap_colpo_device, r_colpo_device, ico_scope),
              cc_TOOLTIPS$equipamentos_ccu$colposcope
            ),
            cc_with_tt(
              card_ui("Colposcopist (20h/week)", d_colpo, cap_colpo_med, r_colpo_med, ico_doctor),
              cc_TOOLTIPS$equipamentos_ccu$colposcopist
            ),
            cc_with_tt(
              card_ui("Cytopathologist", d_cito, cap_citopato, r_citopato, ico_micro),
              cc_TOOLTIPS$equipamentos_ccu$cytopathologist
            ),
            cc_with_tt(
              card_ui("Pathologist (20h/week)", d_ap, cap_patol_med, r_patol_med, ico_lab),
              cc_TOOLTIPS$equipamentos_ccu$pathologist
            )
          ),
          div(class = "ccu-note", build_notes())
        )
      )
    })

  })
}
