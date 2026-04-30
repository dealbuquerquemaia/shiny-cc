# ===========================================================
# 11_mod_resumo_geral.R — Aba "Summary" do dashboard
# -----------------------------------------------------------
# Renderiza a tela de KPIs consolidados do cenário:
#   Target population → Work-up → Treatment and follow-up
# É essencialmente uma camada fina em cima do engine:
#   input_global()  ->  cfg (reactive)  ->  cc_engine_run()  ->  cards
#
# Dependências:
#   - cc_engine_settings / cc_engine_run / cc_engine_summary_dt (02_engine_capacity_cc.R)
#   - fmt_int (01_utils_cc.R)
#   - HPV_DEFAULTS / CITO_DEFAULTS (via engine fallback)
#   - cc_TOOLTIPS (00_constants_cc.R) — textos de ajuda dos cards
#
# Nota: usa helpers internos `val_or` e `fmt_or_dash` — eles
# duplicam lógica equivalente em app.R (.val_or) e utils
# (fmt_int). Ver docs/INVENTORY.md > Observações.
# ===========================================================

# -----------------------------------------------------------
# UI — cabeçalho + container dinâmico dos cards
# -----------------------------------------------------------
mod_resumo_geral_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "cc-page-header",
      div(class = "cc-page-title", "Summary"),
      # Subtítulo dinâmico: descreve geografia + recorte populacional
      div(class = "cc-page-subtitle", textOutput(ns("geo_desc")))
    ),
    # Cards renderizados no server (3 seções: target / work-up / treatment)
    uiOutput(ns("cards_resumo"))
  )
}


########################## SERVER ##############################
# Assinatura:
#   df_completo      : data.table GLOBOCAN (incidência/mortalidade/pop)
#   dim_age          : dicionário de faixas etárias (não usado hoje; mantido p/ futuro)
#   dim_country      : dicionário de países (para resolver label do país)
#   input_global     : reactive devolvido por mod_filters_cc_server (SSOT da sidebar)
#   pop_mun_regional : data.table de populações BR por município/regional
# ---------------------------------------------------------------
mod_resumo_geral_server <- function(
    id,
    df_completo,
    dim_age,
    dim_country,
    input_global,
    pop_mun_regional
) {
  moduleServer(id, function(input, output, session) {

    # -----------------------------------------------------------
    # 1) Helpers locais
    #    val_or      : fallback NULL/NA → default
    #    fmt_or_dash : formata inteiro ou devolve "–" (en-dash)
    #    (Ambos têm análogos em app.R/utils — ver pendências)
    # -----------------------------------------------------------
    val_or <- function(x, default) {
      if (is.null(x) || length(x) == 0L || all(is.na(x))) default else x
    }
    fmt_or_dash <- function(x) {
      if (is.null(x) || length(x) == 0L || is.na(x)) "–" else fmt_int(round(x))
    }

    # -----------------------------------------------------------
    # 2) Identificação do país selecionado
    #    Helper único cc_country_info() em 01_utils_cc.R devolve
    #    list(code, label, is_brazil) a partir de input_global() e
    #    dim_country. Substitui o trio replicado em 11/14/15/16/17.
    # -----------------------------------------------------------
    # Resolve o código GLOBOCAN do Brasil uma única vez (module scope).
    # Se falhar, guarda NA_integer_ e ci()$is_brazil fica sempre FALSE.
    br_code <- tryCatch(
      {
        x <- dim_country[dim_country$population_name == "Brazil", "population_code"]
        if (length(x) == 0L || is.na(x[1])) NA_integer_ else as.integer(x[1])
      },
      error = function(e) NA_integer_
    )

    ci <- reactive(cc_country_info(input_global(), dim_country, br_code))

    # pick_all(): helper para exibir um único rótulo ou "primeiro (n=K)".
    # Mantido no arquivo para reuso futuro; hoje o código efetivo usa add_if()
    # inline dentro de output$geo_desc. Candidato a limpeza.
    pick_all <- function(x) {
      if (is.null(x) || !length(x)) return("–")
      x <- as.character(x)
      x <- x[!is.na(x) & nzchar(x)]
      if (!length(x)) return("–")
      if (length(x) == 1L) return(x[1])
      paste0(x[1], " (n=", length(x), ")")
    }
    
    # -----------------------------------------------------------
    # 3) Subtítulo dinâmico da página (geo_desc)
    #    - Não-Brasil → apenas nome do país (ou "World")
    #    - Brasil     → "Brazil - <Total/SUS> - [UF] - [Macro] - [Região] - [Município]"
    #      Cada filtro só entra se tiver seleção; múltiplas seleções viram
    #      "Primeiro (n=K)". Helper único cc_geo_label() em 01_utils_cc.R.
    # -----------------------------------------------------------
    output$geo_desc <- renderText({
      cc_geo_label(input_global(), mode = "concat",
                   dim_country = dim_country, br_code = br_code,
                   pop_tipo = TRUE)
    })
    
    
    # -----------------------------------------------------------
    # 4) cfg: reactive que traduz input_global() em cc_engine_settings()
    #    Wrapper único: cc_cfg_from_input() em 02_engine_capacity_cc.R
    #    encapsula coerção/clamp/retrocompat (programa, proto1_age_*) e
    #    é compartilhado entre Summary/Equipment/Pathway/Capacity/Compare.
    # -----------------------------------------------------------
    cfg <- reactive({
      cc_cfg_from_input(input_global(), br_code)
    })
    
    # -----------------------------------------------------------
    # 5) Execução do engine
    #    - res_engine : roda cc_engine_run; captura erro como objeto
    #      (permite mostrar "No data" no card sem derrubar a sessão).
    #    - dt_sum     : achata o resultado em data.table 1 linha
    #      (NULL se res_engine devolveu erro).
    # -----------------------------------------------------------
    res_engine <- reactive({
      req(df_completo)
      tryCatch(
        cc_engine_run(df_completo, cfg(), pop_mun_regional = pop_mun_regional),
        error = function(e) e
      )
    })

    dt_sum <- reactive({
      r <- res_engine()
      if (inherits(r, "error")) return(NULL)
      cc_engine_summary_dt(r)
    })
    
    # -----------------------------------------------------------
    # 6) Render dos cards (3 seções)
    #    Sections: Target population | Work-up | Treatment & follow-up
    #    Cada seção é um `div.ccu-section` com título + cartões; entre
    #    elas, `connector` (seta SVG) visualiza o fluxo.
    #    Notas (HTML) abaixo de cada seção exibem os parâmetros que
    #    alimentaram os números (% por braço, etc.).
    # -----------------------------------------------------------
    output$cards_resumo <- renderUI({
      dt <- dt_sum()
      tt <- cc_TOOLTIPS$resumo_geral_ccu

      # Mensagem defensiva: engine retornou vazio/erro
      if (is.null(dt) || !nrow(dt)) {
        return(tags$div(
          div(style = "margin:6px; opacity:0.8;", "No data for the current selection.")
        ))
      }

      s <- dt[1]
      method <- as.character(s$screen_method)
      if (!method %in% c("hpv", "cytology")) method <- "hpv"

      g <- input_global()

      # Helpers locais para formatar as notas (não exportados)
      num_or_na <- function(x) {
        x <- suppressWarnings(as.numeric(x))
        if (!is.finite(x) || is.na(x)) NA_real_ else x
      }
      pct2 <- function(x) {
        x <- num_or_na(x)
        if (is.na(x)) "NA" else sprintf("%.2f%%", x)
      }

      # -- Ícones SVG inline (22x22, stroke #fff, stroke-width 1.6)
      #    Pequenos o suficiente para colocar direto no card via HTML();
      #    alternativa (sprite.svg em www/) foi avaliada e descartada pela
      #    baixa contagem de ícones.
      ico_pop <- '<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><circle cx="9" cy="7" r="3"/><path d="M3 21v-2a4 4 0 0 1 4-4h4a4 4 0 0 1 4 4v2"/><circle cx="18" cy="7" r="2.5"/><path d="M21 21v-1.5a3.5 3.5 0 0 0-2.5-3.35"/></svg>'
      ico_cal <- '<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="4" width="18" height="17" rx="2"/><line x1="3" y1="9" x2="21" y2="9"/><line x1="8" y1="2" x2="8" y2="6"/><line x1="16" y1="2" x2="16" y2="6"/><polyline points="9,14 11.2,16.5 15.5,12"/></svg>'
      ico_tube <- '<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><path d="M9 3h6"/><path d="M10 3v9l-4 6a1 1 0 0 0 .85 1.5h10.3A1 1 0 0 0 18 18l-4-6V3"/><line x1="8.5" y1="15" x2="15.5" y2="15"/></svg>'
      ico_scope <- '<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><circle cx="11" cy="11" r="7"/><line x1="16.5" y1="16.5" x2="21" y2="21"/><line x1="8" y1="11" x2="14" y2="11"/><line x1="11" y1="8" x2="11" y2="14"/></svg>'
      ico_biopsy <- '<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><rect x="10" y="5" width="4" height="3" rx="0.8"/><line x1="12" y1="5" x2="12" y2="8"/><path d="M11.2 8 L8 17.8"/><path d="M12.8 8 L16 17.8"/><circle cx="8" cy="20" r="2.2"/><circle cx="16" cy="20" r="2.2"/></svg>'
      ico_cone <- '<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="4" x2="4" y2="18"/><line x1="12" y1="4" x2="20" y2="18"/><ellipse cx="12" cy="18" rx="8" ry="2.5"/><line x1="12" y1="6" x2="12" y2="15.5" stroke-dasharray="1.8 2"/></svg>'
      ico_refresh <- '<svg class="card-ccu-icon" viewBox="0 0 24 24" fill="none" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><path d="M3 12a9 9 0 0 1 15-6.7L21 8"/><path d="M21 3v5h-5"/><path d="M21 12a9 9 0 0 1-15 6.7L3 16"/><path d="M3 21v-5h5"/></svg>'

      # Seta entre seções (renderizada 2× abaixo). `connector` é um
      # objeto `div` — reutilizar o mesmo objeto é seguro (htmltools
      # não o muta depois de criado).
      connector <- div(
        class = "ccu-connector",
        div(
          class = "ccu-connector-arrow",
          HTML('<svg viewBox="0 0 12 12" fill="none" stroke="#888" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><polyline points="2,3 6,9 10,3"/></svg>')
        )
      )

      # Fábrica de card (título + valor + ícone + tooltip HTML nativo).
      # tooltip é aplicado via attr `title` (renderização pelo navegador).
      card <- function(label, value, icon_svg = NULL, tooltip = NULL) {
        div(
          class = "card-ccu",
          title = tooltip,
          if (!is.null(icon_svg)) HTML(icon_svg),
          div(class = "card-ccu-label", label),
          div(class = "card-ccu-value", fmt_or_dash(value))
        )
      }

      # ---------------------------
      # Notas (strings em inglês — padrão institucional do dashboard)
      # Abaixo de cada seção exibimos os parâmetros-chave do cenário.
      # ---------------------------
      age_txt  <- sprintf("Ages %s\u2013%s", fmt_or_dash(s$target_age_min), fmt_or_dash(s$target_age_max))
      cov_txt  <- sprintf("Coverage %s%%", fmt_or_dash(s$coverage_percent))
      test_txt <- if (identical(method, "hpv")) "HPV every 5 years" else "Cytology every 3 years"
      note_target <- HTML(paste(age_txt, cov_txt, test_txt, sep = " &nbsp;&middot;&nbsp; "))

      if (identical(method, "hpv")) {
        note_workup <- HTML(paste0(
          "HPV 16/18+: ", pct2(g$p16_18),
          " &nbsp;&middot;&nbsp; Other HR-HPV+: ", pct2(g$poutros),
          " &nbsp;&middot;&nbsp; Negative: ", pct2(g$pneg),
          " &nbsp;&middot;&nbsp; Reflex cytology positivity: ", pct2(g$cito_out_pos),
          "<br/>Colposcopy positivity \u2013 HPV 16/18+: ", pct2(g$colpo16_pos),
          " &nbsp;&middot;&nbsp; Other HR-HPV+: ", pct2(g$colpoout_pos)
        ))

        cin2p_16  <- if (is.na(num_or_na(g$b16_nic23))) "NA" else sprintf("%.2f%%", num_or_na(g$b16_nic23))
        cin2p_out <- if (is.na(num_or_na(g$bo_nic23)))  "NA" else sprintf("%.2f%%", num_or_na(g$bo_nic23))
        can_16    <- if (is.na(num_or_na(g$b16_cancer))) "NA" else sprintf("%.2f%%", num_or_na(g$b16_cancer))
        can_out   <- if (is.na(num_or_na(g$bo_cancer)))  "NA" else sprintf("%.2f%%", num_or_na(g$bo_cancer))

        note_trt <- HTML(paste0(
          "Biopsy positivity (CIN2+) \u2013 HPV 16/18+: ", cin2p_16,
          " &nbsp;&middot;&nbsp; Other HR-HPV+: ", cin2p_out,
          "<br/>Cancer \u2013 HPV 16/18+: ", can_16,
          " &nbsp;&middot;&nbsp; Other HR-HPV+: ", can_out,
          " &nbsp;&middot;&nbsp; Follow-up HPV positivity: ", pct2(g$hpv_followup_pos_pct)
        ))

      } else {
        note_workup <- HTML(paste0(
          "First-time exams: ", pct2(g$first_time_pct),
          " &nbsp;&middot;&nbsp; Unsatisfactory: ", pct2(g$unsatisfactory_pct),
          "<br/>Results ASC-H+: ", pct2(g$res_asch_pct),
          " &nbsp;&middot;&nbsp; Other abnormal: ", pct2(g$res_other_pct),
          " &nbsp;&middot;&nbsp; Negative: ", pct2(g$res_neg_pct),
          "<br/>Colposcopy referral (ASC-H+): ", pct2(g$colpo_asch_pct),
          " &nbsp;&middot;&nbsp; Other abnormal: ", pct2(g$colpo_other_follow_pct),
          "<br/>Colposcopy positivity (ASC-H+): ", pct2(g$biopsy_pos_asch_pct),
          " &nbsp;&middot;&nbsp; Other abnormal: ", pct2(g$biopsy_pos_other_pct)
        ))

        note_trt <- HTML(paste0(
          "Biopsy (ASC-H+) \u2013 CIN2/3: ", pct2(g$b_asch_nic23_pct),
          " &nbsp;&middot;&nbsp; Cancer: ", pct2(g$b_asch_cancer_pct),
          " &nbsp;&middot;&nbsp; Neg/CIN1: ", pct2(g$b_asch_neg_nic1_pct),
          "<br/>Biopsy (Other abnormal) \u2013 CIN2/3: ", pct2(g$b_other_nic23_pct),
          " &nbsp;&middot;&nbsp; Cancer: ", pct2(g$b_other_cancer_pct),
          " &nbsp;&middot;&nbsp; Neg/CIN1: ", pct2(g$b_other_neg_nic1_pct),
          "<br/>EZT follow-up: 6 cytologies and 2 colposcopies per EZT"
        ))
      }

      # =========================
      # Seção 1) Target population — pop elegível e pop rastreada/ano
      # =========================
      sec_target <- div(
        class = "ccu-section ccu-section-1",
        div(class = "ccu-section-title", "Target population"),
        div(
          class = "cards-ccu-wrap",
          card("Selected population", s$pop_selected,     ico_pop, tt$common$pop_selected),
          card("Screened per year",   s$screened_per_year, ico_cal, tt$common$screened_year)
        ),
        div(class = "ccu-note", note_target)
      )

      # =========================
      # Seção 2) Work-up — encaminhamentos diagnósticos
      #   HPV: cito reflexa + colposcopia + biópsia
      #   Cito: cito diagnóstica + colposcopia + biópsia
      # =========================
      sec_workup <- if (identical(method, "hpv")) {
        div(
          class = "ccu-section ccu-section-2",
          div(class = "ccu-section-title", "Work-up"),
          div(
            class = "cards-ccu-wrap",
            card("Reflex cytology",      s$cito_reflexa,     ico_tube,   tt$hpv$cito_reflexa),
            card("Colposcopy indicated", s$colpo_indicada,   ico_scope,  tt$hpv$colpo_indicada),
            card("Biopsy indicated",     s$biopsia_indicada, ico_biopsy, tt$hpv$biopsia_indicada)
          ),
          div(class = "ccu-note", note_workup)
        )
      } else {
        div(
          class = "ccu-section ccu-section-2",
          div(class = "ccu-section-title", "Work-up"),
          div(
            class = "cards-ccu-wrap",
            card("Diagnostic cytology", s$cit_diagnostica,  ico_tube,   tt$cytology$cit_diagnostica),
            card("Colposcopy",          s$colpo_indicada,   ico_scope,  tt$cytology$colpo_indicada),
            card("Biopsy",              s$biopsia_indicada, ico_biopsy, tt$cytology$biopsia_indicada)
          ),
          div(class = "ccu-note", note_workup)
        )
      }

      # =========================
      # Seção 3) Treatment & follow-up — EZT + seguimento
      #   HPV: EZT + follow-up HPV + follow-up colposcopia
      #   Cito: EZT + citologias de seguimento + colposcopias de seguimento
      # =========================
      sec_trt <- if (identical(method, "hpv")) {
        div(
          class = "ccu-section ccu-section-3",
          div(class = "ccu-section-title", "Treatment and follow-up"),
          div(
            class = "cards-ccu-wrap",
            card("Excision indicated (EZT)", s$ezt,                ico_cone,    tt$hpv$ezt),
            card("Follow-up HPV",            s$retorno_1ano,       ico_refresh, tt$hpv$retorno_1ano),
            card("Follow-up colposcopy",     s$followup_colposcopy, ico_scope,  tt$hpv$followup_colposcopy)
          ),
          div(class = "ccu-note", note_trt)
        )
      } else {
        div(
          class = "ccu-section ccu-section-3",
          div(class = "ccu-section-title", "Treatment and follow-up"),
          div(
            class = "cards-ccu-wrap",
            card("Excision indicated (EZT)", s$ezt,                  ico_cone,    tt$cytology$ezt),
            card("Follow-up cytologies",     s$followup_cytologies,  ico_tube,    (tt$cytology$followup_cytologies %||% NULL)),
            card("Follow-up colposcopies",   s$followup_colposcopies, ico_scope,  (tt$cytology$followup_colposcopies %||% NULL))
          ),
          div(class = "ccu-note", note_trt)
        )
      }

      # Layout final: as 3 seções conectadas por setas (ccu-flow é o
      # container flex definido em www/style.css).
      div(
        class = "ccu-flow",
        sec_target,
        connector,
        sec_workup,
        connector,
        sec_trt
      )
    })



  })
}
