# ===========================================================
# 20_mod_tabela_detalhada.R
# Módulo "Detailed table" — tabela linha-a-linha de necessidade vs
# produção SUS/SIA 2024 por nível geográfico (UF / Macro / Reg / Mun).
#
# Particularidades deste módulo:
#   - Aba **só-Brasil** (igual a Capacity / Maps); fora do BR exibe placeholder.
#   - Requer **pop IBGE** (total ou SUS). NÃO funciona com pop manual
#     (custom_pop não pode ser distribuído entre municípios).
#   - **NÃO consome a saída por país do engine**. Em vez disso, roda o engine
#     UMA vez com `custom_pop = 1` (cenário "per-capita") e depois multiplica
#     as 4 taxas (tests/colpo/biopsia/EZT por mulher-alvo) pela população real
#     de cada linha da agregação. Vantagem: 1 chamada do engine cobre N linhas.
#   - **Granularidade dinâmica**: usuário escolhe "By municipality" (sempre mun)
#     ou "By smallest selected level" (UF→Macro→Reg→Mun conforme cascata).
#   - **Download CSV** com separador ";" + decimal "," + BOM (compat Excel-BR).
#   - **DT** com `formatStyle` por faixa de cobertura (<50/<100/≥100) usando
#     hex literais (DT não consome CSS vars).
#
# Pipeline server (em 11 blocos):
#   1) br_code + is_brazil + pop_is_manual + available  — gating triplo
#   2) output$geo_desc                                  — subtítulo (geo)
#   3) effective_gran                                   — granularidade efetiva
#   4) per_capita                                       — engine 1× com pop=1
#   5) pop_agg                                          — pop agregada por nível
#   6) sia_agg                                          — SIA agregada por nível
#   7) tabela_base                                      — merge pop × SIA × per-capita
#   8) params_line + col_labels + build_display_dt      — helpers de exibição
#   9) output$body                                      — gating UI (Brazil/IBGE)
#  10) output$params_text + output$table                — render principal
#  11) output$download_csv                              — handler de download
#
# Dependências:
#   - cc_engine_settings + cc_engine_run (engine, com custom_pop=1)
#   - HPV_PRESETS (rótulo do preset)
#   - %||% (utils)
# ===========================================================

# ── UI ──────────────────────────────────────────────────────────────────────
# Header padrão "Detailed table" + subtítulo dinâmico (geo) + body via uiOutput
# (o body é construído no server porque depende do gating Brazil/IBGE).

mod_detailed_table_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "cc-page-header",
      div(class = "cc-page-title", "Detailed table"),
      div(class = "cc-page-subtitle", textOutput(ns("geo_desc")))
    ),

    uiOutput(ns("body"))
  )
}

# ── Server ───────────────────────────────────────────────────────────────────

mod_detailed_table_server <- function(id,
                                      pop_mun_regional,
                                      sia_cc_resumo,
                                      regional_sus_map,
                                      dim_country,
                                      input_global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Bloco 1: gating triplo (Brazil + IBGE + disponibilidade geral) ─────
    # 7ª replicação do padrão `is_brazil`. `available()` resume os 2 gates
    # (BR + pop não-manual) que travam todos os reactives a jusante.
    br_code <- tryCatch({
      x <- dim_country[dim_country$population_name == "Brazil", "population_code"]
      if (length(x) == 0L || is.na(x[1])) NA_integer_ else as.integer(x[1])
    }, error = function(e) NA_integer_)

    is_brazil <- reactive({
      g <- input_global()
      !is.na(br_code) && isTRUE(as.integer(g$pais_sel) == br_code)
    })

    pop_is_manual <- reactive({
      g <- input_global()
      isTRUE(g$pop_mode == "other")
    })

    # Gate central: tudo a jusante checa `available()`.
    available <- reactive({
      is_brazil() && !pop_is_manual()
    })

    # ── Bloco 2: subtítulo dinâmico (geo) ──────────────────────────────────
    # Concatena "Brazil - UF - Macro - Reg - Mun"; fora do BR mostra o nome
    # do país. Helper único cc_geo_label() em 01_utils_cc.R.
    output$geo_desc <- renderText({
      cc_geo_label(input_global(), mode = "concat",
                   dim_country = dim_country, br_code = br_code)
    })

    # ── Bloco 3: granularidade efetiva ────────────────────────────────────
    # Resolve "By municipality" (sempre mun) vs "By smallest selected level":
    # se cascata vai até mun → mun; se vai até região → reg; etc.
    # Usuário ganha: tabela menor quando filtra alto (UF apenas, p.ex.).
    effective_gran <- reactive({
      if (!available()) return("mun")
      toggle <- input$granularity %||% "mun"
      if (toggle == "mun") return("mun")
      g <- input_global()
      has_mun   <- !is.null(g$filt_mun)   && length(g$filt_mun)   > 0
      has_reg   <- !is.null(g$filt_reg)   && length(g$filt_reg)   > 0
      has_macro <- !is.null(g$filt_macro) && length(g$filt_macro) > 0
      has_uf    <- !is.null(g$filt_uf)    && length(g$filt_uf)    > 0
      if (has_mun)                       return("mun")
      if (has_uf && has_macro && has_reg) return("mun")
      if (has_uf && has_macro)           return("reg")
      if (has_uf)                        return("macro")
      "uf"
    })

    # ── Bloco 4: per_capita (cenário pop=1) ───────────────────────────────
    # Truque central do módulo: roda o engine 1× com `pop_mode="other"` e
    # `custom_pop=1` → resultado é interpretado como **taxa por mulher-alvo**.
    # Depois multiplicamos pelas pop reais (Bloco 7) para projeção por linha.
    # Vantagem: 1 chamada do engine cobre N linhas da tabela (escala bem).
    # Importante: `is_brazil = FALSE` aqui evita que o engine tente agregar
    # por filtros geográficos (queremos só as taxas).
    per_capita <- reactive({
      if (!available()) return(NULL)
      g <- input_global()

      cfg_pc <- tryCatch(
        cc_engine_settings(
          country_code     = br_code,
          pop_mode         = "other",
          coverage         = g$coverage %||% 70,
          screen_method    = g$screen_method %||% "hpv",
          target_age_min   = g$target_age_min %||% 25,
          target_age_max   = g$target_age_max %||% 64,
          custom_pop       = 1,
          p16_18           = g$p16_18,
          poutros          = g$poutros,
          pneg             = g$pneg,
          cito_out_pos     = g$cito_out_pos,
          cito_out_neg     = g$cito_out_neg,
          colpo16_pos      = g$colpo16_pos,
          colpo16_neg      = g$colpo16_neg,
          colpoout_pos     = g$colpoout_pos,
          colpoout_neg     = g$colpoout_neg,
          b16_neg_nic1     = g$b16_neg_nic1,
          b16_nic23        = g$b16_nic23,
          b16_cancer       = g$b16_cancer,
          bo_neg_nic1      = g$bo_neg_nic1,
          bo_nic23         = g$bo_nic23,
          bo_cancer        = g$bo_cancer,
          first_time_pct         = g$first_time_pct,
          unsatisfactory_pct     = g$unsatisfactory_pct,
          res_asch_pct           = g$res_asch_pct,
          res_other_pct          = g$res_other_pct,
          res_neg_pct            = g$res_neg_pct,
          colpo_asch_pct         = g$colpo_asch_pct,
          colpo_other_follow_pct = g$colpo_other_follow_pct,
          biopsy_pos_asch_pct    = g$biopsy_pos_asch_pct,
          biopsy_pos_other_pct   = g$biopsy_pos_other_pct,
          b_asch_nic23_pct       = g$b_asch_nic23_pct,
          b_asch_cancer_pct      = g$b_asch_cancer_pct,
          b_asch_neg_nic1_pct    = g$b_asch_neg_nic1_pct,
          b_other_nic23_pct      = g$b_other_nic23_pct,
          b_other_cancer_pct     = g$b_other_cancer_pct,
          b_other_neg_nic1_pct   = g$b_other_neg_nic1_pct,
          is_brazil = FALSE
        ),
        error = function(e) NULL
      )
      if (is.null(cfg_pc)) return(NULL)

      res_pc <- tryCatch(
        cc_engine_run(NULL, cfg_pc, pop_mun_regional = NULL),
        error = function(e) NULL
      )
      if (is.null(res_pc)) return(NULL)

      m <- res_pc$metrics
      method <- res_pc$screen_method

      # HPV → 1 teste por mulher-alvo (rastreada).
      # Citologia → soma rastreamento + diagnóstica (necessidade total).
      tests_pc <- if (identical(method, "hpv")) {
        as.numeric(m$rastreada[1])
      } else {
        r <- as.numeric(m$cit_rastreamento[1])
        d <- as.numeric(m$cit_diagnostica[1])
        (if (is.finite(r)) r else 0) + (if (is.finite(d)) d else 0)
      }

      list(
        method   = method,
        tests    = tests_pc,
        colpo    = as.numeric(m$colpo_indicada[1]),
        biopsia  = as.numeric(m$biopsia_indicada[1]),
        ezt      = as.numeric(m$ezt[1])
      )
    })

    # ── Bloco 5: pop_agg — população alvo agregada por nível ───────────────
    # Aplica filtro de faixa etária (target_age_min/max), depois cascata de
    # filtros geo, e agrega pop_total + pop_sus pelo `by_cols` do nível.
    pop_agg <- reactive({
      if (!available()) return(NULL)
      g   <- input_global()
      gran <- effective_gran()

      age_min <- as.numeric(g$target_age_min %||% 25)
      age_max <- as.numeric(g$target_age_max %||% 64)

      dt <- data.table::as.data.table(pop_mun_regional)
      dt <- dt[from >= age_min & to <= age_max]

      if (!is.null(g$filt_uf)    && length(g$filt_uf))    dt <- dt[UF %in% g$filt_uf]
      if (!is.null(g$filt_macro) && length(g$filt_macro)) dt <- dt[`Macrorregiao de Saude` %in% g$filt_macro]
      if (!is.null(g$filt_reg)   && length(g$filt_reg))   dt <- dt[`Regiao de Saude` %in% g$filt_reg]
      if (!is.null(g$filt_mun)   && length(g$filt_mun))   dt <- dt[Municipio %in% g$filt_mun]

      if (!nrow(dt)) return(data.table::data.table())

      by_cols <- switch(gran,
        uf    = c("UF"),
        macro = c("UF", "Macrorregiao de Saude"),
        reg   = c("UF", "Macrorregiao de Saude", "Regiao de Saude"),
        mun   = c("UF", "Macrorregiao de Saude", "Regiao de Saude", "Municipio")
      )

      dt[, .(pop_total = sum(pop_total, na.rm = TRUE),
             pop_sus   = sum(pop_sus,   na.rm = TRUE)),
         by = by_cols]
    })

    # ── Bloco 6: sia_agg — produção SIA agregada por nível ─────────────────
    # Filtra por geo_ref (atendimento/residência) + cascata geo, agrega
    # `total_all` por categoria, depois pivota long → wide (1 coluna por
    # categoria). `dcast` exige escapar nomes com espaço (UF/Macro/Reg/Mun).
    sia_agg <- reactive({
      if (!available()) return(NULL)
      g    <- input_global()
      gran <- effective_gran()

      geo_ref_sel <- g$sia_geo_ref %||% "care"
      if (!geo_ref_sel %in% c("care", "res")) geo_ref_sel <- "care"

      dt <- data.table::as.data.table(sia_cc_resumo)
      dt <- dt[geo_ref == geo_ref_sel]

      if (!is.null(g$filt_uf)    && length(g$filt_uf))    dt <- dt[UF %in% g$filt_uf]
      if (!is.null(g$filt_macro) && length(g$filt_macro)) dt <- dt[`Macrorregiao de Saude` %in% g$filt_macro]
      if (!is.null(g$filt_reg)   && length(g$filt_reg))   dt <- dt[`Regiao de Saude` %in% g$filt_reg]
      if (!is.null(g$filt_mun)   && length(g$filt_mun))   dt <- dt[Municipio %in% g$filt_mun]

      if (!nrow(dt)) return(data.table::data.table())

      by_cols <- switch(gran,
        uf    = c("UF"),
        macro = c("UF", "Macrorregiao de Saude"),
        reg   = c("UF", "Macrorregiao de Saude", "Regiao de Saude"),
        mun   = c("UF", "Macrorregiao de Saude", "Regiao de Saude", "Municipio")
      )

      cats <- c("citologia", "colposcopia", "biopsia", "tratamento")
      dt_filt <- dt[categoria %in% cats]

      dt_long <- dt_filt[, .(total = sum(total_all, na.rm = TRUE)),
                         by = c(by_cols, "categoria")]

      lhs <- paste(
        sapply(by_cols, function(x) if (grepl(" ", x, fixed = TRUE)) paste0("`", x, "`") else x),
        collapse = "+"
      )
      dt_wide <- data.table::dcast(dt_long,
                                   formula   = as.formula(paste(lhs, "~ categoria")),
                                   value.var = "total",
                                   fill      = 0)

      # Garante que as 4 categorias existem mesmo se ausentes no SIA.
      for (cat in cats) {
        if (!cat %in% names(dt_wide)) dt_wide[, (cat) := 0L]
      }

      dt_wide[]
    })

    # ── Bloco 7: tabela_base — junção pop × SIA × per-capita ──────────────
    # **Núcleo do módulo**. Combina:
    #   pop_agg (denominador) × per_capita (taxas) → 4 colunas *_needed
    #   pop_agg × sia_agg                           → 4 colunas (citologia/colpo/biopsia/tratamento)
    #   produzido / necessário                      → 4 colunas cov_* em %
    # Decisão: pop_alvo_col = "pop_sus" se br_pop_tipo="sus", senão "pop_total".
    # `safe_pct` evita divisão por zero (NA quando den ≤ 0).
    tabela_base <- reactive({
      if (!available()) return(NULL)
      g   <- input_global()
      gran <- effective_gran()
      pc  <- per_capita()
      if (is.null(pc)) return(NULL)

      dt_pop <- pop_agg()
      dt_sia <- sia_agg()
      if (is.null(dt_pop) || !nrow(dt_pop)) return(data.table::data.table())

      by_cols <- switch(gran,
        uf    = c("UF"),
        macro = c("UF", "Macrorregiao de Saude"),
        reg   = c("UF", "Macrorregiao de Saude", "Regiao de Saude"),
        mun   = c("UF", "Macrorregiao de Saude", "Regiao de Saude", "Municipio")
      )

      pop_alvo_col <- if (isTRUE(g$br_pop_tipo == "sus")) "pop_sus" else "pop_total"

      if (!is.null(dt_sia) && nrow(dt_sia)) {
        dt <- merge(dt_pop, dt_sia, by = by_cols, all.x = TRUE)
      } else {
        dt <- data.table::copy(dt_pop)
        dt[, `:=`(citologia = NA_real_, colposcopia = NA_real_,
                  biopsia = NA_real_, tratamento = NA_real_)]
      }

      for (col in c("citologia", "colposcopia", "biopsia", "tratamento")) {
        if (col %in% names(dt)) {
          dt[is.na(get(col)), (col) := 0]
        } else {
          dt[, (col) := 0]
        }
      }

      dt[, `:=`(
        tests_needed   = round(get(pop_alvo_col) * pc$tests),
        colpo_needed   = round(get(pop_alvo_col) * pc$colpo),
        biopsia_needed = round(get(pop_alvo_col) * pc$biopsia),
        ezt_needed     = round(get(pop_alvo_col) * pc$ezt)
      )]

      safe_pct <- function(num, den) {
        data.table::fifelse(
          !is.na(den) & den > 0,
          100 * num / den,
          NA_real_
        )
      }

      dt[, `:=`(
        cov_cito  = safe_pct(citologia,   tests_needed),
        cov_colpo = safe_pct(colposcopia, colpo_needed),
        cov_biop  = safe_pct(biopsia,     biopsia_needed),
        cov_ezt   = safe_pct(tratamento,  ezt_needed)
      )]

      dt[]
    })

    # ── Bloco 8a: params_line — resumo do cenário (rodapé do header) ──────
    # Linha em itálico mostrada acima da tabela: método + idades + cobertura
    # + preset. **Apenas o preset HPV** é resolvido para label legível
    # (citologia usa o `cito_param_source` direto — ver pendência).
    params_line <- reactive({
      g  <- input_global()
      pc <- per_capita()
      method_lbl <- if (!is.null(pc) && pc$method == "cytology") "Cytology" else "HPV"
      preset_lbl <- as.character(g$hpv_param_source %||% "Default")
      if (exists("HPV_PRESETS") && preset_lbl %in% names(HPV_PRESETS))
        preset_lbl <- HPV_PRESETS[[preset_lbl]]$label %||% preset_lbl

      age_min <- g$target_age_min %||% 25
      age_max <- g$target_age_max %||% 64
      cov     <- g$coverage %||% 70

      paste0(
        "Method: ", method_lbl,
        "  \u00B7  Ages ", age_min, "\u2013", age_max,
        "  \u00B7  Coverage ", cov, "%",
        "  \u00B7  Preset: ", preset_lbl
      )
    })

    # ── Bloco 8b: col_labels — rótulos por nível (para o filtro top do DT) ──
    # NOTA: hoje não é chamada no módulo (a renomeação acontece em
    # `build_display_dt`). Mantida como helper público de label, porém
    # candidata a remoção (código morto — ver pendência).
    col_labels <- function(gran, method) {
      test_lbl <- if (identical(method, "hpv")) "HPV tests needed" else "Pap smears needed"
      id_cols <- switch(gran,
        uf    = c("UF"),
        macro = c("Macro-region", "UF"),
        reg   = c("Health region", "Macro-region", "UF"),
        mun   = c("Municipality", "Health region", "Macro-region", "UF")
      )
      c(id_cols,
        "Female pop (total)", "Female pop (SUS-dep.)",
        test_lbl, "Colposcopies needed", "Biopsies needed", "EZTs needed",
        "Cytologies produced", "Colposcopies produced", "Biopsies produced", "EZTs produced",
        "Cov. — cytology/primary (%)", "Cov. — colposcopy (%)",
        "Cov. — biopsy (%)", "Cov. — EZT (%)")
    }

    # ── Bloco 8c: build_display_dt — pipeline de exibição ──────────────────
    # Reordena colunas (geo do nível mais fino → mais grosso) + renomeia para
    # rótulos amigáveis em inglês. Usa `..all_cols` (data.table .SD-style)
    # para subset por nomes. `intersect()` é defensivo (nem toda granularidade
    # tem todas as colunas geo).
    build_display_dt <- function(dt, gran, method) {
      if (is.null(dt) || !nrow(dt)) return(dt)

      geo_cols <- switch(gran,
        uf    = c("UF"),
        macro = c("Macrorregiao de Saude", "UF"),
        reg   = c("Regiao de Saude", "Macrorregiao de Saude", "UF"),
        mun   = c("Municipio", "Regiao de Saude", "Macrorregiao de Saude", "UF")
      )

      data_cols <- c("pop_total", "pop_sus",
                     "tests_needed", "colpo_needed", "biopsia_needed", "ezt_needed",
                     "citologia", "colposcopia", "biopsia", "tratamento",
                     "cov_cito", "cov_colpo", "cov_biop", "cov_ezt")

      all_cols <- c(geo_cols, data_cols)
      all_cols <- intersect(all_cols, names(dt))
      dt_out <- dt[, ..all_cols]

      test_lbl <- if (identical(method, "hpv")) "HPV tests needed" else "Pap smears needed"
      rename_map <- c(
        "UF"                        = "UF",
        "Macrorregiao de Saude"     = "Macro-region",
        "Regiao de Saude"           = "Health region",
        "Municipio"                 = "Municipality",
        "pop_total"                 = "Female pop (total)",
        "pop_sus"                   = "Female pop (SUS-dep.)",
        "tests_needed"              = test_lbl,
        "colpo_needed"              = "Colposcopies needed",
        "biopsia_needed"            = "Biopsies needed",
        "ezt_needed"                = "EZTs needed",
        "citologia"                 = "Cytologies produced",
        "colposcopia"               = "Colposcopies produced",
        "biopsia"                   = "Biopsies produced",
        "tratamento"                = "EZTs produced",
        "cov_cito"                  = "Cov. cytology/primary (%)",
        "cov_colpo"                 = "Cov. colposcopy (%)",
        "cov_biop"                  = "Cov. biopsy (%)",
        "cov_ezt"                   = "Cov. EZT (%)"
      )
      new_names <- rename_map[names(dt_out)]
      new_names[is.na(new_names)] <- names(dt_out)[is.na(new_names)]
      data.table::setnames(dt_out, old = names(dt_out), new = as.character(new_names))

      dt_out
    }

    # ── Bloco 9: output$body — gating de UI ────────────────────────────────
    # Renderiza placeholder se:
    #   (a) país != Brasil → "This view is only available for Brazil."
    #   (b) pop manual    → "Detailed table requires IBGE population..."
    # Caso contrário monta: radio de granularidade + linha de params + botão
    # CSV + DT + nota explicativa sobre HPV vs Cito production gap.
    output$body <- renderUI({
      if (!is_brazil()) {
        return(div(
          class = "cc-kpi-card",
          # [C1] era #666 → var(--cc-gray2); font-size:14px → var(--t-base)
          style = "color:var(--cc-gray2); font-size:var(--t-base); padding:20px;",
          "This view is only available for Brazil."
        ))
      }
      if (pop_is_manual()) {
        return(div(
          class = "cc-kpi-card",
          style = "color:var(--cc-gray2); font-size:var(--t-base); padding:20px;",
          "Detailed table is only available with IBGE population (total or SUS-dependent).",
          " Manual population cannot be distributed across municipalities."
        ))
      }

      tagList(
        div(
          style = "margin-bottom:12px;",
          radioButtons(
            ns("granularity"),
            label    = NULL,
            choices  = c("By municipality"             = "mun",
                         "By smallest selected level"  = "auto"),
            selected = "mun",
            inline   = TRUE
          )
        ),

        div(
          style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:10px;",
          div(
            # [C1] era #888 → var(--cc-gray2); font-size:12px → var(--t-xs)
            style = "font-size:var(--t-xs); color:var(--cc-gray2); font-style:italic;",
            textOutput(ns("params_text"), inline = TRUE)
          ),
          downloadButton(
            ns("download_csv"),
            label = "Download CSV",
            # [T3] era #0B7285 hardcoded → var(--cc-dark)
            style = "font-size:var(--t-xs); padding:4px 12px; background:var(--cc-dark); color:#fff; border:none; border-radius:5px; cursor:pointer;"
          )
        ),

        DT::DTOutput(ns("table")),

        div(
          class = "cap-note",
          style = "margin-top:12px;",
          HTML(paste0(
            "SIA production shows actual SUS procedures (mostly cytology). ",
            "When HPV is selected, the gap reflects the shift from cytology-based ",
            "to HPV-based screening."
          ))
        )
      )
    })

    # ── Bloco 10a: output$params_text — render do resumo do cenário ──────
    output$params_text <- renderText({
      req(available())
      params_line()
    })

    # ── Bloco 10b: output$table — render principal (DT) ───────────────────
    # Pipeline:
    #   1) tabela_base() → renomeia via build_display_dt
    #   2) Arredonda colunas de cobertura para 1 casa
    #   3) Ordena pela 1ª col Cov.* (asc) — destaca quem tem maior gap
    #   4) Formata inteiros com vírgula de milhar
    #   5) Aplica `formatStyle` de cor de fundo por faixa de cobertura:
    #      <50% danger / 50–100% warning / ≥100% success
    output$table <- DT::renderDT({
      req(available())
      dt <- tabela_base()
      pc <- per_capita()
      if (is.null(dt) || !nrow(dt)) {
        return(DT::datatable(data.frame(Message = "No municipalities match the current filters."),
                             rownames = FALSE, options = list(dom = "t")))
      }

      gran   <- effective_gran()
      method <- if (!is.null(pc)) pc$method else "hpv"
      dt_out <- build_display_dt(dt, gran, method)

      gap_cols <- grep("Cov\\.", names(dt_out))
      for (j in gap_cols) {
        set(dt_out, j = j, value = round(dt_out[[j]], 1))
      }

      sort_col_idx <- if (length(gap_cols) > 0L) gap_cols[1] - 1L else 0L

      tbl <- DT::datatable(
        dt_out,
        rownames = FALSE,
        filter   = "top",
        options  = list(
          pageLength     = 20,
          dom            = "frtip",
          scrollX        = TRUE,
          scrollY        = "370px",
          scrollCollapse = TRUE,
          order          = list(list(sort_col_idx, "asc"))
        )
      )

      int_cols <- grep("Female pop|needed|produced", names(dt_out), value = TRUE)
      if (length(int_cols)) {
        tbl <- DT::formatCurrency(tbl, columns = int_cols,
                                  currency = "", digits = 0,
                                  mark = ",", before = FALSE)
      }

      if (length(gap_cols)) {
        gap_col_names <- names(dt_out)[gap_cols]
        tbl <- DT::formatStyle(
          tbl,
          columns         = gap_col_names,
          # DT não suporta CSS vars — usar hex dos tokens semânticos
          # --cc-danger #C0392B / --cc-warning #B06000 / --cc-success #2E7D52
          backgroundColor = DT::styleInterval(
            cuts   = c(50, 100),
            values = c("#FDECEA", "#FFF3E0", "#E6F5EE")
          )
        )
      }

      tbl
    })

    # ── Bloco 11: output$download_csv — handler de download ───────────────
    # CSV pt-BR-friendly: separador `;`, decimal `,`, BOM UTF-8 (Excel-BR).
    # Filename: "detailed_table_<method>_<gran>_YYYYMMDD.csv"
    output$download_csv <- downloadHandler(
      filename = function() {
        g    <- input_global()
        meth <- g$screen_method %||% "hpv"
        gran <- effective_gran()
        paste0("detailed_table_", meth, "_", gran, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        dt <- tabela_base()
        pc <- per_capita()
        if (is.null(dt) || !nrow(dt)) {
          data.table::fwrite(data.table::data.table(Message = "No data"),
                             file, sep = ";", dec = ",", bom = TRUE)
          return(invisible(NULL))
        }
        gran   <- effective_gran()
        method <- if (!is.null(pc)) pc$method else "hpv"
        dt_out <- build_display_dt(dt, gran, method)
        data.table::fwrite(dt_out, file, sep = ";", dec = ",", bom = TRUE)
      }
    )
  })
}
