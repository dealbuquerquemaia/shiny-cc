# ===========================================================
# Shiny-cc — 15_mod_fluxo.R  (aba Pathway)
# ===========================================================
# Diagrama do fluxo de rastreamento + diagnóstico com contagens
# absolutas e percentuais em cada nó. Chama o engine
# (cc_engine_run) apenas para obter N rastreada; o restante
# das contagens é recalculado aqui a partir de input_global()
# (percentuais) para alimentar os renderizadores
# render_pathway_hpv() / render_pathway_cytology() de
# 01_utils_cc.R.
#
# Blocos do server:
#   1. ci() / br_code                      — identificação país via
#      cc_country_info() (helper único em 01_utils_cc.R)
#   2. geo_label                           — rótulo geográfico
#      (OBS: geo_label devolve UMA string — nível mais fino
#      com seleção — diferente do geo_desc concatenado em
#      Summary/Equipment.)
#   3. cfg_reactive                        — tradução sidebar →
#      cc_engine_settings (padrão replicado entre módulos)
#   4. engine_res                          — roda cc_engine_run
#   5. fluxo                               — reativo principal:
#      contagens de cada nó do diagrama (HPV ou Citologia)
#   6. caption_txt                         — legenda do diagrama
#   7. output$pathway_ui                   — despacha render p/
#      render_pathway_hpv() ou render_pathway_cytology()
# ===========================================================

# ---- UI ----
# Injeta pathway.css e um MutationObserver que escalona
# qualquer .pathway-scalable proporcionalmente ao contêiner
# (baseline 1200x780). Mantém o diagrama responsivo em telas
# menores sem reflow dos nós (tudo escala via CSS transform).
mod_fluxo_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "pathway.css"),
      tags$script(HTML("
(function () {
  function scalePathway() {
    document.querySelectorAll('.pathway-scalable').forEach(function (el) {
      var wrapper = el.parentElement;
      if (!wrapper) return;

      var rect   = wrapper.getBoundingClientRect();
      var availH = window.innerHeight - rect.top + 55;
      if (availH <= 0) return;

      var scale = Math.min(1, availH / 780);
      el.style.transform       = 'scale(' + scale + ')';
      el.style.transformOrigin = 'top left';
      wrapper.style.height = Math.ceil(780 * scale) + 'px';
      wrapper.style.width  = Math.ceil(1200 * scale) + 'px';
      });
  }

  window.addEventListener('resize', scalePathway);
  var obs = new MutationObserver(scalePathway);
  document.addEventListener('DOMContentLoaded', function () {
    obs.observe(document.body, { childList: true, subtree: true });
    scalePathway();
  });
})();
"))
    ),
    div(
      class = "cc-page-header",
      div(class = "cc-page-title", "Pathway")
    ),
    uiOutput(ns("pathway_ui"))
  )
}

mod_fluxo_server <- function(id,
                             df_completo,
                             dim_age,
                             dim_country,
                             input_global,
                             pop_mun_regional) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -----------------------------------------------------------------
    # Bloco 1 — País selecionado
    # -----------------------------------------------------------------
    # Helper único cc_country_info() em 01_utils_cc.R devolve
    # list(code, label, is_brazil) a partir de input_global() e
    # dim_country. Substitui o trio replicado em 11/14/15/16/17.
    # -----------------------------------------------------------------
    br_code <- tryCatch(
      {
        x <- dim_country[dim_country$population_name == "Brazil", "population_code"]
        if (length(x) == 0L || is.na(x[1])) NA_integer_ else as.integer(x[1])
      },
      error = function(e) NA_integer_
    )

    ci <- reactive(cc_country_info(input_global(), dim_country, br_code))

    # -----------------------------------------------------------------
    # Bloco 2 — Rótulo geográfico (geo_label)
    # -----------------------------------------------------------------
    # Devolve UMA string com o nível geográfico mais fino com seleção
    # (Municipality > Health region > Macro-region > State > "Brazil").
    # Diferente de geo_desc (Summary/Equipment), que concatena todos
    # os níveis. Helper único cc_geo_label() em 01_utils_cc.R.
    # -----------------------------------------------------------------
    geo_label <- reactive({
      cc_geo_label(input_global(), mode = "shortest",
                   dim_country = dim_country, br_code = br_code)
    })

    # -----------------------------------------------------------------
    # Bloco 3 — cfg (tradução sidebar → cc_engine_settings)
    # -----------------------------------------------------------------
    # Wrapper único: cc_cfg_from_input() em 02_engine_capacity_cc.R
    # encapsula a tradução input_global() → cc_engine_settings() e é
    # compartilhado entre Summary/Equipment/Pathway/Capacity/Compare.
    # -----------------------------------------------------------------
    cfg_reactive <- reactive({
      cc_cfg_from_input(input_global(), br_code)
    })

    # -----------------------------------------------------------------
    # Bloco 4 — engine_res
    # -----------------------------------------------------------------
    # Roda cc_engine_run(). Aqui consumimos essencialmente
    # $metrics$rastreada (HPV) ou $metrics$cit_rastreamento
    # (Citologia) como "N" inicial. As demais contagens do
    # diagrama são RECALCULADAS no bloco fluxo() abaixo, a partir
    # dos percentuais de input_global() — por isso o bloco
    # reimplementa boa parte do modelo clínico. Ver pendência.
    # -----------------------------------------------------------------
    engine_res <- reactive({
      cfg <- cfg_reactive()
      cc_engine_run(
        df_completo,
        cfg,
        pop_mun_regional = pop_mun_regional
      )
    })

    # -----------------------------------------------------------------
    # Bloco 5 — fluxo (contagens por nó do diagrama)
    # -----------------------------------------------------------------
    # Reativo principal. Devolve list nomeada com 1 campo por nó
    # do diagrama (escolha das chaves = contrato com
    # render_pathway_hpv / render_pathway_cytology em utils).
    #
    # Estratégia: pega N rastreada do engine e REAPLICA os % do
    # modelo para obter as contagens "amarradas" a cada braço.
    # p2() converte valor em % (0-100) p/ proporção (0-1) de
    # forma defensiva; se falha, devolve NA.
    #
    # Observação: as fórmulas abaixo replicam parte da lógica
    # que já roda em cc_workup_metrics()/modelo_hpv() do engine.
    # Duplicação proposital (aqui a granularidade é maior — ex.:
    # colpo_asch_pos/_neg, biopsy_neg_cin1_asch, etc. — que o
    # engine não expõe). Candidato de médio prazo: engine expor
    # esses nós para o módulo só renderizar.
    # -----------------------------------------------------------------
    fluxo <- reactive({
      res <- engine_res()
      if (is.null(res)) return(NULL)

      method <- as.character(cfg_reactive()$screen_method %||% input_global()$screen_method %||% "hpv")

      m <- res$metrics
      data.table::setDT(m)

      # p2(): % (0-100) → proporção (0-1) com fallback NA.
      p2 <- function(x, def = NA_real_) {
        x <- suppressWarnings(as.numeric(x))
        if (!is.finite(x) || is.na(x)) x <- def
        if (!is.finite(x) || is.na(x)) return(NA_real_)
        x / 100
      }

      g <- input_global()

      # ── Caminho Citologia ──────────────────────────────────────────
      # Fluxo: Pap → (ASC-H+ / outras alterações / negativo) →
      # colposcopia (por braço) → biópsia → desfechos
      # (NIC1/neg, NIC2/3, câncer).
      if (identical(method, "cytology")) {
        # N = rastreamento de citologia (já calculado pelo engine).
        N <- suppressWarnings(as.numeric(m$cit_rastreamento[1]))
        if (!is.finite(N) || is.na(N) || N <= 0) {
          return(list(method = method, women_screened = NA_real_))
        }

        # Braços do resultado do Pap (ASC-H+ / outras / negativo).
        # Negativo sai por subtração p/ fechar em N.
        p_asch  <- p2(g$res_asch_pct)
        p_other <- p2(g$res_other_pct)

        n_asch  <- N * p_asch
        n_other <- N * p_other
        n_neg   <- pmax(N - n_asch - n_other, 0)

        # "Diagnostic cytology" das outras alterações: só parte
        # segue pra colpo (colpo_other_follow_pct).
        n_diag_tot <- n_other
        p_diag_pos <- p2(g$colpo_other_follow_pct)
        n_diag_pos <- n_diag_tot * p_diag_pos
        n_diag_neg <- pmax(n_diag_tot - n_diag_pos, 0)

        # Colposcopia: ASC-H+ segue via colpo_asch_pct; outras via
        # filtro diagnóstico acima.
        p_col_asch <- p2(g$colpo_asch_pct)
        n_col_asch <- n_asch * p_col_asch

        n_col_other <- n_diag_pos

        # Biópsia: positividade por braço.
        p_bpos_asch  <- p2(g$biopsy_pos_asch_pct)
        p_bpos_other <- p2(g$biopsy_pos_other_pct)

        n_b_asch  <- n_col_asch  * p_bpos_asch
        n_b_other <- n_col_other * p_bpos_other

        n_colneg_asch  <- pmax(n_col_asch  - n_b_asch,  0)
        n_colneg_other <- pmax(n_col_other - n_b_other, 0)

        # Desfechos da biópsia por braço (NIC1/neg, NIC2/3, câncer).
        p_asch_neg1  <- p2(g$b_asch_neg_nic1_pct)
        p_asch_nic23 <- p2(g$b_asch_nic23_pct)
        p_asch_can   <- p2(g$b_asch_cancer_pct)

        p_oth_neg1   <- p2(g$b_other_neg_nic1_pct)
        p_oth_nic23  <- p2(g$b_other_nic23_pct)
        p_oth_can    <- p2(g$b_other_cancer_pct)

        return(list(
          method = method,
          women_screened = N,

          negative = n_neg,
          asch     = n_asch,
          other    = n_other,

          diag_tot = n_diag_tot,
          diag_pos = n_diag_pos,
          diag_neg = n_diag_neg,

          colpo_asch_pos = n_b_asch,
          colpo_asch_neg = n_colneg_asch,

          colpo_other_pos = n_b_other,
          colpo_other_neg = n_colneg_other,

          biopsy_neg_cin1_asch = n_b_asch  * p_asch_neg1,
          cin2_3_asch          = n_b_asch  * p_asch_nic23,
          cancer_asch          = n_b_asch  * p_asch_can,

          biopsy_neg_cin1_other = n_b_other * p_oth_neg1,
          cin2_3_other          = n_b_other * p_oth_nic23,
          cancer_other          = n_b_other * p_oth_can
        ))
      }

      # ── Caminho HPV ───────────────────────────────────────────────
      # Fluxo: HPV → (16/18+ / outros HR+ / neg) → (16/18+) colposcopia
      # direta; (outros HR+) citologia reflexa → colposcopia → biópsia →
      # desfechos. Braço negativo fica em seguimento (não aparece abaixo).
      if (!identical(method, "hpv")) {
        return(list(method = method))
      }

      N <- suppressWarnings(as.numeric(m$rastreada[1]))
      if (!is.finite(N) || is.na(N) || N <= 0) {
        return(list(method = method, women_screened = NA_real_))
      }

      # Resultado do HPV teste: 3 braços. Renormaliza p/ soma=1
      # (defensivo caso usuário tenha desmarcado "Lock" e somado ≠ 100).
      p16  <- p2(g$p16_18)
      pout <- p2(g$poutros)
      pneg <- p2(g$pneg)

      tot <- p16 + pout + pneg
      if (!is.finite(tot) || is.na(tot) || tot <= 0) tot <- 1
      w16  <- p16 / tot
      wout <- pout / tot
      wneg <- pneg / tot

      n_neg <- N * wneg
      n16   <- N * w16
      nout  <- N * wout

      # Citologia reflexa no braço "outros HR+".
      cito_out_pos <- p2(g$cito_out_pos)
      cito_out_neg <- p2(g$cito_out_neg)

      n_cito_pos <- nout * cito_out_pos
      n_cito_neg <- nout * cito_out_neg

      # Colposcopia: 16/18+ vem direto do teste; outros HR+ vêm
      # da citologia reflexa positiva.
      colpo16_pos  <- p2(g$colpo16_pos)
      colpoout_pos <- p2(g$colpoout_pos)

      n_colpo_pos_16  <- n16        * colpo16_pos
      n_colpo_pos_out <- n_cito_pos * colpoout_pos

      n_colpo_neg_16  <- pmax(n16        - n_colpo_pos_16,  0)
      n_colpo_neg_out <- pmax(n_cito_pos - n_colpo_pos_out, 0)

      # Desfechos da biópsia por braço.
      b16_neg_nic1 <- p2(g$b16_neg_nic1)
      b16_nic23    <- p2(g$b16_nic23)
      b16_cancer   <- p2(g$b16_cancer)

      bo_neg_nic1 <- p2(g$bo_neg_nic1)
      bo_nic23    <- p2(g$bo_nic23)
      bo_cancer   <- p2(g$bo_cancer)

      list(
        method = method,

        women_screened = N,
        negative       = n_neg,
        hpv16_18       = n16,
        other_hrhpv    = nout,

        cytology_pos = n_cito_pos,
        cytology_neg = n_cito_neg,

        colpo_pos_16  = n_colpo_pos_16,
        colpo_neg_16  = n_colpo_neg_16,
        colpo_pos_out = n_colpo_pos_out,
        colpo_neg_out = n_colpo_neg_out,

        biopsy_neg_cin1_16  = n_colpo_pos_16  * b16_neg_nic1,
        cin2_3_16           = n_colpo_pos_16  * b16_nic23,
        cancer_16           = n_colpo_pos_16  * b16_cancer,

        biopsy_neg_cin1_out = n_colpo_pos_out * bo_neg_nic1,
        cin2_3_out          = n_colpo_pos_out * bo_nic23,
        cancer_out          = n_colpo_pos_out * bo_cancer
      )
    })

    # -----------------------------------------------------------------
    # Bloco 6 — caption_txt (legenda sob o diagrama)
    # -----------------------------------------------------------------
    # "Method: X screening every N years. Age range: 25-64 years.
    #  Coverage: 70%. Population source: WPP 2025. HPV parameters: …"
    # -----------------------------------------------------------------
    caption_txt <- reactive({
      g   <- input_global()
      cfg <- cfg_reactive()

      method_lbl <- if (identical(cfg$screen_method, "cytology"))
        "Cytology screening"
      else
        "HPV screening"

      interval_lbl <- if (identical(cfg$screen_method, "cytology"))
        "every 3 years"
      else
        "every 5 years"

      preset_lbl <- if (!is.null(g$hpv_param_source) && nzchar(g$hpv_param_source))
        paste0(" HPV parameters: ", g$hpv_param_source, ".")
      else
        ""

      age_min <- cfg$target_age_min %||% 25
      age_max <- cfg$target_age_max %||% 64
      cov     <- cfg$coverage %||% 70

      paste0(
        "Method: ", method_lbl, " ", interval_lbl, ".",
        " Age range: ", age_min, "\u2013", age_max, " years.",
        " Coverage: ", cov, "%.",
        " Population source: World Population Prospects 2025.",
        preset_lbl
      )
    })

    # -----------------------------------------------------------------
    # Bloco 7 — Render final
    # -----------------------------------------------------------------
    # Valida (f não vazio, método conhecido) e despacha para o
    # renderizador apropriado em 01_utils_cc.R
    # (render_pathway_hpv / render_pathway_cytology).
    # -----------------------------------------------------------------
    output$pathway_ui <- renderUI({
      f <- fluxo()

      if (is.null(f) || is.na(f$women_screened) || f$women_screened <= 0) {
        return(tags$p(
          style = "color:var(--cc-dark); margin-top:40px; text-align:center;",
          "Flow not available for current settings."
        ))
      }

      if (!f$method %in% c("hpv", "cytology")) {
        return(tags$p(
          style = "color:var(--cc-dark); margin-top:40px; text-align:center;",
          "Flow not available for current settings."
        ))
      }

      geo  <- geo_label()
      cap  <- caption_txt()

      if (identical(f$method, "hpv")) {
        render_pathway_hpv(f, geo, cap)
      } else {
        render_pathway_cytology(f, geo, cap)
      }
    })

  })
}
