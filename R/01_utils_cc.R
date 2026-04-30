# ===========================================================
# 01_utils_cc.R
# -----------------------------------------------------------
# Utilitários de uso geral, compartilhados por todos os módulos:
#   - Parsing de faixas etárias (GLOBOCAN usa rótulos "50-54", "85+", etc.)
#   - Formatação numérica padrão IARC (vírgula como separador de milhar,
#     ponto como separador decimal)
#   - Helper de tooltip (Bootstrap) e operador %||% (null-coalescing)
#   - Helpers de renderização do diagrama de fluxo (aba Pathway) — HPV e
#     Citologia. Produzem um card HTML absolutamente posicionado com nós,
#     conectores SVG e cabeçalhos de fase.
#
# Convenção: funções com prefixo "." são internas ao arquivo (uso restrito
# aos helpers de pathway); não devem ser chamadas por módulos.
# ===========================================================

# -----------------------------------------------------------
# Idades — parser de rótulos de faixa etária (versão robusta)
# Aceita: "50-54" (range), "85+" (aberto à direita), NA ou string vazia.
# -----------------------------------------------------------

# Retorna idade mínima da faixa ("50-54" → 50; "85+" → 85).
age_band_min <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(trimws(x))] <- NA_character_
  
  out <- rep(NA_integer_, length(x))
  is_plus <- !is.na(x) & grepl("\\+$", x)
  is_range <- !is.na(x) & grepl("^[0-9]+\\s*-\\s*[0-9]+$", x)
  
  out[is_plus]  <- suppressWarnings(as.integer(sub("\\+$", "", trimws(x[is_plus]))))
  out[is_range] <- suppressWarnings(as.integer(sub("\\s*-.*$", "", trimws(x[is_range]))))
  
  out
}

# Retorna idade máxima da faixa ("50-54" → 54; "85+" → 120 como limite superior).
age_band_max <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(trimws(x))] <- NA_character_
  
  out <- rep(NA_integer_, length(x))
  is_plus <- !is.na(x) & grepl("\\+$", x)
  is_range <- !is.na(x) & grepl("^[0-9]+\\s*-\\s*[0-9]+$", x)
  
  out[is_plus]  <- 120L
  out[is_range] <- suppressWarnings(as.integer(sub("^.*-\\s*", "", trimws(x[is_range]))))
  
  out
}

# ===========================================================
# Formatação numérica (padrão IARC = notação internacional:
# vírgula separa milhar, ponto separa decimal — ex.: 12,345.6)
# NA, NULL ou vetor todo-NA retornam "–" (en-dash), usado consistentemente
# em cards/KPIs do app como placeholder.
# ===========================================================

# Contagens inteiras (ex.: 12,345). Arredonda antes de formatar.
fmt_int <- function(x) {
  if (is.null(x) || all(is.na(x))) return("–")
  formatC(
    as.integer(round(x)),
    format       = "d",
    big.mark     = ",",
    decimal.mark = "."
  )
}


# Taxas / percentuais / valores com casas decimais (ex.: 12,345.6 ou 123.4).
# `digits` controla casas decimais (default 1).
fmt_rate <- function(x, digits = 1) {
  if (is.null(x) || all(is.na(x))) return("–")
  formatC(
    x,
    format       = "f",
    digits       = digits,
    big.mark     = ",",
    decimal.mark = "."
  )
}

# ===========================================================
# Tooltip Bootstrap — envolve uma tag HTML adicionando os atributos
# `title`, `data-toggle`, `data-placement` e `data-container`. A ativação
# de fato acontece no JS do `app.R` (`$('[data-toggle="tooltip"]').tooltip()`).
# Se `tooltip` for NULL/NA/"", retorna a tag inalterada.
# ===========================================================

cc_with_tt <- function(tag, tooltip, placement = "top") {
  if (is.null(tooltip) || all(is.na(tooltip))) return(tag)
  tooltip <- as.character(tooltip)[1L]
  if (is.na(tooltip) || !nzchar(trimws(tooltip))) return(tag)
  
  htmltools::tagAppendAttributes(
    tag,
    title = tooltip,
    `data-toggle` = "tooltip",
    `data-placement` = placement,
    `data-container` = "body"
  )
}

# Operador null-coalescing: `a %||% b` retorna `a` se não for NULL, senão `b`.
# Usado amplamente pelos módulos pra dar default a inputs/reativos opcionais.
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ===========================================================
# cc_check_schema() — validação de presença de colunas
# -----------------------------------------------------------
# Helper único que substitui 3 implementações com assinatura quase
# idêntica espalhadas pelo projeto:
#   - data-raw/04_checks_cc.R   `expect_cols(dt, cols, nm)`  (cli/emoji)
#   - data-raw/01_prepare_cc.R  `must_have_cols(dt, cols)`   (stop)
#   - R/16_mod_capacidade.R     bloco inline                  (stop)
#
# Args:
#   dt       data.table | data.frame
#   expected character — colunas obrigatórias
#   context  character(1) — descrição usada na mensagem
#                           (e.g., "build_population_base", "sus_proc_resumo")
#   on_fail  "stop"  → stop() imediato (default; uso em ETL e em runtime)
#            "warn"  → warning() + invisible(FALSE)
#            "alert" → cli::cli_alert_danger() (ou message() se cli ausente)
#                      + invisible(FALSE) — para scripts de QA que coletam
#                      múltiplas anomalias antes de continuar
#
# Returns: invisible(TRUE) se schema OK; comportamento conforme on_fail caso
#          contrário. Mensagem padronizada em inglês para casar com o resto
#          do app: "[<context>] missing column(s): a, b, c".
#
# Escopo: só checa **presença** de colunas. Tipo/coerção é fora do escopo
# (eventual extensão futura).
# ===========================================================
cc_check_schema <- function(dt, expected, context,
                            on_fail = c("stop", "warn", "alert")) {
  on_fail <- match.arg(on_fail)
  miss <- setdiff(expected, names(dt))
  if (!length(miss)) return(invisible(TRUE))

  msg <- sprintf("[%s] missing column(s): %s",
                 context, paste(miss, collapse = ", "))

  switch(on_fail,
    stop  = stop(msg, call. = FALSE),
    warn  = { warning(msg, call. = FALSE); invisible(FALSE) },
    alert = {
      if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_alert_danger(msg)
      } else {
        message(msg)
      }
      invisible(FALSE)
    }
  )
}

# ===========================================================
# cc_country_info() — info derivada do país selecionado
# -----------------------------------------------------------
# Helper único para o trio `country_code` + `country_label` + `is_brazil`
# que era replicado em 5 módulos plenos (11/14/15/16/17). Centraliza a
# coerção de `g$pais_sel` em integer, o lookup defensivo do nome do país
# em `dim_country` e o cálculo de `is_brazil`.
#
# Args:
#   g           list — output de input_global() (sidebar)
#   dim_country data.frame/data.table — dicionário (population_code, population_name)
#   br_code     integer — código GLOBOCAN do Brasil (default 1001L; o app.R
#               resolve o real via lookup e injeta nos módulos)
#
# Returns: list(code, label, is_brazil)
#   - code:      integer — req() em g$pais_sel; halta a reactive se NULL/NA
#   - label:     character — population_name correspondente; "Selected country"
#                como fallback defensivo (lookup falhou ou erro)
#   - is_brazil: logical — code == br_code (FALSE se br_code é NA)
#
# Uso típico (dentro de moduleServer):
#   ci <- reactive(cc_country_info(input_global(), dim_country, br_code))
#   ci()$code; ci()$label; ci()$is_brazil
# ===========================================================
cc_country_info <- function(g, dim_country, br_code = 1001L) {
  shiny::req(g$pais_sel)
  code <- suppressWarnings(as.integer(g$pais_sel))
  shiny::req(!is.na(code))

  label <- tryCatch({
    nm <- dim_country$population_name[dim_country$population_code == code]
    if (length(nm) == 0L || is.na(nm[1])) "Selected country" else as.character(nm[1])
  }, error = function(e) "Selected country")

  list(
    code      = code,
    label     = label,
    is_brazil = !is.na(br_code) && isTRUE(code == br_code)
  )
}

# ===========================================================
# cc_geo_label() — rótulo geográfico unificado
# -----------------------------------------------------------
# Helper único que substitui as 5 implementações distintas de "rótulo
# geográfico" antes espalhadas em 11/14/15/16/17/20:
#   - geo_desc       (11/14/16/20) — concat com/sem pop_tipo
#   - geo_label      (15)          — shortest com prefixo de nível
#   - cap_geo_label  (16)          — shortest com fallback non_br literal
#   - geo_name_label (16)          — shortest sem prefixo, multi "(+K-1)"
#   - geo_text       (17)          — concat com pop_tipo, só primeiro nível
#
# Modos:
#   "concat"   — concatena Brazil [+ pop_tipo] + filtros selecionados.
#                Cada filtro vira "<valor>" ou "<valor> (n=K)" (multi).
#                Com first_level_only = TRUE, só o primeiro nível
#                (UF > Macro > Reg > Mun) com seleção entra.
#   "shortest" — devolve UMA string com o nível MAIS FINO selecionado
#                (Mun > Reg > Macro > UF). Com level_label = TRUE,
#                vira "<Nível>: <valor>"; senão só "<valor>".
#                Sem nenhum filtro: br_empty_label (default "Brazil";
#                se NULL, devolve NULL — replica geo_name_label).
#
# Args:
#   g                list — output de input_global() (precisa pais_sel,
#                    filt_uf/macro/reg/mun, br_pop_tipo se pop_tipo=TRUE)
#   mode             "concat" | "shortest"
#   dim_country      data.frame/data.table (opcional) — para resolver
#                    nome do país quando não-Brasil
#   br_code          integer — código GLOBOCAN do Brasil (default 1001L)
#   pop_tipo         logical — em modo concat, prepende pop tipo ("Total
#                    population" / "SUS-dependent") após "Brazil"
#   first_level_only logical — em modo concat, restringe a 1 nível (geo_text)
#   sep              chr — separador do modo concat (default " - ")
#   level_label      logical — em modo shortest, prefixa "<Nível>: "
#   multi_format     "count" → "(n=K)" / "increment" → "(+K-1)"
#   non_br_label     chr|NULL — fora do Brasil, retorna esta string literal;
#                    se NULL, faz lookup em dim_country (ou "Selected country")
#   br_empty_label   chr|NULL — em modo shortest, valor de fallback quando
#                    Brasil sem filtros (NULL devolve NULL)
#
# Returns: character(1) ou NULL (quando br_empty_label = NULL e Brasil sem
#          filtros em modo shortest).
# ===========================================================
cc_geo_label <- function(g,
                         mode = c("concat", "shortest"),
                         dim_country = NULL,
                         br_code = 1001L,
                         pop_tipo = FALSE,
                         first_level_only = FALSE,
                         sep = " - ",
                         level_label = TRUE,
                         multi_format = c("count", "increment"),
                         non_br_label = NULL,
                         br_empty_label = "Brazil") {
  mode <- match.arg(mode)
  multi_format <- match.arg(multi_format)

  # Resolução defensiva do país (sem req() — função pura)
  code <- suppressWarnings(as.integer(g$pais_sel))
  is_brazil <- !is.null(code) && length(code) == 1L && !is.na(code) &&
               !is.na(br_code) && isTRUE(code == br_code)

  # ── Não-Brasil: literal customizado ou nome do país ─────────────
  if (!is_brazil) {
    if (!is.null(non_br_label)) return(as.character(non_br_label))
    if (!is.null(dim_country)) {
      lab <- tryCatch({
        nm <- dim_country$population_name[dim_country$population_code == code]
        if (length(nm) == 0L || is.na(nm[1])) "Selected country" else as.character(nm[1])
      }, error = function(e) "Selected country")
      return(lab)
    }
    return("Selected country")
  }

  # ── Brasil: níveis em ordem hierárquica (UF → Macro → Reg → Mun) ─
  levels_list <- list(
    list(x = g$filt_uf,    name = "State"),
    list(x = g$filt_macro, name = "Macro-region"),
    list(x = g$filt_reg,   name = "Health region"),
    list(x = g$filt_mun,   name = "Municipality")
  )

  # Sanitiza um vetor de seleção (NULL/NA/empty → character(0))
  norm_x <- function(x) {
    if (is.null(x) || !length(x)) return(character(0))
    x <- as.character(x)
    x[!is.na(x) & nzchar(x)]
  }

  # Formata 1 ou K seleções: "valor" ou "valor (n=K)" / "valor (+K-1)"
  fmt_multi <- function(x) {
    if (length(x) == 1L) return(x[1])
    if (multi_format == "count") return(paste0(x[1], " (n=", length(x), ")"))
    paste0(x[1], " (+", length(x) - 1L, ")")
  }

  # ── Modo "shortest": só o nível mais fino (Mun > Reg > Macro > UF) ─
  # Importante: com level_label = TRUE, o sufixo "(n=K)/(+K-1)" anexa ao
  # nível ("State (n=2): SP"), replicando geo_label/cap_geo_label originais.
  # Com level_label = FALSE, anexa ao valor ("SP (+1)"), replicando
  # geo_name_label original.
  if (mode == "shortest") {
    for (lvl in rev(levels_list)) {
      vals <- norm_x(lvl$x)
      if (length(vals)) {
        if (!level_label) return(fmt_multi(vals))
        if (length(vals) == 1L) return(paste0(lvl$name, ": ", vals[1]))
        suffix <- if (multi_format == "count")
                    paste0(" (n=", length(vals), ")")
                  else
                    paste0(" (+", length(vals) - 1L, ")")
        return(paste0(lvl$name, suffix, ": ", vals[1]))
      }
    }
    return(br_empty_label)  # NULL → devolve NULL (replica geo_name_label)
  }

  # ── Modo "concat": Brazil [- pop_tipo] [- filtros] ───────────────
  parts <- "Brazil"
  if (pop_tipo) {
    pop_lbl <- if (isTRUE(g$br_pop_tipo == "sus")) "SUS-dependent" else "Total population"
    parts <- c(parts, pop_lbl)
  }

  if (first_level_only) {
    # Só o primeiro nível com seleção (ordem: UF → Macro → Reg → Mun)
    for (lvl in levels_list) {
      vals <- norm_x(lvl$x)
      if (length(vals)) { parts <- c(parts, fmt_multi(vals)); break }
    }
  } else {
    for (lvl in levels_list) {
      vals <- norm_x(lvl$x)
      if (length(vals)) parts <- c(parts, fmt_multi(vals))
    }
  }

  paste(parts, collapse = sep)
}

# ===========================================================
# Helpers do diagrama de fluxo (aba Pathway) — HPV & Citologia
# -----------------------------------------------------------
# O diagrama é um card HTML com nós posicionados em absoluto (px) dentro
# de `.pathway-container`. Conectores são desenhados em um <svg> sobreposto
# com `preserveAspectRatio="none"` (viewBox 1200x780) que escala junto.
# Cabeçalhos de fase ("Screening", "HPV result", etc.) ficam no topo.
#
# Paleta de nós (`.NODE_PROPS`):
#   n0 — branco (origem: mulheres rastreadas)
#   n1 — cinza-azulado claro (negativos / biópsia negativa / CIN1)
#   n2 — verde-azulado médio (outros HR-HPV / outras alterações cito)
#   n3 — verde-azulado escuro (positivos que seguem para colposcopia)
#   n4 — verde-azulado muito escuro (desfechos CIN2+/câncer — chamar atenção)
# ===========================================================

.NODE_PROPS <- list(
  n0 = list(bg = "#ffffff", bd = "1.5px solid #b7d1ce", col = "#1a3c3a"),
  n1 = list(bg = "#E8F0EF", bd = "1px solid #c6d8d5",   col = "#1a3c3a"),
  n2 = list(bg = "#9FC2BE", bd = "1px solid #82adaa",   col = "#0f2928"),
  n3 = list(bg = "#649a95", bd = "1px solid #3d7370",   col = "#0f2928"),
  n4 = list(bg = "#2D6B65", bd = "1px solid #245e58",   col = "#ffffff")
)

# Gera um <div.node> para o diagrama.
#   cls       — uma das chaves de .NODE_PROPS ("n0".."n4")
#   left/top  — posição absoluta em px dentro do container
#   label     — texto principal do nó (ex.: "Colposcopy +")
#   count_fmt — valor já formatado (string; usar .pfmt)
#   pct_txt   — texto "X% of <label>" (opcional; usar .ppct)
.pnode <- function(cls, left, top, label, count_fmt, pct_txt = NULL) {
  p <- .NODE_PROPS[[cls]]
  style <- paste0(
    "left:", left, "px; top:", top, "px; ",
    "background:", p$bg, "; border:", p$bd, "; color:", p$col, ";"
  )
  pct_div <- if (!is.null(pct_txt)) tags$div(class = "pct", pct_txt) else NULL
  tags$div(
    class = "node",
    style = style,
    tags$div(class = "label", label),
    tags$div(class = "count", count_fmt),
    pct_div
  )
}

# Formata contagem pra diagrama: coerção segura para numeric, NA/Inf → em-dash.
.pfmt <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!is.finite(x) || is.na(x)) return("\u2014")
  fmt_int(round(x))
}

# Calcula percentual "X% of <label>" (1 casa decimal).
# Retorna NULL (em vez de string) se o denominador for 0/NA — o chamador
# não renderiza o <div.pct> nesse caso.
.ppct <- function(num, denom, label) {
  num   <- suppressWarnings(as.numeric(num))
  denom <- suppressWarnings(as.numeric(denom))
  if (!is.finite(num) || !is.finite(denom) || denom <= 0) return(NULL)
  paste0(round(100 * num / denom, 1), "% of ", label)
}

# Envelope comum dos dois diagramas: título + card com wrapper escalável
# (`.pathway-scalable-wrapper`) que mantém o aspecto do SVG conforme a
# largura da janela. `caption_txt` é o rodapé opcional do card.
.wrap_pathway <- function(geo_label, phase_headers, svg_paths, nodes, caption_txt) {
  tagList(
    tags$h3(
      style = "font-size:var(--t-lg); font-weight:600; color:var(--cc-darker); margin-bottom:12px;",
      paste0("Screening and diagnostic pathway \u2014 ", geo_label)
    ),
    tags$div(
      class = "pathway-card",
      tags$div(
        class = "pathway-scalable-wrapper",
        tags$div(
          class = "pathway-scalable pathway-container",
          phase_headers,
          svg_paths,
          nodes
        )
      ),
      tags$div(class = "caption", caption_txt)
    )
  )
}

# Renderiza o diagrama do fluxo HPV (aba Pathway).
# `f` é a lista de contagens do engine (campos esperados: women_screened,
# hpv16_18, other_hrhpv, negative, cytology_pos/neg, colpo_pos_*/colpo_neg_*,
# biopsy_neg_cin1_*, cin2_3_*, cancer_*). Pct calculado sempre em relação
# ao nó anterior (ex.: colposcopy+ 16/18 → % de HPV 16/18).
# 5 fases: Screening → HPV result → Reflex cytology → Colposcopy → Biopsy.
render_pathway_hpv <- function(f, geo_label, caption_txt) {
  N <- as.numeric(f$women_screened)

  nodes <- tagList(
    .pnode("n0", 20,  435, "Women screened",       .pfmt(N)),
    .pnode("n3", 260, 176, "HPV 16/18",            .pfmt(f$hpv16_18),
           .ppct(f$hpv16_18, N, "screened")),
    .pnode("n2", 260, 546, "Other high-risk HPV",  .pfmt(f$other_hrhpv),
           .ppct(f$other_hrhpv, N, "screened")),
    .pnode("n1", 260, 696, "HPV negative",         .pfmt(f$negative),
           .ppct(f$negative, N, "screened")),
    .pnode("n3", 500, 471, "Cytology +",           .pfmt(f$cytology_pos),
           .ppct(f$cytology_pos, f$other_hrhpv, "other HR HPV")),
    .pnode("n1", 500, 621, "Cytology \u2212",      .pfmt(f$cytology_neg),
           .ppct(f$cytology_neg, f$other_hrhpv, "other HR HPV")),
    .pnode("n3", 740, 101, "Colposcopy +",         .pfmt(f$colpo_pos_16),
           .ppct(f$colpo_pos_16, f$hpv16_18, "HPV 16/18")),
    .pnode("n1", 740, 251, "Colposcopy \u2212",    .pfmt(f$colpo_neg_16),
           .ppct(f$colpo_neg_16, f$hpv16_18, "HPV 16/18")),
    .pnode("n3", 740, 396, "Colposcopy +",         .pfmt(f$colpo_pos_out),
           .ppct(f$colpo_pos_out, f$cytology_pos, "cytology +")),
    .pnode("n1", 740, 546, "Colposcopy \u2212",    .pfmt(f$colpo_neg_out),
           .ppct(f$colpo_neg_out, f$cytology_pos, "cytology +")),
    .pnode("n1", 980,  25, "Biopsy \u2212 / CIN1", .pfmt(f$biopsy_neg_cin1_16),
           .ppct(f$biopsy_neg_cin1_16, f$colpo_pos_16, "colposcopy +")),
    .pnode("n4", 980, 110, "CIN2 / CIN3",          .pfmt(f$cin2_3_16),
           .ppct(f$cin2_3_16, f$colpo_pos_16, "colposcopy +")),
    .pnode("n4", 980, 195, "Cancer",               .pfmt(f$cancer_16),
           .ppct(f$cancer_16, f$colpo_pos_16, "colposcopy +")),
    .pnode("n1", 980, 320, "Biopsy \u2212 / CIN1", .pfmt(f$biopsy_neg_cin1_out),
           .ppct(f$biopsy_neg_cin1_out, f$colpo_pos_out, "colposcopy +")),
    .pnode("n4", 980, 405, "CIN2 / CIN3",          .pfmt(f$cin2_3_out),
           .ppct(f$cin2_3_out, f$colpo_pos_out, "colposcopy +")),
    .pnode("n4", 980, 490, "Cancer",               .pfmt(f$cancer_out),
           .ppct(f$cancer_out, f$colpo_pos_out, "colposcopy +"))
  )

  svg_paths <- tags$svg(
    class = "connectors", `viewBox` = "0 0 1200 780", preserveAspectRatio = "none",
    tags$path(d = "M 220 464 C 240 464 240 205 260 205"),
    tags$path(d = "M 220 464 C 240 464 240 575 260 575"),
    tags$path(d = "M 220 464 C 240 464 240 725 260 725"),
    tags$path(d = "M 460 205 C 600 205 600 130 740 130"),
    tags$path(d = "M 460 205 C 600 205 600 280 740 280"),
    tags$path(d = "M 460 575 C 480 575 480 500 500 500"),
    tags$path(d = "M 460 575 C 480 575 480 650 500 650"),
    tags$path(d = "M 700 500 C 720 500 720 425 740 425"),
    tags$path(d = "M 700 500 C 720 500 720 575 740 575"),
    tags$path(d = "M 940 130 C 960 130 960  60 980  60"),
    tags$path(d = "M 940 130 C 960 130 960 145 980 145"),
    tags$path(d = "M 940 130 C 960 130 960 230 980 230"),
    tags$path(d = "M 940 425 C 960 425 960 355 980 355"),
    tags$path(d = "M 940 425 C 960 425 960 440 980 440"),
    tags$path(d = "M 940 425 C 960 425 960 525 980 525")
  )

  phase_headers <- tagList(
    tags$div(class = "phase-header", style = "left:20px;",  "Screening"),
    tags$div(class = "phase-header", style = "left:260px;", "HPV result"),
    tags$div(class = "phase-header", style = "left:500px;", "Reflex cytology"),
    tags$div(class = "phase-header", style = "left:740px;", "Colposcopy"),
    tags$div(class = "phase-header", style = "left:980px;", "Biopsy")
  )

  .wrap_pathway(geo_label, phase_headers, svg_paths, nodes, caption_txt)
}

# Renderiza o diagrama do fluxo Citologia (aba Pathway).
# Campos esperados em `f`: women_screened, asch, other, negative,
# diag_pos/diag_neg/diag_tot (colposcopia diagnóstica para "other abnormalities"),
# colpo_asch_*, colpo_other_*, biopsy_neg_cin1_*, cin2_3_*, cancer_*.
# 5 fases: Screening → Cytology result → Diagnostic cytology → Colposcopy → Biopsy.
render_pathway_cytology <- function(f, geo_label, caption_txt) {
  N <- as.numeric(f$women_screened)

  nodes <- tagList(
    .pnode("n0", 20,  370, "Screening cytologies",               .pfmt(N)),
    .pnode("n3", 260, 150, "ASC-H+", .pfmt(f$asch),
           .ppct(f$asch, N, "screened")),
    .pnode("n2", 260, 430, "Other abnormal",                .pfmt(f$other),
           .ppct(f$other, N, "screened")),
    .pnode("n1", 260, 610, "Negative",                           .pfmt(f$negative),
           .ppct(f$negative, N, "screened")),
    .pnode("n3", 500, 390, "Diagnostic cytology +",              .pfmt(f$diag_pos),
           .ppct(f$diag_pos, f$diag_tot, "other abnormal")),
    .pnode("n1", 500, 530, "Diagnostic cytology \u2212",         .pfmt(f$diag_neg),
           .ppct(f$diag_neg, f$diag_tot, "other abnormal")),
    .pnode("n3", 740,  90, "Colposcopy +",    .pfmt(f$colpo_asch_pos),
           .ppct(f$colpo_asch_pos, f$asch, "ASC-H+")),
    .pnode("n1", 740, 230, "Colposcopy \u2212", .pfmt(f$colpo_asch_neg),
           .ppct(f$colpo_asch_neg, f$asch, "ASC-H+")),
    .pnode("n3", 740, 360, "Colposcopy +",    .pfmt(f$colpo_other_pos),
           .ppct(f$colpo_other_pos, f$diag_pos, "diagnostic cytology +")),
    .pnode("n1", 740, 500, "Colposcopy \u2212", .pfmt(f$colpo_other_neg),
           .ppct(f$colpo_other_neg, f$diag_pos, "diagnostic cytology +")),
    .pnode("n1", 980,  20, "Biopsy \u2212 / CIN1", .pfmt(f$biopsy_neg_cin1_asch),
           .ppct(f$biopsy_neg_cin1_asch, f$colpo_asch_pos, "colposcopy +")),
    .pnode("n4", 980, 105, "CIN2 / CIN3",          .pfmt(f$cin2_3_asch),
           .ppct(f$cin2_3_asch, f$colpo_asch_pos, "colposcopy +")),
    .pnode("n4", 980, 190, "Cancer",               .pfmt(f$cancer_asch),
           .ppct(f$cancer_asch, f$colpo_asch_pos, "colposcopy +")),
    .pnode("n1", 980, 290, "Biopsy \u2212 / CIN1", .pfmt(f$biopsy_neg_cin1_other),
           .ppct(f$biopsy_neg_cin1_other, f$colpo_other_pos, "colposcopy +")),
    .pnode("n4", 980, 375, "CIN2 / CIN3",          .pfmt(f$cin2_3_other),
           .ppct(f$cin2_3_other, f$colpo_other_pos, "colposcopy +")),
    .pnode("n4", 980, 460, "Cancer",               .pfmt(f$cancer_other),
           .ppct(f$cancer_other, f$colpo_other_pos, "colposcopy +"))
  )

  svg_paths <- tags$svg(
    class = "connectors", `viewBox` = "0 0 1200 780", preserveAspectRatio = "none",
    tags$path(d = "M 220 399 C 240 399 240 179 260 179"),
    tags$path(d = "M 220 399 C 240 399 240 459 260 459"),
    tags$path(d = "M 220 399 C 240 399 240 639 260 639"),
    tags$path(d = "M 460 179 C 600 179 600 119 740 119"),
    tags$path(d = "M 460 179 C 600 179 600 259 740 259"),
    tags$path(d = "M 460 459 C 480 459 480 419 500 419"),
    tags$path(d = "M 460 459 C 480 459 480 559 500 559"),
    tags$path(d = "M 700 419 C 720 419 720 389 740 389"),
    tags$path(d = "M 700 419 C 720 419 720 529 740 529"),
    tags$path(d = "M 940 119 C 960 119 960  55 980  55"),
    tags$path(d = "M 940 119 C 960 119 960 140 980 140"),
    tags$path(d = "M 940 119 C 960 119 960 225 980 225"),
    tags$path(d = "M 940 389 C 960 389 960 325 980 325"),
    tags$path(d = "M 940 389 C 960 389 960 410 980 410"),
    tags$path(d = "M 940 389 C 960 389 960 495 980 495")
  )

  phase_headers <- tagList(
    tags$div(class = "phase-header", style = "left:20px;",  "Screening"),
    tags$div(class = "phase-header", style = "left:260px;", "Cytology result"),
    tags$div(class = "phase-header", style = "left:500px;", "Diagnostic cytology"),
    tags$div(class = "phase-header", style = "left:740px;", "Colposcopy"),
    tags$div(class = "phase-header", style = "left:980px;", "Biopsy")
  )

  .wrap_pathway(geo_label, phase_headers, svg_paths, nodes, caption_txt)
}

