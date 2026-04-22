# ===========================================================
# 01_utils_cc.R

# -----------------------------------------------------------
# Idades — versão corrigida e robusta
# -----------------------------------------------------------

# Retorna idade mínima ("50-54" → 50; "85+" → 85)
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

# Retorna idade máxima ("50-54" → 54; "85+" → 120)
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
# Numeric formatting (IARC default = international)
# ===========================================================

# Contagens inteiras (ex: 12,345)
fmt_int <- function(x) {
  if (is.null(x) || all(is.na(x))) return("–")
  formatC(
    as.integer(round(x)),
    format       = "d",
    big.mark     = ",",
    decimal.mark = "."
  )
}


# Taxas / percentuais / valores com casas decimais
# (ex: 12,345.6   ou   123.4)
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
# Tooltips (Bootstrap)
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

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ===========================================================
# Pathway diagram helpers (HPV & cytology)
# ===========================================================

.NODE_PROPS <- list(
  n0 = list(bg = "#ffffff", bd = "1.5px solid #b7d1ce", col = "#1a3c3a"),
  n1 = list(bg = "#E8F0EF", bd = "1px solid #c6d8d5",   col = "#1a3c3a"),
  n2 = list(bg = "#9FC2BE", bd = "1px solid #82adaa",   col = "#0f2928"),
  n3 = list(bg = "#649a95", bd = "1px solid #518682",   col = "#ffffff"),
  n4 = list(bg = "#3D7C77", bd = "1px solid #31645f",   col = "#ffffff")
)

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

.pfmt <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!is.finite(x) || is.na(x)) return("\u2014")
  fmt_int(round(x))
}

.ppct <- function(num, denom, label) {
  num   <- suppressWarnings(as.numeric(num))
  denom <- suppressWarnings(as.numeric(denom))
  if (!is.finite(num) || !is.finite(denom) || denom <= 0) return(NULL)
  paste0(round(100 * num / denom, 1), "% of ", label)
}

.wrap_pathway <- function(geo_label, phase_headers, svg_paths, nodes, caption_txt) {
  tagList(
    tags$h3(
      style = "font-size:20px; font-weight:600; color:#1a3c3a; margin-bottom:12px;",
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

render_pathway_cytology <- function(f, geo_label, caption_txt) {
  N <- as.numeric(f$women_screened)

  nodes <- tagList(
    .pnode("n0", 20,  370, "Screening cytologies",               .pfmt(N)),
    .pnode("n3", 260, 150, "HSIL / ASC-H / AOI / AIS / Carcinoma", .pfmt(f$asch),
           .ppct(f$asch, N, "screened")),
    .pnode("n2", 260, 430, "Other abnormalities",                .pfmt(f$other),
           .ppct(f$other, N, "screened")),
    .pnode("n1", 260, 610, "Negative",                           .pfmt(f$negative),
           .ppct(f$negative, N, "screened")),
    .pnode("n3", 500, 390, "Diagnostic cytology +",              .pfmt(f$diag_pos),
           .ppct(f$diag_pos, f$diag_tot, "other abnormalities")),
    .pnode("n1", 500, 530, "Diagnostic cytology \u2212",         .pfmt(f$diag_neg),
           .ppct(f$diag_neg, f$diag_tot, "other abnormalities")),
    .pnode("n3", 740,  90, "Colposcopy +",    .pfmt(f$colpo_asch_pos),
           .ppct(f$colpo_asch_pos, f$asch, "HSIL/ASC-H+")),
    .pnode("n1", 740, 230, "Colposcopy \u2212", .pfmt(f$colpo_asch_neg),
           .ppct(f$colpo_asch_neg, f$asch, "HSIL/ASC-H+")),
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

