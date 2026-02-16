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



