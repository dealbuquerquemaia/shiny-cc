
# ------------------------------------------------
# tests/testthat/test-engines.R — teste simples
# ------------------------------------------------
library(testthat)
source("R/engine_programming.R")
source("R/engine_capacity.R")

test_that("calc_screening_needs retorna colunas esperadas", {
  dt <- data.table::data.table(
    geo_id="XX", geo_name="XX", level="UF", pop_target=10000
  )
  out <- calc_screening_needs(dt, default_params_hpv)
  expect_true(all(c("screened","positives","colpos","biopsy","pathology_slides","treatment") %in% names(out)))
})

test_that("capacity_required calcula recursos mínimos", {
  needs <- data.table::data.table(
    geo_id="XX", geo_name="XX", level="UF",
    screened=600, positives=72, colpos=58, biopsy=29, pathology_slides=58, treatment=5
  )
  out <- capacity_required(needs, default_prod)
  expect_true(all(c("req_colposcopistas","req_citotecnicos","req_patologistas","req_equip_trat") %in% names(out)))
})
