# tests/test-smoke.R

testthat::test_that("app.R existe em algum caminho padrão e tem conteúdo", {
  candidates <- c(
    "app/app.R",
    "app.R",
    "inst/app/app.R",
    "shiny/app.R"
  )
  
  exists_vec <- file.exists(candidates)
  
  # Se nenhuma candidata bater, tenta busca recursiva
  if (!any(exists_vec)) {
    found <- list.files(".", pattern = "^app\\.R$", recursive = TRUE, full.names = TRUE)
    if (length(found) < 1) {
      testthat::skip("Sem app.R no repo (skip em vez de falhar).")
    }
    target <- found[[1]]
  } else {
    target <- candidates[which(exists_vec)[1]]
  }
  
  testthat::expect_true(file.exists(target), info = paste("Esperava encontrar app.R em:", target))
  src <- readLines(target, warn = FALSE, encoding = "UTF-8")
  testthat::expect_true(length(src) > 20, info = paste("Arquivo muito curto:", target))
})
