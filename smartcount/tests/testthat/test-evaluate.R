test_that("evaluate_model returns stats for Poisson", {
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem + mar, model = "poisson")
  result <- evaluate_model(fit)

  expect_type(result, "list")
  expect_true(is.numeric(result$stats$AIC))
  expect_true(is.numeric(result$stats$mcfadden_r2))
})

test_that("evaluate_model handles ZIP without crashing", {
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem + mar, model = "zip")
  result <- evaluate_model(fit)
  expect_true(!is.na(result$stats$AIC))
  expect_true(is.na(result$stats$mcfadden_r2))
})

test_that("evaluate_model compares two models", {
  data("bioChemists", package = "pscl")
  fit1 <- fit_count(bioChemists, art ~ fem, model = "poisson")
  fit2 <- fit_count(bioChemists, art ~ fem + mar, model = "poisson")
  result <- evaluate_model(fit1, fit2)

  expect_true(!is.null(result$comparison))
})

test_that("evaluate_model without fit2 has no comparison", {
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem, model = "poisson")
  result <- evaluate_model(fit)

  expect_false("comparison" %in% names(result))
})
