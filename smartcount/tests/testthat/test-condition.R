test_that("check_conditions works for Poisson", {
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem + mar + kid5, model = "poisson")
  result <- check_conditions(fit)

  expect_type(result, "list")
  expect_true("pearson_ratio" %in% names(result))
  expect_true("dispersion_diagnosis" %in% names(result))
  expect_true("zero_inflation_pvalue" %in% names(result))
  expect_true("suggestion" %in% names(result))
  expect_true("vif" %in% names(result))
  expect_true("sample_size" %in% names(result))
})

test_that("check_conditions works for Quasi-Poisson", {
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem + mar + kid5, model = "quasipoisson")
  result <- check_conditions(fit)

  expect_type(result, "list")
  expect_true("dispersion" %in% names(result))
  expect_true("mean_r2" %in% names(result))
  expect_true("diagnosis" %in% names(result))
  expect_true("plot" %in% names(result))
})

test_that("check_conditions works for Negative Binomial", {
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem + mar + kid5, model = "negbin")
  result <- check_conditions(fit)

  expect_type(result, "list")
  expect_true("theta" %in% names(result))
  expect_true("zero_inf" %in% names(result))
  expect_true("vif" %in% names(result))
  expect_true("suggestion" %in% names(result))
})

test_that("check_conditions works for ZIP", {
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem + mar, model = "zip")
  result <- check_conditions(fit)

  expect_type(result, "list")
  expect_true("dispersion_ratio" %in% names(result))
  expect_true("suggestion" %in% names(result))
})

test_that("check_conditions erros when fit isb not from fit_count",{
  data("bioChemists", package = "pscl")
  fit_raw <- glm(art ~ fem, data = bioChemists, family = poisson)   # no smartcount_model attribute
  expect_error(check_conditions(fit_raw))
})
test_that("check_conditions returns reasonable Pearson ratio for Poisson", {
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem + mar + kid5, model = "poisson")
  result <- check_conditions(fit)

  # Pearson ratio should be a positive number
  expect_true(is.numeric(result$pearson_ratio))
  expect_true(result$pearson_ratio > 0)
})