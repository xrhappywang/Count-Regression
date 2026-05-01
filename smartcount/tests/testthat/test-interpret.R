test_that("interpret returns data frame for Poisson", {
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem + mar, model = "poisson")
  result <- interpret(fit)

  expect_s3_class(result, "data.frame")
  expect_true("variable" %in% names(result))
  expect_true("estimated_log" %in% names(result))
  expect_true("estimated_exp" %in% names(result))
  expect_true("z_value" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("interpretation" %in% names(result))
})

test_that("interpret handles ZIP with count_ and zero_ prefixes", {
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem + mar, model = "zip")
  result <- interpret(fit)

  expect_s3_class(result, "data.frame")
  # ZIP coefficients have count_ and zero_ prefixes
  expect_true(any(grepl("^count_", result$variable)))
  expect_true(any(grepl("^zero_", result$variable)))
})

test_that("interpret detects interaction terms", {
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem * mar, model = "poisson")
  result <- interpret(fit)

  # interaction info should be stored as attribute
  expect_true(!is.null(attr(result, "interactions")))
})
