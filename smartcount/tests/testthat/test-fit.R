test_that("fit_count works for Poisson", {
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem + mar, model = "poisson")
  expect_s3_class(fit, "glm")
  expect_equal(attr(fit, "smartcount_model"), "poisson")
})

test_that("fit_count works for Quasi-Poisson",{
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem + mar, model = "quasipoisson")

  expect_s3_class(fit, "glm")
  expect_equal(attr(fit, "smartcount_model"), "quasipoisson")
  expect_equal(fit$family$family, "quasipoisson")
})

test_that("fit_count works for Negative Binomial", {
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem + mar, model = "negbin")

  expect_s3_class(fit, "negbin")
  expect_equal(attr(fit, "smartcount_model"), "negbin")
})

test_that("fit_count works for ZIP", {
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem + mar, model = "zip")

  expect_s3_class(fit, "zeroinfl")
  expect_equal(attr(fit, "smartcount_model"), "zip")
  expect_equal(fit$dist, "poisson")

})

test_that("fit_count works for ZINB",{
  data("bioChemists", package = "pscl")
  fit <- fit_count(bioChemists, art ~ fem + mar, model = "zinb")

  expect_s3_class(fit, "zeroinfl")
  expect_equal(attr(fit, "smartcount_model"), "zinb")
  expect_equal(fit$dist, "negbin")
})

test_that("fit_count errors on invalid model name", {
  data("bioChemists", package = "pscl")
  expect_error(fit_count(bioChemists, art ~ fem, model = "wrong_model"))
})

test_that("fit_count errors on invalid data or formula", {
  data("bioChemists", package = "pscl")
  expect_error(fit_count(list(), art ~ fem))           # data is not a data frame
  expect_error(fit_count(bioChemists, "art ~ fem"))    # formula is a string
})
