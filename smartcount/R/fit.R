#' Fit a Count Regression Model
#'
#' @param data data frame
#' @param formula formula
#' @param model specifying model type
#'
#' @return fitted model
#' @export
fit_count <- function(data, formula, model = "poisson") {

  # input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  if (!inherits(formula, "formula")) {
    stop("'formula' must be a formula object (e.g., y ~ x1 + x2).")
  }

  model <- tolower(model)
  if (model == "poisson") {
    fit <- stats::glm(formula, data = data, family = poisson(link = "log"))
  } else if (model == "quasipoisson") {
    fit <- stats::glm(formula, data = data, family = quasipoisson(link = "log"))
  } else if (model == "negbin") {
    fit <- MASS::glm.nb(formula, data = data)
  } else if (model == "zip") {
    fit <- pscl::zeroinfl(formula, data = data, dist = "poisson")
  } else if (model == "zinb") {
    fit <- pscl::zeroinfl(formula, data = data, dist = "negbin")
  } else if (model=="genpois"){
    fit <- glmmTMB::glmmTMB(formula, data=data, family = glmmTMB::genpois())
  } else {
    stop("Model is not supported. Choose: poisson, quasipoisson, negbin, zip, zinb, genpois")
  }
  attr(fit, "smartcount_model") <- model
  fit
}

