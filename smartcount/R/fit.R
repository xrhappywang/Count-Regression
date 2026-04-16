#' Fit a Count Regression Model
#'
#' @param data A data frame.
#' @param formula A formula (e.g., y ~ x1 + x2).
#' @param model A character string specifying model type.
#'
#' @return A fitted model object.
#' @export
fit_count <- function(data, formula, model = "poisson") {
  stats::glm(formula, data = data, family = poisson(link = "log"))
}
