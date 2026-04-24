#' Fit a Count Regression Model
#'
#' @param data data frame
#' @param formula formula
#' @param model specifying model type
#'
#' @return fitted model
#' @export
fit_count <- function(data, formula, model = "poisson") {
  stats::glm(formula, data = data, family = poisson(link = "log"))
}
