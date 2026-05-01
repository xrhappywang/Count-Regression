# Helper: basic fit statistics
model_stats <- function(fit) {
  list(
    AIC = AIC(fit),
    BIC = BIC(fit),
    logLik = as.numeric(logLik(fit))
  )
}

# Helper: compare two models
compareModel <- function(fit1, fit2) {
  # zero-inflated vs zero-inflated
  if (class(fit1)[1] == "zeroinfl" || class(fit2)[1] == "zeroinfl") {
    result <- pscl::vuong(fit1, fit2)
  } else {
    # glm-type models
    family_name <- fit1$family$family
    if (family_name == "quasipoisson") {
      result <- anova(fit1, fit2, test = "F")
    } else {
      result <- anova(fit1, fit2, test = "Chisq")
    }
  }
  result
}

#' Evaluate a Count Regression Model
#'
#' @param fit a fitted model object
#' @param fit2 (optional) a second fitted model for comparison
#'
#' @return a list with model statistics and (optionally) a model comparison
#' @export
evaluate_model <- function(fit, fit2 = NULL) {
  result <- list(
    stats = model_stats(fit)
  )
  if (!is.null(fit2)) {
    result$comparison <- compareModel(fit, fit2)
  }
  result
}
