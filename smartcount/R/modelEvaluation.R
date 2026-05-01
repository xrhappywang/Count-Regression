# Helper: basic fit statistics
model_stats <- function(fit) {

  # Special case for zeroinfl: McFadden R^2 needs different handling
  if (inherits(fit, "zeroinfl")) {
    return(list(
      AIC = AIC(fit),
      BIC = BIC(fit),
      logLik = as.numeric(logLik(fit)),
      mcfadden_r2 = NA,
      mcfadden_interpretation = "Not available for zero-inflated models",
      deviance_r2 = NA,
      deviance_interpretation = "Not available for zero-inflated models"
    ))
  }

  # Special case for glmmTMB (e.g., Generalized Poisson)
  if (inherits(fit, "glmmTMB")) {
    return(list(
      AIC = AIC(fit),
      BIC = BIC(fit),
      logLik = as.numeric(logLik(fit)),
      mcfadden_r2 = NA,
      mcfadden_interpretation = "Not available for glmmTMB models",
      deviance_r2 = NA,
      deviance_interpretation = "Not available for glmmTMB models"
    ))
  }

  basic <- list(
    AIC = AIC(fit),
    BIC = BIC(fit),
    logLik = as.numeric(logLik(fit))
  )

  # Deviance R Square
  if (!is.null(fit$deviance) && !is.null(fit$null.deviance)) {
    deviance_r2 <- 1 - fit$deviance / fit$null.deviance
  } else {
    deviance_r2 <- NA
  }

  if (is.na(deviance_r2)) {
    deviance_interp <- "Not available"
  } else if (deviance_r2 < 0.1) {
    deviance_interp <- "Weak"
  } else if (deviance_r2 < 0.3) {
    deviance_interp <- "Moderate"
  } else if (deviance_r2 < 0.5) {
    deviance_interp <- "Strong"
  } else {
    deviance_interp <- "Very Strong"
  }

  # McFadden's R Square
  the_data <- fit$model
  response_name <- names(the_data)[1]
  null_formula <- as.formula(paste(response_name, "~ 1"))
  fit_null <- tryCatch(
    stats::glm(null_formula, data = the_data, family = fit$family),
    error = function(e) NULL
  )
  if (!is.null(fit_null)) {
    loglik_null <- as.numeric(logLik(fit_null))
    mcfadden_r2 <- 1 - basic$logLik / loglik_null
  } else {
    mcfadden_r2 <- NA
  }
  if (is.na(mcfadden_r2)) {
    mcfadden_interp <- "Not available"
  } else if (mcfadden_r2 < 0.1) {
    mcfadden_interp <- "Weak"
  } else if (mcfadden_r2 < 0.2) {
    mcfadden_interp <- "Moderate"
  } else if (mcfadden_r2 < 0.4) {
    mcfadden_interp <- "Strong"
  } else {
    mcfadden_interp <- "Very Strong"
  }

  list(
    AIC = basic$AIC,
    BIC = basic$BIC,
    logLik = basic$logLik,
    mcfadden_r2 = mcfadden_r2,
    mcfadden_interpretation = mcfadden_interp,
    deviance_r2 = deviance_r2,
    deviance_interpretation = deviance_interp
  )
}



# Helper: compare two models
compareModel <- function(fit1, fit2) {

  # detect model types
  is_zeroinfl1 <- inherits(fit1, "zeroinfl")
  is_zeroinfl2 <- inherits(fit2, "zeroinfl")
  is_negbin1 <- inherits(fit1, "negbin")
  is_negbin2 <- inherits(fit2, "negbin")
  is_glmmTMB1 <- inherits(fit1, "glmmTMB")
  is_glmmTMB2 <- inherits(fit2, "glmmTMB")

  # 1. ZIP/ZINB involved -> Vuong test (non-nested)
  if (is_zeroinfl1 || is_zeroinfl2) {
    return(pscl::vuong(fit1, fit2))
  }

  # 2. glmmTMB involved -> use anova (supports nested glmmTMB; for cross-class,
  # use AIC comparison as fallback)
  if (is_glmmTMB1 || is_glmmTMB2) {
    return(tryCatch(
      anova(fit1, fit2),
      error = function(e) {
        # fallback: simple AIC/BIC comparison
        data.frame(
          model = c(deparse(substitute(fit1)), deparse(substitute(fit2))),
          AIC = c(AIC(fit1), AIC(fit2)),
          BIC = c(BIC(fit1), BIC(fit2))
        )
      }
    ))
  }

  # 3. Poisson vs NB -> likelihood ratio test with boundary correction
  if (xor(is_negbin1, is_negbin2)) {
    lrt <- lmtest::lrtest(fit1, fit2)
    # boundary correction: divide p-value by 2
    lrt$`Pr(>Chisq)` <- lrt$`Pr(>Chisq)` / 2
    attr(lrt, "note") <- "P-value divided by 2 for boundary correction (Poisson vs NB)."
    return(lrt)
  }

  # 4. nested glm comparison (Poisson, Quasi-Poisson, NB)
  family_name <- fit1$family$family
  if (!is.null(family_name) && family_name == "quasipoisson") {
    return(anova(fit1, fit2, test = "F"))
  } else {
    return(anova(fit1, fit2, test = "Chisq"))
  }
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
