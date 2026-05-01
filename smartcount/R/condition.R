# Helper function 1
check_overdispersion <- function(fit) {
  # check overdispersion
  ####################################################
  # RQR Plot
  ####################################################
  counts <- fit$y
  lambdas <- fitted(fit)
  rqr <- rep(NA, length(lambdas))
  for (i in 1:length(lambdas)) {
    ai <- ppois(counts[i] - 1, lambda = lambdas[i])
    bi <- ppois(counts[i], lambda = lambdas[i])
    # this works even when ai=bi
    ui <- ai + runif(1) * (bi - ai)
    ui <- max(min(ui, 1 - 10^(-6)), 10^(-6))
    rqr[i] <- qnorm(ui)
  }
  pearson.ratio <- sum(residuals(fit, type = "pearson")^2) / fit$df.residual

  # Diagnosis text based on dispersion ratio
  diagnosis <- if (pearson.ratio > 1.5) {
    "Overdispersion detected. Consider Negative Binomial or Quasi-Poisson."
  } else if (pearson.ratio < 0.8) {
    "Underdispersion detected (uncommon)."
  } else {
    "Dispersion appears OK for Poisson."
  }

  # Short label for plot title
  diagnosis_label <- if (pearson.ratio > 1.5) {
    " (Overdispersed)"
  } else if (pearson.ratio < 0.8) {
    " (Underdispersed)"
  } else {
    " (OK)"
  }

  p1 <- ggplot2::ggplot(data = tibble::tibble(lambda = lambdas, e = rqr)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
    ggplot2::geom_point(ggplot2::aes(x = lambda, y = e)) +
    ggplot2::theme_bw() +
    ggplot2::xlab(bquote(lambda)) +
    ggplot2::ylab("Randomized Quantile Residuals")
  p2 <- ggplot2::ggplot(data = tibble::tibble(e = rqr)) +
    ggplot2::stat_qq(ggplot2::aes(sample = e)) +
    ggplot2::stat_qq_line(ggplot2::aes(sample = e)) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(paste0("Dispersion Ratio = ", round(pearson.ratio, 4), diagnosis_label))

  result <- list(
    pearson_ratio = pearson.ratio,
    diagnosis = diagnosis,
    plot = patchwork::wrap_plots(p1, p2)
  )
  return(result)
}

# Helper function 2
check_zeroInflation <- function(fit) {
  zero_inf <- DHARMa::testZeroInflation(fit)
  zero_val <- zero_inf$p.value
  zero_val
}


# Helper function 3
checkPoissonAssumptions <- function(fit){
  y <- fit$y
  if (min(y)< 0){
    stop ("Response variable cannot have negative values.")
  }
  if (any(y!=round(y))){
    stop ("Response variable must be integer.")
  }
  total <- sum(y)
  p <- length(coef(fit))
  is_sufficient <- total>= 10*p
  if (!is_sufficient){
    warning(paste0("Total events is ", total, " and is less than 10 x predictors (", 10*p, "). Sample size may be too small for Poisson regression."))
  }
  list(
    total_events =total,
    n_predictors =p,
    threshold =10*p,
    is_sufficient=is_sufficient
  )
}

# Helper 4
check_Multicollinearity <- function(fit){
  if (length(coef(fit))<=2){
    return(NULL)
  }
  col <- car::vif(fit)
  
  if (any(col>=5 & col <=10)){
    warning("Moderate multicollinearity detected")
  }
  if (any(col>10)){
    warning("Severe Multicollinearity ")
  }
  col
}


check_poisson_conditions <- function(fit) {
  result <- NULL
  sample_size_check <- checkPoissonAssumptions(fit)
  vif_result <- check_Multicollinearity(fit)
  od <- check_overdispersion(fit)
  dispersion <- od$pearson_ratio
  zi <- check_zeroInflation(fit)
  zeroinf <- zi
  if (dispersion >= 0.8 && dispersion <= 1.2) {
    if (zeroinf < 0.05) {
      suggestion <- "Zero Inflation Problem. Consider Zero-inflated Poisson!"
    } else {
      suggestion <- "Poisson appears fine"
    }
  } else if (dispersion <= 1.5) {
    if (zeroinf < 0.05) {
      suggestion <- "Zero Inflation and Mild Overdispersion Problem. Consider Zero-inflated Poisson!"
    } else {
      suggestion <- "Mild overdispersion. Consider Quasi Poisson!"
    }
  } else {
    if (zeroinf < 0.05) {
      suggestion <- "Zero Inflation and Overdispersion Problem. Consider Zero-inflated Negative Binomial!"
    } else {
      suggestion <- "Overdispersion. Consider Negative Binomial!"
    }
  }
  result <- list(
    plot = od$plot,
    pearson_ratio = od$pearson_ratio,
    dispersion_diagnosis = od$diagnosis,
    zero_inflation_pvalue = zeroinf,
    suggestion = suggestion,
    vif = vif_result,
    sample_size = sample_size_check
  )
  result
}

#Quasi-Poisson
check_quasi_poisson <- function(fit){
  vif <- check_Multicollinearity(fit)
  # DHARMa does not support quasipoisson, so wrap in tryCatch
  zi <- tryCatch(
    check_zeroInflation(fit),
    error = function(e) NA
  )
  disp <- summary(fit)$dispersion

  # Lambda vs r^2 plot (following professor's code)
  ggdat <- tibble::tibble(
    r2 = resid(fit, type = "pearson")^2,
    lambdas = fitted(fit)
  )

  p <- ggplot2::ggplot(ggdat) +
    ggplot2::geom_point(ggplot2::aes(x = lambdas, y = r2)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "red") +
    ggplot2::geom_smooth(ggplot2::aes(x = lambdas, y = r2)) +
    ggplot2::theme_bw() +
    ggplot2::xlab(bquote(lambda)) +
    ggplot2::ylab(bquote(r^2)) +
    ggplot2::ggtitle(paste0("Lambda vs r^2 (Dispersion = ", round(disp, 3), ")"))

  mean_r2 <- mean(ggdat$r2)

  # Diagnosis
  diagnosis <- if (abs(disp - 1) < 0.2) {
    "Dispersion close to 1; Quasi-Poisson may be unnecessary."
  } else {
    "Quasi-Poisson is providing variance adjustment. Inspect lambda vs r^2 plot: a wedge or curve suggests considering Negative Binomial or Generalized Poisson."
  }


  list(
    dispersion = disp,
    mean_r2 = mean_r2,
    diagnosis = diagnosis,
    plot = p,
    zero_inflation_pvalue = zi,
    zero_inflation_suggestion = if (is.na(zi)) {
      "Zero inflation test not available for Quasi-Poisson (no likelihood)."
    } else if (zi < 0.05) {
      "Zero inflation problem. Consider ZIP or ZINB."
    } else {
      "No significant zero inflation."
    },
    vif = vif
  )
}
# Negative Binomial
check_nb_conditions <- function(fit){
  sample_size <- checkPoissonAssumptions(fit)
  vif <- check_Multicollinearity(fit)
  zi <- check_zeroInflation(fit)
  theta <- fit$theta

  list(theta=theta,
  zero_inf=zi,
  sample_size=sample_size,
  vif=vif,
  suggestion =ifelse(zi<0.05, "Zero inflation problem. Consider ZINB", "No zero inflation problem."))

}

# Zero inflated
check_zeroinf_conditions <- function(fit){
  #randomized quantile residual
  #dispersion ratio (overdispersion --> ZINB)
  # r^2 vs lambda plot
  # McFadden's R^2
  # sample size
  y <- model.response(model.frame(fit))
  pi_hat <- predict(fit, type = "zero")
  lambda_hat <- predict(fit, type = "count")
  mu_hat <- (1 - pi_hat) * lambda_hat
  var_hat <- lambda_hat * (1 - pi_hat) * (1 + pi_hat * lambda_hat)

  n <- length(y)
  p <- length(coef(fit, "count"))
  q <- length(coef(fit, "zero"))
  dispersion <- sum((y - mu_hat)^2) / (n - p - q) / mean(var_hat)

  suggestion <- if (dispersion > 1.5) {
    "Extra overdispersion detected. Consider ZINB."
  } else {
    "ZIP appears appropriate."
  }
  
  list(
    dispersion_ratio = dispersion,
    suggestion = suggestion
  )
}

#' Check Conditions for Count Regression
#'
#' @param fit a fitted model object from fit_count()
#'
#' @return diagnostic results based on model type
#' @export
check_conditions <- function(fit){
  category <- attr(fit, "smartcount_model")
  if (is.null(category)){
    stop("Invalid model type. Please fit your model using fit_count()")
  }

  if (category=="poisson"){
    result <- check_poisson_conditions(fit)
  }else if (category=="quasipoisson"){
    result <- check_quasi_poisson(fit)
  }else if (category =="negbin"){
    result <- check_nb_conditions(fit)
  }else if (category=="zip"|| category=="zinb"){
    result <- check_zeroinf_conditions(fit)
  }else if (category=="genpois"){
    result <- check_genpois_conditions(fit)
  }

  result
}

check_genpois_conditions <- function(fit){
  pearson_resid <- residuals(fit, type = "pearson")
  pearson_ratio <- sum(pearson_resid^2) / df.residual(fit)

  y <- model.frame(fit)[[1]]

  total_events <- sum(y)
  p <- length(glmmTMB::fixef(fit)$cond) 

  threshold <- 10 * p
  is_sufficient <- total_events >= threshold
  
  if (!is_sufficient) {
    warning(paste0(
      "Total events (", total_events, ") < 10 × predictors (", threshold, ")."
    ))
  }

  diagnosis <- if (abs(pearson_ratio - 1) < 0.5) {
    "Pearson ratio close to 1 — Generalized Poisson appears to handle the dispersion well."
  } else {
    paste0("Pearson ratio = ", round(pearson_ratio, 3), 
           ". May indicate misspecification. Consider checking residual plots.")
  }

  list(
    pearson_ratio = pearson_ratio,
    diagnosis = diagnosis,
    sample_size = list(
      total_events = total_events,
      n_predictors = p,
      threshold = threshold,
      is_sufficient = is_sufficient
    )
  )

}
