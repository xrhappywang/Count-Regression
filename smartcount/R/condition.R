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
    ggplot2::ggtitle(paste("Dispersion Ratio =", round(pearson.ratio, 4)))
  result <- list(
    pearson_ratio = pearson.ratio,
    plot = p1 + p2
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


#' Check Conditions for Count Regression
#'
#' @param fit a fitted model object
#'
#' @return diagnostic results based on model type
#' @export
check_poisson_conditions <- function(fit) {
  result <- NULL
  checkPoissonAssumptions(fit)
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
    zero_inflation_pvalue = zeroinf,
    suggestion = suggestion,
    vif=vif_result
  )
  result
}

#Quasi-Poisson
check_quasi_poisson <- function(fit){
  vif <- check_Multicollinearity(fit)
  zi <- check_zeroInflation(fit)
  disp <- summary(fit)$dispersion

  list(dispersion=disp,
  zero_inf=zi,
  vif=vif,
  suggestion =ifelse(zi<0.05, "Zero inflation problem. Consider ZIP or ZINB"))

}
# Negative Binomial
check_nb_conditions <- function(fit){
  vif <- check_Multicollinearity(fit)
  zi <- check_zeroInflation(fit)
  theta <- fit$theta

  list(theta=theta,
  zero_inf=zi,
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
  }

  result
}

