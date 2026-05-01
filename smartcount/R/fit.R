#' Fit a Count Regression Model
#'
#' @param data data frame
#' @param formula formula
#' @param model specifying model type
#'
#' @return fitted model
#' @export
fit_count <- function(data, formula, model="poisson") {
  model <- tolower(model)
  if (model=="poisson"){
    fit<-stats::glm(formula, data = data, family = poisson(link = "log"))
  }else if(model=="quasipoisson"){
    fit <- stats::glm(formula, data=data, family=quasipoisson(link="log"))
  }else if(model=="negbin"){
    fit <- MASS::glm.nb(formula, data=data)
  }else if(model=="zip"){
    fit <- pscl::zeroinfl(formula, data=data, dist="poisson")
  }else if(model=="zinb"){
    fit <- pscl::zeroinfl(formula, data=data, dist="negbin")
  }else{
    stop("Model is not supported. Choose: poisson, quasipoisson, negbin, zip, zinb")
  }
  attr(fit, "smartcount_model") <- model
  fit
}

