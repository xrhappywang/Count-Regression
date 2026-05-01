#' Interpret a count regression model
#' 
#' @param fit a fitted model object
#' 
#' @return data frame with rate ratios and interpretations
#' @export
interpret<- function(fit){
  if (inherits(fit,"zeroinfl")){
    interpret_zip(fit)
  }else{
    interpret_glm(fit)
  }
}
# check whether the model is ZIP/ZINB, or Poisson/Quasi/NB
# interpret glm: for Poisson/Quasi/NB
# interpret zeroinf: for ZIP/ZINB


interpret_glm <- function(fit){
  # on original scale
  coef <- coef(fit) #coefficients on log scale
  ci <-confint(fit) # 95% confidence interval on log scale

  #exponentiate
  rate <- exp(coef)
  ci_exp <- exp(ci)

  # compute percentage change
  pct_change <- 100*(rate-1)
  pct_lower <- 100*(ci_exp[,1]-1)
  pct_upper <- 100*(ci_exp[,2]-1)

  # distinguish if a varaible is continous or categorical

  data <- names(fit$model)
  interpretations <- character(length=length(coef))


  for (i in 1:length(coef)){
    variable <- names(coef)[i]
    percentage <- round(pct_change[i],3)
    lower <- round(pct_lower[i],3)
    upper <- round(pct_upper[i],3)
    baseline <- round(rate[i],3)



    if (variable =="(Intercept)"){
      interpretations[i] <- paste("The expected count is", baseline, "when all other predictors are zero and all categorical predictors are at their reference level.")
    } else if(variable %in% data && is.numeric(fit$model[[variable]])){
      # continuous
      interpretations[i] <- paste("For every one-unit increase in", variable, ",","the expected count changes by", percentage,"%", "(95% CI:", lower, "% to", upper, "%),", "adjusting for simultaneous linear change in other predictors.")
      }else{
      #categorical
      interpretations[i] <- paste("For observations satisfying", variable, ",", "the expected count changes by", percentage, "%", "(95% CI:", lower, "% to", upper, "%),", "compared to the reference category, adjusting for simultaneous linear change in other predictors.")
    }
      
    }
  

  result <- data.frame(
    variable = names(coef),
    estimated_log = round(coef, 3),
    estimated_exp = round(rate, 3),
    percentage = round(pct_change, 3),
    ci_lower_pct = round(pct_lower, 3),
    ci_upper_pct = round(pct_upper, 3),
    interpretation = interpretations,
    row.names = NULL
  )
  result
}

interpret_zip <- function(fit){
  # on original scale
  coef <- coef(fit) #coefficients on log scale
  ci <-confint(fit) # 95% confidence interval on log scale

  #exponentiate
  rate <- exp(coef)
  ci_exp <- exp(ci)

  # compute percentage change
  pct_change <- 100*(rate-1)
  pct_lower <- 100*(ci_exp[,1]-1)
  pct_upper <- 100*(ci_exp[,2]-1)

  interpretations<- character(length(coef))

  for (i in 1:length(coef)){
    variable <- names(coef)[i]

    is_count <- grepl("^count_", variable)
    is_zero <- grepl("^zero_", variable)

    variable_clean <- gsub("^count_|^zero_", "", variable) 
    
    percentage <- round(pct_change[i],3)
    lower <- round(pct_lower[i],3)
    upper <- round(pct_upper[i],3)
  

    log_odds <- round(coef[i],3)
    odds_ratio <- round(rate[i],3)

    # if the variable is an intercept
    if (variable_clean =="(Intercept)"){
      if (is_count){
        interpretations[i] <- paste("The expected count is ", odds_ratio, "for observations that are not structural zeros, when all continuous predictors are zero and all categorical predictors are at their reference level.")
      }else if(is_zero){
        interpretations[i] <- paste("The log odds of being a structural zero is ", log_odds, "when all continuous predictors are zero and all categorical predictors are at their reference level.")
      }
    }else if (is_count){
      interpretations[i] <- paste(
        "For", variable_clean, ", the expected count changes by", percentage, "%", "(95% CI:", lower, "% to", upper, "%).")
      
    }else if(is_zero){
      interpretations[i] <- paste(
        "For", variable_clean, ", the log odds of being a structural zero changes by", log_odds,
        "(odds ratio:", odds_ratio, ")."
      )
    }
    
  }
  result <- data.frame(
    variable = names(coef),
    estimated_log = round(coef, 3),
    estimated_exp = round(rate, 3),
    percentage = round(pct_change, 3),
    ci_lower_pct = round(pct_lower, 3),
    ci_upper_pct = round(pct_upper, 3),
    interpretation = interpretations,
    row.names = NULL
  )
  result


}