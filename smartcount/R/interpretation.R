#' Interpret a count regression model
#' 
#' @param fit a fitted model object
#' 
#' @return data frame with rate ratios and interpretations
#' @export
interpret<- function(fit){
  # check if user input has an offset
  if (!is.null(fit$offset) && length(fit$offset)>0){
    message("This model has an offset. ",
    "Coefficients describe rates per unit of the offset variable."
    )
  }

  # main interpretation
  result <- if (inherits(fit, "zeroinfl")) {
    interpret_zip(fit)
  } else {
    interpret_glm(fit)
  }

  # check interaction terms
  interaction_results <- interpret_interaction(fit)
  if (!is.null(interaction_results)) {
    attr(result, "interactions") <- interaction_results
    message("Interaction term(s) detected. Check attr(result, 'interactions') for details.")
  }

  result
}
# check whether the model is ZIP/ZINB, or Poisson/Quasi/NB
# interpret glm: for Poisson/Quasi/NB
# interpret zeroinf: for ZIP/ZINB


interpret_glm <- function(fit){
  # on original scale
  has_offset <-!is.null(fit$offset) && length(fit$offset)>0

  


  coef <- coef(fit) #coefficients on log scale
  ci <-confint(fit) # 95% confidence interval on log scale

  # extract z (or t) value and p-value from summary
  summary_table <- summary(fit)$coefficients
  if (!is.null(fit$family) && fit$family$family == "quasipoisson") {
    # Quasi-Poisson uses t distribution
    z_values <- summary_table[, "t value"]
    p_values <- summary_table[, "Pr(>|t|)"]
  } else {
    # Poisson and Negative Binomial use z distribution
    z_values <- summary_table[, "z value"]
    p_values <- summary_table[, "Pr(>|z|)"]
  }

  # count the number of predictors for simultaneoous linear change
  n_predictors <- length(coef)-1
  adjustment <- if (n_predictors >1){
    ", adjusting for simultaneous linear change in other predictors."
  }else{
    "."
  }

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



    if (variable == "(Intercept)") {
      if (has_offset) {
        interpretations[i] <- paste(
          "The expected rate per unit of offset is", baseline,
          "when all other predictors are zero (for continuous) or at their reference level (for categorical)."
        )
      } else {
        interpretations[i] <- paste(
          "The expected count is", baseline,
          "when all other predictors are zero and all categorical predictors are at their reference level."
        )
      }
    } else if (variable %in% data && is.numeric(fit$model[[variable]])) {
      # continuous
      if (has_offset) {
        interpretations[i] <- paste0(
          paste(
            "For every one-unit increase in", variable,
            ", the rate of events changes by", percentage, "%",
            "(95% CI:", lower, "% to", upper, "%)"
          ),
          adjustment
        )
      } else {
        interpretations[i] <- paste0(
          paste(
            "For every one-unit increase in", variable,
            ", the expected count changes by", percentage, "%",
            "(95% CI:", lower, "% to", upper, "%)"
          ),
          adjustment
        )
      }
    } else {
      # categorical
      if (has_offset) {
        interpretations[i] <- paste0(
          paste(
            "For observations satisfying", variable,
            ", the rate of events changes by", percentage, "%",
            "(95% CI:", lower, "% to", upper, "%),",
            "compared to the reference category"
          ),
          adjustment
        )
      } else {
        interpretations[i] <- paste0(
          paste(
            "For observations satisfying", variable,
            ", the expected count changes by", percentage, "%",
            "(95% CI:", lower, "% to", upper, "%),",
            "compared to the reference category"
          ),
          adjustment
        )
      }
    }
  }
  

  result <- data.frame(
    variable = names(coef),
    estimated_log = round(coef, 3),
    estimated_exp = round(rate, 3),
    percentage = round(pct_change, 3),
    ci_lower_pct = round(pct_lower, 3),
    ci_upper_pct = round(pct_upper, 3),
    z_value = round(z_values, 3),
    p_value = round(p_values, 4),
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

# Helper function: interaction terms
interpret_interaction <- function(fit){
  formula_obj <- formula(fit)
  predictors <- as.character(formula_obj)[3]

  has_colon <- grepl(":", predictors)
  has_star <- grepl("\\*", predictors)

  if (!has_colon && !has_star){
    return(NULL)
  }

  # extract names of all interaction terms
  all_terms <- attr(terms(fit), "term.labels")
  interaction_terms <- all_terms[grepl(":", all_terms)]

  results <- list()

  for (term in interaction_terms) {
    variables <- strsplit(term, ":")[[1]]
    v1 <- variables[1]
    v2 <- variables[2]

    v1_iscont <- is.numeric(fit$model[[v1]])
    v2_iscont <- is.numeric(fit$model[[v2]])

    # three cases
    if (v1_iscont && v2_iscont){
      # continuous x continuous -> johnson_neyman
      method <- "johnson_neyman"
      result <- tryCatch(
        do.call(
          interactions::johnson_neyman,
          list(model = fit, pred = as.name(v1), modx = as.name(v2))
        ),
        error = function(e) NULL
      )
    } else if (!v1_iscont && !v2_iscont) {
      # categorical x categorical -> emmeans
      method <- "emmeans"
      formula_em <- as.formula(paste("~", v1, "*", v2))
      result <- tryCatch(
        emmeans::emmeans(fit, formula_em, type = "response"),
        error = function(e) NULL
      )

    } else {
      # continuous x categorical -> emtrends
      method <- "emtrends"

      if (v1_iscont) {
        cont_var <- v1
        cat_var <- v2
      } else {
        cont_var <- v2
        cat_var <- v1
      }

      formula_et <- as.formula(paste("~", cat_var))
      result <- tryCatch(
        emmeans::emtrends(fit, formula_et, var = cont_var),
        error = function(e) NULL
      )
    }

    results[[term]] <- list(
      method = method,
      output = result
    )
  }

  return(results)
}
