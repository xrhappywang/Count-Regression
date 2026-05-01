#' Summarize Count Data
#'
#' @param data data frame
#' @param response response variable
#'
#' @return summary statistics
#' @export
summarize_count_data <- function(data, response) {

# check input 
# the data user use must be a data frame, and response varible must exist
  if(!is.data.frame(data)){
    stop("Dataset must be a data frame!")
  }
  if(!response %in% names(data)){
    stop(paste("Variable", response, "not found in data."))
  }
  y <- data[[response]]
  y <- y[!is.na(y)]


  list(
    n = length(y),
    mean = mean(y),
    variance = var(y),
    min = min(y),
    max = max(y),
    n_zeros = sum(y == 0),
    pct_zeros = sum(y == 0) / length(y) * 100,
    var_mean_ratio = var(y) / mean(y)
  )
}
