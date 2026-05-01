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
  if (!is.data.frame(data)) {
    stop("Dataset must be a data frame!")
  }
  if (!response %in% names(data)) {
    stop(paste("Variable", response, "not found in data."))
  }

  y <- data[[response]]

  # response must be numeric
  if (!is.numeric(y)) {
    stop(paste("Variable", response, "must be numeric for count regression."))
  }

  y <- y[!is.na(y)]

  # must have at least one non-NA value
  if (length(y) == 0) {
    stop("Response variable has no non-NA values.")
  }

  y_mean <- mean(y)
  y_var <- var(y)

  list(
    n = length(y),
    mean = y_mean,
    variance = y_var,
    min = min(y),
    max = max(y),
    n_zeros = sum(y == 0),
    pct_zeros = sum(y == 0) / length(y) * 100,
    var_mean_ratio = if (y_mean == 0) NA else y_var / y_mean
  )
}
