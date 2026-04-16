#' Summarize Count Data
#'
#' @param data A data frame.
#' @param response A character string naming the response variable.
#'
#' @return A list with summary statistics.
#' @export
summarize_count <- function(data, response) {
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
