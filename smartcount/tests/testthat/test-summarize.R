test_that("summarize_count_data returns correct structure", {
  df <- data.frame(y = c(1, 0, 20, 20, 3, 4, 5, 6, 7, 5.6, 4.23), x = 1.1:11.1)
  result <- summarize_count_data(df, "y")

  expect_type(result, "list")
  expect_named(result, c("n", "mean", "variance", "min", "max",
                         "n_zeros", "pct_zeros", "var_mean_ratio"))
  expect_equal(result$n, 11)
  expect_equal(result$n_zeros, 1)
})

test_that("summarize_count_data handles NA/NaN", {
  df_na <- data.frame(y = c(1, 2, NA, 4, NaN, 0))
  result <- summarize_count_data(df_na, "y")

  expect_equal(result$n, 4)
})

test_that("summarize_count_data errors on invalid input", {
  expect_error(summarize_count_data(list(), "y"))                       # not a data frame
  expect_error(summarize_count_data(data.frame(x = 1), "y"))            # column does not exist
  expect_error(summarize_count_data(data.frame(y = c("a", "b")), "y"))  # not numeric
})
