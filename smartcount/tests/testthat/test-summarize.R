test_that("multiplication works", {

  # regular test

  df <- data.frame(y=c(1,0,20,20,3,4,5,6,7,5.6,4.23), x=1.1:11.1)
  result <- summarize_count_data(df, "y")

  expect_type(result, "list")
  expect_named(result, c("n", "mean","variance","min","max","n_zeros", "pct_zeros","var_mean_ratio"))

  expect_equal(result$n, 11)
  expect_equal(result$n_zeros,1)

  # with NaN input
  
})
