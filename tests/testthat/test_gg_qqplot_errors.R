test_that("gg_qqplot errors on unknown variable name", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = rnorm(10))
  expect_error(gg_qqplot(df, "not_a_column"))
})
