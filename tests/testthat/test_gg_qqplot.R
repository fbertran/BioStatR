test_that("gg_qqplot returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = rnorm(50))
  p <- gg_qqplot(df, "x")
  expect_s3_class(p, "ggplot")
})
