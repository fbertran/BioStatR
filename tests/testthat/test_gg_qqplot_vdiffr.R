test_that("gg_qqplot visual snapshot (default)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("vdiffr")
  set.seed(123)
  df <- data.frame(x = rnorm(200))
  p <- gg_qqplot(df, "x")
  vdiffr::expect_doppelganger("gg_qqplot-default", p)
})

test_that("gg_qqplot visual snapshot (with qq.line)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("vdiffr")
  set.seed(123)
  df <- data.frame(x = rnorm(200))
  p <- gg_qqplot(df, "x", qq.line = TRUE)
  vdiffr::expect_doppelganger("gg_qqplot-with-line", p)
})
