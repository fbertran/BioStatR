test_that("panel.hist visual snapshot inside pairs", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("datasets")
  # vdiffr can snapshot a function that draws on the device.
  fig_fn <- function() {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    pairs(iris[1:3], diag.panel = panel.hist)
  }
  vdiffr::expect_doppelganger("panel-hist-in-pairs", fig_fn)
})
