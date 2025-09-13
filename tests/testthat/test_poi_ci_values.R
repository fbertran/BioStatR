test_that("poi.ci equals chi-square-based interval", {
  set.seed(42)
  x <- rpois(100, lambda = 8)
  cl <- 0.95
  m <- poi.ci(x, conf.level = cl)
  expect_true(is.matrix(m))
  nn <- length(x)
  LCI <- qchisq((1 - cl)/2, 2*sum(x))/2/nn
  UCI <- qchisq(1 - (1 - cl)/2, 2*(sum(x)+1))/2/nn
  expect_equal(unname(m[1, "PointEst"]), mean(x), tolerance = 1e-12)
  expect_equal(unname(m[1, "95% LCI"]), LCI, tolerance = 1e-12)
  expect_equal(unname(m[1, "95% UCI"]), UCI, tolerance = 1e-12)
})

test_that("poi.ci input must be numeric", {
  expect_error(poi.ci(as.factor(c(1,2,3))))
})
