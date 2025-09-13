test_that("poi.ci returns a 1x3 matrix with named columns", {
  set.seed(1)
  x <- rpois(50, lambda = 10)
  m <- poi.ci(x, conf.level = 0.95)
  expect_true(is.matrix(m))
  expect_equal(dim(m), c(1L, 3L))
  expect_identical(colnames(m),
                   c("PointEst","95% LCI","95% UCI"))
  # PointEst equals mean(x)
  expect_equal(unname(m[1,1]), mean(x), tolerance = 1e-12)
  # LCI <= PointEst <= UCI
  expect_lte(m[1,2], m[1,1])
  expect_gte(m[1,3], m[1,1])
  # Non-negative
  expect_true(all(m >= 0))
})
