test_that("binom.ci returns expected structure and bounds", {
  # Single case, all methods
  m <- binom.ci(50, 100, conf.level = 0.95, method = "all")
  expect_true(is.matrix(m))
  expect_identical(colnames(m), c("PointEst","Lower","Upper"))
  expect_equal(rownames(m), c("Exact","Wilson","Wald"))
  # Point estimate equals x/n
  expect_equal(unname(m[,"PointEst"]), rep(0.5, nrow(m)))
  # Lower < Upper and within [0,1]
  expect_true(all(m[,"Lower"] < m[,"Upper"]))
  expect_true(all(m[,"Lower"] >= 0))
  expect_true(all(m[,"Upper"] <= 1))
  
  # Vectorized input with one method
  xs <- c(0, 10, 90, 100)
  ns <- rep(100, length(xs))
  mm <- binom.ci(xs, ns, conf.level = 0.90, method = "Wilson")
  expect_true(is.matrix(mm))
  expect_identical(colnames(mm), c("PointEst","Lower","Upper"))
  expect_equal(nrow(mm), length(xs))
  # PointEst equals x/n
  expect_equal(as.vector(mm[, "PointEst"]), xs/ns, tolerance = 1e-12)
  # Bounds within [0,1]
  expect_true(all(mm[, "Lower"] >= 0))
  expect_true(all(mm[, "Upper"] <= 1))
})
