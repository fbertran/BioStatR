test_that("binom.ci matches known formulas (Exact, Wilson, Wald)", {
  x <- 50; n <- 100; cl <- 0.95
  m <- binom.ci(x, n, conf.level = cl, method = "all")
  expect_true(is.matrix(m))
  z <- qnorm(1 - (1-cl)/2)
  phat <- x/n
  # Wald
  wald_lo <- phat - z*sqrt(phat*(1-phat)/n)
  wald_hi <- phat + z*sqrt(phat*(1-phat)/n)
  expect_equal(unname(m["Wald","Lower"]), wald_lo, tolerance = 1e-10)
  expect_equal(unname(m["Wald","Upper"]), wald_hi, tolerance = 1e-10)
  expect_equal(unname(m["Wald","PointEst"]), phat, tolerance = 1e-12)
  # Exact (Clopper-Pearson) via binom.test
  bt <- binom.test(x, n, conf.level = cl)
  expect_equal(unname(m["Exact","Lower"]), bt$conf.int[1], tolerance = 1e-12)
  expect_equal(unname(m["Exact","Upper"]), bt$conf.int[2], tolerance = 1e-12)
  # Wilson (sans correction de continuité)
  wilson_lo <- (phat + z^2/(2*n) - z*sqrt((phat*(1-phat) + z^2/(4*n))/n))/(1 + z^2/n)
  wilson_hi <- (phat + z^2/(2*n) + z*sqrt((phat*(1-phat) + z^2/(4*n))/n))/(1 + z^2/n)
  expect_equal(unname(m["Wilson","Lower"]), wilson_lo, tolerance = 1e-10)
  expect_equal(unname(m["Wilson","Upper"]), wilson_hi, tolerance = 1e-10)
})

test_that("binom.ci handles edge cases x=0 and x=n", {
  n <- 40; cl <- 0.95
  m0 <- binom.ci(0, n, conf.level = cl, method = "all")
  mn <- binom.ci(n, n, conf.level = cl, method = "all")
  # Point estimates
  expect_equal(unname(m0[,"PointEst"]), rep(0, nrow(m0)))
  expect_equal(unname(mn[,"PointEst"]), rep(1, nrow(mn)))
  # Bounds are in [0,1]
  expect_true(all(m0[,"Lower"] >= 0 & m0[,"Upper"] <= 1))
  expect_true(all(mn[,"Lower"] >= 0 & mn[,"Upper"] <= 1))
  # Exact should coincide with binom.test
  bt0 <- binom.test(0, n, conf.level = cl)$conf.int
  btn <- binom.test(n, n, conf.level = cl)$conf.int
  expect_equal(unname(m0["Exact","Lower"]), bt0[1], tolerance=1e-12)
  expect_equal(unname(m0["Exact","Upper"]), bt0[2], tolerance=1e-12)
  expect_equal(unname(mn["Exact","Lower"]), btn[1], tolerance=1e-12)
  expect_equal(unname(mn["Exact","Upper"]), btn[2], tolerance=1e-12)
})
