test_that("cvar computes 100*sd/mean and handles simple vectors", {
  x <- c(1,2,3,4,5)
  expect_equal(cvar(x), 100*sd(x)/mean(x))
  # Numeric scalar
  expect_type(cvar(x), "double")
  # Positive values lead to non-negative CV
  y <- rexp(100, rate = 2)
  expect_gte(cvar(y), 0)
})
