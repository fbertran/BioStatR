test_that("cvar returns NA with NA inputs (no na.rm handled)", {
  x <- c(1,2,NA,4)
  expect_true(is.na(cvar(x)))
})
