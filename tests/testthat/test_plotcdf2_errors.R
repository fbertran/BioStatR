test_that("plotcdf2 errors when matrix dimensions mismatch", {
  x <- c(0,1,2)
  y <- c(0,1,2)
  f <- matrix(0.1, nrow = 2, ncol = 3) # wrong nrow
  expect_error(plotcdf2(x, y, f, xaxe = "x", yaxe = "y"))
})
