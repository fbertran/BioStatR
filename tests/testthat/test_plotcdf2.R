test_that("plotcdf2 returns named list with F,z,x,y", {
  x <- c(0, 1, 2)
  y <- c(0, 1)
  f <- matrix(c(0.2, 0.3, 0.1, 0.4, 0.0, 0.0), nrow = 3, byrow = TRUE)
  res <- plotcdf2(x, y, f, xaxe = "X", yaxe = "Y", col = NA, border = NA)
  expect_type(res, "list")
  expect_true(all(c("F","z","x","y") %in% names(res)))
})
