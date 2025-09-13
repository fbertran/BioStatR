test_that("eta2 errors with invalid inputs", {
  data(Mesures5, package = "BioStatR")
  # y must be a factor (in data it's a factor); force a numeric to check behavior
  expect_error(eta2(Mesures5$taille, as.numeric(Mesures5$espece)))
})
