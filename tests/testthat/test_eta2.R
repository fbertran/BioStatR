test_that("eta2 returns a scalar in [0,1] on example data", {
  data(Mesures5, package = "BioStatR")
  e <- eta2(Mesures5$taille, Mesures5$espece)
  expect_type(e, "double")
  expect_true(length(e) == 1L)
  expect_gte(e, 0)
  expect_lte(e, 1)
})
