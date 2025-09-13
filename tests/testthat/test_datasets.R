test_that("datasets load with expected structure", {
  data(Europe, package = "BioStatR")
  expect_s3_class(Europe, "data.frame")
  expect_true(all(c("Pays","Duree") %in% names(Europe)))
  expect_equal(ncol(Europe), 2L)
  expect_true(nrow(Europe) >= 20) # per docs ~25 obs
  
  data(Extrait_Taille, package = "BioStatR")
  expect_s3_class(Extrait_Taille, "data.frame")
  expect_true(all(c("masse","taille","espece") %in% names(Extrait_Taille)))
  
  data(Mesures, package = "BioStatR")
  expect_s3_class(Mesures, "data.frame")
  expect_true(all(c("masse","taille","espece") %in% names(Mesures)))
  
  data(Mesures5, package = "BioStatR")
  expect_s3_class(Mesures5, "data.frame")
  expect_true(all(c("masse","taille","graines","masse_sec","espece") %in% names(Mesures5)))
  
  data(Quetelet, package = "BioStatR")
  expect_s3_class(Quetelet, "data.frame")
  expect_true(all(c("sexe","poids","taille") %in% names(Quetelet)))
})
