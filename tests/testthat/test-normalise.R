context("normalise()")

test_that("normalise", {
  normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 10,
    remove_outliers = FALSE,
    outliers_thresholds = list(
      BD = c(0.1, 2.25),
      FoV = 75,
      LoD = 2,
      PC = 0.95,
      Positive_factor = c(1/4, 4),
      House_factor = c(1/11, 11)
    )
  )
})