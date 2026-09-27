test_that("Default check_outliers", {
  expect_true(S7::S7_inherits(check_outliers(salmon_nacho), NACHO:::nacho))
})

test_that("missing object", {
  expect_error(check_outliers(), class = "nacho_error_bad_object")
})

test_that("check_outliers() refuses a NACHO 2 list", {
  old <- structure(list(nacho = data.frame()), class = "nacho")
  expect_error(check_outliers(old), class = "nacho_error_bad_object")
})

test_that("PlexSet objects ignore PCL and LoD when flagging outliers", {
  plexset <- salmon_nacho
  samples <- plexset@samples
  samples[["PCL"]] <- 0
  samples[["LoD"]] <- 0
  plexset@samples <- samples
  expect_identical(
    nacho_qc(check_outliers(plexset))[["is_outlier"]],
    nacho_qc(check_outliers(salmon_nacho))[["is_outlier"]]
  )
})
