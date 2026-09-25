test_that("Default check_outliers", {
  expect_s3_class(check_outliers(GSE74821), "nacho")
})

test_that("missing object", {
  expect_error(check_outliers())
})

test_that("wrong attribute", {
  attr(GSE74821, "RCC_type") <- "something_wrong"
  expect_error(check_outliers(GSE74821))
})

test_that("PlexSet objects ignore PCL and LoD when flagging outliers", {
  plexset <- salmon_nacho
  plexset[["nacho"]][["PCL"]] <- 0
  plexset[["nacho"]][["LoD"]] <- 0
  expect_identical(
    check_outliers(plexset)[["nacho"]][["is_outlier"]],
    check_outliers(salmon_nacho)[["nacho"]][["is_outlier"]]
  )
})
