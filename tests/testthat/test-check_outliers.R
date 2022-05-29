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
