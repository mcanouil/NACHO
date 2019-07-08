context("render_nacho()")

test_that("Default parameters", {
  testthat::expect_success(render_nacho(nacho_object = GSE74821))
})
