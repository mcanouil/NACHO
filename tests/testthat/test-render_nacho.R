context("render_nacho()")

test_that("Default parameters", {
  testthat::expect_success(render_nacho(nacho_object = GSE74821))
})


test_that("with legend", {
  testthat::expect_success(render_nacho(nacho_object = GSE74821, legend = TRUE))
})

test_that("with Rmd", {
  testthat::expect_success(render_nacho(nacho_object = GSE74821, keep_rmd = TRUE))
})
