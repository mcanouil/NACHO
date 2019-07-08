context("render_nacho()")

test_that("Default parameters", {
  testthat::expect_message(render_nacho(nacho_object = GSE74821), "Output created: NACHO_QC.html")
})


test_that("with legend", {
  testthat::expect_message(render_nacho(nacho_object = GSE74821, legend = TRUE), "Output created: NACHO_QC.html")
})

test_that("with Rmd", {
  testthat::expect_message(render_nacho(nacho_object = GSE74821, keep_rmd = TRUE), "Output created: NACHO_QC.html")
})
