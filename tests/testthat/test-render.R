context("render()")

test_that("Default parameters", {
  expect_true(
    render(nacho_object = GSE74821, output_dir = NULL)
  )
})

test_that("with legend", {
  expect_true(
    render(nacho_object = GSE74821, output_dir = NULL, show_legend = TRUE)
  )
})

test_that("with Rmd", {
  expect_true(
    render(nacho_object = GSE74821, output_dir = NULL, keep_rmd = TRUE)
  )
})


context("print_nacho()")

test_that("default", {
  expect_null(
    print_nacho(GSE74821, colour = "CartridgeID", size = 0.5, show_legend = FALSE)
  )
})

test_that("show_legend to TRUE", {
  expect_null(
    print_nacho(GSE74821, colour = "CartridgeID", size = 0.5, show_legend = TRUE)
  )
})
