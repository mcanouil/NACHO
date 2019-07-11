context("render()")

test_that("Default parameters", {
  expect_true(
    object = render(nacho_object = GSE74821, output_dir = NULL)
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
