context("render()")

test_that("Default parameters", {
  expect_message(
    object = render(nacho_object = GSE74821, output_dir = NULL),
    regexp = "Output created"
  )
})

test_that("with legend", {
  expect_message(
    object = render(nacho_object = GSE74821, output_dir = NULL, show_legend = TRUE),
    regexp = "Output created"
  )
})

test_that("with Rmd", {
  expect_message(
    object = render(nacho_object = GSE74821, output_dir = NULL, keep_rmd = TRUE),
    regexp = "Output created"
  )
})
