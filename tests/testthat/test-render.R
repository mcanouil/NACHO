test_that("Default parameters", {
  out <- suppressWarnings(render(nacho_object = GSE74821))
  expect_null(out)
})

test_that("with legend", {
  out <- suppressWarnings(render(nacho_object = GSE74821, show_legend = TRUE))
  expect_null(out)
})

test_that("with Rmd", {
  out <- suppressWarnings(render(nacho_object = GSE74821, clean = FALSE))
  expect_null(out)
})
