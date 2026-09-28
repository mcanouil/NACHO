test_that("visualise() refuses to start outside an interactive session", {
  withr::local_options(rlang_interactive = FALSE)
  expect_error(visualise(GSE74821), class = "nacho_error_not_interactive")
})

test_that("visualise() rejects an object that is not a nacho object", {
  expect_error(visualise(iris), class = "nacho_error_bad_object")
})

test_that("visualise() needs an object", {
  expect_error(visualise(), class = "nacho_error_bad_object")
})

test_that("visualise() refuses a NACHO 2 list", {
  old <- structure(list(nacho = data.frame()), class = "nacho")
  expect_error(visualise(old), class = "nacho_error_bad_object")
})

test_that("visualise() hands the nacho object to the app", {
  skip_if_not_installed("markdown")
  withr::local_options(rlang_interactive = TRUE)
  shared <- NULL
  local_mocked_bindings(
    runApp = function(...) {
      shared <<- shiny::getShinyOption("nacho_object")
    },
    .package = "shiny"
  )
  visualise(GSE74821)
  expect_true(S7::S7_inherits(shared, NACHO:::nacho))
  expect_identical(nacho_qc(shared), nacho_qc(GSE74821))
  expect_null(shiny::getShinyOption("nacho_object"))
})
