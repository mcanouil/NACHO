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

test_that("visualise() runs the app and returns what Done returns", {
  withr::local_options(rlang_interactive = TRUE)
  local_mocked_bindings(
    runApp = function(appDir, ...) {
      expect_s3_class(appDir, "shiny.appobj")
      GSE74821
    },
    .package = "shiny"
  )
  result <- withVisible(visualise(GSE74821))
  expect_false(result$visible)
  expect_identical(result$value, GSE74821)
})

test_that("visualise() builds the app with the Done button", {
  withr::local_options(rlang_interactive = TRUE)
  built <- NULL
  local_mocked_bindings(
    nacho_app = function(x = NULL, done = FALSE) {
      built <<- done
      shiny::shinyApp(shiny::fluidPage(), function(input, output) NULL)
    }
  )
  local_mocked_bindings(
    runApp = function(appDir, ...) {
      force(appDir)
      GSE74821
    },
    .package = "shiny"
  )
  visualise(GSE74821)
  expect_true(built)
})
