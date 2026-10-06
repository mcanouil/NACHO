skip_if_no_browser <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  testthat::skip_if(
    is.null(chromote::find_chrome()),
    "Chrome is not available."
  )
}

test_that("the app flags samples when a threshold moves", {
  skip_if_no_browser()
  app <- shinytest2::AppDriver$new(
    nacho_app(GSE74821),
    name = "smoke",
    load_timeout = 60000,
    timeout = 20000
  )
  on.exit(app$stop(), add = TRUE)
  expect_identical(app$get_value(output = "overview-samples"), "48")
  expect_identical(app$get_value(output = "overview-flagged_count"), "0")
  app$set_inputs(`thresholds-FoV` = 99.9)
  app$wait_for_idle(duration = 1000)
  expect_false(identical(app$get_value(output = "overview-flagged_count"), "0"))
})

test_that("the app loads the example data from an empty start", {
  skip_if_no_browser()
  app <- shinytest2::AppDriver$new(
    nacho_app(),
    name = "empty",
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$click("data-example")
  app$wait_for_idle(duration = 1000)
  expect_identical(app$get_value(output = "overview-samples"), "48")
})
