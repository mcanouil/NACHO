skip_if_no_browser <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  testthat::skip_if(
    is.null(suppressMessages(chromote::find_chrome())),
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
  expect_identical(
    app$wait_for_value(output = "overview-samples", ignore = list(NULL, "")),
    "48"
  )
  expect_identical(
    app$wait_for_value(
      output = "overview-flagged_count",
      ignore = list(NULL, "")
    ),
    "0"
  )
  app$set_inputs(`thresholds-FoV` = 99.9)
  flagged <- app$wait_for_value(
    output = "overview-flagged_count",
    ignore = list(NULL, "", "0")
  )
  expect_false(identical(flagged, "0"))
  app$run_js("document.querySelector('.nacho-summary-info').focus()")
  app$wait_for_js("document.querySelector('.tooltip') !== null")
  expect_match(
    app$get_js("document.querySelector('.tooltip').textContent"),
    "FoV"
  )
  logs <- app$get_logs()
  errors <- logs[logs$location == "shiny" & logs$level == "stderr", ]
  expect_false(any(grepl("^(Error|Warning)|Unhandled promise", errors$message)))
})

test_that("the app loads the example data from an empty start", {
  skip_if_no_browser()
  app <- shinytest2::AppDriver$new(
    nacho_app(),
    name = "empty",
    load_timeout = 60000,
    timeout = 20000
  )
  on.exit(app$stop(), add = TRUE)
  app$click("data-example")
  expect_identical(
    app$wait_for_value(output = "overview-samples", ignore = list(NULL, "")),
    "48"
  )
})

test_that("Cite NACHO opens the citation", {
  skip_if_no_browser()
  app <- shinytest2::AppDriver$new(
    nacho_app(GSE74821),
    name = "cite",
    load_timeout = 60000,
    timeout = 20000
  )
  on.exit(app$stop(), add = TRUE)
  app$run_js("document.querySelector('#cite').click()")
  app$wait_for_js("document.querySelector('.modal.show') !== null")
  expect_match(
    app$get_js("document.querySelector('.modal.show').textContent"),
    "NACHO: an R package for quality control",
    fixed = TRUE
  )
})
