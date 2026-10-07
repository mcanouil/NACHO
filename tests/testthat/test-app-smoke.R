skip_if_no_browser <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  testthat::skip_if(
    is.null(suppressMessages(chromote::find_chrome())),
    "Chrome is not available."
  )
}

accessible_name_in <- function(app, selector) {
  cdp <- app$get_chromote_session()
  root <- cdp$DOM$getDocument()$root$nodeId
  node <- cdp$DOM$querySelector(root, selector)$nodeId
  tree <- cdp$Accessibility$getPartialAXTree(
    nodeId = node,
    fetchRelatives = FALSE
  )
  tree$nodes[[1]]$name$value
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
  app$run_js("document.querySelector('.nacho-summary-info').blur()")
  app$wait_for_js("document.querySelector('.tooltip') === null")
  expect_match(accessible_name_in(app, ".nacho-summary-info"), "FoV")
  app$run_js("document.querySelector('a[data-value=\"QC metrics\"]').click()")
  girafe_boxes <- function(width) {
    app$set_window_size(width, 1000)
    app$wait_for_idle(duration = 1000, timeout = 60000)
    app$get_js(
      "Array.from(document.querySelectorAll('.nacho-girafe .html-widget'))
        .filter(e => e.offsetParent)
        .map(e => e.getBoundingClientRect())
        .map(r => [Math.round(r.width), Math.round(r.height)])"
    )
  }
  narrow <- girafe_boxes(1280)
  app$run_js(
    "window.girafeRenders = 0;
    $(document).on('shiny:value', function(e) {
      if (/-girafe$/.test(e.name)) window.girafeRenders++;
    });"
  )
  girafe_boxes(1290)
  expect_identical(app$get_js("window.girafeRenders"), 0L)
  wide <- girafe_boxes(1700)
  expect_length(narrow, 4L)
  expect_identical(unique(vapply(narrow, `[[`, integer(1), 2)), 350L)
  expect_identical(unique(vapply(wide, `[[`, integer(1), 2)), 350L)
  expect_gt(wide[[1]][[1]], narrow[[1]][[1]])
  expect_identical(app$get_js("window.girafeRenders"), 4L)
  app$run_js("document.querySelector('#cite').click()")
  app$wait_for_js("document.querySelector('.modal.show') !== null")
  expect_match(
    app$get_js("document.querySelector('.modal.show').textContent"),
    "NACHO: an R package for quality control",
    fixed = TRUE
  )
  app$wait_for_js(
    "getComputedStyle(document.querySelector('.modal.show')).opacity === '1'"
  )
  app$wait_for_idle(duration = 500)
  app$run_js("document.querySelector('.modal.show .btn').click()")
  app$wait_for_js("document.querySelector('.modal.show') === null")
  app$wait_for_js(
    "document.activeElement.getAttribute('data-value') === 'Help'"
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
