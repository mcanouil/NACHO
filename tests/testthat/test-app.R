test_that("nacho_app() builds a Shiny app", {
  expect_s3_class(nacho_app(), "shiny.appobj")
  expect_s3_class(nacho_app(GSE74821), "shiny.appobj")
  expect_error(nacho_app(iris), class = "nacho_error_bad_object")
})

test_that("the app flags samples when a threshold moves", {
  shiny::testServer(NACHO:::app_server(GSE74821, done = TRUE), {
    session$flushReact()
    session$elapse(600)
    expect_identical(sum(qc()$status == "fail"), 0L)
    session$setInputs(`thresholds-FoV` = 99.9)
    session$elapse(600)
    expect_gt(sum(qc()$status == "fail"), 0L)
  })
})

test_that("the app normalises again when the method changes", {
  shiny::testServer(NACHO:::app_server(GSE74821, done = TRUE), {
    session$setInputs(
      `thresholds-method` = "RUVg",
      `thresholds-ruv_k` = 1,
      `thresholds-background` = "none",
      `thresholds-background_mode` = "threshold"
    )
    session$elapse(600)
    expect_identical(tuned()@settings$normalisation_method, "RUVg")
    expect_true("W_1" %in% names(nacho_samples(tuned())))
  })
})

test_that("a method the data cannot support shows a message, not a crash", {
  toy <- toy_nacho(6L)
  shiny::testServer(NACHO:::app_server(toy, done = TRUE), {
    session$setInputs(`thresholds-method` = "RUVg", `thresholds-ruv_k` = 3)
    session$elapse(600)
    expect_error(tuned(), class = "shiny.silent.error")
  })
})

test_that("Done returns the tuned object", {
  returned <- NULL
  local_mocked_bindings(
    stopApp = function(returnValue = NULL) returned <<- returnValue,
    .package = "shiny"
  )
  shiny::testServer(NACHO:::app_server(GSE74821, done = TRUE), {
    session$setInputs(`thresholds-FoV` = 99.9)
    session$elapse(600)
    session$setInputs(done = 1)
  })
  expect_true(S7::S7_inherits(returned, NACHO:::nacho))
  expect_identical(returned@thresholds$FoV, 99.9)
})

test_that("help pages render with shiny::markdown()", {
  for (name in c("nacho", "bd", "fov", "pcl", "lod", "pf", "hgf")) {
    expect_s3_class(NACHO:::help_page(name), "html")
  }
})

test_that("Done warns instead of closing when the settings fail", {
  stopped <- FALSE
  warned <- NULL
  local_mocked_bindings(
    stopApp = function(returnValue = NULL) {
      if (!is.null(returnValue)) stopped <<- TRUE
    },
    .package = "shiny"
  )
  local_mocked_bindings(
    notify_user = function(message, type) warned <<- type
  )
  shiny::testServer(NACHO:::app_server(toy_nacho(6L), done = TRUE), {
    session$setInputs(`thresholds-method` = "RUVg", `thresholds-ruv_k` = 3)
    session$elapse(600)
    session$setInputs(done = 1)
  })
  expect_false(stopped)
  expect_identical(warned, "warning")
})

test_that("each plot module draws its own plot type", {
  shiny::testServer(NACHO:::app_server(GSE74821, done = TRUE), {
    session$flushReact()
    session$elapse(600)
    expect_identical(output$`BD-plot`$alt, plot_alt_texts[["BD"]])
    expect_identical(output$`PCBatch-plot`$alt, plot_alt_texts[["PCBatch"]])
  })
})

test_that("the page holds every navigation panel and plot card", {
  html <- as.character(NACHO:::app_ui(done = TRUE))
  panels <- c(
    "Data",
    "QC metrics",
    "Controls",
    "Counts",
    "Normalisation",
    "Batch",
    "Flagged samples",
    "About"
  )
  for (title in c(panels, NACHO:::app_plot_titles)) {
    expect_match(html, title, fixed = TRUE)
  }
})

test_that("Done uses the thresholds as they stand, before the debounce", {
  returned <- NULL
  local_mocked_bindings(
    stopApp = function(returnValue = NULL) returned <<- returnValue,
    .package = "shiny"
  )
  shiny::testServer(NACHO:::app_server(GSE74821, done = TRUE), {
    session$flushReact()
    session$setInputs(`thresholds-FoV` = 99.9)
    session$setInputs(done = 1)
  })
  expect_identical(returned@thresholds$FoV, 99.9)
})

test_that("the Done button only exists when the app is allowed to stop", {
  expect_no_match(
    as.character(NACHO:::app_ui(done = FALSE)),
    "Done",
    fixed = TRUE
  )
  expect_match(
    as.character(NACHO:::app_ui(done = TRUE)),
    "Done",
    fixed = TRUE
  )
})

test_that("a deployed app never stops on Done", {
  stopped <- FALSE
  local_mocked_bindings(
    stopApp = function(returnValue = NULL) stopped <<- TRUE,
    .package = "shiny"
  )
  shiny::testServer(NACHO:::app_server(GSE74821, done = FALSE), {
    session$flushReact()
    session$setInputs(done = 1)
  })
  expect_false(stopped)
})

test_that("Done explains why the object cannot be returned", {
  messages <- character()
  local_mocked_bindings(
    notify_user = function(message, type) messages <<- c(messages, message)
  )
  shiny::testServer(NACHO:::app_server(toy_nacho(6L), done = TRUE), {
    session$setInputs(`thresholds-method` = "RUVg", `thresholds-ruv_k` = 3)
    session$setInputs(done = 1)
  })
  expect_match(messages, "before clicking Done")
  expect_gt(
    nchar(messages),
    nchar(
      "Choose a normalisation method these data support before clicking Done."
    )
  )
})

test_that("closing the page ends visualise() but not a deployed app", {
  stopped <- 0L
  local_mocked_bindings(
    stopApp = function(returnValue = NULL) stopped <<- stopped + 1L,
    .package = "shiny"
  )
  shiny::testServer(NACHO:::app_server(GSE74821, done = FALSE), {
    session$close()
  })
  expect_identical(stopped, 0L)
  shiny::testServer(NACHO:::app_server(GSE74821, done = TRUE), {
    session$close()
  })
  expect_identical(stopped, 1L)
})

test_that("closing the page after Done keeps the tuned object", {
  values <- list()
  local_mocked_bindings(
    stopApp = function(returnValue = NULL) {
      values[[length(values) + 1L]] <<- returnValue
    },
    .package = "shiny"
  )
  shiny::testServer(NACHO:::app_server(GSE74821, done = TRUE), {
    session$flushReact()
    session$setInputs(done = 1)
    session$close()
  })
  expect_length(values, 1L)
  expect_false(is.null(values[[1]]))
})
