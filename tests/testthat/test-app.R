test_that("nacho_app() builds a Shiny app", {
  expect_s3_class(nacho_app(), "shiny.appobj")
  expect_s3_class(nacho_app(GSE74821), "shiny.appobj")
  expect_error(nacho_app(iris), class = "nacho_error_bad_object")
})

test_that("the app flags samples when a threshold moves", {
  shiny::testServer(NACHO:::app_server(GSE74821), {
    session$flushReact()
    session$elapse(600)
    expect_identical(sum(qc()$status == "fail"), 0L)
    session$setInputs(`thresholds-FoV` = 99.9)
    session$elapse(600)
    expect_gt(sum(qc()$status == "fail"), 0L)
  })
})

test_that("the app normalises again when the method changes", {
  shiny::testServer(NACHO:::app_server(GSE74821), {
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
  shiny::testServer(NACHO:::app_server(toy), {
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
  shiny::testServer(NACHO:::app_server(GSE74821), {
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
    stopApp = function(returnValue = NULL) stopped <<- TRUE,
    .package = "shiny"
  )
  local_mocked_bindings(
    notify_user = function(message, type) warned <<- type
  )
  shiny::testServer(NACHO:::app_server(toy_nacho(6L)), {
    session$setInputs(`thresholds-method` = "RUVg", `thresholds-ruv_k` = 3)
    session$elapse(600)
    session$setInputs(done = 1)
  })
  expect_false(stopped)
  expect_identical(warned, "warning")
})

test_that("each plot module draws its own plot type", {
  shiny::testServer(NACHO:::app_server(GSE74821), {
    session$flushReact()
    session$elapse(600)
    expect_identical(output$`BD-plot`$alt, plot_alt_texts[["BD"]])
    expect_identical(output$`PCBatch-plot`$alt, plot_alt_texts[["PCBatch"]])
  })
})

test_that("the page holds every navigation panel and plot card", {
  html <- as.character(NACHO:::app_ui(NULL))
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
  shiny::testServer(NACHO:::app_server(GSE74821), {
    session$flushReact()
    session$setInputs(`thresholds-FoV` = 99.9)
    session$setInputs(done = 1)
  })
  expect_identical(returned@thresholds$FoV, 99.9)
})
