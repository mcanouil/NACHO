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
    session$flushReact()
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
    # The 6-sample toy data makes ggplot2 warn about an empty density layer.
    suppressWarnings(session$flushReact())
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
    session$flushReact()
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
    # The 6-sample toy data makes ggplot2 warn about an empty density layer.
    suppressWarnings(session$flushReact())
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
    "Samples",
    "About"
  )
  for (title in c(panels, NACHO:::app_plot_titles)) {
    expect_match(html, title, fixed = TRUE)
  }
  expect_match(html, "data-value=\"Samples\"", fixed = TRUE)
  expect_no_match(html, "data-value=\"Flagged samples\"", fixed = TRUE)
})

test_that("Done uses the thresholds as they stand, before the debounce", {
  returned <- NULL
  local_mocked_bindings(
    stopApp = function(returnValue = NULL) returned <<- returnValue,
    .package = "shiny"
  )
  shiny::testServer(NACHO:::app_server(GSE74821, done = TRUE), {
    session$flushReact()
    session$elapse(600)
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
    # The 6-sample toy data makes ggplot2 warn about an empty density layer.
    suppressWarnings(session$flushReact())
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

test_that("pages without data explain what to do", {
  html <- as.character(NACHO:::app_ui(done = FALSE))
  expect_match(html, "Load the example data", fixed = TRUE)
  expect_match(html, "output.has_data === true", fixed = TRUE)
  expect_match(html, "output.has_data === false", fixed = TRUE)
  expect_match(html, as.character(shiny::useBusyIndicators()), fixed = TRUE)
  pages <- unique(regmatches(
    html,
    gregexpr(
      '(?<=data-toggle="tab" data-bs-toggle="tab" data-value=")[^"]+',
      html,
      perl = TRUE
    )
  )[[1]])
  with_data <- setdiff(pages, c("Data", "About"))
  expect_gt(length(with_data), 0L)
  expect_equal(
    lengths(regmatches(html, gregexpr("No data yet.", html, fixed = TRUE))),
    length(with_data)
  )
})

test_that("the overview shows only when data is loaded", {
  html <- as.character(NACHO:::app_ui(done = FALSE))
  expect_match(
    html,
    "data-display-if=\"output.has_data === true\"[^>]*>\\s*<div[^>]*bslib-grid",
    perl = TRUE
  )
  expect_match(html, "Flagged samples", fixed = TRUE)
})

test_that("the app reports whether it has data", {
  shiny::testServer(NACHO:::app_server(NULL), {
    expect_false(output$has_data)
  })
  shiny::testServer(NACHO:::app_server(GSE74821), {
    expect_true(output$has_data)
  })
})

test_that("normalisation warnings reach the user once as toasts", {
  messages <- character()
  local_mocked_bindings(
    notify_user = function(message, type) messages <<- c(messages, message)
  )
  shiny::testServer(NACHO:::app_server(GSE74821), {
    session$flushReact()
    session$setInputs(
      `thresholds-method` = "RUVg",
      `thresholds-ruv_k` = 50,
      `thresholds-background` = "none",
      `thresholds-background_mode` = "threshold"
    )
    session$elapse(600)
    tuned()
    session$setInputs(`thresholds-FoV` = 90)
    session$elapse(600)
    tuned()
  })
  expect_length(messages, 1L)
  expect_match(messages, "ruv_k")
})

test_that("changing the normalisation settings announces the warning again", {
  messages <- character()
  local_mocked_bindings(
    notify_user = function(message, type) messages <<- c(messages, message)
  )
  shiny::testServer(NACHO:::app_server(GSE74821), {
    session$flushReact()
    session$setInputs(
      `thresholds-method` = "RUVg",
      `thresholds-ruv_k` = 50,
      `thresholds-background` = "none",
      `thresholds-background_mode` = "threshold"
    )
    session$elapse(600)
    tuned()
    session$setInputs(`thresholds-ruv_k` = 60)
    session$elapse(600)
    tuned()
  })
  expect_length(messages, 2L)
  expect_match(messages, "ruv_k")
})

test_that("an unavailable metric is muffled without a toast", {
  messages <- character()
  local_mocked_bindings(
    notify_user = function(message, type) messages <<- c(messages, message),
    tune_object = function(object, chosen, thresholds) {
      NACHO:::nacho_warn("No such metric.", class = "metric_unavailable")
      object
    }
  )
  expect_no_warning(
    NACHO:::tune_with_toasts(GSE74821, list(), NULL, new.env())
  )
  expect_length(messages, 0L)
})

test_that("Done sends normalisation warnings to the user", {
  messages <- character()
  local_mocked_bindings(
    notify_user = function(message, type) messages <<- c(messages, message),
    .package = "NACHO"
  )
  local_mocked_bindings(
    stopApp = function(returnValue = NULL) NULL,
    .package = "shiny"
  )
  shiny::testServer(NACHO:::app_server(GSE74821, done = TRUE), {
    session$flushReact()
    session$setInputs(
      `thresholds-method` = "RUVg",
      `thresholds-ruv_k` = 50,
      `thresholds-background` = "none",
      `thresholds-background_mode` = "threshold"
    )
    session$setInputs(done = 1)
  })
  expect_match(messages, "ruv_k")
})

test_that("plots follow the dark-mode toggle", {
  html <- as.character(NACHO:::app_ui(done = FALSE))
  expect_match(html, 'id="dark_mode"', fixed = TRUE)
  shiny::testServer(NACHO:::app_server(GSE74821), {
    session$setInputs(dark_mode = "dark")
    expect_true(dark())
    session$setInputs(dark_mode = "light")
    expect_false(dark())
  })
})

test_that("the page loads the bold and italic faces of the brand font", {
  deps <- htmltools::resolveDependencies(
    htmltools::findDependencies(NACHO:::app_ui(done = FALSE))
  )
  fonts <- Filter(function(d) d$name == "nacho-fonts", deps)
  expect_length(fonts, 1L)
  css <- paste(
    readLines(file.path(fonts[[1]]$src$file, fonts[[1]]$stylesheet)),
    collapse = "\n"
  )
  expect_match(css, "font-weight: 700", fixed = TRUE)
  expect_match(css, "font-style: italic", fixed = TRUE)
})

test_that("cards for plots that do not apply are hidden", {
  shiny::testServer(NACHO:::app_server(plexset_nacho), {
    session$flushReact()
    types <- strsplit(output$applicable, ",", fixed = TRUE)[[1]]
    expect_false(any(c("PCL", "LoD") %in% types))
    expect_true("BD" %in% types)
  })
  shiny::testServer(NACHO:::app_server(GSE74821), {
    session$flushReact()
    types <- strsplit(output$applicable, ",", fixed = TRUE)[[1]]
    expect_true(all(c("PCL", "LoD", "HF") %in% types))
  })
  html <- as.character(NACHO:::app_ui(done = FALSE))
  expect_match(html, "output.applicable.indexOf(&#39;,PCL,&#39;)", fixed = TRUE)
})

test_that("a new object is normalised once, with its own settings", {
  geo <- suppressMessages(
    normalise(GSE74821, normalisation_method = "GEO")
  )
  methods <- character()
  tune <- NACHO:::tune_object
  local_mocked_bindings(
    tune_object = function(object, chosen, thresholds) {
      methods <<- c(methods, chosen$normalisation_method)
      tune(object, chosen, thresholds)
    },
    .package = "NACHO"
  )
  shiny::testServer(NACHO:::app_server(geo), {
    session$flushReact()
    tuned()
    session$setInputs(`data-example` = 1)
    session$flushReact()
    tuned()
  })
  expect_identical(methods, c("GEO", "GLM"))
})

test_that("pages grow with their content instead of squeezing it", {
  html <- as.character(NACHO:::app_ui(done = FALSE))
  panes <- regmatches(html, gregexpr('<div class="tab-pane[^"]*"', html))[[1]]
  expect_gt(length(panes), 0L)
  expect_no_match(panes, "html-fill-container")
})
