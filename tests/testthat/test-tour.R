test_that("the tour dependencies point at files of the package", {
  for (dependency in NACHO:::tour_dependencies()) {
    directory <- system.file(dependency$src$file, package = dependency$package)
    files <- c(dependency$script, dependency$stylesheet)
    expect_true(
      all(file.exists(file.path(directory, files))),
      info = dependency$name
    )
  }
})

test_that("the vendored driver.js files name their version and licence", {
  version <- NACHO:::driver_dependency()$version
  for (file in c("driver.js", "driver.css")) {
    header <- readLines(
      system.file("tour", file, package = "NACHO"),
      n = 12,
      warn = FALSE
    )
    expect_true(
      any(grepl(paste("driver.js", version), header, fixed = TRUE)),
      info = file
    )
    expect_true(any(grepl("MIT licence", header, fixed = TRUE)), info = file)
    expect_true(
      any(grepl("github.com/kamranahmedse/driver.js", header, fixed = TRUE)),
      info = file
    )
  }
  copyrights <- readLines(system.file("COPYRIGHTS", package = "NACHO"))
  expect_true(any(grepl("driver.js 1.9.0", copyrights, fixed = TRUE)))
  expect_true(any(grepl(
    "Copyright (c) Kamran Ahmed",
    copyrights,
    fixed = TRUE
  )))
})

tour_fields <- c("element", "title", "description", "page", "open", "side")

test_that("each tour step has a target, a title and a description", {
  steps <- NACHO:::tour_steps()
  expect_length(steps, 9L)
  for (step in steps) {
    expect_true(all(names(step) %in% tour_fields), info = step$title)
    for (field in c("element", "title", "description")) {
      expect_true(rlang::is_string(step[[field]]), info = field)
      expect_true(nzchar(step[[field]]), info = field)
    }
    if (!is.null(step$side)) {
      expect_true(step$side %in% c("left", "right", "top", "bottom"))
    }
  }
})

selector_patterns <- function(selector) {
  patterns <- c(
    if (grepl("^#[A-Za-z0-9_-]+$", selector)) {
      sprintf('id="%s"', sub("^#", "", selector))
    },
    if (grepl("^\\.[A-Za-z0-9_-]+$", selector)) {
      sprintf('class="%s', sub("^\\.", "", selector))
    },
    if (grepl("aria-label='", selector, fixed = TRUE)) {
      sprintf(
        'aria-label="%s"',
        sub(".*aria-label='([^']+)'.*", "\\1", selector)
      )
    },
    if (grepl("data-value='", selector, fixed = TRUE)) {
      sprintf(
        'data-value="%s"',
        sub(".*data-value='([^']+)'.*", "\\1", selector)
      )
    }
  )
  testthat::expect_gt(length(patterns), 0L, label = selector)
  patterns
}

tour_page_html <- function(interactive) {
  as.character(NACHO:::app_ui(done = FALSE, interactive = interactive))
}

expect_tour_targets <- function(html) {
  for (step in NACHO:::tour_steps()) {
    for (pattern in selector_patterns(step$element)) {
      testthat::expect_match(html, pattern, fixed = TRUE, info = step$element)
    }
    if (!is.null(step$page)) {
      testthat::expect_match(
        html,
        sprintf('data-value="%s"', step$page),
        fixed = TRUE
      )
    }
    if (!is.null(step$open)) {
      testthat::expect_match(
        html,
        sprintf('<div class="accordion-item" data-value="%s"', step$open),
        fixed = TRUE
      )
    }
  }
}

test_that("each tour target exists in the app with static plots", {
  expect_tour_targets(tour_page_html(interactive = FALSE))
})

test_that("each tour target exists in the app with interactive plots", {
  testthat::skip_if_not_installed("ggiraph")
  expect_tour_targets(tour_page_html(interactive = TRUE))
})

test_that("the tour loads the example data when the app has none", {
  testthat::skip_on_cran()
  started <- 0L
  local_mocked_bindings(start_tour = function(session) started <<- started + 1L)
  shiny::testServer(NACHO:::app_server(NULL), {
    expect_false(output$has_data)
    session$setInputs(tour = 1)
    expect_identical(started, 1L)
    expect_true(output$has_data)
    expect_identical(colnames(data()@counts), colnames(GSE74821@counts))
  })
})

test_that("the tour keeps data that is already loaded", {
  testthat::skip_on_cran()
  x <- GSE74821[, 1:12]
  local_mocked_bindings(start_tour = function(session) invisible(NULL))
  shiny::testServer(NACHO:::app_server(x), {
    session$setInputs(tour = 1)
    expect_identical(ncol(data()@counts), 12L)
  })
})

test_that("start_tour() sends the steps to the page", {
  session <- shiny::MockShinySession$new()
  sent <- NULL
  session$sendCustomMessage <- function(type, message) {
    sent <<- list(type = type, message = message)
  }
  NACHO:::start_tour(session)
  expect_identical(sent$type, "nacho-tour-start")
  expect_identical(sent$message$steps, NACHO:::tour_steps())
})

test_that("the page loads the tour scripts", {
  html <- htmltools::renderTags(NACHO:::app_ui(done = FALSE))
  names <- vapply(html$dependencies, function(d) d$name, character(1))
  expect_true(all(c("driver.js", "nacho-tour") %in% names))
  expect_lt(match("driver.js", names), match("nacho-tour", names))
})
