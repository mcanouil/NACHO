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

tour_app <- function(x = NULL, envir = parent.frame()) {
  app <- shinytest2::AppDriver$new(
    nacho_app(x),
    name = "tour",
    width = 1280,
    height = 800,
    load_timeout = 60000,
    timeout = 30000
  )
  withr::defer(app$stop(), envir = envir)
  app
}

js_string <- function(app, script) {
  app$get_js(sprintf(
    paste0(
      "(function() { var v = %s; ",
      "return v === null || v === undefined ? '' : String(v); })()"
    ),
    script
  ))
}

wait_for_step <- function(app, n, timeout = 30000) {
  app$wait_for_js(
    sprintf(
      paste0(
        "(document.querySelector('.driver-popover.nacho-tour ",
        ".driver-popover-progress-text') || {}).textContent === '%d of 9'"
      ),
      n
    ),
    timeout = timeout
  )
}

start_tour_in <- function(app) {
  app$run_js("document.getElementById('tour').click()")
  wait_for_step(app, 1)
}

click_next <- function(app) {
  app$run_js("document.querySelector('.driver-popover-next-btn').click()")
}

active_page <- function(app) {
  trimws(js_string(
    app,
    "(document.querySelector('.navbar .nav-link.active') || {}).textContent"
  ))
}

relative_luminance <- function(rgb) {
  channels <- as.numeric(regmatches(rgb, gregexpr("[0-9.]+", rgb))[[1]][1:3]) /
    255
  linear <- ifelse(
    channels <= 0.03928,
    channels / 12.92,
    ((channels + 0.055) / 1.055)^2.4
  )
  sum(c(0.2126, 0.7152, 0.0722) * linear)
}

contrast_ratio <- function(foreground, background) {
  luminance <- sort(
    c(relative_luminance(foreground), relative_luminance(background)),
    decreasing = TRUE
  )
  (luminance[1] + 0.05) / (luminance[2] + 0.05)
}

next_button_contrast <- function(app) {
  style <- "getComputedStyle(document.querySelector('.driver-popover-next-btn'))"
  contrast_ratio(
    js_string(app, paste0(style, ".color")),
    js_string(app, paste0(style, ".backgroundColor"))
  )
}

tour_status <- function(app) {
  js_string(app, "document.getElementById('nacho-tour-status').textContent")
}

wait_for_end <- function(app) {
  app$wait_for_js("document.querySelector('.driver-popover') === null")
  app$wait_for_js(
    "document.activeElement && document.activeElement.getAttribute('data-value') === 'help'"
  )
}

test_that("the tour walks every step and ends where it started", {
  skip_if_no_browser()
  app <- tour_app()
  steps <- NACHO:::tour_steps()
  app$run_js(
    "document.querySelector('bslib-input-dark-mode').setAttribute('mode', 'light')"
  )
  start_tour_in(app)
  expect_gte(next_button_contrast(app), 4.5)
  expect_identical(
    app$wait_for_value(output = "overview-samples", ignore = list(NULL, "")),
    "48"
  )
  for (i in seq_along(steps)) {
    wait_for_step(app, i, timeout = if (i == 1) 30000 else 8000)
    expect_identical(
      js_string(
        app,
        "document.querySelector('.driver-popover-title').textContent"
      ),
      steps[[i]]$title
    )
    expect_identical(
      tour_status(app),
      sprintf("Step %d of 9: %s", i, steps[[i]]$title)
    )
    if (!is.null(steps[[i]]$page)) {
      expect_identical(active_page(app), steps[[i]]$page)
    }
    expect_identical(
      js_string(
        app,
        paste0(
          "(function() { var r = document.querySelector(",
          "'.driver-active-element').getBoundingClientRect(); ",
          "return r.left >= 0 && r.top >= 0 && ",
          "r.right <= window.innerWidth && r.width > 0; })()"
        )
      ),
      "true",
      info = steps[[i]]$element
    )
    click_next(app)
  }
  wait_for_end(app)
  expect_identical(active_page(app), "Data")
})

test_that("Esc ends the tour from a collapsed sidebar in dark mode", {
  skip_if_no_browser()
  app <- tour_app(GSE74821)
  app$run_js("document.querySelector('a[data-value=\"Counts\"]').click()")
  app$run_js(
    "document.querySelector('bslib-input-dark-mode').setAttribute('mode', 'dark')"
  )
  app$run_js(
    "document.querySelector('.bslib-sidebar-layout > .collapse-toggle').click()"
  )
  app$wait_for_js(
    "document.querySelector('.bslib-sidebar-layout.sidebar-collapsed') !== null"
  )
  start_tour_in(app)
  expect_identical(
    js_string(
      app,
      "document.querySelector('.driver-popover-close-btn').getAttribute('aria-label')"
    ),
    "Close the tour"
  )
  expect_identical(
    js_string(
      app,
      "getComputedStyle(document.querySelector('.driver-popover')).backgroundColor"
    ),
    js_string(app, "getComputedStyle(document.body).backgroundColor")
  )
  expect_gte(next_button_contrast(app), 4.5)
  click_next(app)
  wait_for_step(app, 2)
  click_next(app)
  wait_for_step(app, 3)
  expect_identical(
    js_string(
      app,
      "document.querySelector('.bslib-sidebar-layout.sidebar-collapsed') === null"
    ),
    "true"
  )
  app$run_js(
    "window.dispatchEvent(new KeyboardEvent('keyup', {key: 'Escape'}))"
  )
  wait_for_end(app)
  expect_identical(active_page(app), "Counts")
})

test_that("a busy app still shows its step, a missing target is skipped, and a restart gives one tour", {
  skip_if_no_browser()
  app <- tour_app(GSE74821)
  start_tour_in(app)
  app$run_js("document.documentElement.classList.add('shiny-busy')")
  click_next(app)
  wait_for_step(app, 2, timeout = 20000)
  app$run_js("document.documentElement.classList.remove('shiny-busy')")
  app$run_js("document.getElementById('batch-batch-design').remove()")
  for (i in 2:6) {
    wait_for_step(app, i)
    click_next(app)
  }
  wait_for_step(app, 8, timeout = 20000)
  expect_match(tour_status(app), "Step 7 skipped", fixed = TRUE)
  expect_match(tour_status(app), "Step 8 of 9: Export", fixed = TRUE)
  app$run_js("document.getElementById('tour').click()")
  wait_for_step(app, 1)
  expect_identical(
    js_string(app, "document.querySelectorAll('.driver-popover').length"),
    "1"
  )
  app$run_js("document.querySelector('.driver-popover-close-btn').click()")
  wait_for_end(app)
})

test_that("a skipped first step is announced with the next step", {
  skip_if_no_browser()
  app <- tour_app(GSE74821)
  app$run_js("document.getElementById('data-card').remove()")
  app$run_js("document.getElementById('tour').click()")
  wait_for_step(app, 2, timeout = 20000)
  expect_match(
    tour_status(app),
    "Step 1 skipped: its part of the app is not available. Step 2 of 9",
    fixed = TRUE
  )
})
