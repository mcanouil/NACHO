test_that("without a compute profile, a job runs in the app process", {
  local_mocked_bindings(blank_to_null = function(x) paste("ran", x))
  expect_identical(
    NACHO:::run_in_daemon("blank_to_null", list("here")),
    "ran here"
  )
})

test_that("a background job hands the library paths to the daemon", {
  skip_if_not_installed("mirai")
  captured <- NULL
  local_mocked_bindings(
    mirai = function(.expression, ..., .compute = NULL) {
      captured <<- list(
        expression = paste(deparse(substitute(.expression)), collapse = " "),
        args = list(...),
        compute = .compute
      )
      "task"
    },
    .package = "mirai"
  )
  expect_identical(
    NACHO:::run_in_daemon("render", list(GSE74821, format = "html"), "profile"),
    "task"
  )
  expect_identical(captured$compute, "profile")
  expect_match(captured$expression, ".libPaths(libs)", fixed = TRUE)
  expect_match(captured$expression, 'asNamespace("NACHO")', fixed = TRUE)
  expect_identical(captured$args$fun, "render")
  expect_identical(captured$args$args, list(GSE74821, format = "html"))
  expect_identical(captured$args$libs, .libPaths())
})

test_that("a job stopped after the session ends never settles", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  mirai::daemons(1, dispatcher = TRUE, .compute = "nacho-settle")
  withr::defer(mirai::daemons(0, .compute = "nacho-settle"))
  outcome <- function(ended) {
    settled <- "pending"
    running <- mirai::mirai(Sys.sleep(30), .compute = "nacho-settle")
    promises::then(
      NACHO:::settle_while_open(running, function() ended),
      onFulfilled = function(value) settled <<- "resolved",
      onRejected = function(error) settled <<- "rejected"
    )
    mirai::stop_mirai(running)
    for (i in 1:20) {
      later::run_now(0.1)
    }
    settled
  }
  expect_identical(outcome(ended = FALSE), "rejected")
  expect_identical(outcome(ended = TRUE), "pending")
})

test_that("the session daemon starts with the library paths of the app", {
  skip_if_not_installed("mirai")
  seen <- NULL
  local_mocked_bindings(
    has_package = function(package) TRUE,
    run_in_daemon = function(fun, args, compute) "done"
  )
  local_mocked_bindings(
    daemons = function(...) {
      if (is.null(seen)) {
        seen <<- Sys.getenv("R_LIBS")
      }
      invisible(NULL)
    },
    .package = "mirai"
  )
  shiny::testServer(
    function(input, output, session) {
      task <- NACHO:::session_task(
        session,
        "go",
        "run",
        on_success = function(result) NULL,
        on_error = function(message) NULL
      )
    },
    {
      task$invoke()
      wait_for_task(task, session)
    }
  )
  expect_identical(seen, paste(.libPaths(), collapse = .Platform$path.sep))
})

test_that("a job runs in the app process when the daemon cannot start", {
  skip_if_not_installed("mirai")
  results <- NULL
  local_mocked_bindings(
    has_package = function(package) TRUE,
    blank_to_null = function(x) paste("ran", x)
  )
  local_mocked_bindings(
    daemons = function(...) stop("No socket."),
    .package = "mirai"
  )
  shiny::testServer(
    function(input, output, session) {
      task <- NACHO:::session_task(
        session,
        "go",
        "blank_to_null",
        on_success = function(result) results <<- c(results, result),
        on_error = function(message) stop(message)
      )
    },
    {
      task$invoke("here")
      wait_for_task(task, session)
    }
  )
  expect_identical(results, "ran here")
})

test_that("the session daemon starts and loads NACHO when the warm-up input changes", {
  skip_if_not_installed("mirai")
  started <- 0
  warmed <- NULL
  local_mocked_bindings(has_package = function(package) TRUE)
  local_mocked_bindings(
    daemons = function(n, ...) {
      if (n > 0) {
        started <<- started + 1
      }
      invisible(NULL)
    },
    mirai = function(.expr, ..., .compute = NULL) {
      warmed <<- paste(deparse(substitute(.expr)), collapse = " ")
      invisible(NULL)
    },
    .package = "mirai"
  )
  shiny::testServer(
    function(input, output, session) {
      task <- NACHO:::session_task(
        session,
        "go",
        "run",
        on_success = function(result) NULL,
        on_error = function(message) NULL,
        warm_on = shiny::reactive(input$files)
      )
    },
    {
      expect_identical(started, 0)
      session$setInputs(files = "a")
      expect_identical(started, 1)
      session$setInputs(files = "b")
      expect_identical(started, 1)
    }
  )
  expect_match(warmed, ".libPaths(libs)", fixed = TRUE)
  expect_match(warmed, 'loadNamespace("NACHO")', fixed = TRUE)
})

test_that("a failed warm-up keeps the daemon and stops it with the session", {
  skip_if_not_installed("mirai")
  calls <- integer()
  used <- NULL
  local_mocked_bindings(
    has_package = function(package) TRUE,
    run_in_daemon = function(fun, args, compute) {
      used <<- compute
      "done"
    }
  )
  local_mocked_bindings(
    daemons = function(n, ...) {
      calls <<- c(calls, n)
      invisible(NULL)
    },
    mirai = function(...) stop("Queue full."),
    .package = "mirai"
  )
  shiny::testServer(
    function(input, output, session) {
      task <- NACHO:::session_task(
        session,
        "go",
        "run",
        on_success = function(result) NULL,
        on_error = function(message) NULL
      )
    },
    {
      task$invoke()
      wait_for_task(task, session)
      expect_false(is.null(used))
      session$close()
    }
  )
  expect_identical(calls, c(1, 0))
})
