test_that("without mirai, a job runs in the app process", {
  local_no_mirai()
  local_mocked_bindings(blank_to_null = function(x) paste("ran", x))
  expect_identical(
    NACHO:::run_in_daemon("blank_to_null", list("here")),
    "ran here"
  )
})

test_that("a background job hands the library paths to the daemon", {
  skip_if_not_installed("mirai")
  captured <- NULL
  local_mocked_bindings(has_package = function(package) TRUE)
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
