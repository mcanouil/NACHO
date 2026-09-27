test_that("nacho_abort() adds the NACHO classes and keeps the caller", {
  f <- function() {
    NACHO:::nacho_abort("Broken {.val thing}.", class = "bad_argument")
  }
  error <- expect_error(f(), class = "nacho_error_bad_argument")
  expect_s3_class(error, "nacho_error")
  expect_identical(conditionMessage(error), "Broken \"thing\".")
  expect_identical(deparse(error$call), "f()")
})

test_that("nacho_warn() adds the NACHO classes", {
  expect_warning(
    NACHO:::nacho_warn("Careful.", class = "n_comp_reduced"),
    class = "nacho_warning_n_comp_reduced"
  )
  expect_warning(NACHO:::nacho_warn("Careful."), class = "nacho_warning")
})

test_that("nacho.quiet and rlib_message_verbosity silence messages and progress", {
  withr::local_options(nacho.quiet = TRUE)
  expect_no_message(NACHO:::nacho_inform("Hello."))
  expect_no_message((function() NACHO:::nacho_progress_step("Working"))())
  withr::local_options(nacho.quiet = NULL, rlib_message_verbosity = "quiet")
  expect_no_message(NACHO:::nacho_inform("Hello."))
})

test_that("nacho_inform() signals a nacho_message", {
  withr::local_options(nacho.quiet = NULL, rlib_message_verbosity = NULL)
  expect_message(NACHO:::nacho_inform("Hello."), class = "nacho_message")
})

test_that("check helpers reject bad input with a bad_argument class", {
  f <- function(value) NACHO:::check_bool(value)
  expect_error(f(NA), class = "nacho_error_bad_argument")
  expect_error(f(c(TRUE, FALSE)), class = "nacho_error_bad_argument")
  expect_no_error(f(TRUE))
  g <- function(value) NACHO:::check_count(value)
  expect_error(g(0), class = "nacho_error_bad_argument")
  expect_error(g(2.5), class = "nacho_error_bad_argument")
  expect_no_error(g(3))
  h <- function(value) NACHO:::check_string(value, allow_null = TRUE)
  expect_no_error(h(NULL))
  expect_error(h(c("a", "b")), class = "nacho_error_bad_argument")
  expect_error(
    NACHO:::check_column("nope", data.frame(a = 1)),
    class = "nacho_error_bad_argument"
  )
  k <- function(value) NACHO:::check_choice(value, c("GEO", "GLM"))
  expect_identical(k("GLM"), "GLM")
  expect_error(k("geo"), class = "nacho_error_bad_argument")
})

test_that("check_package() names the install command", {
  expect_error(
    NACHO:::check_package(
      "notapackage",
      reason = "to test",
      install = 'install.packages("notapackage")'
    ),
    class = "nacho_error_missing_package"
  )
  expect_snapshot(
    NACHO:::check_package(
      "notapackage",
      reason = "to test",
      install = 'install.packages("notapackage")'
    ),
    error = TRUE
  )
})
