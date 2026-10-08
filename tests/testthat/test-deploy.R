test_that("deploy to temp dir", {
  expect_true(deploy(directory = tempdir()))
})

test_that("deploy() needs a directory", {
  expect_error(deploy(), class = "nacho_error_bad_argument")
})

test_that("the deployed app calls nacho_app() with one plot worker", {
  directory <- withr::local_tempdir()
  expect_true(deploy(directory = directory))
  path <- file.path(directory, "NACHO", "app.R")
  expect_identical(tail(readLines(path), 1), "NACHO::nacho_app()")
  withr::local_options(nacho.plot_workers = NULL)
  local_mocked_bindings(nacho_app = function(...) "app", .package = "NACHO")
  expect_identical(source(path, local = TRUE)$value, "app")
  expect_identical(getOption("nacho.plot_workers"), 1)
  withr::local_options(nacho.plot_workers = 3)
  source(path, local = TRUE)
  expect_identical(getOption("nacho.plot_workers"), 3)
})
