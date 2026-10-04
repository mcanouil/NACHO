test_that("deploy to temp dir", {
  expect_true(deploy(directory = tempdir()))
})

test_that("deploy() needs a directory", {
  expect_error(deploy(), class = "nacho_error_bad_argument")
})

test_that("the deployed app calls nacho_app()", {
  directory <- withr::local_tempdir()
  expect_true(deploy(directory = directory))
  expect_identical(
    readLines(file.path(directory, "NACHO", "app.R")),
    "NACHO::nacho_app()"
  )
})
