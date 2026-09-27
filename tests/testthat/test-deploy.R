test_that("deploy to temp dir", {
  expect_true(deploy(directory = tempdir()))
})

test_that("deploy() needs a directory", {
  expect_error(deploy(), class = "nacho_error_bad_argument")
})
