test_that('deploy to temp dir', {
  expect_true(deploy(directory = tempdir()))
})