test_that('call nacho_conflicts', {
  expect_s3_class(nacho_conflicts(), "nacho_conflicts")
})

test_that('print nacho_conflicts', {
  expect_null(print(nacho_conflicts()))
})

test_that('call nacho_conflicts with conflicts', {
  expect_null({
    library(dplyr)
    print(nacho_conflicts())
  })
})