context("visualise() in non-interactive session")

test_that("visualise shoudl return an error if R session is not interactive", {
  testthat::expect_error(visualise(GSE74821))
})