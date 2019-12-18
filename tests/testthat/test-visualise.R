test_that("visualise should return an error object if R session is not interactive", {
  expect_error(visualise(GSE74821))
})

test_that("not nacho", {
  expect_error(visualise(iris))
})

test_that("nacho missing", {
  expect_error(visualise())
})

test_that("access field missing", {
  GSE74821$access <- NULL
  expect_error(visualise(GSE74821))
})

test_that("housekeeping_genes field missing", {
  GSE74821$housekeeping_genes <- NULL
  expect_error(visualise(GSE74821))
})

test_that("housekeeping_predict field missing", {
  GSE74821$housekeeping_predict <- NULL
  expect_error(visualise(GSE74821))
})

test_that("housekeeping_norm field missing", {
  GSE74821$housekeeping_norm <- NULL
  expect_error(visualise(GSE74821))
})

test_that("normalisation_method field missing", {
  GSE74821$normalisation_method <- NULL
  expect_error(visualise(GSE74821))
})

test_that("remove_outliers field missing", {
  GSE74821$remove_outliers <- NULL
  expect_error(visualise(GSE74821))
})

test_that("n_comp field missing", {
  GSE74821$n_comp <- NULL
  expect_error(visualise(GSE74821))
})

test_that("data_directory field missing", {
  GSE74821$data_directory <- NULL
  expect_error(visualise(GSE74821))
})

test_that("pc_sum field missing", {
  GSE74821$pc_sum <- NULL
  expect_error(visualise(GSE74821))
})

test_that("nacho field missing", {
  GSE74821$nacho <- NULL
  expect_error(visualise(GSE74821))
})

test_that("outliers_thresholds field missing", {
  GSE74821$outliers_thresholds <- NULL
  expect_error(visualise(GSE74821))
})

test_that("raw_counts field missing", {
  GSE74821$raw_counts <- NULL
  expect_error(visualise(GSE74821))
})

test_that("normalised_counts field missing", {
  GSE74821$normalised_counts <- NULL
  expect_error(visualise(GSE74821))
})
