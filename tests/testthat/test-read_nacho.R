nacho_2 <- function() {
  readRDS(testthat::test_path("fixtures", "nacho-2-GSE74821-subset.rds"))
}

upgrade_subset <- function(x) {
  testthat::expect_warning(
    x <- upgrade_nacho(x),
    class = "nacho_warning_n_comp_reduced"
  )
  x
}

test_that("upgrade_nacho() converts a NACHO 2 object and says so once", {
  withr::local_options(nacho.quiet = NULL, rlib_message_verbosity = NULL)
  expect_message(x <- upgrade_subset(nacho_2()), class = "nacho_message")
  expect_true(S7::S7_inherits(x, NACHO:::nacho))
  expect_identical(ncol(x), 6L)
  expect_identical(x@rcc_type, "n1")
  expect_identical(x@provenance$upgraded_from$nacho_version, "2")
})

test_that("upgrade_nacho() keeps the raw counts, settings and thresholds", {
  old <- nacho_2()
  x <- suppressMessages(upgrade_subset(old))
  counts <- nacho_counts(x)
  long <- as.data.frame(old$nacho)
  cell <- long[
    long$IDFILE == colnames(counts)[1] & long$Name == rownames(counts)[1],
    "Count"
  ]
  expect_identical(counts[1, 1], as.integer(cell))
  expect_identical(x@settings$normalisation_method, old$normalisation_method)
  expect_setequal(x@settings$housekeeping_genes, old$housekeeping_genes)
  expect_identical(x@thresholds, old$outliers_thresholds)
  expect_false(any(c("PC01", "Count_Norm") %in% names(x@samples)))
})

test_that("upgrade_nacho() leaves NACHO 3 objects alone and refuses anything else", {
  expect_identical(suppressMessages(upgrade_nacho(GSE74821)), GSE74821)
  expect_error(upgrade_nacho(list(a = 1)), class = "nacho_error_bad_object")
})

test_that("verbs point NACHO 2 objects to upgrade_nacho()", {
  expect_error(normalise(nacho_2()), class = "nacho_error_bad_object")
  expect_snapshot(normalise(nacho_2()), error = TRUE)
})

test_that("autoplot() points NACHO 2 objects to upgrade_nacho()", {
  expect_error(
    autoplot(nacho_2(), type = "BD"),
    class = "nacho_error_bad_object"
  )
  expect_snapshot(autoplot(nacho_2(), type = "BD"), error = TRUE)
})

test_that("check_nacho() names an incomplete NACHO 2 object", {
  broken <- structure(list(access = "IDFILE"), class = "nacho")
  expect_error(normalise(broken), class = "nacho_error_bad_object")
  expect_snapshot(normalise(broken), error = TRUE)
})

test_that("check_nacho() refuses an object from another schema", {
  x <- GSE74821
  attr(x, "provenance")$schema_version <- 99L
  expect_error(nacho_samples(x), class = "nacho_error_bad_object")
  expect_snapshot(nacho_samples(x), error = TRUE)
})
