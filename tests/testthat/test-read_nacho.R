qc_status_of <- function(x) {
  NACHO:::qc_table(
    x@samples,
    x@thresholds,
    x@rcc_type,
    x@settings$id_colname
  )$status
}

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
  expect_identical(
    x@thresholds[names(old$outliers_thresholds)],
    old$outliers_thresholds
  )
  expect_false(any(c("PC01", "Count_Norm", "is_outlier") %in% names(x@samples)))
})

test_that("upgrade_nacho() reads NACHO 2 thresholds under the legacy preset", {
  old <- nacho_2()
  x <- suppressMessages(upgrade_subset(old))
  expect_identical(x@thresholds$preset, "legacy")
  expect_identical(x@thresholds$BD, old$outliers_thresholds$BD)
  expect_identical(
    x@thresholds$instrument,
    NACHO:::detect_instrument(x@samples)
  )
})

test_that("upgrade_nacho() keeps an open House_factor upper bound", {
  old <- nacho_2()
  old$outliers_thresholds$House_factor <- c(1 / 11, Inf)
  x <- suppressMessages(upgrade_subset(old))
  expect_identical(x@thresholds$House_factor, c(1 / 11, Inf))
})

test_that("upgrade_nacho() refuses thresholds without names", {
  old <- nacho_2()
  old$outliers_thresholds <- list(c(0.1, 2.25), 95)
  expect_error(upgrade_nacho(old), class = "nacho_error_bad_argument")
  old$outliers_thresholds <- list(c(9, 9), FoV = 50)
  expect_error(upgrade_nacho(old), class = "nacho_error_bad_argument")
  old$outliers_thresholds <- list(Fov = 50)
  expect_error(upgrade_nacho(old), class = "nacho_error_bad_argument")
})

test_that("upgrade_nacho() and read_nacho() name an infinite LoD", {
  old <- nacho_2()
  old$outliers_thresholds$LoD <- Inf
  error <- expect_error(upgrade_nacho(old), class = "nacho_error_bad_argument")
  expect_match(conditionMessage(error), "LoD", fixed = TRUE)
  expect_match(
    conditionMessage(error),
    "set a finite LoD, or -Inf",
    fixed = TRUE
  )
  x <- GSE74821
  thresholds <- x@thresholds
  thresholds$LoD <- Inf
  attr(x, "thresholds") <- thresholds
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(x, path)
  error <- expect_error(read_nacho(path), class = "nacho_error_bad_object")
  expect_match(
    conditionMessage(error),
    "set a finite LoD, or -Inf",
    fixed = TRUE
  )
  expect_match(conditionMessage(error), basename(path), fixed = TRUE)
  expect_match(conditionMessage(error), "saveRDS", fixed = TRUE)
})

test_that("upgrade_nacho() leaves NACHO 3 objects alone and refuses anything else", {
  expect_identical(suppressMessages(upgrade_nacho(GSE74821)), GSE74821)
  expect_error(upgrade_nacho(list(a = 1)), class = "nacho_error_bad_object")
})

test_that("upgrade_nacho() names a missing RCC type or ID column", {
  no_type <- nacho_2()
  attr(no_type, "RCC_type") <- NULL
  expect_error(
    upgrade_nacho(no_type),
    regexp = "RCC_type",
    class = "nacho_error_bad_object"
  )
  no_access <- nacho_2()
  no_access$access <- NULL
  expect_error(
    upgrade_nacho(no_access),
    regexp = "access",
    class = "nacho_error_bad_object"
  )
})

test_that("upgrade_nacho() names a missing NACHO 2 table or column", {
  bad_access <- nacho_2()
  bad_access$access <- "NOT_A_COLUMN"
  expect_error(
    upgrade_nacho(bad_access),
    regexp = "NOT_A_COLUMN",
    class = "nacho_error_bad_object"
  )
  no_name <- nacho_2()
  no_name$nacho$Name <- NULL
  expect_error(
    upgrade_nacho(no_name),
    regexp = "Name",
    class = "nacho_error_bad_object"
  )
  no_table <- nacho_2()
  no_table$nacho <- list(1)
  expect_error(upgrade_nacho(no_table), class = "nacho_error_bad_object")
})

test_that("upgrade_nacho() checks the NACHO 2 housekeeping settings", {
  for (field in c("housekeeping_norm", "housekeeping_predict")) {
    bad <- nacho_2()
    bad[[field]] <- NA
    expect_error(
      upgrade_nacho(bad),
      regexp = paste0("x\\$", field),
      class = "nacho_error_bad_argument"
    )
  }
  bad_genes <- nacho_2()
  bad_genes$housekeeping_genes <- 1:3
  expect_error(
    upgrade_nacho(bad_genes),
    regexp = "x\\$housekeeping_genes",
    class = "nacho_error_bad_argument"
  )
})

test_that("read_nacho() names a file it cannot read as RDS", {
  path <- withr::local_tempfile(fileext = ".rds")
  writeLines("not an RDS file", path)
  expect_error(read_nacho(path), class = "nacho_error_bad_object")
  expect_error(read_nacho(tempdir()), class = "nacho_error_bad_object")
})

test_that("upgrade_nacho() checks the NACHO 2 settings it keeps", {
  bad_method <- nacho_2()
  bad_method$normalisation_method <- "geo"
  expect_error(
    upgrade_nacho(bad_method),
    regexp = "x\\$normalisation_method",
    class = "nacho_error_bad_argument"
  )
  bad_n_comp <- nacho_2()
  bad_n_comp$n_comp <- 0
  expect_error(
    upgrade_nacho(bad_n_comp),
    regexp = "x\\$n_comp",
    class = "nacho_error_bad_argument"
  )
  bad_type <- nacho_2()
  attr(bad_type, "RCC_type") <- "n4"
  expect_error(
    upgrade_nacho(bad_type),
    regexp = "RCC_type",
    class = "nacho_error_bad_argument"
  )
})

test_that("upgrade_nacho() keeps every PlexSet sample and its counts", {
  old <- readRDS(test_path("fixtures", "nacho-2-plexset.rds"))
  x <- suppressMessages(upgrade_nacho(old))
  expect_identical(x@rcc_type, "n8")
  expect_identical(ncol(x), 16L)
  counts <- nacho_counts(x)
  long <- as.data.frame(old$nacho)
  sample <- colnames(counts)[9]
  probe <- rownames(counts)[1]
  cell <- long[long$IDFILE == sample & long$Name == probe, "Count"]
  expect_identical(counts[probe, sample], as.integer(cell))
})

test_that("upgrade_nacho() checks the schema of a NACHO 3 object", {
  x <- GSE74821
  attr(x, "provenance")$schema_version <- 99L
  expect_error(upgrade_nacho(x), class = "nacho_error_bad_object")
})

test_that("upgrade_nacho() refuses a missing argument", {
  expect_error(upgrade_nacho(), class = "nacho_error_bad_object")
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

test_that("the S3 autoplot() method never returns an object", {
  expect_error(
    NACHO:::autoplot.nacho(GSE74821),
    class = "nacho_error_internal"
  )
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

test_that("read_nacho() migrates the frozen schema-1 object", {
  withr::local_options(nacho.quiet = NULL, rlib_message_verbosity = NULL)
  path <- test_path("fixtures", "nacho-schema-1.rds")
  expect_message(
    expect_warning(
      x <- read_nacho(path),
      class = "nacho_warning_n_comp_reduced"
    ),
    "no negative probes"
  )
  expect_true(S7::S7_inherits(x, NACHO:::nacho))
  expect_identical(dim(x), c(40L, 4L))
  expect_identical(nacho_counts(x), readRDS(path)@counts)
  expect_identical(x@provenance$schema_version, 2L)
  expect_identical(x@provenance$migrated_from_schema, 1L)
  expect_identical(x@settings$background, "none")
  expect_identical(x@settings$background_mode, "subtract")
  expect_identical(x@thresholds$preset, "legacy")
  expect_identical(S7::S7_class(x), NACHO:::nacho)
})

test_that("migrating a schema 1 object with negatives subtracts the geometric mean", {
  properties <- S7::props(readRDS(test_path("fixtures", "nacho-schema-1.rds")))
  negatives <- data.frame(
    CodeClass = "Negative",
    Name = c("NEG_A", "NEG_B"),
    Accession = "",
    is_housekeeping = FALSE,
    is_excluded = FALSE
  )
  negative_counts <- matrix(
    c(4L, 6L, 8L, 10L, 5L, 7L, 9L, 11L),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(negatives$Name, colnames(properties$counts))
  )
  properties$probes <- rbind(properties$probes, negatives)
  properties$counts <- rbind(properties$counts, negative_counts)
  withr::local_options(nacho.quiet = NULL, rlib_message_verbosity = NULL)
  expect_message(
    x <- suppressWarnings(NACHO:::migrate_schema_1(properties)),
    "geometric mean background subtraction"
  )
  expect_identical(x@settings$background, "geo")
  expect_identical(x@settings$background_mode, "subtract")
  expect_equal(
    nacho_samples(x)$Background,
    unname(exp(colMeans(log(negative_counts))))
  )
  expect_identical(x@provenance$migrated_from_schema, 1L)
  without <- properties
  without$probes <- without$probes[without$probes$CodeClass != "Negative", ]
  without$counts <- without$counts[without$probes$Name, ]
  plain <- suppressWarnings(suppressMessages(NACHO:::migrate_schema_1(without)))
  expect_identical(
    nacho_samples(plain)$Background,
    rep(NA_real_, nrow(nacho_samples(plain)))
  )
})

test_that("upgrade_nacho() keeps the NACHO 2 background", {
  old <- readRDS(test_path("fixtures", "nacho-2-GSE74821-subset.rds"))
  x <- suppressWarnings(suppressMessages(upgrade_nacho(old)))
  expect_identical(x@settings$background, "geo")
  expect_identical(x@settings$background_mode, "subtract")
})

test_that("upgrade_nacho() uses no background without negative probes", {
  old <- nacho_2()
  old$nacho <- old$nacho[old$nacho$CodeClass != "Negative", ]
  x <- suppressWarnings(suppressMessages(upgrade_nacho(old)))
  expect_identical(x@settings$background, "none")
  expect_identical(x@settings$background_mode, "subtract")
})

test_that("read_nacho() upgrades a saved NACHO 2 object", {
  expect_warning(
    x <- suppressMessages(read_nacho(test_path(
      "fixtures",
      "nacho-2-GSE74821-subset.rds"
    ))),
    class = "nacho_warning_n_comp_reduced"
  )
  expect_true(S7::S7_inherits(x, NACHO:::nacho))
})

test_that("read_nacho() refuses files without a NACHO object", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(a = 1), path)
  expect_error(read_nacho(path), class = "nacho_error_bad_object")
  expect_error(
    read_nacho(file.path(tempdir(), "none.rds")),
    class = "nacho_error_missing_file"
  )
})

save_with_schema <- function(schema, env = parent.frame()) {
  x <- readRDS(testthat::test_path("fixtures", "nacho-schema-1.rds"))
  attr(x, "provenance")$schema_version <- schema
  path <- withr::local_tempfile(fileext = ".rds", .local_envir = env)
  saveRDS(x, path)
  path
}

test_that("read_nacho() asks for a newer NACHO for a newer schema", {
  path <- save_with_schema(99L)
  expect_error(
    read_nacho(path),
    regexp = "Schema 99 is newer.*Update NACHO",
    class = "nacho_error_bad_object"
  )
})

test_that("read_nacho() does not ask for an update for an older schema", {
  path <- save_with_schema(0L)
  err <- expect_error(read_nacho(path), class = "nacho_error_bad_object")
  expect_no_match(conditionMessage(err), "Update NACHO")
  expect_match(conditionMessage(err), "schema 0")
})

test_that("a missing or malformed schema version is named as such", {
  for (schema in list(1, NULL)) {
    path <- save_with_schema(schema)
    err <- expect_error(read_nacho(path), class = "nacho_error_bad_object")
    expect_match(conditionMessage(err), "missing or malformed")
    expect_no_match(conditionMessage(err), "Update NACHO")
  }
  x <- GSE74821
  attr(x, "provenance")$schema_version <- 1
  err <- expect_error(nacho_samples(x), class = "nacho_error_bad_object")
  expect_match(conditionMessage(err), "missing or malformed")
})

test_that("upgrade_nacho() keeps the NACHO 2 thresholds under the legacy preset", {
  old <- readRDS(test_path("fixtures", "nacho-2-GSE74821-subset.rds"))
  x <- suppressWarnings(
    suppressMessages(upgrade_nacho(old)),
    classes = "nacho_warning_n_comp_reduced"
  )
  expect_identical(x@thresholds$preset, "legacy")
  expect_identical(x@thresholds$BD, old$outliers_thresholds$BD)
})

test_that("migrating a schema 1 object with unknown thresholds gives a classed error", {
  properties <- S7::props(readRDS(test_path("fixtures", "nacho-schema-1.rds")))
  properties$thresholds[["not_a_threshold"]] <- c(0, 1)
  expect_error(
    suppressMessages(NACHO:::migrate_schema_1(properties)),
    class = "nacho_error_bad_argument"
  )
})

test_that("legacy_thresholds() gives Housekeeping_detected 0 to an old list", {
  old <- nacho_2()$outliers_thresholds
  expect_false("Housekeeping_detected" %in% names(old))
  samples <- data.frame(Header.header_FileVersion = "1.7")
  filled <- NACHO:::legacy_thresholds(old, samples)
  expect_identical(filled$Housekeeping_detected, 0)
  expect_identical(filled$preset, "legacy")
})

test_that("upgrade_nacho() carries Housekeeping_detected into the thresholds", {
  x <- suppressMessages(upgrade_subset(nacho_2()))
  expect_identical(x@thresholds$Housekeeping_detected, 0)
})

test_that("read_nacho() reads the frozen schema-2 object as is", {
  path <- test_path("fixtures", "nacho-schema-2.rds")
  saved <- readRDS(path)
  expect_no_message(x <- read_nacho(path))
  expect_identical(x@provenance$schema_version, 2L)
  expect_null(x@provenance$migrated_from_schema)
  expect_identical(dim(x), c(40L, 4L))
  expect_identical(nacho_counts(x), saved@counts)
  expect_identical(nacho_counts(x, normalised = TRUE), saved@normalised)
  expect_identical(nacho_qc(x)$status, qc_status_of(saved))
  expect_identical(S7::S7_class(x), NACHO:::nacho)
})

expect_round_trip <- function(x) {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(x, path)
  testthat::expect_no_message(y <- read_nacho(path))
  testthat::expect_identical(y@settings, x@settings)
  testthat::expect_identical(
    y@provenance$content_probes,
    x@provenance$content_probes
  )
  testthat::expect_identical(
    grep("^W_", names(y@samples), value = TRUE),
    grep("^W_", names(x@samples), value = TRUE)
  )
  testthat::expect_identical(y@samples, x@samples)
  testthat::expect_identical(
    nacho_counts(y, normalised = TRUE),
    nacho_counts(x, normalised = TRUE)
  )
  testthat::expect_identical(nacho_qc(y)$status, nacho_qc(x)$status)
  y
}

test_that("a RUVg object survives a save and read round trip", {
  x <- normalise(GSE74821, normalisation_method = "RUVg", ruv_k = 1)
  expect_true(any(grepl("^W_", names(x@samples))))
  y <- expect_round_trip(x)
  expect_identical(y@settings$ruv_k, 1L)
})

test_that("a miRNA object survives a save and read round trip", {
  x <- mirna_fixture(normalisation_method = "stable_mirna")
  expect_identical(x@settings$panel, "mirna")
  expect_false(is.null(x@provenance$content_probes))
  expect_round_trip(x)
})
