upload_to_app <- function(data_directory, sample_sheet = NULL) {
  testthat::skip_if_not_installed("markdown")
  rcc_files <- list.files(
    data_directory,
    pattern = "\\.RCC$",
    full.names = TRUE
  )
  upload_directory <- tempfile("upload")
  dir.create(upload_directory)
  on.exit(unlink(upload_directory, recursive = TRUE))
  upload_paths <- file.path(upload_directory, seq_along(rcc_files))
  file.copy(rcc_files, upload_paths)
  uploaded <- data.frame(
    name = basename(rcc_files),
    size = file.size(rcc_files),
    type = "",
    datapath = upload_paths
  )
  if (!is.null(sample_sheet)) {
    sheet_path <- file.path(upload_directory, "sheet")
    utils::write.csv(sample_sheet, sheet_path, row.names = FALSE)
    uploaded <- rbind(
      uploaded,
      data.frame(
        name = "samplesheet.csv",
        size = file.size(sheet_path),
        type = "text/csv",
        datapath = sheet_path
      )
    )
  }

  nacho <- NULL
  # nolint start: object_usage_linter. testServer() provides session and the app reactives.
  shiny::testServer(
    shiny::shinyAppDir(system.file("app", package = "NACHO")),
    {
      session$setInputs(norm_method = "GEO", rcc_files = uploaded)
      nacho <<- nacho_react()
    }
  )
  # nolint end
  nacho
}

test_that("app loads uploaded RCC files without a sample sheet", {
  expect_s3_class(upload_to_app("salmon_data"), "nacho")
})

test_that("app loads uploaded PlexSet RCC files", {
  nacho <- upload_to_app("plexset_data")
  expect_s3_class(nacho, "nacho")
  expect_type(nacho[["nacho"]][["plexset_id"]], "character")
})

test_that("app merges an uploaded sample sheet", {
  sample_sheet <- expand.grid(
    IDFILE = basename(list.files("salmon_data", pattern = "\\.RCC$")),
    plexset_id = paste0("S", seq_len(8)),
    stringsAsFactors = FALSE
  )
  sample_sheet[["group"]] <- "case"
  nacho <- upload_to_app("salmon_data", sample_sheet)
  expect_true("group" %in% names(nacho[["nacho"]]))
})

test_that("app discards a sample sheet without IDFILE", {
  sample_sheet <- data.frame(file = "salmon_01_01.RCC", group = "case")
  nacho <- suppressWarnings(upload_to_app("salmon_data", sample_sheet))
  expect_s3_class(nacho, "nacho")
  expect_false("group" %in% names(nacho[["nacho"]]))
})

test_that("app merges a sample sheet by IDFILE for single-sample RCC files", {
  single_directory <- tempfile("single")
  dir.create(single_directory)
  on.exit(unlink(single_directory, recursive = TRUE))
  for (rcc in list.files(
    "salmon_data",
    pattern = "\\.RCC$",
    full.names = TRUE
  )) {
    writeLines(
      gsub("Endogenous8s", "Endogenous", readLines(rcc)),
      file.path(single_directory, basename(rcc))
    )
  }

  merged <- upload_to_app(
    single_directory,
    data.frame(IDFILE = list.files(single_directory), group = "case")
  )
  expect_true("group" %in% names(merged[["nacho"]]))

  discarded <- suppressWarnings(upload_to_app(
    single_directory,
    data.frame(file = list.files(single_directory), group = "case")
  ))
  expect_s3_class(discarded, "nacho")
  expect_false("group" %in% names(discarded[["nacho"]]))
})

test_that("app discards a PlexSet sample sheet without plexset_id", {
  sample_sheet <- data.frame(
    IDFILE = basename(list.files("salmon_data", pattern = "\\.RCC$")),
    group = "case"
  )
  nacho <- suppressWarnings(upload_to_app("salmon_data", sample_sheet))
  expect_s3_class(nacho, "nacho")
  expect_false("group" %in% names(nacho[["nacho"]]))
})

test_that("instrument presets give a full binding density range", {
  app_utils <- new.env()
  sys.source(
    system.file("app", "utils.R", package = "NACHO"),
    envir = app_utils
  )
  expect_identical(app_utils[["bd_range"]]("MAX/FLEX"), c(0.1, 2.25))
  expect_identical(app_utils[["bd_range"]]("SPRINT"), c(0.1, 1.8))
})
