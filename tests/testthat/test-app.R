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
