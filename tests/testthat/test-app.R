upload_to_app <- function(
  data_directory,
  sample_sheet = NULL,
  rcc_type = ""
) {
  testthat::skip_if_not_installed("markdown")
  rcc_files <- list.files(
    data_directory,
    pattern = "\\.(rcc|rcc\\.gz|zip)$",
    ignore.case = TRUE,
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
    type = rcc_type,
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
  nacho <- upload_to_app("salmon_data", sample_sheet)
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

  discarded <- upload_to_app(
    single_directory,
    data.frame(file = list.files(single_directory), group = "case")
  )
  expect_s3_class(discarded, "nacho")
  expect_false("group" %in% names(discarded[["nacho"]]))
})

test_that("app discards a PlexSet sample sheet without plexset_id", {
  sample_sheet <- data.frame(
    IDFILE = basename(list.files("salmon_data", pattern = "\\.RCC$")),
    group = "case"
  )
  nacho <- upload_to_app("salmon_data", sample_sheet)
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

test_that("app loads a zip archive whatever MIME type the browser sends", {
  skip_if(!nzchar(Sys.which("zip")), "zip is not available.")
  archive_directory <- withr::local_tempdir()
  withr::with_dir(
    test_path("salmon_data"),
    utils::zip(
      file.path(archive_directory, "salmon.zip"),
      list.files(pattern = "\\.RCC$"),
      flags = "-q"
    )
  )
  for (mime_type in c("application/zip", "application/x-zip-compressed")) {
    expect_s3_class(
      upload_to_app(archive_directory, rcc_type = mime_type),
      "nacho"
    )
  }
})

test_that("app matches file extensions regardless of case", {
  lower_directory <- withr::local_tempdir()
  rcc_files <- list.files("salmon_data", pattern = "\\.RCC$")
  file.copy(
    file.path("salmon_data", rcc_files),
    file.path(lower_directory, sub("\\.RCC$", ".rcc", rcc_files))
  )
  sample_sheet <- expand.grid(
    IDFILE = sub("\\.RCC$", ".rcc", rcc_files),
    plexset_id = paste0("S", seq_len(8)),
    stringsAsFactors = FALSE
  )
  sample_sheet[["group"]] <- "case"
  nacho <- upload_to_app(lower_directory, sample_sheet)
  expect_true("group" %in% names(nacho[["nacho"]]))
})

test_that("app keeps gzipped RCC files next to a sample sheet", {
  gz_directory <- withr::local_tempdir()
  for (rcc in list.files("salmon_data", pattern = "\\.RCC$")) {
    connection <- gzfile(file.path(gz_directory, paste0(rcc, ".gz")), "w")
    writeLines(readLines(file.path("salmon_data", rcc)), connection)
    close(connection)
  }
  sample_sheet <- expand.grid(
    IDFILE = list.files(gz_directory),
    plexset_id = paste0("S", seq_len(8)),
    stringsAsFactors = FALSE
  )
  sample_sheet[["group"]] <- "case"
  nacho <- upload_to_app(gz_directory, sample_sheet)
  expect_true("group" %in% names(nacho[["nacho"]]))
})

test_that("app tells the user when it discards a sample sheet", {
  notified <- NULL
  local_mocked_bindings(
    showNotification = function(ui, ...) {
      notified <<- ui
    },
    .package = "shiny"
  )
  sample_sheet <- data.frame(file = "salmon_01_01.RCC", group = "case")
  upload_to_app("salmon_data", sample_sheet)
  expect_match(notified, "IDFILE")
})
