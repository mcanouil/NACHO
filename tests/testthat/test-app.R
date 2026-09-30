expect_nacho <- function(object) {
  testthat::expect_true(S7::S7_inherits(object, NACHO:::nacho))
}

upload_to_app <- function(
  data_directory,
  sample_sheet = NULL,
  rcc_type = ""
) {
  testthat::skip_on_cran()
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

  app <- suppressPackageStartupMessages(
    shiny::shinyAppDir(system.file("app", package = "NACHO"))
  )
  nacho <- NULL
  # nolint start: object_usage_linter. testServer() provides session and the app reactives.
  withCallingHandlers(
    shiny::testServer(
      app,
      {
        session$setInputs(norm_method = "GEO", rcc_files = uploaded)
        nacho <<- nacho_react()
      }
    ),
    nacho_warning_metric_unavailable = function(cnd) {
      invokeRestart("muffleWarning")
    }
  )
  # nolint end
  nacho
}

test_that("app loads uploaded RCC files without a sample sheet", {
  expect_nacho(upload_to_app("salmon_data"))
})

test_that("app treats single-sample files that mention Endogenous8s as single-sample", {
  source_files <- list.files(
    geo_fixture("GSE178516")[["dir"]],
    pattern = "\\.RCC\\.gz$",
    full.names = TRUE
  )
  directory <- withr::local_tempdir()
  for (source_file in source_files) {
    lines <- readLines(source_file)
    endogenous <- grep("^Endogenous,", lines)[1]
    lines[endogenous] <- sub(
      "^Endogenous,[^,]*,",
      "Endogenous,Endogenous8s_like,",
      lines[endogenous]
    )
    writeLines(
      lines,
      file.path(directory, sub("\\.gz$", "", basename(source_file)))
    )
  }
  expect_warning(
    nacho <- upload_to_app(directory),
    class = "nacho_warning_n_comp_reduced"
  )
  expect_identical(nacho@rcc_type, "n1")
})

test_that("app loads uploaded PlexSet RCC files", {
  expect_warning(
    nacho <- upload_to_app("plexset_data"),
    class = "nacho_warning_no_housekeeping"
  )
  expect_nacho(nacho)
  expect_false(nacho@settings[["housekeeping_norm"]])
  expect_type(nacho_samples(nacho)[["plexset_id"]], "character")
})

test_that("app merges an uploaded sample sheet", {
  sample_sheet <- expand.grid(
    IDFILE = basename(list.files("salmon_data", pattern = "\\.RCC$")),
    plexset_id = paste0("S", seq_len(8)),
    stringsAsFactors = FALSE
  )
  sample_sheet[["group"]] <- "case"
  nacho <- upload_to_app("salmon_data", sample_sheet)
  expect_true("group" %in% names(nacho_samples(nacho)))
})

test_that("app discards a sample sheet without IDFILE", {
  sample_sheet <- data.frame(file = "salmon_01_01.RCC", group = "case")
  nacho <- upload_to_app("salmon_data", sample_sheet)
  expect_nacho(nacho)
  expect_false("group" %in% names(nacho_samples(nacho)))
})

test_that("app merges a sample sheet by IDFILE for single-sample RCC files", {
  single_directory <- geo_fixture("GSE178516")[["dir"]]
  rcc_files <- list.files(single_directory, pattern = "\\.RCC\\.gz$")

  expect_warning(
    merged <- upload_to_app(
      single_directory,
      data.frame(IDFILE = rcc_files, group = "case")
    ),
    class = "nacho_warning_n_comp_reduced"
  )
  expect_true("group" %in% names(nacho_samples(merged)))

  expect_warning(
    discarded <- upload_to_app(
      single_directory,
      data.frame(file = rcc_files, group = "case")
    ),
    class = "nacho_warning_n_comp_reduced"
  )
  expect_nacho(discarded)
  expect_false("group" %in% names(nacho_samples(discarded)))
})

test_that("app discards a PlexSet sample sheet without plexset_id", {
  sample_sheet <- data.frame(
    IDFILE = basename(list.files("salmon_data", pattern = "\\.RCC$")),
    group = "case"
  )
  nacho <- upload_to_app("salmon_data", sample_sheet)
  expect_nacho(nacho)
  expect_false("group" %in% names(nacho_samples(nacho)))
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
    expect_nacho(upload_to_app(archive_directory, rcc_type = mime_type))
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
  expect_true("group" %in% names(nacho_samples(nacho)))
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
  expect_true("group" %in% names(nacho_samples(nacho)))
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

test_that("app help pages render without writing under R CMD check", {
  skip_if_not_installed("markdown")
  app_directory <- withr::local_tempdir()
  expect_true(file.copy(
    system.file("app", package = "NACHO"),
    app_directory,
    recursive = TRUE
  ))
  app_directory <- file.path(app_directory, "app")
  markdown_files <- list.files(
    file.path(app_directory, "www"),
    pattern = "\\.md$",
    full.names = TRUE
  )
  expect_gt(length(markdown_files), 0)
  checksums <- tools::md5sum(markdown_files)
  app_utils <- new.env()
  sys.source(file.path(app_directory, "utils.R"), envir = app_utils)
  withr::local_envvar(`_R_CHECK_PACKAGE_NAME_` = "NACHO")
  withr::local_dir(app_directory)
  for (about in c("nacho", app_utils[["about_pages"]])) {
    expect_s3_class(app_utils[["include_about"]](about), "html")
  }
  expect_identical(tools::md5sum(markdown_files), checksums)
})

test_that("app help links match the help page lookup", {
  skip_if_not_installed("markdown")
  app_directory <- system.file("app", package = "NACHO")
  app_utils <- new.env()
  sys.source(file.path(app_directory, "utils.R"), envir = app_utils)
  request <- new.env()
  request[["REQUEST_METHOD"]] <- "GET"
  request[["PATH_INFO"]] <- "/"
  request[["QUERY_STRING"]] <- ""
  request[["HTTP_HOST"]] <- "localhost"
  response <- shiny::shinyAppDir(app_directory)[["httpHandler"]](request)
  page <- response[["content"]]
  if (is.raw(page)) {
    page <- rawToChar(page)
  }
  link_ids <- regmatches(page, gregexpr("id=\"about_[a-z]+\"", page))[[1]]
  expect_setequal(
    gsub("^id=\"about_|\"$", "", link_ids),
    unname(app_utils[["about_pages"]])
  )
})
