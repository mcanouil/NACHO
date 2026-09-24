upload_to_app <- function(data_directory) {
  skip_if_not_installed("markdown")
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
