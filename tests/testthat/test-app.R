test_that("app loads uploaded RCC files without a sample sheet", {
  rcc_files <- list.files("salmon_data", pattern = "\\.RCC$", full.names = TRUE)
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

  shiny::testServer(
    shiny::shinyAppDir(system.file("app", package = "NACHO")),
    {
      session$setInputs(norm_method = "GEO", rcc_files = uploaded)
      expect_s3_class(nacho_react(), "nacho")
    }
  )
})
