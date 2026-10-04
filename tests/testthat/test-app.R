test_that("instrument presets give a full binding density range", {
  app_utils <- new.env()
  sys.source(
    system.file("app", "utils.R", package = "NACHO"),
    envir = app_utils
  )
  expect_identical(app_utils[["bd_range"]]("MAX/FLEX"), c(0.1, 2.25))
  expect_identical(app_utils[["bd_range"]]("SPRINT"), c(0.1, 1.8))
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
