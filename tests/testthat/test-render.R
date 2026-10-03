test_that("render() checks its object and options", {
  expect_error(render(), class = "nacho_error_bad_object")
  expect_error(render(iris), class = "nacho_error_bad_object")
  expect_error(
    render(GSE74821, format = "docx"),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    render(GSE74821, colour = "nope"),
    class = "nacho_error_bad_argument"
  )
})

test_that("render() needs the quarto package", {
  local_mocked_bindings(has_package = function(package) package != "quarto")
  expect_error(render(GSE74821), class = "nacho_error_missing_package")
})

test_that("render() needs Quarto 1.9 or newer", {
  local_mocked_bindings(
    has_package = function(package) TRUE,
    quarto_cli_version = function() NULL
  )
  expect_error(render(GSE74821), class = "nacho_error_missing_quarto")
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.8.27")
  })
  expect_error(render(GSE74821), "1.8.27", class = "nacho_error_missing_quarto")
})

test_that("render() passes the library paths to Quarto and cleans up", {
  skip_if_not_installed("quarto")
  seen <- NULL
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(input, output_format, execute_params, ...) {
      seen <<- list(
        r_libs = Sys.getenv("R_LIBS"),
        files = list.files(dirname(input), recursive = TRUE),
        params = execute_params
      )
      writeLines(
        "<html></html>",
        file.path(dirname(input), "nacho-report.html")
      )
    },
    .package = "quarto"
  )
  before <- Sys.getenv("R_LIBS", unset = NA)
  output_dir <- withr::local_tempdir()
  path <- render(GSE74821, output_dir = output_dir)
  expect_identical(
    path,
    normalizePath(file.path(output_dir, "nacho-report.html"))
  )
  expect_true(file.exists(path))
  expect_identical(
    strsplit(seen$r_libs, .Platform$path.sep, fixed = TRUE)[[1]],
    .libPaths()
  )
  expect_identical(Sys.getenv("R_LIBS", unset = NA), before)
  expect_true(all(
    c(
      "nacho-report.qmd",
      "_brand.yml",
      "nacho_hex.png",
      "fonts/SourceSans3-Regular.ttf"
    ) %in%
      seen$files
  ))
  expect_identical(basename(seen$params$nacho_rds), "nacho.rds")
  expect_length(list.files(tempdir(), pattern = "^nacho-report-"), 0)
})

test_that("render() tells when Quarto writes nothing", {
  skip_if_not_installed("quarto")
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(...) invisible(),
    .package = "quarto"
  )
  expect_error(
    render(GSE74821, output_dir = withr::local_tempdir()),
    class = "nacho_error_render_failed"
  )
})

test_that("render() writes an HTML report", {
  skip_on_cran()
  skip_if_not(NACHO:::quarto_available())
  output_dir <- withr::local_tempdir()
  path <- render(flagged_gse(), output_dir = output_dir)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "Flagged samples: [1-9]")
  expect_match(html, "callout-warning")
})

test_that("render() writes a Typst PDF report", {
  skip_on_cran()
  skip_if_not(NACHO:::quarto_available())
  output_dir <- withr::local_tempdir()
  path <- render(GSE74821, format = "typst", output_dir = output_dir)
  expect_identical(basename(path), "nacho-report.pdf")
  expect_gt(file.size(path), 10000)
})

test_that("render() keeps a folder named tmp_nacho in output_dir", {
  skip_on_cran()
  skip_if_not(NACHO:::quarto_available())
  output_dir <- withr::local_tempdir()
  user_folder <- file.path(output_dir, "tmp_nacho")
  dir.create(user_folder)
  writeLines("keep me", file.path(user_folder, "notes.txt"))
  render(GSE74821, output_dir = output_dir)
  expect_true(file.exists(file.path(user_folder, "notes.txt")))
})
