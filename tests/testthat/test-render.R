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
        quarto_r = Sys.getenv("QUARTO_R"),
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
  withr::local_envvar(R_LIBS = "sentinel", QUARTO_R = "sentinel-r")
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
  expect_identical(Sys.getenv("R_LIBS"), "sentinel")
  expect_identical(seen$quarto_r, R.home("bin"))
  expect_identical(Sys.getenv("QUARTO_R"), "sentinel-r")
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

test_that("render() lets Quarto talk only when nacho.quiet is off", {
  skip_if_not_installed("quarto")
  seen <- NULL
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(input, quiet, ...) {
      seen <<- quiet
      writeLines(
        "<html></html>",
        file.path(dirname(input), "nacho-report.html")
      )
    },
    .package = "quarto"
  )
  withr::local_options(nacho.quiet = FALSE)
  render(GSE74821, output_dir = withr::local_tempdir())
  expect_false(seen)
  withr::local_options(nacho.quiet = TRUE)
  render(GSE74821, output_dir = withr::local_tempdir())
  expect_true(seen)
})

test_that("render() tells when it cannot write to output_dir", {
  skip_if_not_installed("quarto")
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(input, ...) {
      writeLines(
        "<html></html>",
        file.path(dirname(input), "nacho-report.html")
      )
    },
    .package = "quarto"
  )
  not_a_folder <- withr::local_tempfile()
  writeLines("a file", not_a_folder)
  expect_error(
    render(GSE74821, output_dir = not_a_folder),
    class = "nacho_error_render_failed"
  )
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

skip_unless_real_render <- function() {
  ready <- NACHO:::quarto_available() &&
    any(file.exists(file.path(.libPaths(), "NACHO", "DESCRIPTION")))
  if (ready) {
    return(invisible())
  }
  reason <- "Quarto or an installed NACHO is missing for the real render"
  if (identical(Sys.getenv("NACHO_REQUIRE_QUARTO"), "true")) {
    stop(reason, call. = FALSE)
  }
  testthat::skip(reason)
}

test_that("render() writes an HTML report", {
  skip_on_cran()
  skip_unless_real_render()
  output_dir <- withr::local_tempdir()
  path <- render(flagged_gse(), output_dir = output_dir)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "Flagged samples: [1-9]")
  expect_match(html, "callout-warning")
})

test_that("render() writes a Typst PDF report", {
  skip_on_cran()
  skip_unless_real_render()
  output_dir <- withr::local_tempdir()
  path <- render(GSE74821, format = "typst", output_dir = output_dir)
  expect_identical(basename(path), "nacho-report.pdf")
  expect_gt(file.size(path), 10000)
})

test_that("render() creates output_dir before it renders", {
  skip_if_not_installed("quarto")
  rendered <- FALSE
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(input, ...) {
      rendered <<- TRUE
      writeLines(
        "<html></html>",
        file.path(dirname(input), "nacho-report.html")
      )
    },
    .package = "quarto"
  )
  output_dir <- file.path(withr::local_tempdir(), "a", "b")
  path <- render(GSE74821, output_dir = output_dir)
  expect_true(file.exists(path))
  not_a_folder <- withr::local_tempfile()
  writeLines("a file", not_a_folder)
  rendered <- FALSE
  expect_error(
    render(GSE74821, output_dir = not_a_folder),
    class = "nacho_error_render_failed"
  )
  expect_false(rendered)
})

test_that("render() shows the quiet hint only when output is quiet", {
  skip_if_not_installed("quarto")
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(...) invisible(),
    .package = "quarto"
  )
  withr::local_options(nacho.quiet = FALSE, rlib_message_verbosity = "default")
  loud <- expect_error(
    render(GSE74821, output_dir = withr::local_tempdir()),
    class = "nacho_error_render_failed"
  )
  expect_no_match(conditionMessage(loud), "nacho.quiet")
  withr::local_options(nacho.quiet = TRUE)
  quiet <- expect_error(
    render(GSE74821, output_dir = withr::local_tempdir()),
    class = "nacho_error_render_failed"
  )
  expect_match(conditionMessage(quiet), "rlib_message_verbosity")
})

test_that("render() does not create output_dir when the options are wrong", {
  output_dir <- file.path(withr::local_tempdir(), "report")
  expect_error(
    render(GSE74821, colour = "nope", output_dir = output_dir),
    class = "nacho_error_bad_argument"
  )
  expect_false(dir.exists(output_dir))
})

test_that("render() turns a Quarto failure into render_failed and cleans up", {
  skip_if_not_installed("quarto")
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(...) stop("boom"),
    .package = "quarto"
  )
  withr::local_options(nacho.quiet = TRUE)
  error <- expect_error(
    render(GSE74821, output_dir = withr::local_tempdir()),
    class = "nacho_error_render_failed"
  )
  expect_match(conditionMessage(error), "rlib_message_verbosity")
  expect_s3_class(error$parent, "error")
  expect_length(list.files(tempdir(), pattern = "^nacho-report-"), 0)
})

test_that("render() checks title and author", {
  expect_snapshot(render(GSE74821, title = 1), error = TRUE)
  expect_snapshot(render(GSE74821, author = c("a", "b")), error = TRUE)
})

test_that("render() passes the cover metadata to Quarto", {
  skip_if_not_installed("quarto")
  seen <- NULL
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(input, metadata, ...) {
      seen <<- metadata
      writeLines(
        "<html></html>",
        file.path(dirname(input), "nacho-report.html")
      )
    },
    .package = "quarto"
  )
  render(
    GSE74821,
    title = "Run A",
    author = "Jane Doe",
    output_dir = withr::local_tempdir()
  )
  expect_identical(seen$title, "Run A")
  expect_identical(seen$author, "Jane Doe")
  expect_match(seen$nacho$prepared, "^Prepared by Jane Doe")
})

test_that("a real render receives the cover metadata", {
  skip_on_cran()
  skip_unless_real_render()
  output_dir <- withr::local_tempdir()
  path <- render(
    GSE74821,
    title = "Run A",
    author = "Jane Doe",
    output_dir = output_dir
  )
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "<title>Run A", fixed = TRUE)
})
