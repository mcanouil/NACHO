#' Render the quality-control report of a nacho object
#'
#' Writes a report with Quarto: the quality-control summary first, one callout
#' for each sample that fails a threshold, then each plot of [autoplot()] with
#' a short explanation.
#'
#' The report needs the Quarto command-line interface 1.9.18 or newer, and
#' the quarto, knitr and rmarkdown packages.
#' RStudio and Positron bundle Quarto; elsewhere, install it from
#' <https://quarto.org/docs/get-started/>.
#' The PDF goes through Typst, which Quarto bundles, so no LaTeX is needed.
#'
#' @param x A `nacho` object from [load_rcc()] or [normalise()].
#' @param format `"html"` (the default) for a self-contained HTML file, or
#'   `"typst"` for a PDF.
#' @param output_dir The directory to write `nacho-report.html` or
#'   `nacho-report.pdf` to. It is created when it does not exist.
#' @param colour The column of `nacho_samples(x)` that colours the samples.
#' @param group The column of `nacho_samples(x)` with the biological groups, or
#'   `NULL`. With a group, the report checks whether batches and groups are
#'   confounded; see [batch_diagnostics()].
#' @param size The point size.
#' @param show_legend If `FALSE`, hide the colour legends.
#' @param outliers_factor The size of flagged samples, relative to `size`.
#' @param outliers_labels The column of `nacho_samples(x)` that labels the
#'   flagged samples, or `NULL` for no labels.
#' @param title The report title.
#'   `NULL` or empty uses "NanoString quality-control report".
#'   A missing value (`NA`) is an error.
#' @param author Who prepared the report, as one string, for example
#'   `"Jane Doe, Genomics Core"`.
#'   `NULL` or empty leaves it out.
#'   A missing value (`NA`) is an error.
#'
#' @return The path of the report, invisibly.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   data(GSE74821)
#'   render(GSE74821, output_dir = tempdir())
#'   render(GSE74821, format = "typst", output_dir = tempdir())
#' }
render <- function(
  x,
  format = c("html", "typst"),
  output_dir = ".",
  colour = "CartridgeID",
  group = NULL,
  size = 1,
  show_legend = TRUE,
  outliers_factor = 1,
  outliers_labels = NULL,
  title = NULL,
  author = NULL
) {
  check_nacho(x)
  format <- check_choice(format, c("html", "typst"))
  check_string(output_dir)
  check_cover_text(title)
  check_cover_text(author)
  options <- check_report_options(
    x,
    colour = colour,
    group = group,
    size = size,
    show_legend = show_legend,
    outliers_factor = outliers_factor,
    outliers_labels = outliers_labels
  )
  check_quarto()

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  if (!dir.exists(output_dir)) {
    nacho_abort(
      c(
        "Could not create {.path {output_dir}} for the report.",
        i = "Check that {.arg output_dir} is a folder you can write to."
      ),
      class = "render_failed"
    )
  }

  work_dir <- tempfile("nacho-report-")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  staged <- file.copy(
    c(
      list.files(dirname(report_template_path()), full.names = TRUE),
      list.files(brand_path(), full.names = TRUE)
    ),
    work_dir,
    recursive = TRUE
  )
  if (!all(staged)) {
    nacho_abort(
      "Could not copy the report files to {.path {work_dir}}.",
      class = "render_failed"
    )
  }
  rds <- file.path(work_dir, "nacho.rds")
  saveRDS(list(object = x, options = options), rds)

  rlang::try_fetch(
    with_library_paths(quarto::quarto_render(
      input = file.path(work_dir, "nacho-report.qmd"),
      output_format = format,
      execute_params = list(nacho_rds = rds),
      metadata = report_metadata_literal(
        report_metadata(x, title = title, author = author)
      ),
      quiet = nacho_is_quiet()
    )),
    error = function(cnd) {
      nacho_abort(
        c(
          "Quarto could not render the report.",
          if (nacho_is_quiet()) {
            c(
              i = paste(
                "Quarto's messages are hidden;",
                "{.code options(nacho.quiet = FALSE, rlib_message_verbosity = \"default\")}",
                "shows them."
              )
            )
          }
        ),
        class = "render_failed",
        parent = cnd
      )
    }
  )

  output <- file.path(
    work_dir,
    paste0("nacho-report.", c(html = "html", typst = "pdf")[[format]])
  )
  if (!file.exists(output)) {
    nacho_abort(
      c(
        "Quarto finished without writing {.file {basename(output)}}.",
        if (nacho_is_quiet()) {
          c(
            i = paste(
              "Quarto's messages are hidden;",
              "{.code options(nacho.quiet = FALSE, rlib_message_verbosity = \"default\")}",
              "shows them."
            )
          )
        }
      ),
      class = "render_failed"
    )
  }
  target <- file.path(output_dir, basename(output))
  if (!suppressWarnings(file.copy(output, target, overwrite = TRUE))) {
    nacho_abort(
      c(
        "Could not write {.file {basename(output)}} to {.path {output_dir}}.",
        i = "Check that {.arg output_dir} is a folder you can write to."
      ),
      class = "render_failed"
    )
  }
  invisible(normalizePath(target))
}

#' Path of a file of the report in the installed package
#'
#' @param file The file name in `inst/report`.
#'
#' @noRd
report_template_path <- function(file = "nacho-report.qmd") {
  path <- system.file("report", file, package = "NACHO")
  if (!nzchar(path)) {
    nacho_abort(
      "The report file {.file {file.path('report', file)}} is missing from the installed package.",
      class = "missing_file"
    )
  }
  path
}

#' Quarto CLI version, or NULL when Quarto is not found
#'
#' @noRd
quarto_cli_version <- function() {
  if (is.null(quarto::quarto_path())) {
    return(NULL)
  }
  quarto::quarto_version()
}

#' Tell whether the report can render
#'
#' @noRd
quarto_available <- function() {
  all(vapply(c("quarto", "knitr", "rmarkdown"), has_package, logical(1))) &&
    isTRUE(quarto_cli_version() >= "1.9.18")
}

#' Check that the report can render
#'
#' @noRd
check_quarto <- function(call = rlang::caller_env()) {
  for (package in c("quarto", "knitr", "rmarkdown")) {
    check_package(package, reason = "to render the report", call = call)
  }
  version <- quarto_cli_version()
  if (is.null(version) || version < "1.9.18") {
    nacho_abort(
      c(
        if (is.null(version)) {
          "The Quarto command-line interface is needed to render the report."
        } else {
          "Quarto {version} is too old to render the report; it needs 1.9.18 or newer."
        },
        i = "Install Quarto from {.url https://quarto.org/docs/get-started/}.",
        i = "RStudio and Positron bundle Quarto."
      ),
      class = "missing_quarto",
      call = call
    )
  }
  invisible(TRUE)
}

#' Evaluate code with R_LIBS and QUARTO_R pointing at the current R
#'
#' The quarto package (1.5.1) does not pass the library paths to the Quarto
#' process, so a package in a user or renv library would not load there.
#' QUARTO_R makes Quarto run the R that is running now.
#'
#' @noRd
with_library_paths <- function(code) {
  old <- Sys.getenv(c("R_LIBS", "QUARTO_R"), unset = NA)
  Sys.setenv(
    R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep),
    QUARTO_R = R.home("bin")
  )
  on.exit(
    for (name in names(old)) {
      if (is.na(old[[name]])) {
        Sys.unsetenv(name)
      } else {
        do.call(Sys.setenv, stats::setNames(list(old[[name]]), name))
      }
    },
    add = TRUE
  )
  code
}
