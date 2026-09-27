# Upload a built NACHO tarball to CRAN from GitHub Actions.
#
# devtools::submit_cran() asks questions and refuses to run
# non-interactively, so this calls the internal devtools:::upload_cran().
# resolve_upload_cran() stops with a clear message if devtools renames or
# reshapes it; the fallback is a local devtools::submit_cran().
#
# Usage: Rscript .github/scripts/cran-upload.R <tarball> [--submit]
# Without --submit the script checks its inputs and uploads nothing.

read_package_fields <- function(pkg) {
  path <- file.path(pkg, "DESCRIPTION")
  if (!file.exists(path)) {
    stop(sprintf("No DESCRIPTION file in '%s'.", pkg), call. = FALSE)
  }
  fields <- read.dcf(path, fields = c("Package", "Version"))
  list(
    package = unname(fields[1L, "Package"]),
    version = unname(fields[1L, "Version"])
  )
}

resolve_upload_cran <- function(ns = NULL) {
  if (is.null(ns)) {
    if (!requireNamespace("devtools", quietly = TRUE)) {
      stop(
        "devtools isn't installed. ",
        "Install it, or submit locally with devtools::submit_cran().",
        call. = FALSE
      )
    }
    ns <- asNamespace("devtools")
  }
  upload <- get0("upload_cran", envir = ns, inherits = FALSE)
  expected <- c("pkg", "built_path")
  if (
    !is.function(upload) ||
      !identical(names(formals(upload))[1:2], expected)
  ) {
    stop(
      "devtools no longer provides upload_cran(pkg, built_path). ",
      "Submit locally with devtools::submit_cran(), then update ",
      ".github/scripts/cran-upload.R.",
      call. = FALSE
    )
  }
  upload
}

write_cran_submission <- function(pkg, version, sha, time) {
  record <- data.frame(
    Version = version,
    Date = format(time, tz = "UTC", usetz = TRUE),
    SHA = sha
  )
  write.dcf(record, file = file.path(pkg, "CRAN-SUBMISSION"))
}

cran_upload <- function(
  tarball,
  pkg = ".",
  dry_run = TRUE,
  sha = NULL,
  upload = NULL,
  time = Sys.time()
) {
  if (!file.exists(tarball)) {
    stop(
      sprintf("The tarball '%s' does not exist. ", tarball),
      "Build it first with pkgbuild::build().",
      call. = FALSE
    )
  }
  fields <- read_package_fields(pkg)
  expected <- sprintf("%s_%s.tar.gz", fields[["package"]], fields[["version"]])
  if (!identical(basename(tarball), expected)) {
    stop(
      sprintf(
        "Expected the tarball '%s' for this DESCRIPTION, got '%s'.",
        expected,
        basename(tarball)
      ),
      call. = FALSE
    )
  }
  if (dry_run) {
    message(sprintf("Dry run: %s is ready; nothing was uploaded.", expected))
    return(invisible(FALSE))
  }
  if (is.null(sha) || !nzchar(sha)) {
    stop(
      "The submitted commit SHA is missing. ",
      "Set SUBMITTED_SHA or run inside a git checkout.",
      call. = FALSE
    )
  }
  if (is.null(upload)) {
    upload <- resolve_upload_cran()
  }
  upload(pkg, tarball)
  write_cran_submission(pkg, fields[["version"]], sha, time)
  message(
    sprintf("Uploaded %s. Confirm the submission from CRAN's email.", expected)
  )
  invisible(TRUE)
}

main <- function(args) {
  if (length(args) < 1L) {
    stop(
      "Usage: Rscript .github/scripts/cran-upload.R <tarball> [--submit]",
      call. = FALSE
    )
  }
  sha <- Sys.getenv("SUBMITTED_SHA")
  if (!nzchar(sha)) {
    sha <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
  }
  cran_upload(
    tarball = args[[1L]],
    dry_run = !("--submit" %in% args),
    sha = sha
  )
}

if (sys.nframe() == 0L) {
  main(commandArgs(trailingOnly = TRUE))
}
