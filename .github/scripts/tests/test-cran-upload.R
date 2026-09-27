source(file.path(".github", "scripts", "cran-upload.R"))

expect_error_message <- function(expr, pattern) {
  message <- tryCatch(
    {
      force(expr)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )
  if (is.na(message) || !grepl(pattern, message, fixed = TRUE)) {
    stop(
      sprintf("Expected an error with '%s', got: %s", pattern, message),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

make_pkg <- function(version = "2.0.7") {
  dir <- tempfile("nacho-")
  dir.create(dir)
  writeLines(
    c("Package: NACHO", paste("Version:", version)),
    file.path(dir, "DESCRIPTION")
  )
  tarball <- file.path(dir, sprintf("NACHO_%s.tar.gz", version))
  file.create(tarball)
  list(dir = dir, tarball = tarball)
}

record_path <- function(pkg) file.path(pkg[["dir"]], "CRAN-SUBMISSION")

never_upload <- function(pkg, built_path) {
  stop("upload must not run in a dry run", call. = FALSE)
}

pkg <- make_pkg()
result <- cran_upload(
  pkg[["tarball"]],
  pkg = pkg[["dir"]],
  dry_run = TRUE,
  sha = "abc",
  upload = never_upload
)
stopifnot(identical(result, FALSE), !file.exists(record_path(pkg)))

calls <- new.env()
calls[["log"]] <- list()
fake_upload <- function(pkg, built_path) {
  calls[["log"]] <- c(
    calls[["log"]],
    list(list(pkg = pkg, built_path = built_path))
  )
  invisible(TRUE)
}
pkg <- make_pkg()
result <- cran_upload(
  pkg[["tarball"]],
  pkg = pkg[["dir"]],
  dry_run = FALSE,
  sha = "0123abc",
  upload = fake_upload,
  time = as.POSIXct("2026-10-01 10:00:00", tz = "UTC")
)
record <- read.dcf(record_path(pkg))
stopifnot(
  identical(result, TRUE),
  length(calls[["log"]]) == 1L,
  identical(calls[["log"]][[1L]][["built_path"]], pkg[["tarball"]]),
  identical(unname(record[1L, "Version"]), "2.0.7"),
  identical(unname(record[1L, "SHA"]), "0123abc"),
  identical(unname(record[1L, "Date"]), "2026-10-01 10:00:00 UTC")
)

pkg <- make_pkg()
wrong <- file.path(pkg[["dir"]], "NACHO_2.0.6.tar.gz")
invisible(file.create(wrong))
expect_error_message(
  cran_upload(wrong, pkg = pkg[["dir"]], dry_run = TRUE, sha = "abc"),
  "NACHO_2.0.7.tar.gz"
)

expect_error_message(
  cran_upload(
    file.path(pkg[["dir"]], "missing.tar.gz"),
    pkg = pkg[["dir"]],
    dry_run = TRUE,
    sha = "abc"
  ),
  "does not exist"
)

expect_error_message(
  cran_upload(
    pkg[["tarball"]],
    pkg = pkg[["dir"]],
    dry_run = FALSE,
    sha = "",
    upload = fake_upload
  ),
  "SHA"
)

failing_upload <- function(pkg, built_path) {
  stop("CRAN is closed", call. = FALSE)
}
pkg <- make_pkg()
expect_error_message(
  cran_upload(
    pkg[["tarball"]],
    pkg = pkg[["dir"]],
    dry_run = FALSE,
    sha = "abc",
    upload = failing_upload
  ),
  "CRAN is closed"
)
stopifnot(!file.exists(record_path(pkg)))

expect_error_message(resolve_upload_cran(ns = new.env()), "submit_cran")
reshaped <- new.env()
reshaped[["upload_cran"]] <- function(x) NULL
expect_error_message(resolve_upload_cran(ns = reshaped), "submit_cran")

cat("All cran-upload checks passed\n")
