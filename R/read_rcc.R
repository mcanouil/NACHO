#' Read one RCC file
#'
#' Reads the file once and matches section tags exactly, so a probe named
#' like a section is never mistaken for one.
#' Gzipped files are read directly.
#'
#' @param file Path to an `.RCC` or `.RCC.gz` file.
#'
#' @keywords internal
#' @noRd
read_rcc <- function(file) {
  lines <- trim_trailing_space(readLines(file, warn = FALSE))
  has_bar <- grepl("|", lines, fixed = TRUE)
  lines[has_bar] <- sub(
    "[|]+[[:digit:]]+\\.*[[:digit:]]*",
    "",
    lines[has_bar]
  )

  section <- function(tag, required = TRUE) {
    start <- match(paste0("<", tag, ">"), lines)
    end <- match(paste0("</", tag, ">"), lines)
    if (is.na(start) || is.na(end) || end < start) {
      if (!required) {
        return(character(0))
      }
      nacho_abort(
        c(
          "{.file {basename(file)}} is not a valid RCC file.",
          x = "Its {.field {tag}} section is missing or not closed."
        ),
        class = "rcc_parse",
        call = NULL
      )
    }
    if (end - start < 2) character(0) else lines[(start + 1):(end - 1)]
  }

  key_values <- function(section_lines, prefix) {
    if (length(section_lines) == 0) {
      return(character(0))
    }
    comma <- regexpr(",", section_lines, fixed = TRUE)
    keys <- ifelse(
      comma > 0,
      substr(section_lines, 1, comma - 1),
      section_lines
    )
    values <- ifelse(comma > 0, substring(section_lines, comma + 1), "")
    stats::setNames(values, paste0(prefix, keys))
  }

  code_summary <- data.table::fread(
    text = section("Code_Summary"),
    sep = ",",
    header = TRUE,
    colClasses = "character",
    data.table = FALSE,
    showProgress = FALSE
  )
  missing_columns <- setdiff(
    c("CodeClass", "Name", "Accession", "Count"),
    names(code_summary)
  )
  if (length(missing_columns) > 0) {
    nacho_abort(
      c(
        "{.file {basename(file)}} is not a valid RCC file.",
        x = "Its {.field Code_Summary} section has no {.field {missing_columns}} column{?s}."
      ),
      class = "rcc_parse",
      call = NULL
    )
  }
  code_summary <- code_summary[, c("CodeClass", "Name", "Accession", "Count")]
  code_summary[["Count"]] <- as.integer(code_summary[["Count"]])

  list(
    attributes = c(
      key_values(section("Header"), "Header.header_"),
      key_values(section("Sample_Attributes"), "Sample_Attributes.sample_"),
      key_values(section("Lane_Attributes"), "Lane_Attributes.lane_")
    ),
    messages = paste(section("Messages", required = FALSE), collapse = "; "),
    code_summary = code_summary
  )
}

#' Remove trailing white space from lines
#'
#' Runs the regular expression only on lines that do not end in a printable
#' ASCII character, since the others have nothing to trim.
#' The result is the same as `sub("[[:space:]]+$", "", lines)`, much faster.
#'
#' @param lines A character vector.
#'
#' @keywords internal
#' @noRd
trim_trailing_space <- function(lines) {
  last_character <- substring(lines, nchar(lines))
  to_trim <- !grepl("[!-~]", last_character)
  lines[to_trim] <- sub("[[:space:]]+$", "", lines[to_trim])
  lines
}

#' Tell whether a code class vector is a PlexSet RCC file
#'
#' `TRUE` when `Endogenous1s` to `Endogenous8s` are all present, matched exactly.
#'
#' @param code_class A character vector, the `CodeClass` column of a `Code_Summary` section.
#'
#' @keywords internal
#' @noRd
is_plexset_classes <- function(code_class) {
  all(paste0("Endogenous", seq_len(8), "s") %in% code_class)
}

#' Tell whether an RCC file is a PlexSet file
#'
#' Reads the file cheaply, without requiring every section to be present or
#' closed, so it also works on partial files.
#'
#' @param file Path to an `.RCC` or `.RCC.gz` file.
#'
#' @keywords internal
#' @noRd
#'
#' @return `TRUE` when the file holds all eight PlexSet code classes,
#'   `Endogenous1s` to `Endogenous8s`, matched exactly.
is_plexset_rcc <- function(file) {
  lines <- trim_trailing_space(readLines(file, warn = FALSE))
  is_plexset_classes(sub(",.*$", "", lines))
}

#' Split a parsed RCC file into its samples
#'
#' One element for single-sample files, eight named `S1` to `S8` for
#' PlexSet files, with the control probes repeated in each and `CodeClass`
#' stripped of its `1s` to `8s` suffix.
#'
#' @param parsed The output of `read_rcc()`.
#'
#' @keywords internal
#' @noRd
rcc_samples <- function(parsed) {
  code_summary <- parsed[["code_summary"]]
  if (!is_plexset_classes(code_summary[["CodeClass"]])) {
    samples <- list(code_summary)
  } else {
    is_control <- code_summary[["CodeClass"]] %in% c("Positive", "Negative")
    controls <- code_summary[is_control, , drop = FALSE]
    others <- code_summary[!is_control, , drop = FALSE]
    row <- sub("^[A-Za-z]+", "", others[["CodeClass"]])
    unsuffixed <- unique(others[["CodeClass"]][
      !row %in% paste0(seq_len(8), "s")
    ])
    if (length(unsuffixed) > 0) {
      nacho_abort(
        c(
          "PlexSet code classes must end in {.val 1s} to {.val 8s}.",
          x = "Not suffixed: {.val {unsuffixed}}."
        ),
        class = "rcc_parse",
        call = NULL
      )
    }
    samples <- lapply(paste0(seq_len(8), "s"), function(plex_row) {
      sample <- others[row == plex_row, , drop = FALSE]
      sample[["CodeClass"]] <- sub("[0-8]+s$", "", sample[["CodeClass"]])
      rbind(sample, controls)
    })
    names(samples) <- paste0("S", seq_len(8))
  }
  for (sample in samples) {
    duplicated_names <- unique(sample[["Name"]][duplicated(sample[["Name"]])])
    if (length(duplicated_names) > 0) {
      nacho_abort(
        c(
          "Probe names must be unique within an RCC sample.",
          x = "Duplicated: {.val {utils::head(duplicated_names, 5)}}."
        ),
        class = "rcc_parse",
        call = NULL
      )
    }
  }
  samples
}
