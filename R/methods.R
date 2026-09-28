#' @include nacho-class.R
NULL

#' Rebuild the NACHO 2 long layout
#'
#' One row per probe and sample, with every sample column, the PCA scores,
#' `CodeClass`, `Name`, `Accession`, `Count` and `Count_Norm`.
#'
#' @param x A `nacho` object.
#' @param rows Probe row indices to keep; `NULL` keeps every probe.
#'
#' @keywords internal
#' @noRd
long_table <- function(x, rows = NULL) {
  if (is.null(rows)) {
    rows <- seq_len(nrow(x@counts))
  }
  counts <- x@counts[rows, , drop = FALSE]
  normalised <- x@normalised[rows, , drop = FALSE]
  n_probes <- nrow(counts)
  n_samples <- ncol(counts)
  sample_rows <- rep(seq_len(n_samples), each = n_probes)
  probe_rows <- rep(seq_len(n_probes), times = n_samples)
  scores <- as.data.frame(x@pca[["scores"]])
  probes <- x@probes[rows, c("CodeClass", "Name", "Accession"), drop = FALSE]
  pieces <- list(
    x@samples[sample_rows, , drop = FALSE],
    if (ncol(scores) > 0) scores[sample_rows, , drop = FALSE],
    probes[probe_rows, , drop = FALSE],
    data.frame(Count = as.vector(counts), Count_Norm = as.vector(normalised))
  )
  long <- do.call(cbind, Filter(Negate(is.null), pieces))
  rownames(long) <- NULL
  data.table::as.data.table(long)
}

format_nacho <- function(x, ...) {
  housekeeping <- x@probes[["Name"]][x@probes[["is_housekeeping"]]]
  kind <- if (x@rcc_type == "n8") "PlexSet" else "single-sample"
  uses_housekeeping <- isTRUE(x@settings[["housekeeping_norm"]]) &&
    length(housekeeping) > 0
  c(
    cli::format_inline(
      "<nacho> {ncol(x@counts)} sample{?s}, {nrow(x@counts)} probe{?s}, from ",
      kind,
      " RCC files"
    ),
    cli::format_inline(
      "Normalisation: {x@settings[['normalisation_method']]}, ",
      if (uses_housekeeping) {
        "with {length(housekeeping)} housekeeping gene{?s}"
      } else {
        "without housekeeping genes"
      }
    ),
    cli::format_inline(
      "Flagged samples: {sum(x@samples[['is_outlier']] %in% TRUE)} of {ncol(x@counts)}"
    ),
    cli::format_inline(
      "Created with NACHO {x@provenance[['nacho_version']]}"
    )
  )
}

print_nacho <- function(x, ...) {
  cat(format_nacho(x), sep = "\n")
  invisible(x)
}

summary_nacho <- function(object, ...) {
  metrics <- c("BD", "FoV", "PCL", "LoD", "Positive_factor", "House_factor")
  if (object@rcc_type == "n8") {
    metrics <- setdiff(metrics, c("PCL", "LoD"))
  }
  metrics <- intersect(metrics, names(object@samples))
  thresholds <- object@thresholds
  data.frame(
    metric = metrics,
    lower = vapply(metrics, function(m) min(thresholds[[m]]), numeric(1)),
    upper = vapply(
      metrics,
      function(m) {
        if (length(thresholds[[m]]) == 2) max(thresholds[[m]]) else NA_real_
      },
      numeric(1)
    ),
    n_fail = vapply(
      metrics,
      function(m) sum(metric_fails(object@samples[[m]], thresholds[[m]])),
      integer(1)
    ),
    n_missing = vapply(
      metrics,
      function(m) sum(is.na(object@samples[[m]])),
      integer(1)
    ),
    row.names = NULL
  )
}

as_data_frame_nacho <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  long = FALSE
) {
  check_bool(long)
  if (long) {
    return(as.data.frame(long_table(x)))
  }
  nacho_samples(x)
}

#' Basic methods for a nacho object
#'
#' @section Usage:
#' ```r
#' print(x)
#' format(x)
#' summary(object)
#' dim(x)
#' as.data.frame(x, long = FALSE)
#' x[i, j]
#' ```
#'
#' @section Arguments:
#' * `x`, `object`: A `nacho` object.
#' * `long`: If `TRUE`, `as.data.frame()` returns one row per probe and
#'   sample (the NACHO 2 `$nacho` layout) instead of one row per sample.
#' * `i`: Probes to keep.
#' * `j`: Samples to keep.
#'
#' @name nacho-methods
#' @usage NULL
#'
#' @examples
#' data(GSE74821)
#' print(GSE74821)
#' summary(GSE74821)
#' dim(GSE74821)
#' head(as.data.frame(GSE74821, long = TRUE)[, 1:5])
#' GSE74821[, 1:12]
NULL

resolve_index <- function(index, names, arg, call = rlang::caller_env()) {
  # Used only inside the cli glue strings below.
  what <- if (identical(arg, "i")) "probe" else "sample" # nolint: object_usage_linter.
  n <- length(names)
  if (is.logical(index)) {
    if (length(index) != n) {
      nacho_abort(
        paste0(
          "{.arg {arg}} is a logical vector of length {length(index)}, ",
          "but {cli::qty(n)}{?there is/there are} {n} {what}{cli::qty(n)}{?s}."
        ),
        class = "bad_argument",
        call = call
      )
    }
    if (anyNA(index)) {
      nacho_abort(
        "{.arg {arg}} contains a missing value; every element must be {.code TRUE} or {.code FALSE}.",
        class = "bad_argument",
        call = call
      )
    }
  }
  positions <- unname(stats::setNames(seq_along(names), names)[index])
  n_missing <- sum(is.na(positions))
  if (n_missing > 0) {
    nacho_abort(
      paste0(
        "{.arg {arg}} selects {n_missing} {what}{cli::qty(n_missing)}{?s} ",
        "that {cli::qty(n_missing)}{?does/do} not exist."
      ),
      class = "bad_argument",
      call = call
    )
  }
  if (anyDuplicated(positions) > 0) {
    nacho_abort(
      "{.arg {arg}} selects the same {what} more than once.",
      class = "bad_argument",
      call = call
    )
  }
  positions
}

subset_nacho <- function(x, i, j, ..., drop = FALSE) {
  # `i` or `j` given by name (e.g. `x[i = 1:2]`) is unambiguous even without
  # a comma; only a bare positional single index (or `x[]`) is refused.
  named_index <- any(names(sys.call()) %in% c("i", "j"))
  if (nargs() < 3 && !named_index) {
    nacho_abort(
      c(
        "{.code x[i]} does not say whether it selects probes or samples.",
        i = "Use {.code x[i, ]} to select probes or {.code x[, j]} to select samples."
      ),
      class = "bad_argument"
    )
  }
  rows <- if (missing(i)) {
    seq_len(nrow(x@counts))
  } else {
    resolve_index(i, rownames(x@counts), "i")
  }
  columns <- if (missing(j)) {
    seq_len(ncol(x@counts))
  } else {
    resolve_index(j, colnames(x@counts), "j")
  }
  counts <- x@counts[rows, columns, drop = FALSE]
  samples <- x@samples[columns, , drop = FALSE]
  rownames(samples) <- NULL
  samples[["is_outlier"]] <- compute_outliers(samples, x@thresholds, x@rcc_type)
  probes <- x@probes[rows, , drop = FALSE]
  rownames(probes) <- NULL
  nacho(
    counts = counts,
    normalised = x@normalised[rows, columns, drop = FALSE],
    probes = probes,
    samples = samples,
    settings = x@settings,
    thresholds = x@thresholds,
    pca = compute_pca(counts, x@settings[["n_comp"]]),
    rcc_type = x@rcc_type,
    provenance = x@provenance
  )
}

S7::method(print, nacho) <- print_nacho
S7::method(format, nacho) <- format_nacho
S7::method(summary, nacho) <- summary_nacho
S7::method(dim, nacho) <- function(x) dim(x@counts)
S7::method(as.data.frame, nacho) <- as_data_frame_nacho
S7::method(`[`, nacho) <- subset_nacho

#' Point NACHO 2 objects to the converters
#'
#' NACHO 2 objects are S3 lists of class `nacho`, which the S7 methods above
#' never see. `print()` and `format()` describe the object in one line, and
#' the other methods refuse it, so base R does not dump or coerce the list.
#' `S7::method<-` leaves local copies of the base generics in the namespace,
#' so these methods name `base::` to reach the base methods table.
#'
#' @param x,object A NACHO 2 object.
#' @param row.names,optional,... Ignored.
#'
#' @keywords internal
#' @noRd
#' @exportS3Method base::format
format.nacho <- function(x, ...) {
  if (is_nacho_v2(x)) {
    cli::format_inline(
      "<nacho> A NACHO 2 object. ",
      "Convert it with {.code upgrade_nacho()}, or read the saved file with {.code read_nacho()}."
    )
  } else {
    cli::format_inline(
      "<nacho> A list with the NACHO 2 class but not the NACHO 2 data. ",
      "Create a NACHO 3 object with {.code load_rcc()}."
    )
  }
}

#' @noRd
#' @exportS3Method base::print
print.nacho <- function(x, ...) {
  cat(format.nacho(x), sep = "\n")
  invisible(x)
}

#' @noRd
#' @exportS3Method base::summary
summary.nacho <- function(object, ...) {
  abort_nacho_v2(object)
}

#' @noRd
#' @exportS3Method base::dim
dim.nacho <- function(x) {
  abort_nacho_v2(x)
}

#' @noRd
#' @exportS3Method base::as.data.frame
as.data.frame.nacho <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  abort_nacho_v2(x)
}

#' @noRd
#' @exportS3Method base::"[" nacho
`[.nacho` <- function(x, ...) {
  abort_nacho_v2(x)
}
