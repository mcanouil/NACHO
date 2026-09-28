#' (Re)normalise a nacho object
#'
#' @param nacho_object A `nacho` object from [load_rcc()] or [normalise()].
#' @inheritParams load_rcc
#' @param outliers_thresholds A list of quality-control thresholds with the
#'   elements `BD`, `FoV`, `LoD`, `PCL`, `Positive_factor` and `House_factor`.
#' @param ... Must be empty.
#'
#' @details When only `outliers_thresholds` changes, `normalise()` keeps the
#'   counts and recomputes the outlier flags.
#'   Otherwise it computes the quality-control metrics, the normalisation
#'   factors, the normalised counts and the PCA again.
#'
#'   Outliers are samples with a binding density (`BD`) outside its range, a
#'   field of view (`FoV`) below its limit, a positive factor or a
#'   housekeeping factor outside its range, and, for single-sample RCC files,
#'   a positive control linearity (`PCL`) or a limit of detection (`LoD`)
#'   below its limit. See [exclude_outliers()] to drop them.
#'
#' @return A `nacho` object.
#' @export
#'
#' @examples
#' data(GSE74821)
#' GSE74821_geo <- normalise(GSE74821, normalisation_method = "GEO")
#' nacho_qc(GSE74821_geo)
normalise <- function(
  nacho_object,
  housekeeping_genes = nacho_object@settings[["housekeeping_genes"]],
  housekeeping_predict = nacho_object@settings[["housekeeping_predict"]],
  housekeeping_norm = nacho_object@settings[["housekeeping_norm"]],
  normalisation_method = nacho_object@settings[["normalisation_method"]],
  n_comp = nacho_object@settings[["n_comp"]],
  outliers_thresholds = nacho_object@thresholds,
  ...
) {
  check_nacho(nacho_object)
  dots <- list(...)
  if ("remove_outliers" %in% names(dots)) {
    nacho_abort(
      c(
        "{.arg remove_outliers} was removed in NACHO 3.0.0.",
        i = "Drop flagged samples with {.fn exclude_outliers}, or subset with {.code x[, keep]}."
      ),
      class = "bad_argument"
    )
  }
  if (length(dots) > 0) {
    nacho_abort(
      "Unknown argument{?s}: {.arg {names(dots)}}.",
      class = "bad_argument"
    )
  }
  normalisation_method <- check_settings(
    housekeeping_genes,
    housekeeping_predict,
    housekeeping_norm,
    normalisation_method,
    n_comp
  )
  check_thresholds(outliers_thresholds)

  settings <- list(
    id_colname = nacho_object@settings[["id_colname"]],
    housekeeping_genes = housekeeping_genes,
    housekeeping_predict = housekeeping_predict,
    housekeeping_norm = housekeeping_norm,
    normalisation_method = normalisation_method,
    n_comp = as.integer(n_comp)
  )
  changed <- vapply(
    names(settings),
    function(name) !identical(settings[[name]], nacho_object@settings[[name]]),
    logical(1)
  )
  changed[["housekeeping_genes"]] <- !setequal(
    housekeeping_genes,
    nacho_object@settings[["housekeeping_genes"]]
  )
  thresholds_changed <- !identical(outliers_thresholds, nacho_object@thresholds)

  if (!any(changed) && !thresholds_changed) {
    nacho_inform(
      "The settings are the same as in the input, so {.fn normalise} returns it unchanged."
    )
    return(nacho_object)
  }
  if (!any(changed)) {
    nacho_object@thresholds <- outliers_thresholds
    return(check_outliers(nacho_object))
  }
  nacho_inform(c(
    "Normalising again with new settings:",
    stats::setNames(names(changed)[changed], rep("*", sum(changed)))
  ))
  run_normalisation(nacho_object, settings, outliers_thresholds)
}

#' @export
#' @rdname normalise
#' @usage NULL
normalize <- normalise

run_normalisation <- function(x, settings, thresholds) {
  build_nacho(
    counts = x@counts,
    probes = x@probes,
    samples = x@samples,
    settings = settings,
    thresholds = thresholds,
    rcc_type = x@rcc_type,
    provenance = x@provenance
  )
}

#' Flag outliers of a nacho object
#'
#' Recomputes `is_outlier` in [nacho_qc()] from the object's thresholds.
#' [normalise()] and [load_rcc()] already do this, so you only need it after
#' changing thresholds by hand.
#'
#' @inheritParams normalise
#' @return A `nacho` object.
#' @export
#' @examples
#' data(GSE74821)
#' table(nacho_qc(check_outliers(GSE74821))$is_outlier)
check_outliers <- function(nacho_object) {
  check_nacho(nacho_object)
  samples <- nacho_object@samples
  samples[["is_outlier"]] <- compute_outliers(
    samples,
    nacho_object@thresholds,
    nacho_object@rcc_type
  )
  nacho_object@samples <- samples
  nacho_object
}

#' Drop outliers and normalise the other samples again
#'
#' Removes the samples flagged in [nacho_qc()] and runs the normalisation again
#' on the samples that are left, with the same settings and thresholds.
#' The new factors can flag more samples; call `exclude_outliers()` again to
#' drop those too.
#'
#' @inheritParams normalise
#' @return A `nacho` object without the flagged samples.
#' @export
#' @examples
#' data(GSE74821)
#' ncol(exclude_outliers(GSE74821))
exclude_outliers <- function(nacho_object) {
  check_nacho(nacho_object)
  flagged <- nacho_object@samples[["is_outlier"]] %in% TRUE
  if (!any(flagged)) {
    nacho_inform("No sample is flagged, so nothing is removed.")
    return(nacho_object)
  }
  if (all(flagged)) {
    nacho_abort(
      c(
        "Every sample is flagged, so no sample would be left.",
        i = "Check the thresholds with {.code summary(x)}."
      ),
      class = "bad_argument"
    )
  }
  nacho_inform(
    "Removing {sum(flagged)} flagged sample{?s} and normalising the other {sum(!flagged)} again."
  )
  kept <- nacho_object[, !flagged]
  run_normalisation(kept, kept@settings, kept@thresholds)
}

check_thresholds <- function(
  thresholds,
  arg = rlang::caller_arg(thresholds),
  call = rlang::caller_env()
) {
  problems <- if (is.list(thresholds)) {
    validate_thresholds(thresholds)
  } else {
    "Thresholds must be a list."
  }
  if (length(problems) > 0) {
    nacho_abort(
      c(
        "{.arg {arg}} is not a valid set of thresholds.",
        stats::setNames(
          sub("^@thresholds", "thresholds", problems),
          rep("x", length(problems))
        )
      ),
      class = "bad_argument",
      call = call
    )
  }
  invisible(thresholds)
}
