#' (Re)normalise a nacho object
#'
#' @param nacho_object A `nacho` object from [load_rcc()] or [normalise()].
#' @inheritParams load_rcc
#' @param housekeeping_norm [[logical]] Boolean to indicate whether the housekeeping normalisation
#'   should be performed.
#'   The default is the setting stored in `nacho_object`, which is `TRUE` or `FALSE`.
#' @param ruv_k [[numeric]] The number of unwanted factors RUVg removes.
#'   `normalise()` reuses the `ruv_k` stored in the object, so pass
#'   `ruv_k = NULL` to have [suggest_ruv_k()] choose again.
#'   Other methods ignore it.
#' @param outliers_thresholds A list of quality-control thresholds, as
#'   returned by [nacho_thresholds()].
#' @param ... Must be empty.
#'
#' @details When only the limits in `outliers_thresholds` change and the preset stays
#'   the same, `normalise()` keeps the counts and stores the new thresholds; the flags follow them whenever you
#'   read [nacho_qc()].
#'   Otherwise it computes the quality-control metrics, the normalisation
#'   factors, the normalised counts and the PCA again.
#'
#'   [nacho_qc()] lists the flags, and [exclude_outliers()] drops the flagged
#'   samples.
#'
#'   The normalisation runs in this order: raw counts, negative probe
#'   exclusion, background (`background` and `background_mode`), positive
#'   control factor (`normalisation_method`), then content factor
#'   (housekeeping genes when `housekeeping_norm` is `TRUE`).
#'   RUVg works on `log2(count + 1)` after the positive factor and returns
#'   counts on the count scale, floored at 0.
#'   It replaces the housekeeping scaling, so `housekeeping_norm` has no effect
#'   and `House_factor` is not computed, and it corrects only the endogenous
#'   and housekeeping probes.
#'   Normalised counts are otherwise neither rounded nor floored; use
#'   `nacho_counts(x, normalised = TRUE, log2 = TRUE)` for `log2(count + 1)`.
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
  background = nacho_object@settings[["background"]],
  background_mode = nacho_object@settings[["background_mode"]],
  ruv_k = nacho_object@settings[["ruv_k"]],
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
  choices <- check_settings(
    housekeeping_genes,
    housekeeping_predict,
    housekeeping_norm,
    normalisation_method,
    n_comp,
    background,
    background_mode,
    ruv_k
  )
  check_thresholds(outliers_thresholds)

  settings <- list(
    id_colname = nacho_object@settings[["id_colname"]],
    housekeeping_genes = housekeeping_genes,
    housekeeping_predict = housekeeping_predict,
    housekeeping_norm = housekeeping_norm,
    normalisation_method = choices[["normalisation_method"]],
    ruv_k = if (!is.null(ruv_k)) as.integer(ruv_k),
    background = choices[["background"]],
    background_mode = choices[["background_mode"]],
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
  definitions_changed <- !identical(
    outliers_thresholds[["preset"]],
    nacho_object@thresholds[["preset"]]
  )
  if (!any(changed) && !definitions_changed) {
    nacho_object@thresholds <- outliers_thresholds
    return(nacho_object)
  }
  changes <- c(
    names(changed)[changed],
    if (definitions_changed) "thresholds preset"
  )
  nacho_inform(c(
    "Normalising again with new settings:",
    stats::setNames(changes, rep("*", length(changes)))
  ))
  run_normalisation(nacho_object, settings, outliers_thresholds)
}

#' @export
#' @rdname normalise
#' @usage NULL
normalize <- normalise

run_normalisation <- function(
  x,
  settings,
  thresholds,
  call = rlang::caller_env()
) {
  build_nacho(
    counts = x@counts,
    probes = x@probes,
    samples = x@samples,
    settings = settings,
    thresholds = thresholds,
    rcc_type = x@rcc_type,
    provenance = x@provenance,
    warn_missing = FALSE,
    call = call
  )
}

#' Drop outliers and normalise the other samples again
#'
#' Removes the samples whose `status` in [nacho_qc()] is `"fail"` and runs the normalisation again
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
  flagged <- flagged_samples(nacho_object)
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
