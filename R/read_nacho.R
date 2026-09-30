#' Settings that reproduce the NACHO 2 background correction
#'
#' Without negative probes the NACHO 2 geometric mean method subtracted
#' nothing, so `background = "none"` reproduces it.
#'
#' @param settings A list of settings.
#' @param probes The probe table, with a `CodeClass` column.
#'
#' @noRd
legacy_settings <- function(settings, probes) {
  has_negatives <- "Negative" %in% probes[["CodeClass"]]
  settings[["background"]] <- if (has_negatives) "geo" else "none"
  settings[["background_mode"]] <- "subtract"
  settings
}

#' Rebuild a schema 1 object with the definitions that made it
#'
#' @param properties The properties of the saved object, from `S7::props()`.
#'
#' @noRd
migrate_schema_1 <- function(properties) {
  provenance <- properties[["provenance"]]
  provenance[["schema_version"]] <- nacho_schema_version
  provenance[["migrated_from_schema"]] <- 1L
  settings <- legacy_settings(
    properties[["settings"]],
    properties[["probes"]]
  )
  migrated <- build_nacho(
    counts = properties[["counts"]],
    probes = properties[["probes"]],
    samples = properties[["samples"]],
    settings = settings,
    thresholds = properties[["thresholds"]],
    rcc_type = properties[["rcc_type"]],
    provenance = provenance,
    warn_missing = FALSE
  )
  nacho_inform(c(
    "Read an object saved with schema 1 and rebuilt it with schema {nacho_schema_version}.",
    i = if (settings[["background"]] == "geo") {
      "Normalised counts are recomputed without rounding, with the NACHO 2 geometric mean background subtraction it was made with. A GLM object no longer subtracts the model intercept."
    } else {
      "Normalised counts are recomputed without rounding, and without background subtraction because the object has no negative probes."
    }
  ))
  migrated
}

#' Convert a NACHO 2 object to NACHO 3
#'
#' NACHO 3 stores data in an S7 `nacho` object instead of the NACHO 2 list.
#' `upgrade_nacho()` converts a NACHO 2 object once: it keeps the raw counts,
#' the sample sheet, the settings and the thresholds, and computes the
#' quality-control metrics, the normalised counts, the PCA and the outlier
#' flags again with the NACHO 3 definitions.
#'
#' @param x A NACHO 2 object, as returned by `load_rcc()` or `normalise()` in
#'   NACHO 2.
#'
#' @return A `nacho` object.
#' @seealso [read_nacho()]
#' @export
#' @examples
#' # A NACHO 2 object is converted; GSE74821 is already a NACHO 3 object,
#' # so it comes back unchanged.
#' data(GSE74821)
#' identical(upgrade_nacho(GSE74821), GSE74821)
upgrade_nacho <- function(x) {
  if (missing(x)) {
    nacho_abort(
      c(
        "{.arg x} is missing.",
        i = "Pass a NACHO 2 object, as returned by {.fn load_rcc} or {.fn normalise} in NACHO 2."
      ),
      class = "bad_object"
    )
  }
  if (S7::S7_inherits(x, nacho)) {
    check_nacho(x)
    nacho_inform(
      "{.arg x} is already a NACHO 3 object, so it is returned unchanged."
    )
    return(x)
  }
  if (!is_nacho_v2(x)) {
    nacho_abort(
      "{.arg x} must be a NACHO 2 object, not {.obj_type_friendly {x}}.",
      class = "bad_object"
    )
  }
  table <- x[["nacho"]]
  invalid <- c(
    access = !rlang::is_string(x[["access"]]),
    RCC_type = !rlang::is_string(attr(x, "RCC_type")),
    nacho = !is.data.frame(table)
  )
  missing_columns <- if (is.data.frame(table)) {
    setdiff(
      c(
        if (!invalid[["access"]]) x[["access"]],
        "CodeClass",
        "Name",
        "Accession",
        "Count"
      ),
      names(table)
    )
  }
  fields <- c(names(invalid)[invalid], sprintf("nacho$%s", missing_columns))
  if (length(fields) > 0) {
    nacho_abort(
      c(
        "{.arg x} is an incomplete NACHO 2 object.",
        x = "Missing or invalid: {.field {fields}}."
      ),
      class = "bad_object"
    )
  }
  long <- as.data.frame(table)
  id <- x[["access"]]
  dropped <- c(
    "CodeClass",
    "Name",
    "Accession",
    "Count",
    "Count_Norm",
    "file_path"
  )
  sample_columns <- setdiff(names(long), c(dropped, id))
  samples <- long[!duplicated(long[[id]]), c(id, sample_columns), drop = FALSE]
  samples <- samples[
    order(as.character(samples[[id]]), method = "radix"),
    ,
    drop = FALSE
  ]
  rownames(samples) <- NULL
  ids <- as.character(samples[[id]])
  probe_counts <- build_probe_counts(
    codes = long,
    column = match(long[[id]], ids),
    ids = ids,
    clash_message = "{.arg x} has probes whose name appears with more than one code class or accession.",
    clash_class = "bad_object"
  )
  probes <- probe_counts[["probes"]]
  counts <- probe_counts[["counts"]]

  thresholds <- x[["outliers_thresholds"]]
  check_thresholds(thresholds, arg = "x$outliers_thresholds")
  check_choice(
    x[["normalisation_method"]],
    c("GEO", "GLM"),
    arg = "x$normalisation_method"
  )
  check_count(x[["n_comp"]], arg = "x$n_comp")
  check_character(
    x[["housekeeping_genes"]],
    allow_null = TRUE,
    arg = "x$housekeeping_genes"
  )
  check_bool(x[["housekeeping_norm"]], arg = "x$housekeeping_norm")
  check_bool(x[["housekeeping_predict"]], arg = "x$housekeeping_predict")
  check_choice(
    attr(x, "RCC_type"),
    c("n1", "n8"),
    arg = "attr(x, \"RCC_type\")"
  )
  provenance <- new_provenance(
    data_directory = x[["data_directory"]],
    file_version = samples[["Header.header_FileVersion"]],
    software_version = samples[["Header.header_SoftwareVersion"]]
  )
  provenance[["upgraded_from"]] <- list(
    nacho_version = "2",
    remove_outliers = isTRUE(x[["remove_outliers"]])
  )
  upgraded <- build_nacho(
    counts = counts,
    probes = probes,
    samples = samples,
    settings = list(
      id_colname = id,
      housekeeping_genes = x[["housekeeping_genes"]],
      housekeeping_predict = FALSE,
      housekeeping_norm = x[["housekeeping_norm"]],
      normalisation_method = x[["normalisation_method"]],
      n_comp = as.integer(x[["n_comp"]])
    ) |>
      legacy_settings(probes),
    thresholds = thresholds,
    rcc_type = attr(x, "RCC_type"),
    provenance = provenance
  )
  nacho_inform(c(
    "Converted a NACHO 2 object to NACHO 3.",
    i = "The PCA, normalised counts and outlier flags are recomputed with NACHO 3 and may differ from the saved ones.",
    i = if (x[["housekeeping_predict"]]) {
      "The housekeeping genes NACHO 2 predicted are kept, and are not predicted again."
    }
  ))
  upgraded
}

#' Read a saved NACHO object
#'
#' Reads an `.rds` file written with [saveRDS()] from any NACHO version.
#' NACHO 2 objects go through [upgrade_nacho()].
#' NACHO 3 objects are rebuilt with the current class, because each saved S7
#' object carries a copy of the class it was made with.
#'
#' Only read files you trust: [readRDS()] can run code stored in a crafted
#' file, notably in R before 4.4.0.
#'
#' @param path Path to an `.rds` file.
#'
#' @return A `nacho` object.
#' @seealso [upgrade_nacho()]
#' @export
#' @examples
#' path <- tempfile(fileext = ".rds")
#' saveRDS(GSE74821, path)
#' read_nacho(path)
read_nacho <- function(path) {
  call <- rlang::current_env()
  check_string(path)
  if (!file.exists(path)) {
    nacho_abort(
      "The file {.file {path}} does not exist.",
      class = "missing_file"
    )
  }
  abort_unreadable <- function(parent = NULL) {
    nacho_abort(
      "{.file {path}} could not be read as an {.val .rds} file.",
      class = "bad_object",
      parent = parent,
      call = call
    )
  }
  if (dir.exists(path)) {
    abort_unreadable()
  }
  x <- rlang::try_fetch(readRDS(path), error = abort_unreadable)
  if (is_nacho_v2(x)) {
    return(upgrade_nacho(x))
  }
  if (!S7::S7_inherits(x, nacho)) {
    nacho_abort(
      "{.file {path}} does not hold a NACHO object, but {.obj_type_friendly {x}}.",
      class = "bad_object"
    )
  }
  properties <- S7::props(x)
  if (identical(properties[["provenance"]][["schema_version"]], 1L)) {
    return(migrate_schema_1(properties))
  }
  check_schema(
    properties[["provenance"]][["schema_version"]],
    subject = cli::format_inline("{.file {path}}")
  )
  problems <- validate_thresholds(properties[["thresholds"]])
  if (length(problems) > 0) {
    nacho_abort(
      c(
        "{.file {path}} holds thresholds that NACHO 3 refuses.",
        stats::setNames(problems, rep("x", length(problems))),
        i = "Change {.code attr(x, \"thresholds\")} after {.fn readRDS}, then save it with {.fn saveRDS}."
      ),
      class = "bad_object"
    )
  }
  do.call(nacho, properties)
}
