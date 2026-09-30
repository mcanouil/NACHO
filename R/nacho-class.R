#' @include qc.R
NULL

#' Schema version of the fields stored in `@provenance`
#'
#' @noRd
nacho_schema_version <- 2L

bounds_problem <- function(name, value) {
  if (!is.numeric(value) || length(value) != 2 || anyNA(value)) {
    return(sprintf(
      "@thresholds$%s must be two numbers, a lower and an upper bound.",
      name
    ))
  }
  problem <- if (value[1] == Inf || value[2] == -Inf) {
    "-Inf is only allowed as the lower bound and Inf only as the upper bound."
  } else if (value[1] < 0 && value[1] != -Inf) {
    "the lower bound must not be negative; use -Inf for no lower bound."
  } else if (value[2] < 0) {
    "the upper bound must not be negative."
  } else if (value[1] > value[2]) {
    "the bounds must be increasing."
  }
  if (!is.null(problem)) sprintf("@thresholds$%s: %s", name, problem)
}

validate_thresholds <- function(thresholds) {
  required <- names(default_thresholds())
  missing_names <- setdiff(required, names(thresholds))
  if (length(missing_names) > 0) {
    return(sprintf(
      "@thresholds lacks %s.",
      paste(missing_names, collapse = ", ")
    ))
  }
  problems <- character(0)
  for (name in c("BD", "Positive_factor", "House_factor")) {
    problems <- c(problems, bounds_problem(name, thresholds[[name]]))
  }
  lod <- thresholds[["LoD"]]
  if (!is.numeric(lod) || length(lod) != 1 || is.na(lod) || lod == Inf) {
    problems <- c(
      problems,
      "@thresholds$LoD must be one number; Inf flags every sample, so set a finite LoD, or -Inf for no bound."
    )
  }
  limits <- list(FoV = c(0, 100), PCL = c(0, 1))
  for (name in names(limits)) {
    value <- thresholds[[name]]
    if (
      !is.numeric(value) ||
        length(value) != 1 ||
        is.na(value) ||
        value < limits[[name]][1] ||
        value > limits[[name]][2]
    ) {
      problems <- c(
        problems,
        sprintf(
          "@thresholds$%s must be one number between %s and %s.",
          name,
          limits[[name]][1],
          limits[[name]][2]
        )
      )
    }
  }
  problems
}

validate_nacho <- function(self) {
  counts <- self@counts
  if (!is.matrix(counts)) {
    return("@counts must be an integer matrix of probes by samples.")
  }
  problems <- character(0)
  if (
    !is.matrix(self@normalised) ||
      !identical(dim(self@normalised), dim(counts)) ||
      !identical(dimnames(self@normalised), dimnames(counts))
  ) {
    problems <- c(
      problems,
      "@normalised must be a matrix with the same dimensions and names as @counts."
    )
  }
  id <- self@settings[["id_colname"]]
  if (!rlang::is_string(id) || !id %in% names(self@samples)) {
    return(c(problems, "@settings$id_colname must name a column of @samples."))
  }
  ids <- as.character(self@samples[[id]])
  if (!identical(names(self@samples)[1], id)) {
    problems <- c(problems, "The id column must come first in @samples.")
  }
  if (anyDuplicated(ids) > 0) {
    problems <- c(
      problems,
      sprintf(
        "Sample ids must be unique; duplicated: %s.",
        paste(utils::head(unique(ids[duplicated(ids)]), 3), collapse = ", ")
      )
    )
  }
  if (!identical(ids, colnames(counts))) {
    problems <- c(
      problems,
      "The sample ids in @samples must match colnames(@counts), in the same order."
    )
  }
  scores <- self@pca[["scores"]]
  if (!is.null(scores) && !identical(rownames(scores), ids)) {
    problems <- c(
      problems,
      "The row names of @pca$scores must match the sample ids, in the same order."
    )
  }
  probe_columns <- c(
    "CodeClass",
    "Name",
    "Accession",
    "is_housekeeping",
    "is_excluded"
  )
  if (!all(probe_columns %in% names(self@probes))) {
    problems <- c(
      problems,
      sprintf(
        "@probes must have the columns %s.",
        paste(probe_columns, collapse = ", ")
      )
    )
  } else if (
    !identical(as.character(self@probes[["Name"]]), rownames(counts))
  ) {
    problems <- c(
      problems,
      "The probe names in @probes must match rownames(@counts), in the same order."
    )
  } else if (anyDuplicated(self@probes[["Name"]]) > 0) {
    problems <- c(problems, "Probe names must be unique.")
  }
  if (!rlang::is_string(self@rcc_type) || !self@rcc_type %in% c("n1", "n8")) {
    problems <- c(problems, "@rcc_type must be \"n1\" or \"n8\".")
  }
  problems <- c(problems, validate_thresholds(self@thresholds))
  if (!rlang::is_scalar_integer(self@provenance[["schema_version"]])) {
    problems <- c(problems, "@provenance$schema_version must be one integer.")
  }
  if (length(problems) == 0) NULL else problems
}

#' The nacho class
#'
#' NACHO stores RCC data, quality-control metrics and normalised counts in an
#' S7 object. Reach its content through [nacho_counts()], [nacho_samples()],
#' [nacho_probes()] and [nacho_qc()], never through `@`.
#'
#' @noRd
nacho <- S7::new_class(
  name = "nacho",
  package = "NACHO",
  properties = list(
    counts = S7::class_integer,
    normalised = S7::class_double,
    probes = S7::class_data.frame,
    samples = S7::class_data.frame,
    settings = S7::class_list,
    thresholds = S7::class_list,
    pca = S7::class_list,
    rcc_type = S7::class_character,
    provenance = S7::class_list
  ),
  validator = validate_nacho
)
