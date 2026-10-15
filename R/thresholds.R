nacho_instruments <- c("max", "flex", "pro", "sprint")
nacho_presets <- c("nsolver", "legacy")

#' Quality-control thresholds
#'
#' Builds the thresholds that [load_rcc()] and [normalise()] use to flag
#' samples.
#'
#' `"nsolver"` follows the current Bruker nCounter knowledge base
#' (<https://brukerspatialbiology.com/support/>, "nCounter QC flags"): binding density
#' 0.05 to 2.25 on MAX, FLEX and PRO and 0.1 to 1.8 on SPRINT, field of view at
#' least 75 %, positive control linearity at least 0.95 (POS_A to POS_E, with
#' `log2(count + 1)`), limit of detection at least 2, positive factor 0.3 to 3
#' and housekeeping factor 0.1 to 10.
#' It also asks for at least three housekeeping genes above background, as
#' Bruker asks (`Housekeeping_detected`).
#'
#' `"legacy"` reproduces NACHO 2: binding density 0.1 to 2.25 on every
#' instrument, positive control linearity with POS_F, positive factor 1/4 to 4
#' and housekeeping factor 1/11 to 11.
#' With `background = "geo", background_mode = "subtract"`, it gives back the
#' NACHO 2 outlier calls.
#'
#' The preset also chooses how negative probes are excluded and how the
#' positive control linearity is computed, so changing it normalises again.
#'
#' For miRNA panels, `Ligation_order` (the three ligation positive controls
#' in decreasing order), `Ligation_R2` (their log2 counts on a line) and
#' `Ligation_NEG` (the largest ligation negative minus the detection limit)
#' are NACHO's own definitions, since Bruker publishes no threshold.
#' `"legacy"` never flags them.
#'
#' Whole-blood RNA raises `LIG_NEG_A`, and to a lesser extent `LIG_NEG_B`,
#' together with red blood cell miRNAs such as `miR-451a`.
#' The ligation negatives of such samples are above the detection limit, so
#' most samples of a whole-blood study fail `Ligation_NEG`.
#' This pattern occurs in independent PAXgene whole-blood studies on GEO
#' (GSE303669 and GSE317496), and not in plasma, serum or extracellular
#' vesicle studies.
#' When more than half of the samples fail `Ligation_NEG`, [load_rcc()]
#' warns about this pattern.
#' For whole-blood samples, set a higher upper bound for `Ligation_NEG` and
#' pass the thresholds to `normalise(outliers_thresholds = )`.
#' Do not remove these samples with [exclude_outliers()] because of this
#' flag only.
#'
#' Each element can be changed afterwards; two numbers are a lower and an
#' upper bound, and one number is a lower bound. Use `-Inf` or `Inf` for an
#' open bound.
#'
#' @param instrument The nCounter instrument: `"max"`, `"flex"`, `"pro"` or
#'   `"sprint"`.
#' @param preset `"nsolver"` or `"legacy"`.
#' @param haemolysis If `TRUE`, flag samples of a miRNA panel whose `miR-451a` to
#'   `miR-23a-3p` log2 ratio is above 7, a sign of haemolysis in plasma and
#'   serum.
#'   [load_rcc()] does not take it, so pass the thresholds to
#'   `normalise(outliers_thresholds = )`.
#'
#' @return A named list: `preset`, `instrument`, then one element per metric.
#' @export
#' @examples
#' nacho_thresholds()
#' nacho_thresholds("sprint")
#' nacho_thresholds(preset = "legacy")
nacho_thresholds <- function(
  instrument = c("max", "flex", "pro", "sprint"),
  preset = c("nsolver", "legacy"),
  haemolysis = FALSE
) {
  instrument <- check_choice(instrument, nacho_instruments)
  preset <- check_choice(preset, nacho_presets)
  haemolysis <- check_bool(haemolysis)
  limits <- if (preset == "legacy") {
    list(
      BD = c(0.1, 2.25),
      FoV = 75,
      PCL = 0.95,
      LoD = 2,
      Positive_factor = c(1 / 4, 4),
      House_factor = c(1 / 11, 11),
      Housekeeping_detected = 0,
      Ligation_order = 0,
      Ligation_R2 = 0,
      Ligation_NEG = c(-Inf, Inf),
      Haemolysis = c(-Inf, if (haemolysis) 7 else Inf)
    )
  } else {
    list(
      BD = if (instrument == "sprint") c(0.1, 1.8) else c(0.05, 2.25),
      FoV = 75,
      PCL = 0.95,
      LoD = 2,
      Positive_factor = c(0.3, 3),
      House_factor = c(0.1, 10),
      Housekeeping_detected = 3,
      Ligation_order = 1,
      Ligation_R2 = 0.95,
      Ligation_NEG = c(-Inf, 0),
      Haemolysis = c(-Inf, if (haemolysis) 7 else Inf)
    )
  }
  c(list(preset = preset, instrument = instrument), limits)
}

#' Tell the instrument from the RCC header
#'
#' Only file version 1.7, written by the MAX/FLEX Digital Analyzer, is
#' conclusive; MAX, FLEX and PRO share their limits.
#'
#' @noRd
detect_instrument <- function(samples) {
  versions <- unique(samples[["Header.header_FileVersion"]])
  if (identical(as.character(versions), "1.7")) "max" else NA_character_
}

#' Thresholds for loaded samples, detecting the instrument when not given
#'
#' @noRd
thresholds_for_samples <- function(
  samples,
  instrument,
  preset,
  hint = "Set {.arg instrument} to {.or {.val {nacho_instruments}}}."
) {
  if (is.null(instrument)) {
    instrument <- detect_instrument(samples)
    if (is.na(instrument)) {
      nacho_warn(
        c(
          "The RCC files do not say which nCounter instrument made them, so the MAX/FLEX thresholds are used.",
          i = hint
        ),
        class = "instrument_unknown"
      )
      instrument <- "max"
    }
  }
  nacho_thresholds(instrument, preset)
}

bounds_problem <- function(name, value, signed = FALSE) {
  if (!is.numeric(value) || length(value) != 2 || anyNA(value)) {
    return(sprintf(
      "@thresholds$%s must be two numbers, a lower and an upper bound.",
      name
    ))
  }
  problem <- if (value[1] == Inf || value[2] == -Inf) {
    "-Inf is only allowed as the lower bound and Inf only as the upper bound."
  } else if (!signed && value[1] < 0 && value[1] != -Inf) {
    "the lower bound must not be negative; use -Inf for no lower bound."
  } else if (!signed && value[2] < 0) {
    "the upper bound must not be negative."
  } else if (value[1] > value[2]) {
    "the bounds must be increasing."
  }
  if (!is.null(problem)) sprintf("@thresholds$%s: %s", name, problem)
}

lower_bound_problem <- function(name, value, noun) {
  if (
    !is.numeric(value) || length(value) != 1 || is.na(value) || value == Inf
  ) {
    sprintf(
      "@thresholds$%s must be one number; Inf flags every sample, so set a finite %s, or -Inf for no bound.",
      name,
      noun
    )
  }
}

validate_thresholds <- function(thresholds) {
  required <- c(
    "preset",
    "instrument",
    "BD",
    "FoV",
    "LoD",
    "PCL",
    "Positive_factor",
    "House_factor",
    "Housekeeping_detected"
  )
  missing_names <- setdiff(required, names(thresholds))
  if (length(missing_names) > 0) {
    return(sprintf(
      "@thresholds lacks %s.",
      paste(missing_names, collapse = ", ")
    ))
  }
  problems <- character(0)
  if (
    !rlang::is_string(thresholds[["preset"]]) ||
      !thresholds[["preset"]] %in% nacho_presets
  ) {
    problems <- c(
      problems,
      "@thresholds$preset must be \"nsolver\" or \"legacy\"."
    )
  }
  instrument <- thresholds[["instrument"]]
  if (
    !is.character(instrument) ||
      length(instrument) != 1 ||
      !(is.na(instrument) || instrument %in% nacho_instruments)
  ) {
    problems <- c(
      problems,
      "@thresholds$instrument must be one of max, flex, pro, sprint, or NA."
    )
  }
  range_names <- c("BD", "Positive_factor", "House_factor")
  optional_ranges <- intersect(
    c("Ligation_NEG", "Haemolysis"),
    names(thresholds)
  )
  for (name in c(range_names, optional_ranges)) {
    problems <- c(
      problems,
      bounds_problem(
        name,
        thresholds[[name]],
        signed = name %in% optional_ranges
      )
    )
  }
  problems <- c(
    problems,
    lower_bound_problem("LoD", thresholds[["LoD"]], "LoD"),
    lower_bound_problem(
      "Housekeeping_detected",
      thresholds[["Housekeeping_detected"]],
      "count"
    )
  )
  limits <- list(FoV = c(0, 100), PCL = c(0, 1))
  optional_limits <- list(Ligation_order = c(0, 1), Ligation_R2 = c(0, 1))
  limits <- c(
    limits,
    optional_limits[names(optional_limits) %in% names(thresholds)]
  )
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

check_thresholds <- function(
  thresholds,
  arg = rlang::caller_arg(thresholds),
  call = rlang::caller_env()
) {
  if (is.list(thresholds) && !"preset" %in% names(thresholds)) {
    nacho_abort(
      c(
        "{.arg {arg}} has no {.field preset}, as thresholds written for NACHO 2 do.",
        i = "Start from {.code nacho_thresholds()} and change the elements you need."
      ),
      class = "bad_argument",
      call = call
    )
  }
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

#' Write and read quality-control thresholds
#'
#' `write_thresholds()` saves thresholds to a YAML file, and
#' `read_thresholds()` reads them back.
#' Use the file to apply the same quality-control policy to each study, or to
#' load thresholds that you tuned in [nacho_app()].
#' The Export page of the app writes the same file.
#'
#' In the file, `.inf` and `-.inf` are open bounds, and `.na.character` is an
#' unknown instrument.
#' You can edit the file by hand.
#' `read_thresholds()` reads whole numbers such as `80` as numbers, and checks
#' the thresholds as [normalise()] does.
#' If the file does not give a miRNA threshold (`Ligation_order`,
#' `Ligation_R2`, `Ligation_NEG` or `Haemolysis`), `read_thresholds()` uses the
#' value of the preset in the file.
#'
#' @param thresholds A list of thresholds, as returned by [nacho_thresholds()]
#'   or found in the `@thresholds` property of a `nacho` object.
#' @param file The path of the YAML file.
#'
#' @return `write_thresholds()` returns `file`, invisibly.
#'   `read_thresholds()` returns the list of thresholds.
#' @export
#' @examples
#' file <- tempfile(fileext = ".yml")
#' thresholds <- nacho_thresholds("sprint")
#' thresholds$FoV <- 80
#' write_thresholds(thresholds, file)
#' read_thresholds(file)
#' unlink(file)
write_thresholds <- function(thresholds, file) {
  writeLines(thresholds_yaml(thresholds), file, sep = "")
  invisible(file)
}

#' Write thresholds as YAML text
#'
#' @inheritParams write_thresholds
#'
#' @return One string, with a line break after each line.
#'
#' @noRd
thresholds_yaml <- function(thresholds) {
  check_thresholds(thresholds)
  yaml::as.yaml(
    thresholds,
    handlers = list(numeric = function(value) {
      structure(yaml_numbers(value), class = "verbatim")
    })
  )
}

#' @rdname write_thresholds
#' @export
read_thresholds <- function(file) {
  call <- rlang::current_env()
  fail <- function(problems) {
    problems <- gsub("([{}])", "\\1\\1", problems)
    nacho_abort(
      c(
        "{.file {basename(file)}} does not hold valid NACHO thresholds.",
        stats::setNames(problems, rep("x", length(problems))),
        i = "Write the file with {.fn write_thresholds}, or from the Export page of the app."
      ),
      class = "bad_thresholds",
      call = call
    )
  }
  if (!file.exists(file)) {
    fail("The file does not exist.")
  }
  thresholds <- tryCatch(
    yaml::read_yaml(file, eval.expr = FALSE),
    error = function(cnd) fail(conditionMessage(cnd))
  )
  if (!is.list(thresholds) || is.null(names(thresholds))) {
    fail("The file is not a list of named thresholds.")
  }
  thresholds[] <- lapply(thresholds, function(value) {
    if (is.integer(value)) as.double(value) else value
  })
  problems <- validate_thresholds(thresholds)
  if (length(problems) > 0) {
    fail(sub("^@thresholds", "thresholds", problems))
  }
  instrument <- thresholds[["instrument"]]
  preset <- nacho_thresholds(
    instrument = if (is.na(instrument)) "max" else instrument,
    preset = thresholds[["preset"]]
  )
  c(thresholds, preset[setdiff(names(preset), names(thresholds))])
}

#' Write numbers for YAML with the fewest digits that read back exactly
#'
#' The yaml package writes seven significant digits by default, so a bound
#' such as `1 / 11` would change in a write and a read.
#' Each number gets 15, 16 or 17 significant digits, the first that gives the
#' same double back.
#' A number without a decimal point gets `.0`, so YAML reads it as a float.
#'
#' @param value A double vector without `NA`.
#'
#' @return A character vector of YAML scalars.
#'
#' @noRd
yaml_numbers <- function(value) {
  vapply(
    value,
    function(number) {
      if (is.infinite(number)) {
        return(if (number > 0) ".inf" else "-.inf")
      }
      for (digits in 15:17) {
        text <- trimws(formatC(number, digits = digits, format = "g"))
        if (identical(as.numeric(text), number)) {
          break
        }
      }
      if (!grepl(".", text, fixed = TRUE)) {
        text <- sub("^(-?[0-9]+)", "\\1.0", text)
      }
      text
    },
    character(1)
  )
}
