nacho_instruments <- c("max", "flex", "pro", "sprint")
nacho_presets <- c("nsolver", "legacy")

#' Quality-control thresholds
#'
#' Builds the thresholds that [load_rcc()] and [normalise()] use to flag
#' samples.
#'
#' `"nsolver"` follows the current Bruker nCounter knowledge base
#' (<https://nanostring.com/support/>, "nCounter QC flags"): binding density
#' 0.05 to 2.25 on MAX, FLEX and PRO and 0.1 to 1.8 on SPRINT, field of view at
#' least 75 %, positive control linearity at least 0.95 (POS_A to POS_E, with
#' `log2(count + 1)`), limit of detection at least 2, positive factor 0.3 to 3
#' and housekeeping factor 0.1 to 10.
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
#' Each element can be changed afterwards; two numbers are a lower and an
#' upper bound, and one number is a lower bound. Use `-Inf` or `Inf` for an
#' open bound.
#'
#' @param instrument The nCounter instrument: `"max"`, `"flex"`, `"pro"` or
#'   `"sprint"`.
#' @param preset `"nsolver"` or `"legacy"`.
#'
#' @return A named list: `preset`, `instrument`, then one element per metric.
#' @export
#' @examples
#' nacho_thresholds()
#' nacho_thresholds("sprint")
#' nacho_thresholds(preset = "legacy")
nacho_thresholds <- function(
  instrument = c("max", "flex", "pro", "sprint"),
  preset = c("nsolver", "legacy")
) {
  instrument <- check_choice(instrument, nacho_instruments)
  preset <- check_choice(preset, nacho_presets)
  limits <- if (preset == "legacy") {
    list(
      BD = c(0.1, 2.25),
      FoV = 75,
      PCL = 0.95,
      LoD = 2,
      Positive_factor = c(1 / 4, 4),
      House_factor = c(1 / 11, 11)
    )
  } else {
    list(
      BD = if (instrument == "sprint") c(0.1, 1.8) else c(0.05, 2.25),
      FoV = 75,
      PCL = 0.95,
      LoD = 2,
      Positive_factor = c(0.3, 3),
      House_factor = c(0.1, 10)
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
  required <- c(
    "preset",
    "instrument",
    "BD",
    "FoV",
    "LoD",
    "PCL",
    "Positive_factor",
    "House_factor"
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
