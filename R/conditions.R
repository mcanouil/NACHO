#' @include nacho-class.R
NULL

#' Raise a classed NACHO error
#'
#' @param message A cli message, a character vector with optional `i`, `x`
#'   and `*` names for bullets.
#' @param class The specific class, without the `nacho_error_` prefix.
#' @param ... Passed to [cli::cli_abort()].
#' @param call The call reported in the error, the user-facing function.
#' @param .envir The environment where cli interpolates `{}` expressions.
#'
#' @keywords internal
#' @noRd
nacho_abort <- function(
  message,
  class = NULL,
  ...,
  call = rlang::caller_env(),
  .envir = parent.frame()
) {
  cli::cli_abort(
    message,
    class = c(
      if (!is.null(class)) paste0("nacho_error_", class),
      "nacho_error"
    ),
    ...,
    call = call,
    .envir = .envir
  )
}

#' Raise a classed NACHO warning
#'
#' @inheritParams nacho_abort
#' @param class The specific class, without the `nacho_warning_` prefix.
#'
#' @keywords internal
#' @noRd
nacho_warn <- function(message, class = NULL, ..., .envir = parent.frame()) {
  cli::cli_warn(
    message,
    class = c(
      if (!is.null(class)) paste0("nacho_warning_", class),
      "nacho_warning"
    ),
    ...,
    .envir = .envir
  )
}

#' Tell whether NACHO should stay quiet
#'
#' @keywords internal
#' @noRd
nacho_is_quiet <- function() {
  isTRUE(getOption("nacho.quiet")) ||
    identical(getOption("rlib_message_verbosity"), "quiet")
}

#' Send an informative NACHO message
#'
#' @inheritParams nacho_abort
#'
#' @keywords internal
#' @noRd
nacho_inform <- function(message, ..., .envir = parent.frame()) {
  if (nacho_is_quiet()) {
    return(invisible(NULL))
  }
  cli::cli_inform(message, class = "nacho_message", ..., .envir = .envir)
}

#' Report a stage of a long computation
#'
#' The step closes when the calling function exits.
#'
#' @inheritParams nacho_abort
#'
#' @keywords internal
#' @noRd
nacho_progress_step <- function(message, .envir = parent.frame()) {
  if (nacho_is_quiet()) {
    return(invisible(NULL))
  }
  cli::cli_progress_step(message, .envir = .envir)
}

check_bool <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!rlang::is_bool(x)) {
    nacho_abort(
      "{.arg {arg}} must be {.code TRUE} or {.code FALSE}, not {.obj_type_friendly {x}}.",
      class = "bad_argument",
      call = call
    )
  }
  invisible(x)
}

check_string <- function(
  x,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }
  if (!rlang::is_string(x) || !nzchar(x)) {
    nacho_abort(
      "{.arg {arg}} must be a single non-empty string, not {.obj_type_friendly {x}}.",
      class = "bad_argument",
      call = call
    )
  }
  invisible(x)
}

check_character <- function(
  x,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }
  if (!is.character(x) || length(x) == 0 || anyNA(x)) {
    nacho_abort(
      "{.arg {arg}} must be a character vector without missing values, not {.obj_type_friendly {x}}.",
      class = "bad_argument",
      call = call
    )
  }
  invisible(x)
}

check_count <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!rlang::is_scalar_integerish(x, finite = TRUE) || x < 1) {
    nacho_abort(
      "{.arg {arg}} must be a whole number of at least 1, not {.obj_type_friendly {x}}.",
      class = "bad_argument",
      call = call
    )
  }
  invisible(x)
}

check_choice <- function(
  x,
  values,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  rlang::try_fetch(
    rlang::arg_match(x, values, error_arg = arg, error_call = call),
    error = function(cnd) {
      hint <- unname(rlang::cnd_body(cnd))
      nacho_abort(
        c(
          if (rlang::is_string(x)) {
            "{.arg {arg}} must be one of {.or {.val {values}}}, not {.val {x}}."
          } else {
            "{.arg {arg}} must be one of {.or {.val {values}}}, not {.obj_type_friendly {x}}."
          },
          rlang::set_names(hint, rep("i", length(hint)))
        ),
        class = "bad_argument",
        call = call
      )
    }
  )
}

check_column <- function(
  column,
  data,
  data_arg = rlang::caller_arg(data),
  arg = rlang::caller_arg(column),
  call = rlang::caller_env()
) {
  if (!column %in% names(data)) {
    nacho_abort(
      c(
        "{.arg {arg}} must name a column of {.arg {data_arg}}.",
        x = "There is no column {.field {column}}.",
        i = "Available columns: {.field {utils::head(names(data), 10)}}{if (length(names(data)) > 10) ', ...'}."
      ),
      class = "bad_argument",
      call = call
    )
  }
  invisible(column)
}

check_interactive <- function(fn, call = rlang::caller_env()) {
  if (!rlang::is_interactive()) {
    nacho_abort(
      c(
        "{.fn {fn}} needs an interactive R session.",
        i = "Run it from the console, not from a script or {.code Rscript}."
      ),
      class = "not_interactive",
      call = call
    )
  }
  invisible(TRUE)
}

check_package <- function(
  package,
  reason,
  install = sprintf('install.packages("%s")', package),
  call = rlang::caller_env()
) {
  if (!requireNamespace(package, quietly = TRUE)) {
    nacho_abort(
      c(
        "The {.pkg {package}} package is needed {reason}.",
        i = "Install it with {.code {install}}."
      ),
      class = "missing_package",
      call = call
    )
  }
  invisible(TRUE)
}

#' Tell whether an object is a NACHO 2 list
#'
#' @keywords internal
#' @noRd
is_nacho_v2 <- function(x) {
  is.list(x) &&
    inherits(x, "nacho") &&
    !S7::S7_inherits(x) &&
    "nacho" %in% names(x)
}

#' Check that an object is a "nacho" S7 object
#'
#' @inheritParams check_bool
#'
#' @keywords internal
#' @noRd
check_nacho <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (missing(x)) {
    nacho_abort(
      c(
        "{.arg {arg}} is missing.",
        i = "Create a {.cls nacho} object with {.fn load_rcc}."
      ),
      class = "bad_object",
      call = call
    )
  }
  if (is_nacho_v2(x)) {
    nacho_abort(
      c(
        "{.arg {arg}} is a NACHO 2 object, which NACHO 3 cannot use.",
        i = "Convert it with {.code upgrade_nacho({arg})}, or read the saved file with {.fn read_nacho}."
      ),
      class = "bad_object",
      call = call
    )
  }
  if (inherits(x, "nacho") && !S7::S7_inherits(x)) {
    nacho_abort(
      c(
        "{.arg {arg}} has the NACHO 2 class {.cls nacho}, but not the NACHO 2 data.",
        i = "Create a NACHO 3 object with {.fn load_rcc}."
      ),
      class = "bad_object",
      call = call
    )
  }
  if (!S7::S7_inherits(x, nacho)) {
    nacho_abort(
      c(
        "{.arg {arg}} must be a {.cls nacho} object, not {.obj_type_friendly {x}}.",
        i = "Create one with {.fn load_rcc}."
      ),
      class = "bad_object",
      call = call
    )
  }
  if (!identical(x@provenance[["schema_version"]], nacho_schema_version)) {
    nacho_abort(
      c(
        "{.arg {arg}} was made by another version of NACHO.",
        i = "Read the saved file with {.fn read_nacho}, which rebuilds it."
      ),
      class = "bad_object",
      call = call
    )
  }
  invisible(x)
}
