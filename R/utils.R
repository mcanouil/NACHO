#' autoplot generic
#'
#' See \code{ggplot2::\link[ggplot2]{autoplot}} for details.
#'
#' @name autoplot
#' @rdname autoplot
#' @keywords internal
#' @export
#' @importFrom ggplot2 autoplot
NULL

#' Log-10 transform that keeps infinite values
#'
#' Maps `-Inf` and `Inf` to themselves, so shaded bands can extend
#' to the panel edges on a log-10 axis without triggering a warning.
#'
#' @keywords internal
#' @noRd
#'
#' @return A `transform` object from [scales::new_transform()].
transform_log10_infinite <- function() {
  scales::new_transform(
    name = "log-10-infinite",
    transform = function(x) {
      finite <- !is.infinite(x)
      x[finite] <- log10(x[finite])
      x
    },
    inverse = function(x) {
      finite <- !is.infinite(x)
      x[finite] <- 10^x[finite]
      x
    },
    breaks = scales::breaks_log(base = 10),
    domain = c(1e-100, Inf)
  )
}

#' Path to the NACHO logo
#'
#' Installed packages keep `man/figures` under `help/figures`, while a source
#' tree loaded with `pkgload::load_all()` keeps it under `man/figures`.
#'
#' @keywords internal
#' @noRd
#'
#' @return A `character` path to `nacho_hex.png`.
logo_path <- function() {
  path <- system.file("help", "figures", "nacho_hex.png", package = "NACHO")
  if (nzchar(path)) {
    return(path)
  }
  system.file("man", "figures", "nacho_hex.png", package = "NACHO")
}
