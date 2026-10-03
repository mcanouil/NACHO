#' NACHO brand colours
#'
#' The same values as `inst/brand/_brand.yml`; a test keeps them in step.
#'
#' @noRd
nacho_palette <- c(
  navy = "#182430",
  night = "#111821",
  rust = "#B64326",
  amber = "#FCB448",
  yellow = "#F0D83C",
  orange = "#E45430",
  rose = "#D85460"
)

#' Path of a file in the NACHO brand directory
#'
#' @param ... Path components under `inst/brand/`.
#'
#' @return The full path of the file.
#'
#' @noRd
brand_path <- function(...) {
  path <- system.file("brand", ..., package = "NACHO")
  if (!nzchar(path)) {
    nacho_abort(
      "The brand file {.file {file.path('brand', ...)}} is missing from the installed package.",
      class = "missing_file"
    )
  }
  path
}

#' Ink, paper and accent colours of a plot
#'
#' @param dark If `TRUE`, the colours for a dark background.
#'
#' @noRd
plot_colours <- function(dark) {
  if (dark) {
    list(
      ink = "#FFFFFF",
      paper = nacho_palette[["night"]],
      accent = nacho_palette[["amber"]]
    )
  } else {
    list(
      ink = nacho_palette[["navy"]],
      paper = "#FFFFFF",
      accent = nacho_palette[["rust"]]
    )
  }
}

#' Colours for groups of samples
#'
#' Okabe-Ito up to eight groups, without its black on a dark background, then
#' viridis without its end that is too close to the background.
#'
#' @inheritParams plot_colours
#'
#' @return A function of the number of groups.
#'
#' @noRd
group_palette <- function(dark) {
  okabe_ito <- unname(grDevices::palette.colors(8, palette = "Okabe-Ito"))
  colours <- if (dark) okabe_ito[-1] else okabe_ito
  viridis <- if (dark) {
    scales::pal_viridis(begin = 0.25)
  } else {
    scales::pal_viridis(end = 0.85)
  }
  function(n) {
    if (n <= length(colours)) colours[seq_len(n)] else viridis(n)
  }
}

#' The NACHO plot theme
#'
#' @inheritParams plot_colours
#' @param base_size The base font size, in points.
#'
#' @noRd
theme_nacho <- function(dark = FALSE, base_size = 11) {
  check_bool(dark)
  colours <- plot_colours(dark)
  palette <- group_palette(dark)
  ggplot2::theme_minimal(
    base_size = base_size,
    ink = colours[["ink"]],
    paper = colours[["paper"]],
    accent = colours[["accent"]]
  ) +
    ggplot2::theme(
      palette.colour.discrete = palette,
      palette.fill.discrete = palette
    )
}
