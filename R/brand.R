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
#' Okabe-Ito up to eight groups on a light background and seven on a dark one,
#' where its black is left out, then viridis without its end that is too close
#' to the background.
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

#' Bold and italic faces of the brand font
#'
#' `bslib::bs_theme(brand = )` embeds only the first file of each font family,
#' so the other faces of Source Sans 3 come from `inst/brand/fonts/fonts.css`.
#'
#' @noRd
brand_font_dependency <- function() {
  htmltools::htmlDependency(
    name = "nacho-fonts",
    version = as.character(utils::packageVersion("NACHO")),
    src = c(file = brand_path("fonts")),
    stylesheet = "fonts.css"
  )
}

summary_rules <- paste(
  ".nacho-summary {",
  "  display: flex;",
  "  flex-wrap: wrap;",
  "  gap: 0.25rem 1.5rem;",
  "  align-items: center;",
  "  padding: 0.5rem 0.75rem;",
  "  margin-bottom: 1rem;",
  "  border: 1px solid var(--bs-border-color);",
  "  border-radius: var(--bs-border-radius);",
  "  background: var(--bs-body-bg);",
  "}",
  ".nacho-summary-item { display: flex; align-items: center; gap: 0.4rem; }",
  ".nacho-summary-item > i, .nacho-summary-item > svg { color: var(--bs-primary); }",
  ".nacho-summary-info { color: var(--bs-primary); }",
  ".nacho-summary-info:focus-visible {",
  "  outline: 2px solid var(--bs-focus-ring-color, var(--bs-primary));",
  "  outline-offset: 2px;",
  "}",
  ".nacho-summary-label { color: var(--bs-secondary-color); }",
  ".nacho-summary-value { font-weight: 700; }",
  sep = "\n"
)

popover_rules <- paste(
  ".popover.nacho-help {",
  "  --bs-popover-max-width: min(40rem, 90vw);",
  "}",
  ".popover.nacho-help .popover-body {",
  "  max-height: 70vh;",
  "  overflow-y: auto;",
  "}",
  sep = "\n"
)

css_font_family <- function(family, fallback) {
  paste0('"', family, '", ', fallback)
}

#' The NACHO Bootstrap theme
#'
#' Bootstrap 5 with the brand colours and fonts, and the dark-mode rules of
#' `inst/brand/dark.scss`, which `bslib::bs_theme(brand = )` cannot set.
#'
#' @noRd
nacho_theme <- function() {
  brand <- brand.yml::read_brand_yml(brand_path("_brand.yml"))
  dark_rules <- paste(
    readLines(brand_path("dark.scss"), warn = FALSE),
    collapse = "\n"
  )
  typography <- brand$typography
  bslib::bs_theme(version = 5, brand = brand) |>
    bslib::bs_theme_update(
      "font-family-base" = css_font_family(
        typography$base$family,
        "system-ui, sans-serif"
      ),
      "headings-font-family" = css_font_family(
        typography$headings$family,
        "system-ui, sans-serif"
      ),
      "font-family-monospace" = css_font_family(
        typography$monospace$family,
        "ui-monospace, monospace"
      )
    ) |>
    bslib::bs_add_rules(c(summary_rules, popover_rules, dark_rules))
}
