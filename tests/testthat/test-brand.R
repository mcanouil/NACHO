test_that("the brand file holds the NACHO palette", {
  skip_if_not_installed("brand.yml")
  brand <- brand.yml::read_brand_yml(NACHO:::brand_path("_brand.yml"))
  expect_identical(
    unlist(brand$color$palette)[names(NACHO:::nacho_palette)],
    NACHO:::nacho_palette
  )
  expect_identical(brand.yml::brand_color_pluck(brand, "primary"), "#B64326")
  expect_identical(
    brand.yml::brand_color_pluck(brand, "foreground"),
    "#182430"
  )
})

test_that("every font the brand names ships as a TrueType file", {
  skip_if_not_installed("brand.yml")
  brand <- brand.yml::read_brand_yml(NACHO:::brand_path("_brand.yml"))
  paths <- unlist(lapply(brand$typography$fonts, function(font) {
    vapply(font$files, function(file) file$path, character(1))
  }))
  expect_length(paths, 5L)
  for (path in paths) {
    file <- NACHO:::brand_path(path)
    magic <- readBin(file, "raw", n = 4L)
    expect_identical(magic, as.raw(c(0x00, 0x01, 0x00, 0x00)), info = path)
  }
})

test_that("brand_path() names a missing brand file", {
  expect_error(
    NACHO:::brand_path("nope.yml"),
    class = "nacho_error_missing_file"
  )
  expect_true(file.exists(NACHO:::brand_path("nacho_hex.png")))
})

test_that("only the pairs the spec allows carry text", {
  skip_if_not_installed("colorspace")
  palette <- NACHO:::nacho_palette
  ratio <- function(fg, bg) colorspace::contrast_ratio(fg, bg)
  expect_gte(ratio("#FFFFFF", palette[["rust"]]), 4.5)
  expect_gte(ratio(palette[["navy"]], "#FFFFFF"), 4.5)
  expect_gte(ratio(palette[["amber"]], palette[["night"]]), 4.5)
  expect_gte(ratio(palette[["yellow"]], palette[["night"]]), 4.5)
  expect_gte(ratio("#FFFFFF", palette[["night"]]), 4.5)
  expect_gte(ratio(palette[["night"]], palette[["amber"]]), 4.5)
  expect_lt(ratio(palette[["rust"]], palette[["night"]]), 4.5)
  expect_lt(ratio(palette[["navy"]], palette[["rose"]]), 4.5)
})

test_that("plot colours follow light and dark mode", {
  light <- NACHO:::plot_colours(FALSE)
  dark <- NACHO:::plot_colours(TRUE)
  expect_identical(
    light,
    list(ink = "#182430", paper = "#FFFFFF", accent = "#B64326")
  )
  expect_identical(
    dark,
    list(ink = "#FFFFFF", paper = "#111821", accent = "#FCB448")
  )
})

test_that("groups use Okabe-Ito, without black in dark mode", {
  light <- NACHO:::group_palette(FALSE)
  dark <- NACHO:::group_palette(TRUE)
  expect_identical(light(3), c("#000000", "#E69F00", "#56B4E9"))
  expect_identical(dark(3), c("#E69F00", "#56B4E9", "#009E73"))
  expect_length(light(8), 8L)
  expect_false("#000000" %in% dark(7))
})

test_that("group palette beyond eight levels moves to viridis", {
  expect_identical(
    NACHO:::group_palette(FALSE)(9),
    scales::pal_viridis(end = 0.85)(9)
  )
  expect_identical(
    NACHO:::group_palette(TRUE)(8),
    scales::pal_viridis(begin = 0.25)(8)
  )
})

test_that("theme_nacho() sets paper, ink and the group palette", {
  data <- data.frame(x = 1:2, y = 1:2, g = c("a", "b"))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, colour = g)) +
    ggplot2::geom_point() +
    NACHO:::theme_nacho(dark = TRUE)
  expect_identical(
    unique(ggplot2::layer_data(plot)$colour),
    c("#E69F00", "#56B4E9")
  )
  theme <- ggplot2::complete_theme(plot$theme)
  expect_identical(
    ggplot2::calc_element("plot.background", theme)$fill,
    "#111821"
  )
  expect_identical(ggplot2::calc_element("text", theme)$colour, "#FFFFFF")
  expect_error(
    NACHO:::theme_nacho(dark = "yes"),
    class = "nacho_error_bad_argument"
  )
})

test_that("nacho_theme() brands Bootstrap and adds dark-mode rules", {
  theme <- NACHO:::nacho_theme()
  expect_true(bslib::is_bs_theme(theme))
  expect_identical(bslib::theme_version(theme), "5")
  expect_identical(unname(bslib::bs_get_variables(theme, "primary")), "#B64326")
  dependencies <- bslib::bs_theme_dependencies(theme)
  names <- vapply(dependencies, function(d) d$name, character(1))
  expect_true(any(grepl("Source_Sans_3", names, fixed = TRUE)))
  bootstrap <- dependencies[[which(names == "bootstrap")]]
  css <- paste(
    readLines(
      file.path(bootstrap$src$file, bootstrap$stylesheet),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(css, "data-bs-theme=.?dark")
  expect_match(css, "--bs-primary: ?#fcb448", ignore.case = TRUE)
  for (selector in c(".form-check-input:checked", ".text-primary")) {
    dark_rule <- paste0(
      "data-bs-theme=.?dark.?\\]\\s+",
      gsub(".", "\\.", selector, fixed = TRUE),
      "\\s*\\{[^}]*#fcb448"
    )
    expect_match(css, dark_rule, ignore.case = TRUE, info = selector)
  }
})

test_that("dark.scss uses only colours of the NACHO palette", {
  scss <- paste(
    readLines(NACHO:::brand_path("dark.scss"), warn = FALSE),
    collapse = "\n"
  )
  literals <- toupper(regmatches(
    scss,
    gregexpr("#[0-9A-Fa-f]{6}\\b", scss)
  )[[1]])
  expect_gt(length(literals), 0L)
  expect_true(all(literals %in% toupper(NACHO:::nacho_palette)))
})

test_that("the theme sets quoted font families that the browser can parse", {
  deps <- bslib::bs_theme_dependencies(NACHO:::nacho_theme())
  bootstrap <- Filter(function(d) d$name == "bootstrap", deps)[[1]]
  css <- paste(
    readLines(file.path(bootstrap$src$file, bootstrap$stylesheet[[1]])),
    collapse = "\n"
  )
  expect_match(
    css,
    '--bs-body-font-family: "Source Sans 3", system-ui, sans-serif;',
    fixed = TRUE
  )
  expect_match(css, '--bs-font-monospace: "JetBrains Mono"', fixed = TRUE)
})

test_that("the brand font faces point at files in the dependency", {
  dep <- NACHO:::brand_font_dependency()
  css <- readLines(file.path(dep$src$file, dep$stylesheet))
  files <- sub(
    ".*url\\('([^']+)'\\).*",
    "\\1",
    grep("url\\(", css, value = TRUE)
  )
  expect_length(files, 3L)
  expect_true(all(file.exists(file.path(dep$src$file, files))))
})
