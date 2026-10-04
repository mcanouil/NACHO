test_that("qc_failures() lists failing samples with their reasons", {
  x <- flagged_gse()
  failures <- NACHO:::qc_failures(x)
  expect_identical(
    names(failures),
    c("IDFILE", "lane", "CartridgeID", "n_flags", "reason")
  )
  expect_identical(nrow(failures), sum(NACHO:::flagged_samples(x)))
  expect_match(failures$reason, "^FoV .* below 99.9$")
  expect_identical(nrow(NACHO:::qc_failures(GSE74821)), 0L)
})

test_that("the overview counts samples, cartridges and flags", {
  lines <- NACHO:::report_overview(flagged_gse())
  expect_identical(lines[1], "- Samples: 12")
  expect_match(lines[3], "^- Flagged samples: [1-9]")
  expect_match(lines[4], "^- Thresholds: nsolver preset, instrument ")
  expect_match(lines[5], "^- Normalisation: GLM, background none$")
})

test_that("each failing sample gets one callout", {
  x <- flagged_gse()
  lines <- NACHO:::report_callouts(x)
  expect_identical(
    sum(lines == "::: {.callout-warning}"),
    sum(NACHO:::flagged_samples(x))
  )
  expect_match(lines[2], "^## `GSM")
  expect_identical(
    NACHO:::report_callouts(GSE74821),
    "No sample fails a quality-control threshold."
  )
})

test_that("thresholds leave out bounds that never flag", {
  x <- toy_nacho(4L)
  thresholds <- x@thresholds
  thresholds$BD <- c(-Inf, 2.25)
  thresholds$House_factor <- c(1 / 11, Inf)
  thresholds$LoD <- -Inf
  thresholds$PCL <- 0
  x@thresholds <- thresholds
  lines <- NACHO:::report_thresholds(x)
  expect_false(any(grepl("Inf", lines, fixed = TRUE)))
  expect_true("- Binding density (`BD`): at most 2.25" %in% lines)
  expect_true(
    "- Content normalisation factor (`House_factor`): at least 0.0909" %in%
      lines
  )
  expect_false(any(grepl("`LoD`", lines, fixed = TRUE)))
  expect_false(any(grepl("`PCL`", lines, fixed = TRUE)))
})

test_that("sections follow the object", {
  plots <- NACHO:::report_sections(plexset_nacho)$plot
  expect_false(any(c("PCL", "LoD", "HF", "Stability") %in% plots))
  expect_true(all(c("BD", "FoV", "NORM") %in% plots))

  gse <- NACHO:::report_sections(GSE74821)
  expect_true(all(c("PCL", "LoD", "HF", "Stability") %in% gse$plot))
  expect_false(is.na(gse$alt[gse$plot %in% "BD"]))
  expect_true(all(!is.na(gse$alt[!is.na(gse$plot)])))

  ruv <- normalise(GSE74821, normalisation_method = "RUVg", ruv_k = 1)
  expect_false("HF" %in% NACHO:::report_sections(ruv)$plot)
})

test_that("the batch tables put the design first and flag confounding", {
  skip_if_not_installed("knitr")
  lines <- NACHO:::report_batch_tables(GSE74821, group = "tissue type:ch1")
  expect_identical(lines[1], "::: {.callout-important}")
  design <- grep("confounded", lines, fixed = TRUE)[1]
  crosstab <- grep("Groups by `CartridgeID`", lines, fixed = TRUE)
  expect_lt(design, crosstab)
})

test_that("every plot type has alt text", {
  expect_setequal(
    names(NACHO:::plot_alt_texts),
    names(NACHO:::nacho_plot_registry)
  )
  expect_true(all(nzchar(NACHO:::plot_alt_texts)))
})

test_that("report options are checked", {
  expect_error(
    NACHO:::check_report_options(GSE74821, colour = "nope"),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    NACHO:::check_report_options(GSE74821, group = "nope"),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    NACHO:::check_report_options(GSE74821, show_legend = "yes"),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    NACHO:::check_report_options(GSE74821, size = -1),
    "size",
    class = "nacho_error_bad_argument"
  )
  expect_error(
    NACHO:::check_report_options(GSE74821, outliers_factor = 0),
    "outliers_factor",
    class = "nacho_error_bad_argument"
  )
  options <- NACHO:::check_report_options(GSE74821)
  expect_identical(options$colour, "CartridgeID")
  expect_null(options$group)
})

test_that("report_setup() reads and checks what render() saves", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(
    list(object = GSE74821, options = NACHO:::check_report_options(GSE74821)),
    path
  )
  report <- NACHO:::report_setup(path)
  expect_true(S7::S7_inherits(report$object, NACHO:::nacho))
  expect_s3_class(report$sections, "data.frame")
  saveRDS(list(object = iris, options = list()), path)
  expect_error(NACHO:::report_setup(path), class = "nacho_error_bad_object")
})

test_that("report_setup() names the file it cannot use", {
  missing <- file.path(withr::local_tempdir(), "missing.rds")
  expect_error(
    NACHO:::report_setup(missing),
    "missing.rds",
    class = "nacho_error_bad_object"
  )
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS("text", path)
  expect_error(
    NACHO:::report_setup(path),
    basename(path),
    class = "nacho_error_bad_object"
  )
  saveRDS(list(options = list()), path)
  expect_error(
    NACHO:::report_setup(path),
    basename(path),
    class = "nacho_error_bad_object"
  )
})

test_that("report_setup() drops option names it does not know", {
  path <- withr::local_tempfile(fileext = ".rds")
  options <- c(NACHO:::check_report_options(GSE74821), list(extra = 1))
  saveRDS(list(object = GSE74821, options = options), path)
  expect_no_error(report <- NACHO:::report_setup(path))
  expect_false("extra" %in% names(report$options))
})

test_that("thresholds never print a bound at the top of the field of view", {
  x <- toy_nacho(4L)
  thresholds <- x@thresholds
  thresholds$FoV <- c(75, 100)
  S7::prop(x, "thresholds", check = FALSE) <- thresholds
  lines <- NACHO:::report_thresholds(x)
  expect_true("- Field of view (`FoV`): at least 75" %in% lines)
  expect_false(any(grepl("100", lines[grepl("`FoV`", lines)])))
})

test_that("thresholds with two bounds read as a range", {
  x <- toy_nacho(4L)
  thresholds <- x@thresholds
  thresholds$BD <- c(0.1, 2.25)
  x@thresholds <- thresholds
  expect_true(
    "- Binding density (`BD`): 0.1 to 2.25" %in% NACHO:::report_thresholds(x)
  )
})

test_that("only the batch section gets the batch tables", {
  sections <- NACHO:::report_sections(GSE74821)
  expect_type(sections$batch, "logical")
  expect_identical(sections$title[sections$batch], "Batch effects")
})

test_that("report_body() prints headings, help and plots", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(
    list(object = GSE74821, options = NACHO:::check_report_options(GSE74821)),
    path
  )
  report <- NACHO:::report_setup(path)
  output <- utils::capture.output(NACHO:::report_body(report))
  expect_true("# Quality-control metrics" %in% output)
  expect_true("## Binding density" %in% output)
})

test_that("the batch tables use the batch columns the samples have", {
  skip_if_not_installed("knitr")
  x <- toy_nacho(6)
  x@samples[["tissue"]] <- rep(c("a", "b", "c"), 2)
  lines <- NACHO:::report_batch_tables(x, group = "tissue")
  expect_true(any(grepl("Groups by `CartridgeID`", lines, fixed = TRUE)))
  expect_false(any(grepl("`Date`", lines, fixed = TRUE)))

  local_mocked_bindings(
    nacho_samples = function(x) data.frame(IDFILE = "a", tissue = "a")
  )
  lines <- NACHO:::report_batch_tables(x, group = "tissue")
  expect_match(lines[1], "no batch design")
})

test_that("report_body() explains a plot that cannot be drawn", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  x <- toy_nacho()
  report <- list(
    object = x,
    options = NACHO:::check_report_options(x),
    sections = data.frame(
      title = "Stability",
      level = 2,
      plot = "Stability",
      help = NA_character_,
      batch = FALSE,
      alt = "Stability"
    )
  )
  expect_no_warning(
    output <- utils::capture.output(NACHO:::report_body(report))
  )
  expect_true(any(grepl("Stability cannot be drawn", output, fixed = TRUE)))
})

test_that("report_body() passes the report options to the plots", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  x <- flagged_gse()
  x@samples[["batch \"a\""]] <- rep(c("u", "v"), length.out = nrow(x@samples))
  options <- NACHO:::check_report_options(
    x,
    colour = "batch \"a\"",
    outliers_labels = "IDFILE"
  )
  report <- list(
    object = x,
    options = options,
    sections = data.frame(
      title = "Binding density",
      level = 2,
      plot = "BD",
      help = NA_character_,
      batch = FALSE,
      alt = "BD"
    )
  )
  plots <- list()
  real_autoplot <- autoplot
  local_mocked_bindings(
    autoplot = function(...) {
      plots[[length(plots) + 1L]] <<- real_autoplot(...)
      plots[[length(plots)]]
    }
  )
  utils::capture.output(NACHO:::report_body(report))
  expect_length(plots, 1)
  mappings <- c(
    list(plots[[1]]$mapping),
    lapply(plots[[1]]$layers, function(l) l$mapping)
  )
  colours <- vapply(
    mappings,
    function(m) if (is.null(m$colour)) "" else rlang::as_label(m$colour),
    character(1)
  )
  expect_true(any(grepl("batch \"a\"", colours, fixed = TRUE)))
  geoms <- vapply(plots[[1]]$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(geoms %in% c("GeomTextRepel", "GeomLabelRepel", "GeomText")))
})

test_that("report_body() puts the confounding callout before the design table", {
  skip_if_not_installed("knitr")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  path <- withr::local_tempfile(fileext = ".rds")
  options <- NACHO:::check_report_options(
    GSE74821,
    group = "tissue type:ch1"
  )
  saveRDS(list(object = GSE74821, options = options), path)
  output <- utils::capture.output(
    NACHO:::report_body(NACHO:::report_setup(path))
  )
  batch <- which(output == "# Batch effects")
  callout <- grep("Batch and biology are confounded", output, fixed = TRUE)
  table <- grep("Groups by `CartridgeID`", output, fixed = TRUE)
  expect_length(batch, 1)
  expect_true(batch < callout[1] && callout[1] < table[1])
})

test_that("the report template is found, or the error says it is missing", {
  expect_true(file.exists(NACHO:::report_template_path()))
  expect_error(
    NACHO:::report_template_path("no-such-file.qmd"),
    class = "nacho_error_missing_file"
  )
})

test_that("thresholds leave out PCL and LoD for PlexSet data", {
  lines <- NACHO:::report_thresholds(plexset_nacho)
  expect_false(any(grepl("`PCL`|`LoD`", lines)))
  expect_true(any(grepl("`BD`", lines)))
})

test_that("thresholds name the lower bound first whatever the order", {
  x <- toy_nacho(4L)
  thresholds <- x@thresholds
  thresholds$BD <- c(2.25, 0.1)
  S7::prop(x, "thresholds", check = FALSE) <- thresholds
  expect_true(
    "- Binding density (`BD`): 0.1 to 2.25" %in% NACHO:::report_thresholds(x)
  )
})

test_that("a threshold of 0 on the housekeeping count is not printed", {
  x <- toy_nacho(4L)
  x@samples[["Housekeeping_detected"]] <- 1L
  thresholds <- x@thresholds
  thresholds[["Housekeeping_detected"]] <- 0
  x@thresholds <- thresholds
  expect_false(any(grepl(
    "Housekeeping_detected",
    NACHO:::report_thresholds(x),
    fixed = TRUE
  )))
})

test_that("report_settings() lists the settings that shape the data", {
  x <- toy_nacho(4L)
  lines <- NACHO:::report_settings(x)
  expect_true(any(grepl("Housekeeping genes: HK1", lines, fixed = TRUE)))
  expect_true(any(grepl("Principal components: 2", lines, fixed = TRUE)))
  expect_false(any(grepl("RUV", lines)))

  x@settings[["housekeeping_genes"]] <- NULL
  expect_true(any(grepl(
    "Housekeeping genes: none",
    NACHO:::report_settings(x),
    fixed = TRUE
  )))

  x@settings[["normalisation_method"]] <- "RUVg"
  x@settings[["ruv_k"]] <- 2L
  expect_true(any(grepl("RUV factors: 2", NACHO:::report_settings(x))))
})
