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

test_that("thresholds leave out bounds that never flag", {
  x <- toy_nacho(4L)
  thresholds <- x@thresholds
  thresholds$BD <- c(-Inf, 2.25)
  thresholds$House_factor <- c(1 / 11, Inf)
  thresholds$LoD <- -Inf
  thresholds$PCL <- 0
  x@thresholds <- thresholds
  limits <- NACHO:::report_limits(x)
  expect_false(any(grepl("Inf", limits, fixed = TRUE)))
  expect_identical(limits[["BD"]], "at most 2.25")
  expect_identical(limits[["House_factor"]], "at least 0.0909")
  expect_false(any(c("LoD", "PCL") %in% names(limits)))
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
  x <- GSE74821
  x@samples[["grp"]] <- x@samples[["CartridgeID"]]
  lines <- NACHO:::report_batch_tables(x, group = "grp")
  expect_identical(
    lines[1:2],
    c("::: {.callout-important}", "## Batch and biology are confounded")
  )
  expect_false(any(grepl("normalis", lines, fixed = TRUE)))
  design <- grep(": Batch design", lines, fixed = TRUE)
  crosstab <- grep("Groups by `CartridgeID`", lines, fixed = TRUE)
  expect_lt(design, crosstab)
  expect_identical(
    utils::tail(lines[nzchar(lines)], 4)[1:2],
    c("::: {.callout-note}", "## How to read this")
  )
  expect_identical(sum(lines == "## How to read this"), 1L)
})

test_that("the batch design table reads in plain words", {
  skip_if_not_installed("knitr")
  lines <- NACHO:::report_batch_tables(GSE74821, group = "tissue type:ch1")
  expect_true(
    "|Batch       |    Levels|   Cram\u00e9r's V| Levels with one group|Confounded |" %in%
      lines
  )
  rows <- lines[startsWith(lines, "|CartridgeID") | startsWith(lines, "|Date")]
  expect_length(rows, 2L)
  expect_true(all(grepl("not computed", rows, fixed = TRUE)))
  expect_true(all(grepl("|yes", rows, fixed = TRUE)))
  expect_false(any(grepl("NA|TRUE|FALSE|n_levels|cramers_v", lines)))
  expect_true(any(grepl("[4]{.num}", rows, fixed = TRUE)))
})

test_that("a grouping with one level says batch cannot be checked", {
  skip_if_not_installed("knitr")
  lines <- NACHO:::report_batch_tables(GSE74821, group = "tissue type:ch1")
  expect_identical(
    lines[1:2],
    c("::: {.callout-note}", "## Batch cannot be checked against biology")
  )
  expect_match(lines[4], "`tissue type:ch1` has one level", fixed = TRUE)
  expect_false(any(grepl("are confounded", lines, fixed = TRUE)))
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
  expect_identical(NACHO:::report_limits(x)[["FoV"]], "at least 75%")
})

test_that("thresholds with two bounds read as a range", {
  x <- toy_nacho(4L)
  thresholds <- x@thresholds
  thresholds$BD <- c(0.1, 2.25)
  x@thresholds <- thresholds
  expect_identical(NACHO:::report_limits(x)[["BD"]], "0.1 to 2.25")
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
  callout <- grep(
    "Batch cannot be checked against biology",
    output,
    fixed = TRUE
  )
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
  limits <- NACHO:::report_limits(plexset_nacho)
  expect_false(any(c("PCL", "LoD") %in% names(limits)))
  expect_true("BD" %in% names(limits))
})

test_that("thresholds name the lower bound first whatever the order", {
  x <- toy_nacho(4L)
  thresholds <- x@thresholds
  thresholds$BD <- c(2.25, 0.1)
  S7::prop(x, "thresholds", check = FALSE) <- thresholds
  expect_identical(NACHO:::report_limits(x)[["BD"]], "0.1 to 2.25")
})

test_that("a threshold of 0 on the housekeeping count is not printed", {
  x <- toy_nacho(4L)
  x@samples[["Housekeeping_detected"]] <- 1L
  thresholds <- x@thresholds
  thresholds[["Housekeeping_detected"]] <- 0
  x@thresholds <- thresholds
  expect_false("Housekeeping_detected" %in% names(NACHO:::report_limits(x)))
})

test_that("report_sections() leaves out plots that cannot be drawn", {
  expect_false("Stability" %in% NACHO:::report_sections(mirna_fixture())$plot)
  expect_true("Stability" %in% NACHO:::report_sections(GSE74821)$plot)

  local_mocked_bindings(
    nacho_samples = function(x) data.frame(IDFILE = "a")
  )
  expect_false("PCBatch" %in% NACHO:::report_sections(GSE74821)$plot)
})

test_that("report_sections() leaves out component plots with one component", {
  x <- toy_nacho(4L)
  pca <- x@pca
  pca[["scores"]] <- pca[["scores"]][, 1, drop = FALSE]
  S7::prop(x, "pca", check = FALSE) <- pca
  plots <- NACHO:::report_sections(x)$plot
  expect_false(any(c("PCA12", "PCA", "PCBatch") %in% plots))
})

test_that("the report names House_factor the same way everywhere", {
  expect_match(NACHO:::plot_alt_texts[["HF"]], "Content normalization factor")
  sections <- NACHO:::report_sections(GSE74821)
  expect_true("Content normalization factor" %in% sections$title)
  expect_false(any(grepl(
    "Housekeeping factor",
    c(sections$title, NACHO:::plot_alt_texts)
  )))
})

test_that("applicable_plots() drops the plots the data cannot support", {
  everything <- unlist(NACHO:::app_plot_types, use.names = FALSE)
  gse <- NACHO:::applicable_plots(GSE74821)
  expect_true(all(c("PCL", "LoD", "HF") %in% gse))
  expect_true(all(gse %in% everything))
  plexset <- NACHO:::applicable_plots(plexset_nacho)
  expect_false(any(c("PCL", "LoD") %in% plexset))
  ruv <- normalise(GSE74821, normalisation_method = "RUVg", ruv_k = 1)
  expect_false("HF" %in% NACHO:::applicable_plots(ruv))
  expect_true("BD" %in% NACHO:::applicable_plots(ruv))
})

test_that("the report shows exactly the applicable plots", {
  for (x in list(GSE74821, plexset_nacho)) {
    shown <- stats::na.omit(NACHO:::report_sections(x)[["plot"]])
    expect_setequal(shown, NACHO:::applicable_plots(x))
  }
})

test_that("shared report and app text uses US spelling", {
  british <- british_stems
  text <- c(
    NACHO:::qc_metric_labels,
    NACHO:::plot_alt_texts,
    NACHO:::parameter_meanings,
    NACHO:::method_meanings,
    NACHO:::setting_rows(GSE74821)$meaning,
    NACHO:::report_reading_guide,
    NACHO:::report_sections(GSE74821)$title,
    NACHO:::report_methods(GSE74821)
  )
  flagged <- text[grepl(british, text, ignore.case = TRUE)]
  expect_length(flagged, 0L)
  expect_identical(NACHO:::qc_metric_labels[["Haemolysis"]], "Hemolysis")
  about <- list.files(
    system.file("about", package = "NACHO"),
    full.names = TRUE
  )
  expect_gt(length(about), 0L)
  prose <- gsub("`[^`]*`", "", unlist(lapply(about, readLines)))
  expect_false(any(grepl(british, prose, ignore.case = TRUE)))
})

test_that("report_metadata() fills the cover from the object", {
  meta <- report_metadata(GSE74821)
  expect_identical(meta$title, "NanoString quality-control report")
  expect_null(meta$author)
  expect_false(any(c("author", "date") %in% names(meta)))
  expect_identical(meta$nacho$samples, "48")
  expect_identical(meta$nacho$unit, "Cartridges")
  expect_identical(meta$nacho$units, "4")
  expect_identical(meta$nacho$method, "GLM")
  expect_identical(meta$nacho$rcc_version, GSE74821@provenance$file_version)
  expect_identical(
    meta$nacho$nacho_version,
    as.character(utils::packageVersion("NACHO"))
  )
  expect_match(meta$nacho$prepared, "^Prepared on ")
  expect_match(
    meta$nacho$prepared,
    "^Prepared on [A-Z][a-z]+ [0-9]{1,2}, [0-9]{4}$"
  )
  expect_identical(
    meta$nacho$prepared,
    paste("Prepared on", meta$nacho$date)
  )
})

test_that("report_metadata() takes the title and author as typed", {
  meta <- report_metadata(
    GSE74821,
    title = 'Run "A": #1 $5',
    author = "Jane Doe, Genomics Core"
  )
  expect_identical(meta$title, 'Run "A": #1 $5')
  expect_identical(meta$author, "Jane Doe, Genomics Core")
  expect_match(
    meta$nacho$prepared,
    "^Prepared by Jane Doe, Genomics Core · "
  )
})

test_that("an empty title falls back to the default", {
  expect_identical(
    report_metadata(GSE74821, title = "")$title,
    "NanoString quality-control report"
  )
  expect_null(report_metadata(GSE74821, author = "  ")$author)
})

test_that("report_metadata() says unknown when the RCC version is missing", {
  x <- GSE74821
  x@provenance$file_version <- NULL
  expect_identical(report_metadata(x)$nacho$rcc_version, "unknown")
})

test_that("report_metadata_literal() escapes Markdown in the cover text", {
  meta <- report_metadata_literal(report_metadata(
    GSE74821,
    title = 'A "b" *c*',
    author = "Lab & Co"
  ))
  expect_identical(meta$title, 'A \\"b\\" \\*c\\*')
  expect_identical(meta$author, "Lab \\& Co")
})

test_that("report_decisions() counts and lists the flagged samples", {
  x <- tuned_gse(FoV = 95)
  text <- paste(NACHO:::report_decisions(x), collapse = "\n")
  expect_match(text, "[47]{.n} samples pass every check", fixed = TRUE)
  expect_match(text, "[1]{.n .flag} sample is flagged", fixed = TRUE)
  expect_match(text, "[1]{.n} metric drives the flags", fixed = TRUE)
  expect_match(text, "::: {.nacho-verdict}", fixed = TRUE)
  expect_match(
    text,
    "| Sample | Cartridge | Metric | Value | Limit |",
    fixed = TRUE
  )
  expect_match(
    text,
    "| Field of view | [93.94%]{.num} | [at least 95%]{.num} |",
    fixed = TRUE
  )
  expect_match(text, "The other 47 samples pass every check.", fixed = TRUE)
})

test_that("the decision summary puts the table, its help, then the method callouts", {
  lines <- NACHO:::report_decisions(tuned_gse(FoV = 95))
  table <- which(startsWith(lines, "| Sample |"))
  caption <- which(startsWith(lines, ": Flagged samples {#tbl-flagged "))
  guide <- which(lines == "## How to read this")
  method <- which(lines == "## Instrument not in the RCC files")
  expect_length(caption, 1L)
  expect_length(guide, 1L)
  expect_true(table < caption && caption < guide && guide < method)
  expect_match(
    lines[guide + 2L],
    NACHO:::report_reading_guide[["decisions"]],
    fixed = TRUE
  )
  none <- NACHO:::report_decisions(GSE74821)
  expect_false(any(none == "## How to read this"))
  expect_false(any(startsWith(none, ": Flagged samples")))
})

test_that("the parameter table has a caption and its help", {
  lines <- NACHO:::report_parameters(GSE74821)
  caption <- which(startsWith(lines, ": Limits and settings {#tbl-parameters "))
  expect_length(caption, 1L)
  expect_gt(caption, max(which(startsWith(lines, "| "))))
  expect_identical(
    lines[which(lines == "## How to read this") + 2L],
    NACHO:::report_reading_guide[["parameters"]]
  )
})

test_that("each batch table has a caption with its own id", {
  skip_if_not_installed("knitr")
  lines <- NACHO:::report_batch_tables(GSE74821, group = "tissue type:ch1")
  expect_identical(
    grep("^: ", lines, value = TRUE),
    c(
      ": Batch design {#tbl-batch-design}",
      ": Groups by `CartridgeID` {#tbl-batch-cartridgeid}",
      ": Groups by `Date` {#tbl-batch-date}"
    )
  )
})

test_that("report_decisions() says so when nothing is flagged", {
  text <- paste(NACHO:::report_decisions(GSE74821), collapse = "\n")
  expect_match(text, "Every sample passes every check.", fixed = TRUE)
  expect_match(text, "[0]{.n} samples are flagged", fixed = TRUE)
  expect_false(grepl("| Sample |", text, fixed = TRUE))
})

test_that("report_decisions() gives one row per sample and failing metric", {
  x <- tuned_gse(FoV = 95, BD = c(0.05, 0.2))
  rows <- NACHO:::qc_failure_rows(x)
  expect_named(
    rows,
    c(
      "sample",
      "cartridge",
      "lane",
      "lane_samples",
      "metric",
      "metric_label",
      "value",
      "limit"
    )
  )
  expect_identical(
    nrow(rows),
    sum(
      NACHO::nacho_qc(x)[c("FoV_status", "BD_status")] == "fail",
      na.rm = TRUE
    )
  )
  expect_setequal(unique(rows$metric), c("BD", "FoV"))
  lines <- NACHO:::report_decisions(x)
  expect_identical(sum(startsWith(lines, "| GSM")), nrow(rows))
  expect_match(
    paste(lines, collapse = "\n"),
    "[2]{.n} metrics drive the flags",
    fixed = TRUE
  )
})

test_that("md_escape() shows sample ids literally in tables", {
  expect_identical(
    NACHO:::md_escape("a*b_c<d>|e"),
    "a\\*b\\_c&lt;d&gt;\\|e"
  )
  expect_identical(NACHO:::md_escape("[x] & y"), "\\[x\\] &amp; y")
  text <- paste(NACHO:::report_decisions(odd_ids_nacho()), collapse = "\n")
  expect_match(
    text,
    "| a\\*b\\_c&lt;d&gt;\\|e\\[1\\] | C\\_2 | Field of view | [50%]{.num} | [at least 75%]{.num} |",
    fixed = TRUE
  )
})

test_that("report_decisions() names the lane on PlexSet data", {
  x <- NACHO::normalise(
    plexset_nacho,
    outliers_thresholds = utils::modifyList(
      plexset_nacho@thresholds,
      list(BD = c(0.05, 0.5))
    )
  )
  text <- paste(NACHO:::report_decisions(x), collapse = "\n")
  expect_match(text, "| Sample | Lane | Metric | Value | Limit |", fixed = TRUE)
  expect_match(
    text,
    "| All 8 samples of the lane | plexset\\_20191218, lane 1 | Binding density | [1.07]{.num} | [0.05 to 0.5]{.num} |",
    fixed = TRUE
  )
  expect_identical(
    sum(grepl("^\\| All 8 samples", strsplit(text, "\n")[[1]])),
    12L
  )
  expect_match(text, "a lane row stands for every sample", fixed = TRUE)
  expect_match(
    text,
    "| plexset\\_20191218, lane 1 | Binding density |",
    fixed = TRUE
  )
})

test_that("report_parameters() gives meaning and source for each limit", {
  text <- paste(NACHO:::report_parameters(GSE74821), collapse = "\n")
  expect_match(
    text,
    "| Parameter | Value | What it means | Source |",
    fixed = TRUE
  )
  expect_match(text, "| Field of view | [at least 75%]{.num} |", fixed = TRUE)
  expect_match(text, "Field of view[^\n]*\\[nSolver\\]\\{\\.tag\\}")
  expect_match(
    text,
    "Binding density[^\n]*\\[MAX/FLEX/PRO \\(not in the RCC files\\)\\]\\{\\.tag\\}"
  )
  limits <- strsplit(text, "\n", fixed = TRUE)[[1]]
  limits <- limits[grepl("^\\| (Binding density|Field of view) \\|", limits)]
  expect_length(limits, 2L)
  expect_false(any(grepl("your choice", limits, fixed = TRUE)))
  tuned <- tuned_gse(FoV = 95)
  tuned_text <- paste(NACHO:::report_parameters(tuned), collapse = "\n")
  expect_match(
    tuned_text,
    "| Field of view | [at least 95%]{.num} |",
    fixed = TRUE
  )
  expect_match(
    tuned_text,
    "Field of view[^\n]*\\[your choice\\]\\{\\.tag \\.user\\}"
  )
  expect_match(
    tuned_text,
    "Binding density[^\n]*\\[MAX/FLEX/PRO \\(not in the RCC files\\)\\]\\{\\.tag\\}"
  )
})

test_that("report_parameters() credits NACHO 2 for the legacy preset", {
  x <- NACHO::normalise(
    GSE74821,
    outliers_thresholds = NACHO::nacho_thresholds(preset = "legacy")
  )
  text <- paste(NACHO:::report_parameters(x), collapse = "\n")
  expect_match(text, "Binding density[^\n]*\\[NACHO 2\\]\\{\\.tag\\}")
  expect_match(text, "Field of view[^\n]*\\[NACHO 2\\]\\{\\.tag\\}")
  expect_false(grepl("[nSolver]", text, fixed = TRUE))
})

test_that("report_parameters() covers every setting with its source", {
  text <- paste(NACHO:::report_parameters(GSE74821), collapse = "\n")
  expect_match(text, "| Normalization method | [GLM]{.num} |", fixed = TRUE)
  expect_row(text, "| Normalization method |", "[your choice]")
  expect_row(text, "| Background | [none]{.num} |", "[default]")
  expect_row(text, "| Housekeeping genes | [ACTB, GUSB,", "[default]")
  expect_row(text, "| Housekeeping prediction | [no]{.num} |", "[default]")
  expect_row(text, "| Housekeeping normalization | [yes]{.num} |", "[default]")
  expect_row(text, "| Principal components | [10]{.num} |", "[default]")
  expect_false(grepl("RUV", text, fixed = TRUE))

  x <- toy_nacho(4L)
  settings <- x@settings
  settings$normalisation_method <- "RUVg"
  settings$ruv_k <- 2L
  settings$background <- "geo"
  settings$background_mode <- "subtract"
  settings$housekeeping_genes <- "GENE1"
  S7::prop(x, "settings", check = FALSE) <- settings
  text <- paste(NACHO:::report_parameters(x), collapse = "\n")
  expect_match(text, "| RUV factors | [2]{.num} |", fixed = TRUE)
  expect_match(text, "| Normalization method | [RUVg]{.num} |", fixed = TRUE)
  expect_row(text, "| Background | [geo\\, subtract]{.num} |", "[your choice]")
  expect_row(text, "| Housekeeping genes | [GENE1]{.num} |", "[your choice]")
})

test_that("report_parameters() says when RUVg uses the suggested count", {
  suggested <- suppressMessages(NACHO::normalise(
    GSE74821,
    normalisation_method = "RUVg",
    ruv_k = NULL
  ))
  table <- NACHO::suggest_ruv_k(suggested)
  k <- table$k[table$suggested]
  expect_identical(suggested@settings$ruv_k, k)
  text <- paste(NACHO:::report_parameters(suggested), collapse = "\n")
  expect_row(
    text,
    paste0("| RUV factors | [", k, "]{.num} |"),
    "[NACHO suggestion]"
  )
  chosen <- suppressMessages(NACHO::normalise(
    GSE74821,
    normalisation_method = "RUVg",
    ruv_k = if (k == 1L) 2L else 1L
  ))
  text <- paste(NACHO:::report_parameters(chosen), collapse = "\n")
  expect_row(text, "| RUV factors | ", "[your choice]")
})

test_that("report_parameters() leaves out PCL and LoD on PlexSet data", {
  text <- paste(NACHO:::report_parameters(plexset_nacho), collapse = "\n")
  expect_false(grepl("Positive control linearity|Limit of detection", text))
  expect_match(text, "| Binding density | [0.05 to 2.25]{.num} |", fixed = TRUE)
  expect_match(text, "| Housekeeping genes | [none]{.num} |", fixed = TRUE)
})

test_that("report_decisions() leaves out a missing cartridge on PlexSet data", {
  x <- NACHO::normalise(
    plexset_nacho,
    outliers_thresholds = utils::modifyList(
      plexset_nacho@thresholds,
      list(BD = c(0.05, 0.5))
    )
  )
  samples <- x@samples
  samples$CartridgeID <- NA_character_
  S7::prop(x, "samples", check = FALSE) <- samples
  text <- paste(NACHO:::report_decisions(x), collapse = "\n")
  expect_match(text, "| All 8 samples of the lane | lane 1 |", fixed = TRUE)
  expect_false(grepl("NA", text, fixed = TRUE))
})

test_that("report_decisions() counts only assessed samples as passing", {
  x <- tuned_gse(FoV = 95)
  samples <- x@samples
  samples[2, c("BD", "FoV", "PCL", "LoD")] <- NA
  samples[2, c("Positive_factor", "House_factor")] <- NA
  samples[2, "Housekeeping_detected"] <- NA
  S7::prop(x, "samples", check = FALSE) <- samples
  expect_true(is.na(NACHO::nacho_qc(x)$status[2]))
  text <- paste(NACHO:::report_decisions(x), collapse = "\n")
  expect_match(text, "[46]{.n} samples pass every check", fixed = TRUE)
})

test_that("the binding density source names the instrument family", {
  sources <- vapply(
    c("flex", "pro", "sprint"),
    function(instrument) {
      x <- NACHO::normalise(
        GSE74821,
        outliers_thresholds = NACHO::nacho_thresholds(instrument)
      )
      NACHO:::threshold_source(x, "BD")
    },
    character(1)
  )
  expect_identical(
    unname(sources),
    c("MAX/FLEX/PRO", "MAX/FLEX/PRO", "SPRINT")
  )
  expect_identical(
    NACHO:::threshold_source(GSE74821, "BD"),
    "MAX/FLEX/PRO (not in the RCC files)"
  )
  thresholds <- GSE74821@thresholds
  thresholds$instrument <- NA_character_
  unknown <- NACHO::normalise(GSE74821, outliers_thresholds = thresholds)
  expect_identical(
    NACHO:::threshold_source(unknown, "BD"),
    "MAX/FLEX/PRO (not in the RCC files)"
  )
  detected <- GSE74821
  samples <- detected@samples
  samples$Header.header_FileVersion <- "1.7"
  S7::prop(detected, "samples", check = FALSE) <- samples
  expect_identical(NACHO:::threshold_source(detected, "BD"), "MAX/FLEX/PRO")
  expect_false(any(grepl(
    "Instrument not in the RCC files",
    NACHO:::report_method_callouts(detected),
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Instrument not in the RCC files",
    NACHO:::report_method_callouts(GSE74821),
    fixed = TRUE
  )))
  legacy <- NACHO::normalise(
    GSE74821,
    outliers_thresholds = NACHO::nacho_thresholds(preset = "legacy")
  )
  expect_false(any(grepl(
    "Instrument not in the RCC files",
    NACHO:::report_method_callouts(legacy),
    fixed = TRUE
  )))
})

test_that("the hemolysis limit is credited to NACHO, even with legacy", {
  for (preset in c("nsolver", "legacy")) {
    x <- NACHO::normalise(
      GSE74821,
      outliers_thresholds = NACHO::nacho_thresholds(
        preset = preset,
        haemolysis = TRUE
      )
    )
    expect_identical(NACHO:::threshold_source(x, "Haemolysis"), "NACHO")
  }
})

test_that("a GLM fallback shows in the method row and in a callout", {
  x <- suppressMessages(NACHO::normalise(
    GSE74821,
    normalisation_method = "GEO"
  ))
  positives <- NACHO::nacho_probes(x)[["CodeClass"]] == "Positive"
  x@counts[positives, 1] <- rev(x@counts[positives, 1])
  expect_warning(
    y <- suppressMessages(NACHO::normalise(x, normalisation_method = "GLM")),
    class = "nacho_warning_glm_convergence"
  )
  text <- paste(NACHO:::report_parameters(y), collapse = "\n")
  expect_row(
    text,
    "| Normalization method | [GLM \\(GEO used\\)]{.num} |",
    "[your choice]"
  )
  callouts <- paste(NACHO:::report_method_callouts(y), collapse = "\n")
  expect_match(
    callouts,
    "::: {.callout-warning}\n## GLM fell back",
    fixed = TRUE
  )
  expect_match(
    callouts,
    NACHO:::md_escape(colnames(x@counts)[1]),
    fixed = TRUE
  )
  expect_match(
    paste(NACHO:::report_decisions(y), collapse = "\n"),
    "## GLM fell back to the geometric mean",
    fixed = TRUE
  )
})

test_that("predicted housekeeping genes are credited to NACHO", {
  x <- suppressMessages(NACHO::normalise(
    GSE74821,
    housekeeping_predict = TRUE
  ))
  text <- paste(NACHO:::report_parameters(x), collapse = "\n")
  expect_row(text, "| Housekeeping genes | ", "[NACHO prediction]")
  expect_row(text, "| Housekeeping prediction | [yes]{.num} |", "[your choice]")
  callouts <- paste(NACHO:::report_method_callouts(x), collapse = "\n")
  expect_match(
    callouts,
    "::: {.callout-note}\n## Housekeeping genes predicted",
    fixed = TRUE
  )
  expect_match(callouts, x@settings$housekeeping_genes[1], fixed = TRUE)
})

test_that("housekeeping normalization off gets a callout", {
  off <- suppressMessages(NACHO::normalise(
    GSE74821,
    housekeeping_norm = FALSE
  ))
  text <- paste(NACHO:::report_parameters(off), collapse = "\n")
  expect_row(
    text,
    "| Housekeeping normalization | [no]{.num} |",
    "[your choice]"
  )
  expect_match(
    paste(NACHO:::report_method_callouts(off), collapse = "\n"),
    "::: {.callout-note}\n## Housekeeping normalization off",
    fixed = TRUE
  )
  expect_match(
    paste(NACHO:::report_method_callouts(plexset_nacho), collapse = "\n"),
    "::: {.callout-warning}\n## Housekeeping normalization off",
    fixed = TRUE
  )
})

test_that("the housekeeping normalization default follows load_rcc()", {
  x <- toy_nacho(4L)
  settings <- x@settings
  settings$housekeeping_genes <- "GENE1"
  settings$panel <- "mirna"
  S7::prop(x, "settings", check = FALSE) <- settings
  expect_true(NACHO:::default_housekeeping_norm(x))
  settings$housekeeping_genes <- "HK1"
  S7::prop(x, "settings", check = FALSE) <- settings
  expect_false(NACHO:::default_housekeeping_norm(x))
})

test_that("excluded negative controls get a callout", {
  x <- GSE74821
  negatives <- which(NACHO::nacho_probes(x)[["CodeClass"]] == "Negative")
  x@counts[negatives[1], ] <- x@counts[negatives[1], ] * 50L + 500L
  y <- suppressMessages(NACHO::normalise(x, background = "mean"))
  excluded <- y@provenance$excluded_negatives
  expect_length(excluded, 1L)
  callouts <- paste(NACHO:::report_method_callouts(y), collapse = "\n")
  expect_match(
    callouts,
    "::: {.callout-note}\n## Negative controls left out",
    fixed = TRUE
  )
  expect_match(callouts, NACHO:::md_escape(excluded), fixed = TRUE)
  expect_match(callouts, "more than 3-fold", fixed = TRUE)
})

test_that("a migrated object credits NACHO 2 and says so", {
  withr::local_options(nacho.quiet = TRUE)
  x <- NULL
  suppressMessages(expect_warning(
    x <- NACHO::read_nacho(test_path("fixtures", "nacho-schema-1.rds")),
    class = "nacho_warning_n_comp_reduced"
  ))
  text <- paste(NACHO:::report_parameters(x), collapse = "\n")
  expect_row(text, "| Background | [none]{.num} |", "[NACHO 2]")
  expect_match(
    paste(NACHO:::report_method_callouts(x), collapse = "\n"),
    "::: {.callout-note}\n## Migrated from NACHO 2",
    fixed = TRUE
  )
})

test_that("the background mode does not count when there is no background", {
  x <- toy_nacho(4L)
  settings <- x@settings
  settings$background_mode <- "subtract"
  S7::prop(x, "settings", check = FALSE) <- settings
  text <- paste(NACHO:::report_parameters(x), collapse = "\n")
  expect_row(text, "| Background | [none]{.num} |", "[default]")
})

test_that("report_method_callouts() gives nothing when nothing applies", {
  x <- toy_nacho(4L)
  samples <- x@samples
  samples$Header.header_FileVersion <- "1.7"
  S7::prop(x, "samples", check = FALSE) <- samples
  expect_identical(NACHO:::report_method_callouts(x), character(0))
})

test_that("md_escape() keeps three dots as typed in both formats", {
  dot <- "`.`{=html}`\\.`{=typst}"
  expect_identical(
    NACHO:::md_escape("a...b"),
    paste0("a\\.", dot, dot, "b")
  )
  expect_identical(NACHO:::md_escape("a.b"), "a\\.b")
})

test_that("every plot and table has reading help", {
  guide <- NACHO:::report_reading_guide
  expect_setequal(
    names(guide),
    c(names(NACHO:::plot_alt_texts), "decisions", "parameters", "batch")
  )
  sentences <- lengths(regmatches(guide, gregexpr("[.!?](\\s|$)", guide)))
  expect_true(all(sentences >= 2 & sentences <= 4))
})

test_that("a decimal in the reading help does not end a sentence", {
  guide <- NACHO:::report_reading_guide[c("Stability", "LoD")]
  expect_match(guide[["Stability"]], "M = 1.5", fixed = TRUE)
  expect_match(guide[["LoD"]], "0.5 fM", fixed = TRUE)
  expect_identical(
    lengths(regmatches(guide, gregexpr("[.!?](\\s|$)", guide))),
    c(Stability = 4L, LoD = 4L)
  )
})

test_that("the labels autoplot() draws use US spelling", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  british <- "normalis|colour|centre|grey|haemoly|visualis|analys(e|ing)"
  labels <- unlist(lapply(NACHO:::applicable_plots(GSE74821), function(type) {
    plot <- ggplot2::autoplot(GSE74821, type = type)
    built <- ggplot2::ggplot_build(plot)
    facets <- built$layout$layout
    facets <- facets[setdiff(
      names(facets),
      c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y")
    )]
    c(
      vapply(
        ggplot2::get_labs(plot),
        function(l) paste(format(l), collapse = " "),
        ""
      ),
      unlist(lapply(facets, as.character))
    )
  }))
  expect_true("Normalized" %in% labels)
  expect_false(any(grepl(british, labels, ignore.case = TRUE)))
})

test_that("report_sections() carries the reading help of each plot", {
  sections <- NACHO:::report_sections(GSE74821)
  plots <- !is.na(sections$plot)
  expect_identical(
    sections$guide[plots],
    unname(NACHO:::report_reading_guide[sections$plot[plots]])
  )
  expect_true(all(is.na(sections$guide[!plots])))
})

test_that("report_sections() titles use US spelling", {
  titles <- NACHO:::report_sections(GSE74821)$title
  expect_false(any(grepl("isation", titles, fixed = TRUE)))
  expect_true("Normalization" %in% titles)
})

test_that("report_body() prints a How to read this callout under each plot", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  report <- list(
    object = GSE74821,
    options = NACHO:::check_report_options(GSE74821),
    sections = NACHO:::report_sections(GSE74821)
  )
  lines <- utils::capture.output(NACHO:::report_body(report))
  out <- paste(lines, collapse = "\n")
  expect_identical(
    sum(lines == "## How to read this"),
    sum(!is.na(report$sections$plot))
  )
  expect_identical(
    lines[which(lines == "## How to read this") - 1L],
    rep("::: {.callout-note}", sum(!is.na(report$sections$plot)))
  )
  expect_match(out, NACHO:::report_reading_guide[["FoV"]], fixed = TRUE)
})

test_that("the methods appendix cites NACHO and the nSolver guidelines", {
  lines <- NACHO:::report_methods(GSE74821)
  text <- paste(lines, collapse = "\n")
  expect_identical(lines[1], "# Methods {.unnumbered}")
  expect_match(text, "Canouil", fixed = TRUE)
  expect_match(text, "nSolver", fixed = TRUE)
  expect_match(
    text,
    "nCounter Gene Expression Data Analysis Guidelines",
    fixed = TRUE
  )
  expect_false(grepl("_Bioinformatics_", text, fixed = TRUE))
  expect_match(text, "\\'t Hart", fixed = TRUE)
  expect_match(
    text,
    "## Session information {.unnumbered}\n\n```\nR version",
    fixed = TRUE
  )
  expect_identical(sum(lines == "```"), 2L)
  expect_false(grepl("ISSN", text, fixed = TRUE))
  expect_identical(
    lengths(regmatches(
      text,
      gregexpr("10.1093/bioinformatics/btz647", text, fixed = TRUE)
    )),
    1L
  )
  versions <- lines[
    (which(lines == "```")[1] + 1):(which(lines == "```")[2] - 1)
  ]
  expect_false(any(grepl("/", versions, fixed = TRUE)))
  expect_false(any(grepl("time zone|locale|testthat", versions)))
  expect_true(any(startsWith(versions, "NACHO ")))
})

test_that("the methods appendix describes the object", {
  text <- paste(NACHO:::report_methods(GSE74821), collapse = "\n")
  expect_match(text, "binding density (0.05 to 2.25)", fixed = TRUE)
  expect_match(text, "field of view (at least 75%)", fixed = TRUE)
  expect_match(text, "do not name the nCounter instrument", fixed = TRUE)
  expect_match(text, "with the GLM method", fixed = TRUE)
  expect_match(text, "8 housekeeping genes", fixed = TRUE)
  expect_match(text, "POS_A to POS_E", fixed = TRUE)

  sprint <- NACHO::normalise(
    GSE74821,
    outliers_thresholds = NACHO::nacho_thresholds("sprint")
  )
  text <- paste(NACHO:::report_methods(sprint), collapse = "\n")
  expect_match(text, "Binding density uses the SPRINT limits.", fixed = TRUE)
  expect_false(grepl("do not name", text, fixed = TRUE))

  legacy <- NACHO::normalise(
    GSE74821,
    outliers_thresholds = NACHO::nacho_thresholds(preset = "legacy")
  )
  text <- paste(NACHO:::report_methods(legacy), collapse = "\n")
  expect_match(text, "legacy preset", fixed = TRUE)
  expect_match(text, "POS_A to POS_F", fixed = TRUE)
  expect_false(grepl("do not name", text, fixed = TRUE))
})

test_that("the methods appendix names the method NACHO actually used", {
  x <- GSE74821
  provenance <- x@provenance
  provenance$glm_fallback <- colnames(x@counts)[1]
  S7::prop(x, "provenance", check = FALSE) <- provenance
  text <- paste(NACHO:::report_methods(x), collapse = "\n")
  expect_match(text, "GLM \\(GEO used\\)", fixed = TRUE)
  expect_match(text, "geometric mean scaled them all", fixed = TRUE)
})

test_that("the methods appendix credits limits the user changed", {
  x <- GSE74821
  thresholds <- x@thresholds
  thresholds$FoV <- 80
  x <- NACHO::normalise(x, outliers_thresholds = thresholds)
  text <- paste(NACHO:::report_methods(x), collapse = "\n")
  expect_match(text, "field of view (at least 80%)", fixed = TRUE)
  expect_match(text, "You changed the limits for field of view.", fixed = TRUE)
  expect_match(
    text,
    "The other limits come from the nSolver preset",
    fixed = TRUE
  )
})

test_that("the methods appendix names the RUVg factors", {
  ruv <- NACHO::normalise(GSE74821, normalisation_method = "RUVg", ruv_k = 1)
  text <- paste(NACHO:::report_methods(ruv), collapse = "\n")
  expect_match(text, "with the RUVg method.", fixed = TRUE)
  expect_match(
    text,
    "RUVg then removed 1 factor of unwanted variation.",
    fixed = TRUE
  )
})

test_that("the methods appendix gives the load version only when it differs", {
  x <- GSE74821
  provenance <- x@provenance
  provenance$nacho_version <- as.character(utils::packageVersion("NACHO"))
  S7::prop(x, "provenance", check = FALSE) <- provenance
  text <- paste(NACHO:::report_methods(x), collapse = "\n")
  expect_false(grepl("loaded with", text, fixed = TRUE))
  expect_match(text, "The RCC files are of version 1.6.", fixed = TRUE)
  provenance$nacho_version <- "1.0.0"
  S7::prop(x, "provenance", check = FALSE) <- provenance
  text <- paste(NACHO:::report_methods(x), collapse = "\n")
  expect_match(text, "The data were loaded with NACHO 1.0.0.", fixed = TRUE)
})

test_that("report_figure() prints the plot outside knitr", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  withr::local_options(knitr.in.progress = NULL)
  local_mocked_bindings(
    knitr_chunk_options = function() stop("not in a chunk")
  )
  output <- utils::capture.output(
    result <- NACHO:::report_figure(ggplot2::ggplot(), "BD", "Caption", "Alt")
  )
  expect_null(result)
  expect_false(any(grepl("![", output, fixed = TRUE)))
})

test_that("report_figure() writes a numbered figure in a knitr chunk", {
  dir <- withr::local_tempdir()
  withr::local_options(knitr.in.progress = TRUE)
  local_mocked_bindings(
    knitr_chunk_options = function() {
      list(
        fig.path = file.path(dir, "figures", ""),
        fig.width = 4,
        fig.height = 3,
        dpi = 50,
        fig.retina = 2
      )
    }
  )
  output <- utils::capture.output(NACHO:::report_figure(
    ggplot2::ggplot(),
    "PCA12",
    "First [two] *components*",
    'Samples on the "first" components.'
  ))
  path <- file.path(dir, "figures", "fig-pca12.png")
  expect_true(file.exists(path))
  expect_identical(
    paste(output[nzchar(output)], collapse = "\n"),
    paste0(
      "![First \\[two\\] \\*components\\*](",
      path,
      '){#fig-pca12 fig-alt="Samples on the \\"first\\" components." width="4in"}'
    )
  )
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, "raw", 16L)
  expect_identical(
    readBin(con, "integer", 2L, size = 4L, endian = "big"),
    c(400L, 300L)
  )
})
