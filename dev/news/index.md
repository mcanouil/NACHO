# Changelog

## NACHO (development version)

### Dependencies

- In `DESCRIPTION`,
  - build: require R 4.1.0 or newer, ggplot2 4.0.0 or newer, ggforce
    0.5.0 or newer, ggrepel 0.9.6 or newer, and shiny 1.7.4 or newer,
    which pulls in fontawesome 0.4.0 and its Font Awesome 6 icon names.
  - build: require pandoc 2.11 or newer, which has citeproc built in, so
    pandoc-citeproc is no longer needed.
  - build: require knitr 1.39 or newer, which the report needs for
    `include_graphics(rel_path = FALSE)`.
  - build: require scales 1.4.0 or newer, which ggplot2 4.0.0 already
    needs, for the log-10 axes of the `"PFNF"` and `"HF"` plots.

### Chores

- In `inst/app/`,
  - refactor: replace the superseded
    [`shiny::callModule()`](https://rdrr.io/pkg/shiny/man/callModule.html)
    with
    [`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html).
  - refactor: use the Font Awesome 6 icon names `file-arrow-up` and
    `circle-info`.
- In `R/`,
  - refactor: drop `stringsAsFactors = FALSE` from
    [`data.frame()`](https://rdrr.io/r/base/data.frame.html) and
    [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
    calls, where it is the default since R 4.0.0.
  - refactor: hide boxplot outliers with `outliers = FALSE` instead of
    `outlier.shape = NA`.

### Documentation

- In `vignettes/`, `README.Rmd` and the
  [`render()`](https://m.canouil.dev/NACHO/dev/reference/render.md)
  report template,
  - docs: write chunk options as `#|` YAML comments, which needs knitr
    1.35 or newer.
  - docs: add alternative text to the images.
  - docs: install the development version with
    [`pak::pak()`](https://pak.r-lib.org/reference/pak.html) instead of
    `remotes::install_github()`.
- docs: fix typos and grammar, and spell GitHub, NanoString, R Markdown
  and Shiny consistently.
- In `vignettes/`,
  - docs: keep only the GSE70970 samples measured with the
    `NS_H_miR_1.4` CodeSet, since its two CodeSets come from different
    nSolver versions and
    [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
    refuses files that mix versions.
  - docs: attach data.table in the analysis vignette, which uses
    [`dcast()`](https://rdrr.io/pkg/data.table/man/dcast.data.table.html)
    and
    [`as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html).
- In `pkgdown/`,
  - docs: restyle the website for pkgdown 2.2 with the NACHO logo
    colours, a light and dark mode switch, and colour contrast that
    meets WCAG AA.
  - docs: group the reference index by task.
- In `R/normalise.R`, `R/load_rcc.R` and `vignettes/NACHO.Rmd`,
  - docs: remove the `raw_counts` and `normalised_counts` slots, which
    never existed, and say that `nacho` holds one row per sample and
    probe.
- In `R/GSE74821.R`,
  - docs: say that `GSE74821` holds 48 samples, not 20.

### Fixes

- In `inst/CITATION`,
  - fix: list the authors as
    [`person()`](https://rdrr.io/r/utils/person.html) objects, so the
    citation shows their full initials and spells Leen M. ’t Hart
    correctly.
  - fix: cite the version of record, Bioinformatics volume 36, issue 3,
    pages 970 to 971, published in February 2020.
- In `R/autoplot.R`,
  - fix: stop boxplots inheriting the colour aesthetic, which made
    ggplot2 warn that it dropped `colour` for the control probe plots.
  - fix: draw the outlier bands of the `"PFNF"` and `"HF"` plots to the
    panel edges without log-10 warnings about infinite values.
  - fix: keep each sample’s identifier in the `"BD"`, `"FoV"`, `"PCL"`,
    `"LoD"`, `"PN"`, `"Positive"`, `"Negative"` and `"Housekeeping"`
    plots, which drew every point at one position and dropped some of
    them.
- In `R/render.R`,
  - fix: include the logo by its absolute path, so pandoc finds it when
    the temporary directory sits behind a symbolic link.
  - fix: pass the report options as R Markdown parameters, so
    `outliers_labels = "CartridgeID"` works and column names with quotes
    no longer break the report.
- In `inst/app/app.R`,
  - fix: stop writing an `all.rdata` debug file to the working directory
    when uploading RCC files.
  - fix: load uploaded RCC files, which failed because
    [`suppressMessages()`](https://rdrr.io/r/base/message.html) received
    `x` instead of `expr`.
  - fix: make the sample sheet optional again when uploading RCC files,
    instead of failing on a missing `ssheet_dt`.
- In `DESCRIPTION`,
  - fix: suggest markdown, which the app needs to show its help pages,
    and make
    [`visualise()`](https://m.canouil.dev/NACHO/dev/reference/visualise.md)
    ask for it when it is not installed. The
    [`deploy()`](https://m.canouil.dev/NACHO/dev/reference/deploy.md)
    help page notes that the server needs it too.
- In `R/geometric_housekeeping.R`,
  - fix: replace background-corrected housekeeping counts below 1 with
    1, so values between 0 and 1 no longer inflate `House_factor`.
    ([\#53](https://github.com/mcanouil/NACHO/issues/53))
- In `R/qc_pca.R`,
  - fix: compute the PCA with samples as observations and store their
    scores. `PC01` to `PC10` and the variance explained change, and the
    PCA plots now show the main sources of variation between samples.
- In `R/normalise_counts.R`,
  - fix: apply the 0.1 floor after rounding, so normalised counts at or
    below background are 0.1 instead of 0. With
    `housekeeping_predict = TRUE`, the floor can change which
    housekeeping genes are picked, which in turn changes `House_factor`,
    the normalised counts and the outlier flags.
- In `R/qc_pca.R` and `R/normalise_counts.R`,
  - fix: objects created with an earlier version keep the old values, so
    run
    [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
    again to refresh them.
- In `R/normalise.R`,
  - fix: use the `n_comp` passed to
    [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md),
    which was ignored.
  - fix: recompute `is_outlier` when only the thresholds change.
- In `R/qc_features.R`, `R/qc_limit_detection.R` and
  `R/check_outliers.R`,
  - fix: report `PCL` and `LoD` as `NA` when a panel has no `POS_E`
    probe or when the negative controls do not vary, and do not flag a
    sample on a metric that could not be measured.
- In `R/load_rcc.R`,
  - fix: detect PlexSet files from their content, and add `plexset_id`
    `S1` to `S8` when the sample sheet lists each file once.
  - fix: stop converting the caller’s sample sheet to a `data.table`.
  - fix: stop with a clear message when RCC files mix PlexSet and
    single-sample files.
  - fix: stop when a single-sample sample sheet lists the same RCC file
    twice.
- In `R/print.R`,
  - fix: return the object invisibly from
    [`print()`](https://rdrr.io/r/base/print.html).
- In `data/`,
  - fix: rebuild `GSE74821` with the corrected PCA and normalised
    counts.
- In `inst/app/`,
  - fix: set the full binding density range when switching between the
    MAX/FLEX and SPRINT presets.
  - fix: accept zip archives sent as `application/zip`, and `.RCC`,
    `.rcc`, `.RCC.gz` and `.csv` files in any case.
  - fix: stop warning about row names when unpacking a zip archive.
  - fix: show a notification when the sample sheet is discarded, instead
    of a warning in the R console.
  - fix: correct the typos in the card and outlier labels, and open the
    app on the QC metrics tab.

## NACHO 2.0.6

CRAN release: 2024-01-12

### Fixes

- In `R/qc_positive_control.R`,
  - fix: use R-squared instead of Pearson correlation coefficient.
    ([\#48](https://github.com/mcanouil/NACHO/issues/48))

## NACHO 2.0.5

CRAN release: 2023-08-07

### Fixes

- In `R/autoplot.R`,
  - fix: set `height` in
    [`ggplot2::position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html)
    to `0` to avoid vertical dispersion points.
    ([\#45](https://github.com/mcanouil/NACHO/issues/45))
- In `inst/app/www/about-nacho.md`,
  - fix: shiny\[dot\]rstudio\[dot\]com moved to
    <https://shiny.posit.co/>.

### Tests

- In `tests/testthat/test-load_rcc.R`,
  - fix: skip tests to decrease CRAN checks computation time.

**Full Changelog**:
<https://github.com/mcanouil/NACHO/compare/v2.0.4...v2.0.5>

## NACHO 2.0.4

CRAN release: 2023-04-01

### Fixes

- In `inst/CITATION`,
  - fix: convert `citEntry` to `bibentry` from CRAN note.

**Full Changelog**:
<https://github.com/mcanouil/NACHO/compare/v2.0.3...v2.0.4>

## NACHO 2.0.3

### Chores

- In `DESCRIPTION`,
  - chore: update domain name.

**Full Changelog**:
<https://github.com/mcanouil/NACHO/compare/v2.0.2...v2.0.3>

## NACHO 2.0.2

CRAN release: 2022-12-05

### Chores

- In `DESCRIPTION`,
  - chore: update email address.
- chore: remove `ggbeeswarm`.

**Full Changelog**:
<https://github.com/mcanouil/NACHO/compare/v2.0.1...v2.0.2>

## NACHO 2.0.1

CRAN release: 2022-11-24

### Chores

- In `DESCRIPTION`,
  - chore: update email address.

**Full Changelog**:
<https://github.com/mcanouil/NACHO/compare/v2.0.0...v2.0.1>

## NACHO 2.0.0

CRAN release: 2022-05-31

### Major (breaking) changes

- Refactor to use `data.table` instead of `dplyr`/`tidyr`/`purrr`.

### Features

- Ensure RCC files are homogeneous in terms of versions.
  [\#20](https://github.com/mcanouil/NACHO/issues/20)
- Allow to use vector of file paths, named or not.
  [\#33](https://github.com/mcanouil/NACHO/issues/33)
- Allow to upload a CSV file associated with the RCC files within the
  `shiny` application.
  [\#36](https://github.com/mcanouil/NACHO/issues/36)

Full Changelog:
<https://github.com/mcanouil/NACHO/compare/v1.1.0...v2.0.0>

## NACHO 1.1.0

CRAN release: 2021-01-14

### Breaking changes

- In `DESCRIPTION`,
  - Update `ggplot2` version (\>= 3.3.0).
  - Update `dplyr` version (\>= 1.0.2).
- In `R/autoplot.R`,
  - Replace
    [`ggplot2::expand_scale()`](https://ggplot2.tidyverse.org/reference/expansion.html)
    with
    [`ggplot2::expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html).

### Minor improvements and fixes

- In `R/load_rcc.R`,
  - Remove deprecated
    [`dplyr::progress_estimated()`](https://dplyr.tidyverse.org/reference/progress_estimated.html).
- In `R/norm_glm.R`,
  - Remove deprecated
    [`dplyr::progress_estimated()`](https://dplyr.tidyverse.org/reference/progress_estimated.html).
- In `tests`,
  - Small tweaks.
  - Add condition when trying to download files from a GEO dataset (Fix
    CRAN checks).

## NACHO 1.0.2

CRAN release: 2021-01-05

### Minor improvements and fixes

- In `DESCRIPTION`,
  - Update URLs.
- In `R/normalise.R`,
  - Fix missing “outliers_thresholds” field after
    [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md)
    without removing outliers
    ([\#26](https://github.com/mcanouil/NACHO/issues/26)).
- In `R/GSE74821.R`,
  - Now uses `data-raw` root directory.
- In `R/-`,
  - No longer generates `Rd` files for internal functions.

## NACHO 1.0.1

CRAN release: 2020-05-26

### Minor improvements and fixes

- Fix deprecated documentation for `R/load_rcc.R` and `R/normalise.R`.
- Use [`file.path()`](https://rdrr.io/r/base/file.path.html) in examples
  and vignette.
- In `R/autoplot.R`, reduce alpha for ellipses.
- In `inst/app/utils.R`, set default point size (also for outliers) to
  `1`.
- In `R/load_rcc.R`, use
  [`inherits()`](https://rdrr.io/r/base/class.html) instead of
  [`class()`](https://rdrr.io/r/base/class.html).
- Code optimisation.

## NACHO 1.0.0

CRAN release: 2020-01-09

### New features

- In `R/conflicts.R`,
  - conflicts are now printed when attaching `NACHO`.
  - `nacho_conflicts()` can be used to print conflicts.
- New Shiny app in `inst/app/`,
  ([\#4](https://github.com/mcanouil/NACHO/issues/4),
  [\#5](https://github.com/mcanouil/NACHO/issues/5) &
  [\#14](https://github.com/mcanouil/NACHO/issues/14))
  - as a regular app, to load directly RCC files individually or within
    zip archive.
  - within
    [`visualise()`](https://m.canouil.dev/NACHO/dev/reference/visualise.md),
    to load `"nacho"` object from
    [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
    (previous `summarise()`) or from
    [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).
- New [`deploy()`](https://m.canouil.dev/NACHO/dev/reference/deploy.md)
  (`R/deploy.R`) function to easily deploy (copy) the shiny app.
- New raw RCC files (multiplexed) available in `inst/extdata/`.
- New vignette `NACHO-analysis`, which describe how to use `limma` or
  other model after using –NACHO–.
- In `DESCRIPTION`,
  - Order packages in alphabetical order.
  - Add packages’ version.

### Breaking changes

- `summarise()` and `summarize()` have been deprecated and replaced with
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md).
  ([\#12](https://github.com/mcanouil/NACHO/issues/12) &
  [\#15](https://github.com/mcanouil/NACHO/issues/15))
- Counts matrices (`raw_counts` and `normalised_counts`) are no longer
  (directly) available, -i.e.-, counts are available in a long format
  within the `nacho` slot of a nacho object.
- [`visualise()`](https://m.canouil.dev/NACHO/dev/reference/visualise.md),
  now uses a new shiny app (`inst/app/`).

### Minor improvements and fixes

- In `R/visualise.R`, `R/render.R`,
  [`print()`](https://rdrr.io/r/base/print.html), `R/load_rcc.R` and
  `R/normalise.R`,
  - replace function to check for outliers, now uses
    [`check_outliers()`](https://m.canouil.dev/NACHO/dev/reference/check_outliers.md).
- In `R/visualise.R`, replace datatable (render and output) with
  classical table. ([\#13](https://github.com/mcanouil/NACHO/issues/13))
- In `R/autoplot.R`,
  - add `show_outliers` to show outliers differently on plots (-i.e.-,
    in red).
  - add `outliers_factor` to highlight outliers with different point
    size.
  - add `outliers_labels` to print labels on top of outliers.
  - now uses tidyeval via import.
  - remove plexset ID (`_S-`) to remove duplicated QC metrics.
- In `R/print.R`, now print a table with outliers if any (with
  `echo = TRUE`).
- In `R/GSE74821.R`, dataset is up to date according to NACHO functions.

## NACHO 0.6.1

CRAN release: 2019-10-12

### Minor improvements and fixes

- In `DESCRIPTION`, add
  `"SystemRequirements: pandoc (>= 1.12.3) - http://pandoc.org, pandoc-citeproc"`.
- In `R/render.R`,
  - explicit import for `opts_chunk::knitr` in roxygen documentation.
  - explicit import for
    [`sessioninfo::session_info`](https://sessioninfo.r-lib.org/reference/session_info.html)
    in roxygen documentation.
- In `tests/testthat/test-render.R`, now checks if pandoc is available.
- In `tests/testthat/test-summarise.R`, fix tests when connection to GEO
  is alternatively up/down between two tests.

## NACHO 0.6.0

CRAN release: 2019-10-07

### Citation

- Add citation ([\#8](https://github.com/mcanouil/NACHO/issues/8)).

### New features

- [`autoplot()`](https://m.canouil.dev/NACHO/dev/reference/autoplot.md)
  allows to plot a chosen QC plot available in the shiny app
  ([`visualise()`](https://m.canouil.dev/NACHO/dev/reference/visualise.md))
  and/or in the HTML report
  ([`render()`](https://m.canouil.dev/NACHO/dev/reference/render.md)).
- [`print()`](https://rdrr.io/r/base/print.html) allows to print the
  structure or to print text and figures formatted using markdown
  (mainly to be used in a R Markdown chunk).
- [`render()`](https://m.canouil.dev/NACHO/dev/reference/render.md)
  render figures from
  [`visualise()`](https://m.canouil.dev/NACHO/dev/reference/visualise.md)
  in a HTML friendly output.

### Minor improvements and fixes

- In `R/read_rcc.R`, `R/summarise.R`,
  - fix issue ([\#1](https://github.com/mcanouil/NACHO/issues/1)) when
    PlexSet RCC files could not be read.
  - update code to use `tidyr` 1.0.0
    ([\#9](https://github.com/mcanouil/NACHO/issues/9)).
- In `R/summarise.R`,
  - object returned is of S3 class “nacho” for ease of use of
    [`autoplot()`](https://m.canouil.dev/NACHO/dev/reference/autoplot.md).
  - update code to use `tidyr` 1.0.0
    ([\#9](https://github.com/mcanouil/NACHO/issues/9)).
- In `R/normalise.R`,
  - object returned is of S3 class “nacho” for ease of use of
    [`autoplot()`](https://m.canouil.dev/NACHO/dev/reference/autoplot.md).
  - fix missing `outliers_thresholds` component in returned object.
- In `R/visualise.R`,
  - minor code changes.
  - return `app` object in non-interactive session.
- In `vignettes/NACHO.Rmd`,
  - fix several typos.
  - add sections for
    [`autoplot()`](https://m.canouil.dev/NACHO/dev/reference/autoplot.md),
    [`print()`](https://rdrr.io/r/base/print.html) and
    [`render()`](https://m.canouil.dev/NACHO/dev/reference/render.md)
    ([\#7](https://github.com/mcanouil/NACHO/issues/7)).
  - fix chunk output (-i.e.-, remove default `results = "asis"`).
  - fix
    [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md)
    call with custom housekeeping genes (-i.e.-, set
    `housekeeping_predict = FALSE`)
    ([\#10](https://github.com/mcanouil/NACHO/issues/10)).

## NACHO 0.5.6

CRAN release: 2019-04-29

### Minor improvements and fixes

- In `tests/testthat/test-summarise.R`, add condition to handle when
  `GEOQuery` is down and cannot retrieve online data.
- In `vignettes/NACHO.Rmd`, add condition to handle when `GEOQuery` is
  down and cannot retrieve online data.

## NACHO 0.5.5

CRAN release: 2019-04-28

### Minor improvements and fixes

- In `R/summarise.R`, put example in `if (interactive()) {...}` instead
  of `\dontrun{...}`.
- In `R/normalise.R`, put example in `if (interactive()) {...}` instead
  of `\dontrun{...}`.
- In `R/visualise.R`, put example in `if (interactive()) {...}` instead
  of `\dontrun{...}`.
- In `DESCRIPTION` and `README`, description updated for CRAN, by adding
  “messenger-RNA/micro-RNA”.

## NACHO 0.5.4

### Minor improvements and fixes

- In `R/normalise.R`, add short running example for
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).
- In `R/visualise.R`, add short running example for
  [`visualise()`](https://m.canouil.dev/NACHO/dev/reference/visualise.md).
- In `DESCRIPTION`, description updated for CRAN, by removing some
  capital letters and put –NACHO– between single quotes.

## NACHO 0.5.3

### Minor improvements and fixes

- Bold letters for –NACHO– in title.
- In `DESCRIPTION`, title and description updated for CRAN.
- Add NanoString reference in `DESCRIPTION` and vignette

## NACHO 0.5.2

### Minor improvements and fixes

- In `DESCRIPTION`, title and description updated for CRAN.

## NACHO 0.5.1

### Minor improvements and fixes

- Vignette uses bib file for references.
- Update URL in DESCRIPTION.

## NACHO 0.5.0

### New features

- `summarise()` imports and pre-process RCC files.
- [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md)
  allows to change settings used in `summarise()` and exclude outliers.
- [`visualise()`](https://m.canouil.dev/NACHO/dev/reference/visualise.md)
  allows customisation of the quality thresholds.
- Minor changes

### Minor improvements and fixes

- Add a README.
- Add logo.
- In `summarise()`, `ssheet_csv` can take a data.frame or a csv file.
- Change in package title with capital letters corresponding to NACHO.
- Add tests using testthat.

## NACHO 0.4.0

- Fix major errors, bad behaviour and typos.

## NACHO 0.3.1

- Add and fill roxygen documentation.

## NACHO 0.3.0

- Code optimisation in
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md)
  (and internal functions).
- [`visualise()`](https://m.canouil.dev/NACHO/dev/reference/visualise.md)
  replaces the Shiny app.

## NACHO 0.2.2

- Remove S4 class =\> Back to list object.

## NACHO 0.2.1

- Rewrite GEO dataset.

## NACHO 0.2.0

- Complete rewrite of `summarise()` and
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md)
  (and all internal functions).
- Add S4 class object.

## NACHO 0.1.0

- First version.
