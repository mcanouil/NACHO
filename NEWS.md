# NACHO (development version)

## Breaking changes

If you use NACHO 2, you can stay on NACHO 2.*, or convert your saved objects with `upgrade_nacho()` and `read_nacho()`.
The article "Moving from NACHO 2" on the [package website](https://m.canouil.dev/NACHO/) maps NACHO 2 code to NACHO 3.

- In `R/nacho-class.R`, `R/accessors.R` and `R/methods.R`,
  - feat: `load_rcc()` and `normalise()` return an S7 `nacho` object.
    Read it with `nacho_counts()`, `nacho_samples()`, `nacho_probes()` and `nacho_qc()`.
    The `$nacho` long table and the other list slots are gone; `as.data.frame(x, long = TRUE)` gives the long layout.
  - feat: `print()` shows a short summary, and the full report comes from `render()`.
  - feat: The path the data were loaded from is now part of the object's provenance, and `GSE74821` no longer stores it.
- In `R/conditions.R`,
  - feat: Errors, warnings and messages use cli and carry classes such as `nacho_error_bad_argument`, so your code can catch the ones it expects.
- In `R/load_rcc.R`,
  - feat: `load_rcc()` refuses duplicated sample ids in single-sample RCC files.
    PlexSet files can repeat an id, because `plexset_id` tells their samples apart.
  - feat: `load_rcc()` refuses RCC files whose probes clash, that is, share a name but differ in code class or accession, and the error names those probes.
  - feat: `load_rcc()` refuses an RCC sample that repeats a probe name, with a clear error.
  - feat: miRNA panels are no longer normalised with housekeeping genes by default, because their housekeeping mRNAs sit at background.
    To turn it back on, pass `housekeeping_genes`, `housekeeping_predict = TRUE` or `housekeeping_norm = TRUE`.
- In `R/accessors.R` and `R/normalise.R`,
  - feat: `nacho_qc()` replaces `is_outlier` with a status for each metric, `n_flags`, an overall `status` and a readable `reason`.
    `check_outliers()` is gone, because the status always follows the object's thresholds.
  - feat: `exclude_outliers()` drops the flagged samples and normalises the rest again.
    `normalise()` no longer has `remove_outliers`.
- In `R/autoplot.R`,
  - feat: `autoplot()` takes the plot name in `type` instead of `x`, and points NACHO 2 code that still passes `x` to the new argument.
  - feat: Plot labels, help pages and the threshold labels in the app use US spelling, such as "Normalized" and "Hemolysis".
  - feat: Plots use the NACHO brand.
    Groups take the Okabe-Ito colours, up to eight groups on a light background and seven on a dark one, and viridis beyond that.
    Flagged samples are rust triangles instead of red points.
- In `R/render.R`,
  - feat: `render()` builds the report with Quarto instead of R Markdown, as a self-contained HTML file, or as a Typst PDF with `format = "typst"`.
    It needs the Quarto command-line interface 1.9.18 or newer, which RStudio and Positron bundle.
  - feat: `render()` takes the object as `x`, writes `nacho-report.html` or `nacho-report.pdf` to `output_dir`, and returns the path.
    `output_file`, `show_outliers` and `clean` are gone.
- In `R/thresholds.R`,
  - feat: Samples are flagged against the nSolver thresholds by default.
    `nacho_thresholds(preset = "legacy")` keeps the NACHO 2 limits, and with `background = "geo", background_mode = "subtract"` it gives the NACHO 2 outlier calls back.
- In `R/qc.R`,
  - feat: Negative probes are excluded with Bruker's rule, which drops at most two probes that sit 3-fold above the others.
    The legacy preset keeps the NACHO 2 rule.
  - feat: In the nSolver preset, positive control linearity leaves out POS_F and adds 1 to every count.
  - feat: Under the default nSolver preset, the ligation controls of miRNA panels flag samples.
  - feat: Normalised counts are no longer rounded or floored at 0.1.
  - feat: There is no background correction by default (`background = "none"`).
    Choose a statistic with `background`, and whether to threshold or subtract with `background_mode`.
    `background = "geo", background_mode = "subtract"` gives the NACHO 2 correction, and subtracting floors the counts at 0.
  - feat: `Negative_factor` is always the geometric mean of the kept negative controls, with `normalisation_method = "GLM"` too.
    The new `Background` column holds the background that was applied.
    So with `"GLM"`, normalised counts differ from NACHO 2, which subtracted the model intercept.
  - feat: `housekeeping_predict = TRUE` picks the five most stable genes by geNorm, among the genes above background in at least 90% of samples, instead of the five with the smallest spread.
- In `R/interop.R`,
  - feat: `as_nacho()` no longer normalises a miRNA `SummarizedExperiment` without saved settings with housekeeping genes, unless you set `housekeeping_norm = TRUE` or give `housekeeping_genes`.
- In `R/deploy.R`,
  - feat: `deploy()` has no default `directory` any more, so pass the folder yourself; it used to be `/srv/shiny-server`.
- In `DESCRIPTION`,
  - build: NACHO needs R 4.3 or newer.
  - build: knitr and rmarkdown move to Suggests, and quarto joins them.
  - build: The app no longer needs shinyWidgets or markdown, and needs shiny 1.11.0 or newer.
- In `R/visualise.R`,
  - feat: `visualise()` returns the tuned object when you click "Done", and no longer writes `nacho_object.rds` to `tempdir()`.

## New features

- In `R/render.R`,
  - feat: The report reads as a quality-control record.
    It opens with a cover holding the study details, then a summary of the decision, with the flagged samples and notes on how the data were processed.
    A table lists every setting and limit with its meaning and source, each figure and table has a short "How to read this", and an appendix gives the methods, the citation and the package versions.
  - feat: `render()` gains `title` and `author` for the report cover, and the Export page of the app has the same fields.
  - feat: The HTML and PDF reports have the same content and the NACHO look.
    The PDF is a tagged PDF that declares PDF/UA-1, and Typst checks it against the PDF/UA-1 rules it can enforce.
    The HTML report has landmarks, a skip link and labelled tables.
  - feat: `render()` gains `group`, which adds the batch design and its cross-tables, and warns when batch and biology are confounded.
- In `R/nacho-class.R`, `R/accessors.R` and `R/methods.R`,
  - feat: `x[, j]` subsets samples and `x[i, ]` subsets probes, and both recompute the PCA and the outlier flags.
    Subsetting probes, or `filter_detected()`, also removes the dropped genes from the `housekeeping_genes` setting.
    When none are left, the object warns that it uses its Housekeeping probes, or, when it has none, turns `housekeeping_norm` off with a warning.
- In `R/read_nacho.R`,
  - feat: `upgrade_nacho()` converts a NACHO 2 object, and `read_nacho()` reads an object saved by any NACHO version.
  - feat: Upgraded NACHO 2 objects move to the legacy preset, with `background = "geo", background_mode = "subtract"`, so they keep their NACHO 2 flags and background correction.
    Use `normalise()` to switch them to the nSolver preset.
  - feat: `print()` and `format()` on a NACHO 2 object show one line that points to `upgrade_nacho()`.
    `summary()`, `as.data.frame()`, `[` and `autoplot()` refuse it with a classed error.
- In `R/interop.R`,
  - feat: `as_summarized_experiment()` and `as_nacho()` convert between `nacho` objects and `SummarizedExperiment`.
    `as_nacho()` also reads a `NanoStringRccSet` from NanoStringNCTools, with the same probe names as `load_rcc()` on the same RCC files.
- In `R/nacho-class.R`,
  - feat: Outlier thresholds accept `-Inf` as a lower bound, `Inf` as an upper bound, and `-Inf` as `LoD`, to mean no bound.
    `normalise()`, `upgrade_nacho()` and `read_nacho()` refuse `NaN`, an `Inf` lower bound, a negative upper bound and an `Inf` `LoD`, with a classed error.
    To read a saved object with `LoD = Inf`, first change it to a finite value or `-Inf`.
    `autoplot()` draws no threshold line for an open bound, `summary()` shows it as `NA`, and the report leaves it out.
- In `R/thresholds.R` and `R/load_rcc.R`,
  - feat: `nacho_thresholds()` builds thresholds for the MAX, FLEX, PRO and SPRINT instruments.
    `load_rcc()` gains `instrument` and `preset`, and reads the instrument from MAX/FLEX files.
    When it can't read the instrument, it warns with class `nacho_warning_instrument_unknown` and uses the MAX/FLEX values.
- In `R/thresholds.R`, `R/mod_thresholds.R` and `R/mod_export.R`,
  - feat: `write_thresholds()` saves thresholds to a YAML file and `read_thresholds()` reads them back, so you can use one set of limits for every study.
    The Export page of the app writes the same file, and the Preset section of the sidebar loads it.
    A file without a miRNA threshold takes it from the preset named in the file.
- In `R/detection.R` and `R/accessors.R`,
  - feat: A gene counts as detected in a sample when its count is above the mean of the kept negative probes plus two standard deviations.
    `nacho_samples()` gains `Detection_rate`, the share of endogenous genes each sample detects, and `nacho_probes()` gains `detection_rate`, the share of samples that detect each gene.
  - feat: `filter_detected(x, min_rate)` keeps the genes detected often enough, where `min_rate` is the share of samples in which a gene must be detected.
- In `R/qc-table.R`,
  - feat: For PlexSet files, `nacho_qc()` has a `lane_status`, and a lane that fails flags its eight samples.
    Their reason reads "lane fails" followed by the metric name.
- In `R/methods.R`,
  - feat: `print()` lists the excluded negative probes.
- In `R/qc.R`,
  - feat: Quality control runs on data without lane attributes.
    The metrics that need them are `NA`, with one warning.
  - feat: Following Bruker's RNA content check, `nacho_qc()` flags samples with too few housekeeping genes above background.
    `Housekeeping_detected` holds the count, and the nSolver preset flags a sample below 3, while the legacy preset uses 0 and never flags.
  - feat: A sample with fewer than two negative probes with counts has no detection limit, so its `Detection_rate` and `Housekeeping_detected` are missing.
    `load_rcc()` then gives one `metric_unavailable` warning that names those samples.
  - feat: GLM normalisation starts from the least squares line and checks that it converges.
    When a sample doesn't fit, it falls back to the geometric mean with a classed warning, the stored `normalisation_method` reads `"GEO"`, and `provenance$glm_fallback` lists the samples that failed.
- In `R/conditions.R`,
  - feat: `options(nacho.quiet = TRUE)` silences the progress and informative messages.
- In `R/load_rcc.R`,
  - feat: `load_rcc()` and `normalise()` check their arguments before they read any file.
  - feat: `load_rcc()` reads gzipped RCC files directly, and accepts files with Windows line endings.
  - feat: `background` and `background_mode` choose how the negative controls correct each sample.
- In `R/normalise.R`,
  - feat: `normalise()` checks `outliers_thresholds` before it runs.
  - feat: `?normalise` documents the order of the normalisation steps.
  - feat: When you change only `outliers_thresholds` in `normalise()`, it recalculates the flags without rebuilding the object.
  - feat: `normalise()` refuses a thresholds list written for NACHO 2, one without `preset`, and the error points to `nacho_thresholds()`.
- In `R/autoplot.R`,
  - feat: `autoplot()` checks that `colour` and `outliers_labels` name columns of `nacho_samples()`, and `outliers_labels` must be a column name.
  - feat: `autoplot()` gains `dark`, which draws the plot for a dark background, with amber flags.
- In `inst/extdata/`,
  - feat: `GSE346526` holds six mRNA samples in RCC files written by nSolver 4.0.1.8, the most recent nSolver version on GEO in October 2026.
- In `inst/brand/`,
  - feat: NACHO ships its brand file with the Source Sans 3 and JetBrains Mono fonts, so the app, the report and the website look the same, even offline.
- In `R/stability.R`,
  - feat: `housekeeping_stability()` ranks reference genes with geNorm and NormFinder, gives the geNorm pairwise variation, and tests each gene against a biological group.
    `autoplot(x, type = "Stability")` draws the ranking.
- In `R/ruv.R`,
  - feat: `normalisation_method = "RUVg"` removes unwanted variation estimated from the housekeeping genes, and adds the factors to `nacho_samples()` as `W_1`, `W_2`, and so on.
    `suggest_ruv_k()` suggests how many factors to remove.
    `normalise()` lowers an explicit `ruv_k` to two less than the number of samples, or to one less than the number of control genes, with a `nacho_warning_ruv_k_reduced` warning, because a larger `k` fits the samples exactly.
- In `R/mirna.R`,
  - feat: NACHO recognises miRNA panels and offers `normalisation_method = "stable_mirna"`, `"total_mirna"`, `"spike_in"` and `"ligation"`.
  - feat: `nacho_qc()` checks the ligation controls of miRNA panels, and `nacho_thresholds(haemolysis = TRUE)` flags haemolysed plasma and serum samples.
  - feat: `load_rcc()` warns when more than half of the samples fail `Ligation_NEG`, and shows how to relax the threshold.
    This happens with whole-blood RNA, where miRNAs from red blood cells raise `LIG_NEG_A`.
- In `R/batch.R` and `R/autoplot.R`,
  - feat: `batch_diagnostics()` crosses the study groups with cartridges and dates, with Cramér's V and a flag when a batch holds a single group.
    It also tests the QC metrics against each batch, and measures how much of each principal component each batch explains.
    `autoplot()` draws `"RLE"`, `"BatchFactors"` and `"PCBatch"`.
- In `R/app.R`,
  - feat: `nacho_app()` returns the app as a Shiny app object, so you can run it from any R session or deploy it.
    `deploy()` copies a one-line `app.R` that calls it, and the "Done" button only appears with `nacho_app(done = TRUE)`, which `visualise()` uses.
  - feat: The app starts from the object's own thresholds and keeps its preset and instrument.
    It offers RUVg, the miRNA methods and the background settings.
  - feat: The app has one sidebar with every threshold and the instrument presets, and a reset button puts the thresholds back to the preset.
  - feat: A summary strip at the top of every page shows the samples, the cartridges (lanes for PlexSet), the flagged samples with their reasons, and the method.
  - feat: Each plot sits in a card that opens full screen, and interactive plots redraw to fill the card.
    The display options of a card set the colour, legend, point size and labels, and you can download the plot as a PNG at the size you choose.
  - feat: A dark mode toggle in the navbar switches the app and its plots.
  - feat: A guided tour in the Help menu walks you through the app, and loads the example data first when no data are loaded.
  - feat: Each threshold has a help popover next to it, and upload errors and normalisation warnings show once, as toasts.
  - feat: Before the batch plots, the Batch page shows the design and cross-tables of the batches against the biological groups.
  - feat: Pages without data explain what to do, and "Load the example data" opens GSE74821.
  - feat: The app uses the NACHO brand, with a navy navbar and the brand fonts in light and dark mode.
  - feat: With ggiraph installed, hovering over a point shows the sample and its value, and clicking it outlines that sample in every plot that shows one point per sample.
    The "Highlight a sample" list on the Samples page does the same from the keyboard.
  - feat: The Export page downloads the quality-control table as CSV, the thresholds as YAML, the object as RDS, and the report, with an optional title and author.
    The report can be HTML or PDF, and it renders in the background when mirai is installed.
    If you close the app page during a render, the render stops.
  - feat: The Export page also shows and downloads an R script that repeats the analysis of the app with `load_rcc()`, `normalise()` and the thresholds you set.
  - feat: A Help menu in the navbar opens About NACHO, links to the documentation, GitHub Discussions and the issue tracker, and shows how to cite NACHO.
  - feat: The Samples page lists every sample in one table, flagged samples first, with its status as a coloured word, and a "Flagged only" switch hides the others.
    With reactable installed, you can sort and search the table, and click a row to select that sample in the plots.
- In `R/mod_data.R`,
  - feat: When you choose files, the Data page lists the RCC files of the upload before you import them, including those inside zip archives.
    For each file, it shows the zip it came from, whether it is a PlexSet file, and whether the sample sheet lists it.
- In `DESCRIPTION`,
  - build: chromote, gdtools, ggiraph, jsonlite, later, mirai, parallel, reactable and shinytest2 join Suggests, and promises and yaml join Imports.
- In `vignettes/`,
  - docs: The vignette shows the NACHO 3 workflow on the example RCC files shipped with the package.
    The GEO analysis moves to the article "Analyse a GEO study" on the website, with GSE178516.
  - docs: A new article on the website shows what the quality-control report looks like, with screenshots of a real report taken each time the site is built.

## Performance

- In `R/read_rcc.R`, `R/load_rcc.R` and `R/qc.R`,
  - perf: `load_rcc()` reads each RCC file once, with exact section tags, and computes quality control on count matrices.
    Loading 768 samples takes about 2.5 seconds, against about 9 in NACHO 2.
- In `R/autoplot.R`,
  - perf: On 768 samples, the relative log expression, normalisation and positive against negative plots draw in under a second, instead of 2 to 6 seconds.
    The trend line of the last two now fits the mean of each sample, which gives the same line, with a band that shows the spread between samples.
    Studies with fewer than seven samples keep the earlier trend, fitted on every point.
- In `R/mod_qc_plot.R` and `R/plot_worker.R`,
  - perf: With mirai installed, the app builds the plots of a page in parallel, on a small pool of workers shared by all sessions.
    The app stays responsive while they build, and stops building the plots of a page you leave.
    `options(nacho.plot_workers = n)` sets the number of workers, and `0` builds the plots in the app process; the app from `deploy()` uses one worker.
    If a worker stops while the app runs, the app builds the plots itself.
  - perf: An interactive plot is built once and shared between the browser tabs and sessions of the same app.
- In `R/mod_data.R`,
  - perf: With mirai installed, the app reads uploaded RCC files in a background R process, so other sessions of the app stay responsive.
    The Data page shows the current stage of the import under the Import button.

## Fixes

- In `R/app.R`,
  - fix: The app detects PlexSet files from their exact code classes.
- In `R/autoplot.R`,
  - fix: `autoplot()` draws samples with a missing outlier flag as ordinary points, where NACHO 2 left them out of the plot.
- In `R/render.R`,
  - fix: `render()` no longer deletes a folder named `tmp_nacho` in `output_dir`.
- In `R/read_rcc.R`,
  - fix: Empty RCC attributes, such as a blank owner or comment, are read as empty text instead of repeating the attribute name.
  - fix: A probe named like an RCC section, such as `Messages`, no longer breaks parsing.
  - fix: Probe names keep every inner `|` field, so protein names such as `4E-BP1(53H11)|NA|EIF4EBP1|53H11|0` stay intact.
    Only a trailing pipe and number, such as `|0` or `|0.014`, is removed.
  - fix: Attribute values that contain `|` followed by digits, such as the comment `Batch|2`, are kept as written.
  - fix: Accessions are read exactly as written, and probes whose accessions differ between files are reported as a clash.
- In `R/load_rcc.R`,
  - fix: `load_rcc()` checks a `plexset_id` column when you supply one.
    Values outside `S1` to `S8`, and an id that repeats with the same `plexset_id`, raise a classed error before any file is read into a matrix.
  - fix: `load_rcc()` says when the sample sheet lists no file to read, instead of failing with an internal error.
- In `R/qc.R`,
  - fix: Housekeeping prediction no longer returns missing gene names when there are fewer than five candidates.
  - fix: Predicting housekeeping genes no longer misaligns probes when some RCC files lack a probe.
  - fix: PCA components have a fixed sign, so plots no longer flip between machines.

# NACHO 2.0.8

## Bug Fixes

- In `inst/app/`,
  - fix: render the help pages from their text, so the app no longer writes to the installed package when `R CMD check` runs it.
  - fix: open the help pages of the quality-control metrics, whose links never matched their input names.
- In `autoplot()`,
  - fix: set the smoothing formula of the `"PN"` and `"NORM"` plots, which silences the ggplot2 `geom_smooth()` message.

# NACHO 2.0.7

## Dependencies

- In `DESCRIPTION`,
  - build: require R 4.1.0 or newer, ggplot2 4.0.0 or newer, ggforce 0.5.0 or newer, ggrepel 0.9.6 or newer, and shiny 1.7.4 or newer, which pulls in fontawesome 0.4.0 and its Font Awesome 6 icon names.
  - build: require pandoc 2.11 or newer, which has citeproc built in, so pandoc-citeproc is no longer needed.
  - build: require knitr 1.39 or newer, which the report needs for `include_graphics(rel_path = FALSE)`.
  - build: require scales 1.4.0 or newer, which ggplot2 4.0.0 already needs, for the log-10 axes of the `"PFNF"` and `"HF"` plots.

## Chores

- In `inst/app/`,
  - refactor: replace the superseded `shiny::callModule()` with `shiny::moduleServer()`.
  - refactor: use the Font Awesome 6 icon names `file-arrow-up` and `circle-info`.
- In `R/`,
  - refactor: drop `stringsAsFactors = FALSE` from `data.frame()` and `as.data.frame()` calls, where it is the default since R 4.0.0.
  - refactor: hide boxplot outliers with `outliers = FALSE` instead of `outlier.shape = NA`.

## Documentation

- In `vignettes/`, `README.Rmd` and the `render()` report template,
  - docs: write chunk options as `#|` YAML comments, which needs knitr 1.35 or newer.
  - docs: add alternative text to the images.
  - docs: install the development version with `pak::pak()` instead of `remotes::install_github()`.
- docs: fix typos and grammar, and spell GitHub, NanoString, R Markdown and Shiny consistently.
- In `vignettes/`,
  - docs: keep only the GSE70970 samples measured with the `NS_H_miR_1.4` CodeSet, since its two CodeSets come from different nSolver versions and `load_rcc()` refuses files that mix versions.
  - docs: attach data.table in the analysis vignette, which uses `dcast()` and `as.data.table()`.
  - docs: link to GEO over https in the bibliography, since the http address now redirects.
- In `pkgdown/`,
  - docs: restyle the website for pkgdown 2.2 with the NACHO logo colours, a light and dark mode switch, and colour contrast that meets WCAG AA.
  - docs: group the reference index by task.
- In `R/normalise.R`, `R/load_rcc.R` and `vignettes/NACHO.Rmd`,
  - docs: remove the `raw_counts` and `normalised_counts` slots, which never existed, and say that `nacho` holds one row per sample and probe.
- In `R/GSE74821.R`,
  - docs: say that `GSE74821` holds 48 samples, not 20.

## Fixes

- In `inst/CITATION`,
  - fix: list the authors as `person()` objects, so the citation shows their full initials and spells Leen M. 't Hart correctly.
  - fix: cite the version of record, Bioinformatics volume 36, issue 3, pages 970 to 971, published in February 2020.
- In `R/autoplot.R`,
  - fix: stop boxplots inheriting the colour aesthetic, which made ggplot2 warn that it dropped `colour` for the control probe plots.
  - fix: draw the outlier bands of the `"PFNF"` and `"HF"` plots to the panel edges without log-10 warnings about infinite values.
  - fix: keep each sample's identifier in the `"BD"`, `"FoV"`, `"PCL"`, `"LoD"`, `"PN"`, `"Positive"`, `"Negative"` and `"Housekeeping"` plots, which drew every point at one position and dropped some of them.
- In `R/render.R`,
  - fix: include the logo by its absolute path, so pandoc finds it when the temporary directory sits behind a symbolic link.
  - fix: pass the report options as R Markdown parameters, so `outliers_labels = "CartridgeID"` works and column names with quotes no longer break the report.
- In `inst/app/app.R`,
  - fix: stop writing an `all.rdata` debug file to the working directory when uploading RCC files.
  - fix: load uploaded RCC files, which failed because `suppressMessages()` received `x` instead of `expr`.
  - fix: make the sample sheet optional again when uploading RCC files, instead of failing on a missing `ssheet_dt`.
- In `DESCRIPTION`,
  - fix: suggest markdown, which the app needs to show its help pages, and make `visualise()` ask for it when it is not installed. The `deploy()` help page notes that the server needs it too.
- In `R/geometric_housekeeping.R`,
  - fix: replace background-corrected housekeeping counts below 1 with 1, so values between 0 and 1 no longer inflate `House_factor`. ([#53](https://github.com/mcanouil/NACHO/issues/53))
- In `R/qc_pca.R`,
  - fix: compute the PCA with samples as observations and store their scores. `PC01` to `PC10` and the variance explained change, and the PCA plots now show the main sources of variation between samples.
- In `R/normalise_counts.R`,
  - fix: apply the 0.1 floor after rounding, so normalised counts at or below background are 0.1 instead of 0. With `housekeeping_predict = TRUE`, the floor can change which housekeeping genes are picked, which in turn changes `House_factor`, the normalised counts and the outlier flags.
- In `R/qc_pca.R` and `R/normalise_counts.R`,
  - fix: objects created with an earlier version keep the old values, so run `load_rcc()` again to refresh them.
- In `R/normalise.R`,
  - fix: use the `n_comp` passed to `normalise()`, which was ignored.
  - fix: recompute `is_outlier` when only the thresholds change.
- In `R/qc_features.R`, `R/qc_limit_detection.R` and `R/check_outliers.R`,
  - fix: report `PCL` and `LoD` as `NA` when a panel has no `POS_E` probe or when the negative controls do not vary, and do not flag a sample on a metric that could not be measured.
- In `R/load_rcc.R`,
  - fix: detect PlexSet files from their content, and add `plexset_id` `S1` to `S8` when the sample sheet lists each file once.
  - fix: stop converting the caller's sample sheet to a `data.table`.
  - fix: stop with a clear message when RCC files mix PlexSet and single-sample files.
  - fix: stop when a single-sample sample sheet lists the same RCC file twice.
- In `R/print.R`,
  - fix: return the object invisibly from `print()`.
- In `data/`,
  - fix: rebuild `GSE74821` with the corrected PCA and normalised counts.
- In `inst/app/`,
  - fix: set the full binding density range when switching between the MAX/FLEX and SPRINT presets.
  - fix: accept zip archives sent as `application/zip`, and `.RCC`, `.rcc`, `.RCC.gz` and `.csv` files in any case.
  - fix: stop warning about row names when unpacking a zip archive.
  - fix: show a notification when the sample sheet is discarded, instead of a warning in the R console.
  - fix: correct the typos in the card and outlier labels, and open the app on the QC metrics tab.

# NACHO 2.0.6

## Fixes

- In `R/qc_positive_control.R`,
  - fix: use R-squared instead of Pearson correlation coefficient. ([#48](https://github.com/mcanouil/NACHO/issues/48))

# NACHO 2.0.5

## Fixes

- In `R/autoplot.R`,
  - fix: set `height` in `ggplot2::position_jitter()` to `0` to avoid vertical dispersion points. ([#45](https://github.com/mcanouil/NACHO/issues/45))
- In `inst/app/www/about-nacho.md`,
  - fix: shiny[dot]rstudio[dot]com moved to <https://shiny.posit.co/>.

## Tests

- In `tests/testthat/test-load_rcc.R`,
  - fix: skip tests to decrease CRAN checks computation time.

**Full Changelog**: <https://github.com/mcanouil/NACHO/compare/v2.0.4...v2.0.5>

# NACHO 2.0.4

## Fixes

- In `inst/CITATION`,
  - fix: convert `citEntry` to `bibentry` from CRAN note.

**Full Changelog**: <https://github.com/mcanouil/NACHO/compare/v2.0.3...v2.0.4>

# NACHO 2.0.3

## Chores

- In `DESCRIPTION`,
  - chore: update domain name.

**Full Changelog**: <https://github.com/mcanouil/NACHO/compare/v2.0.2...v2.0.3>

# NACHO 2.0.2

## Chores

- In `DESCRIPTION`,
  - chore: update email address.
- chore: remove `ggbeeswarm`.

**Full Changelog**: <https://github.com/mcanouil/NACHO/compare/v2.0.1...v2.0.2>

# NACHO 2.0.1

## Chores

- In `DESCRIPTION`,
  - chore: update email address.

**Full Changelog**: <https://github.com/mcanouil/NACHO/compare/v2.0.0...v2.0.1>

# NACHO 2.0.0

## Major (breaking) changes

- Refactor to use `data.table` instead of `dplyr`/`tidyr`/`purrr`.

## Features

- Ensure RCC files are homogeneous in terms of versions. [#20](https://github.com/mcanouil/NACHO/issues/20)
- Allow to use vector of file paths, named or not. [#33](https://github.com/mcanouil/NACHO/issues/33)
- Allow to upload a CSV file associated with the RCC files within the `shiny` application. [#36](https://github.com/mcanouil/NACHO/issues/36)

Full Changelog: <https://github.com/mcanouil/NACHO/compare/v1.1.0...v2.0.0>

# NACHO 1.1.0

## Breaking changes

- In `DESCRIPTION`,
  - Update `ggplot2` version (>= 3.3.0).
  - Update `dplyr` version (>= 1.0.2).
- In `R/autoplot.R`,
  - Replace `ggplot2::expand_scale()` with `ggplot2::expansion()`.

## Minor improvements and fixes

- In `R/load_rcc.R`,
  - Remove deprecated `dplyr::progress_estimated()`.
- In `R/norm_glm.R`,
  - Remove deprecated `dplyr::progress_estimated()`.
- In `tests`,
  - Small tweaks.
  - Add condition when trying to download files from a GEO dataset (Fix CRAN checks).

# NACHO 1.0.2

## Minor improvements and fixes

- In `DESCRIPTION`,
  - Update URLs.
- In `R/normalise.R`,
  - Fix missing "outliers_thresholds" field after `normalise()` without removing outliers (#26).
- In `R/GSE74821.R`,
  - Now uses `data-raw` root directory.
- In `R/-`,
  - No longer generates `Rd` files for internal functions.

# NACHO 1.0.1

## Minor improvements and fixes

- Fix deprecated documentation for `R/load_rcc.R` and `R/normalise.R`.
- Use `file.path()` in examples and vignette.
- In `R/autoplot.R`, reduce alpha for ellipses.
- In `inst/app/utils.R`, set default point size (also for outliers) to `1`.
- In `R/load_rcc.R`, use `inherits()` instead of `class()`.
- Code optimisation.

# NACHO 1.0.0

## New features

- In `R/conflicts.R`,
  - conflicts are now printed when attaching `NACHO`.
  - `nacho_conflicts()` can be used to print conflicts.
- New Shiny app in `inst/app/`, (#4, #5 & #14)
  - as a regular app, to load directly RCC files individually or within zip archive.
  - within `visualise()`, to load `"nacho"` object from `load_rcc()` (previous `summarise()`)
        or from `normalise()`.
- New `deploy()` (`R/deploy.R`) function to easily deploy (copy) the shiny app.
- New raw RCC files (multiplexed) available in `inst/extdata/`.
- New vignette `NACHO-analysis`, which describe how to use `limma` or other model after using --NACHO--.
- In `DESCRIPTION`,
  - Order packages in alphabetical order.
  - Add packages' version.

## Breaking changes

- `summarise()` and `summarize()` have been deprecated and replaced with `load_rcc()`. (#12 & #15)
- Counts matrices (`raw_counts` and `normalised_counts`) are no longer (directly) available,
  -i.e.-, counts are available in a long format within the `nacho` slot of a nacho object.
- `visualise()`, now uses a new shiny app (`inst/app/`).

## Minor improvements and fixes

- In `R/visualise.R`, `R/render.R`, `print()`, `R/load_rcc.R` and `R/normalise.R`,
  - replace function to check for outliers, now uses `check_outliers()`.
- In `R/visualise.R`, replace datatable (render and output) with classical table. (#13)
- In `R/autoplot.R`,
  - add `show_outliers` to show outliers differently on plots (-i.e.-, in red).
  - add `outliers_factor` to highlight outliers with different point size.
  - add `outliers_labels` to print labels on top of outliers.
  - now uses tidyeval via import.
  - remove plexset ID (`_S-`) to remove duplicated QC metrics.
- In `R/print.R`, now print a table with outliers if any (with `echo = TRUE`).
- In `R/GSE74821.R`, dataset is up to date according to NACHO functions.

# NACHO 0.6.1

## Minor improvements and fixes

- In `DESCRIPTION`, add `"SystemRequirements: pandoc (>= 1.12.3) - http://pandoc.org, pandoc-citeproc"`.
- In `R/render.R`,
  - explicit import for `opts_chunk::knitr` in roxygen documentation.
  - explicit import for `sessioninfo::session_info` in roxygen documentation.
- In `tests/testthat/test-render.R`, now checks if pandoc is available.
- In `tests/testthat/test-summarise.R`, fix tests when connection to GEO is alternatively up/down between two tests.

# NACHO 0.6.0

## Citation

- Add citation (#8).

## New features

- `autoplot()` allows to plot a chosen QC plot available in the shiny app (`visualise()`) and/or
  in the HTML report (`render()`).
- `print()` allows to print the structure or to print text and figures formatted using markdown
  (mainly to be used in a R Markdown chunk).
- `render()` render figures from `visualise()` in a HTML friendly output.

## Minor improvements and fixes

- In `R/read_rcc.R`, `R/summarise.R`,
  - fix issue (#1) when PlexSet RCC files could not be read.
  - update code to use `tidyr` 1.0.0 (#9).
- In `R/summarise.R`,
  - object returned is of S3 class "nacho" for ease of use of `autoplot()`.
  - update code to use `tidyr` 1.0.0 (#9).
- In `R/normalise.R`,
  - object returned is of S3 class "nacho" for ease of use of `autoplot()`.
  - fix missing `outliers_thresholds` component in returned object.
- In `R/visualise.R`,
  - minor code changes.
  - return `app` object in non-interactive session.
- In `vignettes/NACHO.Rmd`,
  - fix several typos.
  - add sections for `autoplot()`, `print()` and `render()` (#7).
  - fix chunk output (-i.e.-, remove default `results = "asis"`).
  - fix `normalise()` call with custom housekeeping genes (-i.e.-, set `housekeeping_predict = FALSE`) (#10).

# NACHO 0.5.6

## Minor improvements and fixes

- In `tests/testthat/test-summarise.R`, add condition to handle when `GEOQuery` is down and cannot retrieve online data.
- In `vignettes/NACHO.Rmd`, add condition to handle when `GEOQuery` is down and cannot retrieve online data.

# NACHO 0.5.5

## Minor improvements and fixes

- In `R/summarise.R`, put example in `if (interactive()) {...}` instead of `\dontrun{...}`.
- In `R/normalise.R`, put example in `if (interactive()) {...}` instead of `\dontrun{...}`.
- In `R/visualise.R`, put example in `if (interactive()) {...}` instead of `\dontrun{...}`.
- In `DESCRIPTION` and `README`, description updated for CRAN, by adding "messenger-RNA/micro-RNA".

# NACHO 0.5.4

## Minor improvements and fixes

- In `R/normalise.R`, add short running example for `normalise()`.
- In `R/visualise.R`, add short running example for `visualise()`.
- In `DESCRIPTION`, description updated for CRAN, by removing some capital letters
 and put --NACHO-- between single quotes.

# NACHO 0.5.3

## Minor improvements and fixes

- Bold letters for --NACHO-- in title.
- In `DESCRIPTION`, title and description updated for CRAN.
- Add NanoString reference in `DESCRIPTION` and vignette

# NACHO 0.5.2

## Minor improvements and fixes

- In `DESCRIPTION`, title and description updated for CRAN.

# NACHO 0.5.1

## Minor improvements and fixes

- Vignette uses bib file for references.
- Update URL in DESCRIPTION.

# NACHO 0.5.0

## New features

- `summarise()` imports and pre-process RCC files.
- `normalise()` allows to change settings used in `summarise()` and exclude outliers.
- `visualise()` allows customisation of the quality thresholds.
- Minor changes

## Minor improvements and fixes

- Add a README.
- Add logo.
- In `summarise()`, `ssheet_csv` can take a data.frame or a csv file.
- Change in package title with capital letters corresponding to NACHO.
- Add tests using testthat.

# NACHO 0.4.0

- Fix major errors, bad behaviour and typos.

# NACHO 0.3.1

- Add and fill roxygen documentation.

# NACHO 0.3.0

- Code optimisation in `normalise()` (and internal functions).
- `visualise()` replaces the Shiny app.

# NACHO 0.2.2

- Remove S4 class => Back to list object.

# NACHO 0.2.1

- Rewrite GEO dataset.

# NACHO 0.2.0

- Complete rewrite of `summarise()` and `normalise()` (and all internal functions).
- Add S4 class object.

# NACHO 0.1.0

- First version.
