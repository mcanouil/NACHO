# NACHO (development version)

## New features

* In `R/conflicts.R`, conflicts are now printed when attaching `NACHO`.
* In `DESCRIPTION`, 
    - Order packages in alphabetical order.
    - Add packages' version.

# NACHO 0.6.1

## Minor improvements and fixes

* In `DESCRIPTION`, add `"SystemRequirements: pandoc (>= 1.12.3) - http://pandoc.org, pandoc-citeproc"`.
* In `R/render.R`, 
  - explicit import for `opts_chunk::knitr` in roxygen documentation.
  - explicit import for `sessioninfo::session_info` in roxygen documentation.
* In `tests/testthat/test-render.R`, now checks if pandoc is available.
* In `tests/testthat/test-summarise.R`, fix tests when connection to GEO is alternatively up/down between two tests.


# NACHO 0.6.0

## Citation

* Add citation (#8).

## New features

* `autoplot()` allows to plot a chosen QC plot available in the shiny app (`visualise()`) and/or
  in the HTML report (`render()`).
* `print()` allows to print the structure or to print text and figures formatted using markdown
  (mainly to be used in a Rmakrdown chunk).
* `render()` render figures from `visualise()` in a HTML friendly output.

## Minor improvements and fixes

* In `R/read_rcc.R`, `R/summarise.R`, 
  - fix issue (#1) when PlexSet RCC files could not be read.
  - update code to use `tidyr` 1.0.0 (#9).
* In `R/summarise.R`, 
  - object returned is of S3 class "nacho" for ease of use of `autoplot()`.
  - update code to use `tidyr` 1.0.0 (#9). 
* In `R/normalise.R`, 
    - object returned is of S3 class "nacho" for ease of use of `autoplot()`.
    - fix missing `outliers_thresholds` component in returned object.
* In `R/visualise.R`, 
    - minor code changes.
    - return `app` object in non-interactive session.
* In `vignettes/NACHO.Rmd`, 
    - fix several typos.
    - add sections for `autoplot()`, `print()` and `render()` (#7).
    - fix chunk output (*i.e.*, remove default `results = "asis"`).
    - fix `normalise()` call with custom housekeeping genes (*i.e.*, set `housekeeping_predict = FALSE`) (#10).


# NACHO 0.5.6

## Minor improvements and fixes

* In `tests/testthat/test-summarise.R`, add condition to handle when `GEOQuery` is down and cannot retrieve online data.
* In `vignettes/NACHO.Rmd`, add condition to handle when `GEOQuery` is down and cannot retrieve online data.


# NACHO 0.5.5

## Minor improvements and fixes

* In `R/summarise.R`, put example in `if (interactive()) {...}` instead of `\dontrun{...}`.
* In `R/normalise.R`, put example in `if (interactive()) {...}` instead of `\dontrun{...}`.
* In `R/visualise.R`, put example in `if (interactive()) {...}` instead of `\dontrun{...}`.
* In `DESCRIPTION` and `README`, description updated for CRAN, by adding "messenger-RNA/micro-RNA".


# NACHO 0.5.4

## Minor improvements and fixes

* In `R/normalise.R`, add short running example for `normalise()`.
* In `R/visualise.R`, add short running example for `visualise()`.
* In `DESCRIPTION`, description updated for CRAN, by removing some capital letters 
 and put **NACHO** between single quotes.


# NACHO 0.5.3

## Minor improvements and fixes

* Bold letters for **NACHO** in title.
* In `DESCRIPTION`, title and description updated for CRAN.
* Add NanoString reference in `DESCRIPTION` and vignette


# NACHO 0.5.2

## Minor improvements and fixes

* In `DESCRIPTION`, title and description updated for CRAN.


# NACHO 0.5.1

## Minor improvements and fixes

* Vignette uses bib file for references.
* Update URL in DESCRIPTION.


# NACHO 0.5.0

## New features

* `summarise()` imports and pre-process RCC files.
* `normalise()` allows to change settings used in `summarise()` and exclude outliers.
* `visualise()` allows customisation of the quality thresholds.
* Minor changes

## Minor improvements and fixes

* Add a README.
* Add logo.
* In `summarise()`, `ssheet_csv` can take a data.frame or a csv file.
* Change in package title with capital letters corresponding to NACHO.
* Add tests using testthat.


# NACHO 0.4.0

* Fix major errors, bad behaviour and typos.


# NACHO 0.3.1

* Add and fill roxygen documentation.


# NACHO 0.3.0 

* Code optimisation in `normalise()` (and internal functions).
* `visualise()` replaces the Shiny app.


# NACHO 0.2.2

* Remove S4 class => Back to list object.


# NACHO 0.2.1

* Rewrite GEO dataset.


# NACHO 0.2.0

* Complete rewrite of `summarise()` and `normalise()` (and all internal functions).
* Add S4 class object.


# NACHO 0.1.0

* First version.
