
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NAnoString quality Control dasHbOard <img src="man/figures/nacho_hex.png" align="right" width="120" alt="NACHO hexagonal logo" />

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![GitHub
tag](https://img.shields.io/github/tag/mcanouil/NACHO.svg?label=latest%20tag&include_prereleases)](https://github.com/mcanouil/NACHO)
[![codecov](https://codecov.io/gh/mcanouil/NACHO/branch/main/graph/badge.svg)](https://app.codecov.io/gh/mcanouil/NACHO?branch=main)
[![R-CMD-check](https://github.com/mcanouil/NACHO/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/mcanouil/NACHO/actions/workflows/R-CMD-check.yml)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-ago/NACHO)](https://cran.r-project.org/package=NACHO)
[![cran
checks_worst](https://badges.cranchecks.info/worst/NACHO.svg)](https://cran.r-project.org/web/checks/check_results_NACHO.html)
[![CRAN_Download_total](https://cranlogs.r-pkg.org/badges/NACHO)](https://cran.r-project.org/package=NACHO)
<!-- badges: end -->

## Installation

``` r
# Install NACHO from CRAN:
install.packages("NACHO")

# Or the development version from GitHub:
# install.packages("pak")
pak::pak("mcanouil/NACHO")
```

## Overview

*NACHO* (**NA**noString quality **C**ontrol das**H**b**O**ard) works
with NanoString nCounter data. An nCounter assay measures messenger RNA
and micro RNA (mRNA/miRNA) expression with fluorescent barcodes. Each
barcode is assigned to an mRNA or miRNA, and it is counted after it
binds its target. Each count of a barcode therefore shows how much of
its target is present.

*NACHO* loads, visualizes and normalizes the exported nCounter data, and
it helps you to check its quality. It shows quality control metrics, the
expression of control genes, principal components and sample-specific
size factors in an interactive web application.

Two functions summarize and visualize the RCC files:

- The `load_rcc()` function preprocesses the data.
- The `visualise()` function starts a [Shiny-based
  dashboard](https://shiny.posit.co/) with all relevant QC plots.

*NACHO* also has a `normalise()` function. It calculates the
sample-specific size factors again and normalizes the data.

- The `normalise()` function returns a `nacho` object that holds your
  settings, the raw counts and the normalized counts.

Since v0.6.0, *NACHO* has two more functions:

- The `render()` function writes a full quality control report with
  Quarto, as an HTML or PDF file, from the result of `load_rcc()` or
  `normalise()`.
- The `autoplot()` function draws the quality control metrics of
  `visualise()` and `render()`.

For more information, see `vignette("NACHO")` and
`vignette("NACHO-analysis")`.

### Shiny Application ([demo](https://mcanouil.shinyapps.io/NACHO_data/))

Open the app on a `nacho` object with `visualise()`.

``` r
visualise(GSE74821)
```

<img src="man/figures/README-nacho_app.gif" alt="Short recording of the NACHO app on the example data. The summary strip changes when the field of view threshold moves to 95 and one sample is flagged. A selected sample is outlined in the QC metrics plots. The recording then shows the Samples, Normalisation and Batch pages, the Help menu, and the Export page with a report ready to download." width="100%" />

The article [Explore quality control in the
app](https://m.canouil.dev/NACHO/articles/nacho-app.html) shows each
page.

## Citing NACHO

<p>

Canouil M, Bouland GA, Bonnefond A, Froguel P, ’t Hart LM, Slieker RC
(2020). “NACHO: an R package for quality control of NanoString nCounter
data.” <em>Bioinformatics</em>, <b>36</b>(3), 970–971. ISSN 1367-4803.
<a href="https://doi.org/10.1093/bioinformatics/btz647">doi:10.1093/bioinformatics/btz647</a>.
</p>

    @Article{Canouil2020,
      title = {{NACHO}: an {R} package for quality control of {NanoString} {nCounter} data},
      author = {Mickaël Canouil and Gerard A. Bouland and Amélie Bonnefond and Philippe Froguel and Leen M. {'t Hart} and Roderick C. Slieker},
      journal = {Bioinformatics},
      year = {2020},
      month = {feb},
      volume = {36},
      number = {3},
      pages = {970--971},
      issn = {1367-4803},
      doi = {10.1093/bioinformatics/btz647},
    }

------------------------------------------------------------------------

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [GitHub](https://github.com/mcanouil/NACHO/issues).\
For questions and other discussion, please contact the package
maintainer.

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).\
By contributing to this project, you agree to abide by its terms.
