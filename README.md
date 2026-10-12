
# NAnoString quality Control dasHbOard <img src="man/figures/nacho_hex.png" align="right" width="120" alt="NACHO hexagonal logo" />

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

## Installation

``` r
# Install NACHO from CRAN:
install.packages("NACHO")

# Or the development version from GitHub:
# install.packages("pak")
pak::pak("mcanouil/NACHO")
```

## Quick start

The package ships six samples of the public study GSE178516, so this
runs offline. `load_rcc()` reads the RCC files, computes the
quality-control metrics and normalises the counts.

``` r
library(NACHO)

data_directory <- system.file("extdata", "GSE178516", package = "NACHO")
nacho <- load_rcc(
  data_directory = data_directory,
  ssheet_csv = file.path(data_directory, "samplesheet.csv"),
  id_colname = "IDFILE",
  instrument = "max",
  n_comp = 5
)
#> ℹ Reading 6 RCC files
#> ✔ Reading 6 RCC files [26ms]
#> 
#> ℹ Assembling 6 samples
#> ✔ Assembling 6 samples [17ms]
#> 
#> ℹ Computing quality-control metrics and normalising
#> ✔ Computing quality-control metrics and normalising [31ms]
#> 
nacho_qc(nacho)[, c("IDFILE", "status", "n_flags", "reason")]
#>                  IDFILE status n_flags reason
#> 1 GSM5393247_S02.RCC.gz   pass       0   <NA>
#> 2 GSM5393248_S13.RCC.gz   pass       0   <NA>
#> 3 GSM5393250_S15.RCC.gz   pass       0   <NA>
#> 4 GSM5393251_S16.RCC.gz   pass       0   <NA>
#> 5 GSM5393267_S46.RCC.gz   pass       0   <NA>
#> 6 GSM5393268_S47.RCC.gz   pass       0   <NA>
```

`autoplot()` draws any quality-control figure, `render()` writes a
report with Quarto, and `visualise()` opens the app.

``` r
autoplot(nacho, type = "BD")
```

<img src="man/figures/README-quick-start-plot-1.png" alt="Box plot of the binding density of each cartridge, with the limits of the quality control drawn as dashed lines." width="100%" />

``` r
render(nacho, output_dir = tempdir())
visualise(nacho)
```

## Overview

NACHO works with NanoString nCounter data. It reads the RCC files, flags
the samples that fail quality control and normalises the counts.

- `load_rcc()` reads the RCC files and a sample sheet, computes the
  quality-control metrics and normalises the counts.
- `visualise()` opens a Shiny app to look at every metric and change the
  limits.
- `autoplot()` draws any quality-control figure as a `ggplot2` plot.
- `render()` writes a quality-control report with Quarto, as an HTML or
  a PDF file.
- `normalise()` runs the normalisation again with new settings.

The [package website](https://m.canouil.dev/NACHO/) shows the app, the
report and a full analysis.

### Shiny Application ([demo](https://mcanouil.shinyapps.io/NACHO_data/))

Open the app on a `nacho` object with `visualise()`.

``` r
visualise(GSE74821)
```

<img src="man/figures/README-nacho_app.gif" alt="Short recording of the NACHO app on the example data. The summary strip changes when the field of view threshold moves to 95 and one sample is flagged. A selected sample is outlined in the QC metrics plots. The recording then shows the Samples, Normalisation and Batch pages, the Help menu, and the Export page with a report ready to download." width="100%" />

The articles “Explore quality control in the app” and “Analyse a GEO
study” on the [package website](https://m.canouil.dev/NACHO/) show each
page of the app and a full analysis of GSE178516. The article “Moving
from NACHO 2” on the same website maps the NACHO 2 code to NACHO 3.

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
Conduct](https://github.com/mcanouil/NACHO/blob/main/.github/CODE_OF_CONDUCT.md).\
By contributing to this project, you agree to abide by its terms.
