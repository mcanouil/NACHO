# NACHO

::: {.nacho-steps}

::: {.nacho-step}
[Step 1]{.nacho-kicker}

## Load

Read the RCC files and a sample sheet.
One call computes the quality-control metrics and normalises the counts.

`load_rcc()`
:::

::: {.nacho-step}
[Step 2]{.nacho-kicker}

## Check

Look at each metric in the app, or plot one metric in your own script.

`visualise()` `autoplot()`
:::

::: {.nacho-step}
[Step 3]{.nacho-kicker}

## Share

Write an HTML or PDF report that records the decisions you made.

`render()`
:::

:::

::: {.nacho-feature}

::: {.nacho-media}
![The quality-control metrics page of the NACHO app, with a box plot for each metric and the limits drawn as dashed lines.](articles/figures/app-qc-metrics.png){width="1280" height="800" loading="lazy"}
:::

::: {.nacho-text}
[Shiny app]{.nacho-kicker}

## See every sample, change every limit

- The summary strip counts the flagged samples while you move a limit.
- Select a sample once, and every plot outlines it.
- Separate pages show the control probes, the normalisation and the batch effects.
- The Export page writes the report from the app.

```r
visualise(nacho)
```

[Explore quality control in the app](articles/nacho-app.html){.nacho-more}
:::

:::

::: {.nacho-feature .nacho-flip}

::: {.nacho-media .nacho-stack}
![The top of the HTML report, with the study header and the decision summary for one flagged sample.](home/report-html.png){.nacho-front width="1280" height="720" loading="lazy"}

![The cover page of the PDF report.](home/report-pdf-cover.png){.nacho-page .nacho-cover width="413" height="585" loading="lazy"}

![A page of the PDF report, with the running header, the decision summary and the page footer.](home/report-pdf-page.png){.nacho-page .nacho-inner-page width="413" height="585" loading="lazy"}
:::

::: {.nacho-text}
[Quarto report]{.nacho-kicker}

## A quality-control record you can hand over

- The same `nacho` object gives an HTML report or a PDF report.
- The decision summary comes first: which samples fail, and why.
- Figures have numbers and captions, and tables are accessible.
- A methods section records the settings and the software versions.

```r
render(nacho, format = "typst")
```

[Write a quality-control report](articles/nacho-report.html){.nacho-more}
:::

:::

::: {.nacho-feature}

::: {.nacho-media}
![Box plot of the binding density of each cartridge, with the limits of the quality control drawn as dashed lines.](reference/figures/README-quick-start-plot-1.png){width="672" height="480" loading="lazy"}
:::

::: {.nacho-text}
[Analysis]{.nacho-kicker}

## Part of your R workflow

- `autoplot()` draws each quality-control figure as a `ggplot2` plot.
- `normalise()` and `exclude_outliers()` run again with new settings.
- `as_summarized_experiment()` gives the counts to Bioconductor.

[Analyse a GEO study](articles/nacho-analysis.html){.nacho-more}
[Moving from NACHO 2](articles/moving-from-nacho-2.html){.nacho-more}
:::

:::

::: {.nacho-closing}

## Installation

Install the release from CRAN:

```r
install.packages("NACHO")
```

If you want the development version without an extra package, install it from R-universe:

```r
install.packages(
  "NACHO",
  repos = c("https://mcanouil.r-universe.dev", "https://cloud.r-project.org")
)
```

If you want the latest commit, install it from GitHub with pak:

```r
# install.packages("pak")
pak::pak("mcanouil/NACHO")
```

:::

::: {.nacho-closing}

## Cite and get help

If you use NACHO in your work, [cite the article in *Bioinformatics*](authors.html#citation).
To report a bug, open an issue on [GitHub](https://github.com/mcanouil/NACHO/issues) with a minimal reproducible example.
This project has a [Contributor Code of Conduct](https://github.com/mcanouil/NACHO/blob/main/.github/CODE_OF_CONDUCT.md).

:::
