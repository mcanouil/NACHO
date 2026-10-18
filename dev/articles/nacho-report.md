# The Quality-Control Report

The [`render()`](https://m.canouil.dev/NACHO/dev/reference/render.md)
function writes the quality-control report of a `nacho` object. The
report is one file, either a self-contained HTML page or a tagged A4
PDF. The HTML file needs nothing else to open, so you can email it or
keep it with the data. The PDF goes through Typst, which Quarto bundles,
so you do not need LaTeX. Both formats hold the same content, and this
article shows a real report of the example data `GSE74821` in each. The
example raises the field-of-view limit to 95, so that one sample is
flagged and the report has something to show. The pictures come from a
report that the website build renders each time.

## Create a report

Pass a `nacho` object to
[`render()`](https://m.canouil.dev/NACHO/dev/reference/render.md), and
choose the format with `format`. The call returns the path of the
report, invisibly.

``` r

library(NACHO)

render(
  GSE74821,
  format = "html",
  output_dir = "reports",
  title = "PAM50 study, batch 3",
  author = "Jane Doe, Genomics Core"
)
```

The main arguments are these.

- `x` is the `nacho` object from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).
- `format` is `"html"` (the default) or `"typst"` for a PDF.
- `output_dir` is the folder that receives `nacho-report.html` or
  `nacho-report.pdf`. NACHO creates the folder when it does not exist.
- `title` replaces the default title, “NanoString quality-control
  report”.
- `author` names who prepared the report, and shows on the cover when
  you give it.
- `group` is a column of `nacho_samples(x)` with the biological groups.
  With a group, the report also checks whether batches and groups are
  confounded.

Other arguments, such as `colour`, `size` and `outliers_labels`, change
how the plots look.
[`?render`](https://m.canouil.dev/NACHO/dev/reference/render.md) lists
all of them. Rendering needs the Quarto command-line interface 1.9.18 or
newer.

## Cover and decision summary

The report opens with a cover that gives the title, the date, the number
of samples and cartridges, the number of flagged samples, the
normalisation method, the RCC version and the NACHO version. The
decision summary follows. It lists every flagged sample with the metrics
that failed, and then adds notes on how the data were processed. For
example, a note tells you when the positive control model did not fit a
sample. Under the table, a “How to read this” box explains the columns.

![The cover of the HTML report on a dark banner. It shows the NACHO
logo, the title "PAM50 study, batch 3", the preparation date and the
cover details: 48 samples, 4 cartridges, 1 flagged, GLM normalisation,
RCC version 1.6 and the NACHO
version.](nacho-report_files/figure-html/report-cover.png)

![The Decision summary section of the HTML report. Three tiles give the
number of samples that pass, the number of flagged samples and the
number of metrics behind the flags. A table lists the flagged sample
with the metric that failed, and a "How to read this" box explains the
table.](nacho-report_files/figure-html/report-flagged.png)

## Settings and thresholds

A table lists each setting and limit that the report used. Every row
gives the value, what it means and where it comes from, for example the
nCounter guidelines or a choice that you made. A reader can tell which
limits were the defaults and which were changed, without opening your
code.

## Figures and tables

Each figure and each table has a “How to read this” box under it. The
box says what one point or one box stands for, which pattern is fine,
and what to do when a pattern looks wrong. The sections follow the
quality-control metrics, the control probes, the counts, the principal
components, the normalisation and the batch effects.

![The Binding density section of the HTML report. Text explains the
metric and its limits, a box plot shows the binding density of the
samples by cartridge with the limits as dashed lines in shaded bands,
and a "How to read this" box under the plot explains the points, the
boxes and the bands.](nacho-report_files/figure-html/report-figure.png)

## Methods appendix

The report ends with a methods appendix. It gives the citation of NACHO,
a description of the methods that the data went through, and the
versions of R and of the packages that made the report. That makes the
file a record that you can attach to a study, and that a colleague can
reproduce.

![The Methods section of the HTML report. Paragraphs describe the limits
and the normalisation, the versions of NACHO and R, and the reference to
cite for NACHO. The session information starts below with the package
versions.](nacho-report_files/figure-html/report-methods.png)

## The PDF

The PDF has the same sections, laid out on A4 pages with a cover page, a
table of contents and running headers. It is tagged and declares
PDF/UA-1, so screen readers can follow it. Quarto may warn that verapdf
is not installed. The warning only means that Quarto skipped an extra
validation step.

![The cover page of the PDF report on a dark A4 page. It shows the NACHO
logo, the title "PAM50 study, batch 3", the preparation date and the
cover details: 48 samples, 4 cartridges, 1 flagged, GLM normalisation
and the versions.](nacho-report_files/figure-html/report-pdf-cover.png)

![A page of the PDF report with a running header. The binding density
box plot sits at the top with its caption, followed by a "How to read
this" box and the start of the Field of view
section.](nacho-report_files/figure-html/report-pdf-figure.png)

## Further reading

[Explore quality control in the
app](https://m.canouil.dev/NACHO/dev/articles/nacho-app.md) shows the
app, which can also create the report. [Analyse a GEO
study](https://m.canouil.dev/NACHO/dev/articles/nacho-analysis.md) loads
and checks a public study from start to finish.
