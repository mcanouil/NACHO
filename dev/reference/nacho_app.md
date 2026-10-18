# Run the NACHO app

Opens the quality-control app on a `nacho` object, or on an empty upload
page. Use
[`visualise()`](https://m.canouil.dev/NACHO/dev/reference/visualise.md)
to get the tuned object back in the R session.

## Usage

``` r
nacho_app(x = NULL, done = FALSE)
```

## Arguments

- x:

  A `nacho` object from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md),
  or `NULL` to start by uploading RCC files.

- done:

  If `TRUE`, the app shows a "Done" button that closes it and returns
  the tuned object. Keep `FALSE` for a deployed app, because closing it
  would stop the server for every user.

## Value

A Shiny app object; print it or pass it to
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Plot workers

When the mirai and ggiraph packages are installed, the app builds the
interactive plots of a page at the same time, in background R processes.
The processes and the plot cache belong to the R process that runs the
app. All users of that R process share them. The app starts the
processes when the first user opens a page with interactive plots, and
stops them when the app stops. By default, the app starts one process
for each CPU core minus one, with a maximum of four. The app from
[`deploy()`](https://m.canouil.dev/NACHO/dev/reference/deploy.md) starts
one process.

Each process keeps a copy of the study that it draws, so its memory
grows with the size of the study. For `GSE74821`, each process uses
about 170 MB. To find the number of processes, divide the memory that
you can give to the plots by the memory of one process. Set the
`nacho.plot_workers` option to that number before you start the app, for
example `options(nacho.plot_workers = 2)`. To build all plots in the app
process, set the option to `0`. If the processes do not start, the app
builds the plots in its own process.

The processes use the installed NACHO package.

## Examples

``` r
if (interactive()) {
  shiny::runApp(nacho_app(GSE74821))
}
```
