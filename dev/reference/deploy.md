# Deploy (copy) the Shiny application to the specified directory

Deploy (copy) the Shiny application to the specified directory

## Usage

``` r
deploy(directory, app_name = "NACHO")
```

## Arguments

- directory:

  \[[character](https://rdrr.io/r/base/character.html)\] A character
  vector of one path to the new location.

- app_name:

  \[[character](https://rdrr.io/r/base/character.html)\] A character
  vector defining the Shiny application name in the new location.

## Value

\[[logical](https://rdrr.io/r/base/logical.html)\] A logical indicating
whether the deployment is successful (`TRUE`) or not (`FALSE`).

## Details

The deployed app calls
[`NACHO::nacho_app()`](https://m.canouil.dev/NACHO/dev/reference/nacho_app.md),
so the server needs NACHO installed. The app sets the
`nacho.plot_workers` option to `1` when the option is not set, so it
starts one background process for the interactive plots. That process
and the plot cache belong to the R process that runs the app, and each
process keeps a copy of the study, so its memory grows with the size of
the study (about 170 MB for `GSE74821`). To change the number of
processes, set the option before the app starts, for example in
`.Rprofile` on the server. See the "Plot workers" section of
[`nacho_app()`](https://m.canouil.dev/NACHO/dev/reference/nacho_app.md).

## Examples

``` r

deploy(directory = tempdir())
#> [1] TRUE

if (interactive()) {
  shiny::runApp("NACHO")
}
```
