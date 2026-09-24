# Deploy (copy) the shiny application to the specified directory

Deploy (copy) the shiny application to the specified directory

## Usage

``` r
deploy(directory = "/srv/shiny-server", app_name = "NACHO")
```

## Arguments

- directory:

  \[[character](https://rdrr.io/r/base/character.html)\] A character
  vector of one path to the new location.

- app_name:

  \[[character](https://rdrr.io/r/base/character.html)\] A character
  vector defining the shiny application name in the new location.

## Value

\[[logical](https://rdrr.io/r/base/logical.html)\] A logical indicating
whether the deployment is successfull (`TRUE`) or not (`FALSE`).

## Examples

``` r

deploy(directory = ".")
#> [1] TRUE

if (interactive()) {
  shiny::runApp("NACHO")
}
```
