# Write and read quality-control thresholds

`write_thresholds()` saves thresholds to a YAML file, and
`read_thresholds()` reads them back. Use the file to apply the same
quality-control policy to each study, or to load thresholds that you
tuned in
[`nacho_app()`](https://m.canouil.dev/NACHO/dev/reference/nacho_app.md).
The Export page of the app writes the same file.

## Usage

``` r
write_thresholds(thresholds, file)

read_thresholds(file)
```

## Arguments

- thresholds:

  A list of thresholds, as returned by
  [`nacho_thresholds()`](https://m.canouil.dev/NACHO/dev/reference/nacho_thresholds.md)
  or found in the `@thresholds` property of a `nacho` object.

- file:

  The path of the YAML file.

## Value

`write_thresholds()` returns `file`, invisibly. `read_thresholds()`
returns the list of thresholds.

## Details

In the file, `.inf` and `-.inf` are open bounds, and `.na.character` is
an unknown instrument. You can edit the file by hand.
`read_thresholds()` reads whole numbers such as `80` as numbers, and
checks the thresholds as
[`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md)
does. If the file does not give a miRNA threshold (`Ligation_order`,
`Ligation_R2`, `Ligation_NEG` or `Haemolysis`), `read_thresholds()` uses
the value of the preset in the file.

## Examples

``` r
file <- tempfile(fileext = ".yml")
thresholds <- nacho_thresholds("sprint")
thresholds$FoV <- 80
write_thresholds(thresholds, file)
read_thresholds(file)
#> $preset
#> [1] "nsolver"
#> 
#> $instrument
#> [1] "sprint"
#> 
#> $BD
#> [1] 0.1 1.8
#> 
#> $FoV
#> [1] 80
#> 
#> $PCL
#> [1] 0.95
#> 
#> $LoD
#> [1] 2
#> 
#> $Positive_factor
#> [1] 0.3 3.0
#> 
#> $House_factor
#> [1]  0.1 10.0
#> 
#> $Housekeeping_detected
#> [1] 3
#> 
#> $Ligation_order
#> [1] 1
#> 
#> $Ligation_R2
#> [1] 0.95
#> 
#> $Ligation_NEG
#> [1] -Inf    0
#> 
#> $Haemolysis
#> [1] -Inf  Inf
#> 
unlink(file)
```
