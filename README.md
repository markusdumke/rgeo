
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rgeo

An R package providing helper functions to work with coordinates.

## Installation

You can install rgeo with:

``` r
remotes::install_github("rgeo")
```

## Example

``` r
library(rgeo)

get_geo_info(
  .longitude = 11, .latitude = 48,
  .naturraum = naturraum_bayern,
  .boundaries = boundaries_bayern
)
#>    longitude latitude        land bundesland regierungsbezirk         landkreis
#> 1:        11       48 Deutschland     Bayern       Oberbayern Landsberg am Lech
#>    gemeinde        ort naturraum_grosslandschaft            naturraum_hauptname
#> 1:   Pürgen Hofstetten              Alpenvorland Voralpines Moor- und Hügelland
#>             naturraum_name                    tk25              tk25_quadrant
#> 1: Ammer-Loisach-Hügelland 7932 Utting am Ammersee 7932 30 Utting am Ammersee
#>          tk25_viertelquadrant
#> 1: 7932 33 Utting am Ammersee
```
