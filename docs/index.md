# ETVapp

The goal of ETVapp is to provide functions to evaluate ICP-MS and
ICP-OES experiments. To this end, currently 4 different workflows are
implemented. The package is based on functions developed by [Vera
Scharek](https://www.linkedin.com/in/vera-scharek-63932b205/). You can
find scientific details in her publications on Online Isotope Dilution
ICP-MS used for [sulfor
quantification](https://pubs.acs.org/doi/full/10.1021/acs.analchem.3c03553)
and [tin
fractionation](https://pubs.acs.org/doi/full/10.1021/acs.analchem.3c03553).

## Installation

You can install the development version of `ETVapp` from
[GitHub](https://github.com/):

``` r
install.packages("janlisec/ETVapp")
```

## Use the App for testing

For users unfamiliar with the R console, the package provides a browser
based app which allows to test the package functions by means of
included test data.

``` r
library(ETVapp)
app()
```
