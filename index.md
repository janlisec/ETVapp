# ETVapp

The goal of ETVapp is to provide functions to evaluate ICP-MS and
ICP-OES experiments. To this end, currently 4 different workflows are
implemented. The package is based on functions developed by [Vera
Scharek](https://www.linkedin.com/in/vera-scharek-63932b205/), publicly
available on her [GitHub profile](https://github.com/VSc811). You can
find scientific details in her publications on Online Isotope Dilution
ICP-MS used for [sulfor
quantification](https://pubs.acs.org/doi/full/10.1021/acs.analchem.3c03553)
and [tin
fractionation](https://pubs.acs.org/doi/full/10.1021/acs.analchem.3c03553).

## Installation

You can install the development version of `ETVapp` package from this
[GitHub page](https://github.com/janlisec/ETVapp) in `R` using:

``` r

install.packages("janlisec/ETVapp")
```

## Use the Shiny-App for testing

For users unfamiliar with the `R` console, the package provides a
browser based app together with appropriate test data which allows to
test the package functions. The app also allows to process user data for
exploratory data analyses. However, it currently does not allow to store
parameter settings. Therefore, for use in publications, it is
recommended to script the analysis steps to ensure reproducibility.

``` r

library(ETVapp)
app()
```

![ETVapp
screenshot](reference/figures/ETVapp_screenshot.png?raw=true "ETVapp screenshot")

ETVapp screenshot
