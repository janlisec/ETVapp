
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ETVapp

<!-- badges: start -->

[![Static
Badge](https://img.shields.io/badge/LiveApp-blue)](https://jali-etvapp.share.connect.posit.cloud/)
[![Static
Badge](https://img.shields.io/github/r-package/v/janlisec/ETVapp)](https://img.shields.io/github/r-package/v/janlisec/ETVapp)
[![R-CMD-check](https://github.com/janlisec/ETVapp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/janlisec/ETVapp/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/janlisec/ETVapp/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/janlisec/ETVapp/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

The goal of ETVapp is to provide functions to evaluate ICP-MS and
ICP-OES experiments. To this end, currently 4 different workflows are
implemented. The package is based on functions developed by Vera
Scharek. You can find scientific details in her publications on
[microplastic](https://link.springer.com/article/10.1007/s00216-025-06146-x),
[fluorine
determination](https://pubs.rsc.org/en/content/articlelanding/2026/ja/d6ja00014b)
and [tin
fractionation](https://link.springer.com/article/10.1007/s00216-025-06064-y).

## Installation

You can install the development version of `ETVapp` package from this
[GitHub page](https://github.com/janlisec/ETVapp) in `R` using:

``` r
devtools::install_github("janlisec/ETVapp")
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

<figure>
<img src="man/figures/ETVapp_screenshot.png?raw=true"
title="ETVapp screenshot" alt="ETVapp screenshot" />
<figcaption aria-hidden="true">ETVapp screenshot</figcaption>
</figure>

## Use package functions to establish reproducible workflows

Use the template workflows described in
[Vignettes](https://janlisec.github.io/ETVapp/articles/) to set up a
local workflow according to your needs. Store this workflow together
with your measurement data to achieve maximum reproducibility.
