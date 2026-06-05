# Calculates the calibration model

`calc_cali_mod` will provide calibration results based on a linear
regression model.

## Usage

``` r
calc_cali_mod(
  df,
  wf = c("ExtCal", "ExtGasCal", "oIDMS"),
  ExtCal_unit = c("pg", "ng", "µg"),
  ExtGasCal_unit = c("nL/min", "µL/min", "mL/min")
)
```

## Arguments

- df:

  Data.frame with two columns.

- wf:

  Calibration method/Workflow (used to make assumptions regarding units
  in df).

- ExtCal_unit:

  The measurement unit of the ExtCal workflow to correctly format the
  output.

- ExtGasCal_unit:

  The measurement unit of the ExtGasCal workflow to correctly format the
  output.

## Value

A data.frame containing slope and intercept with errors and R square.

## Details

Calculates the calibration model.

A calibration curve is provided by a linear fit of peak areas or mean
signal intensities against analyte masses or gas flows from a data.frame
containing at least two entries in rows.

## Examples

``` r
df <- data.frame(x=1:5, y=sort(runif(5)))
calc_cali_mod(df = df, wf = "ExtCal")
#>   Slope [cts/pg] Slope error [cts/pg] Intercept [cts] Intercept error [cts]
#> 1      0.2173878            0.0412136       -0.316073               0.13669
#>   R square 
#> 1 0.9026671
calc_cali_mod(df = df, wf = "ExtGasCal")
#>   Slope [cps s/pg] Slope error [cps s/pg] Intercept [cps] Intercept error [cps]
#> 1        0.2173878              0.0412136       -0.316073               0.13669
#>   R square 
#> 1 0.9026671
calc_cali_mod(df = df, wf = "oIDMS")
#>   Slope [cps L/µg] Slope error [cps L/µg] Intercept [cps] Intercept error [cps]
#> 1        0.2173878              0.0412136       -0.316073               0.13669
#>   R square 
#> 1 0.9026671
```
