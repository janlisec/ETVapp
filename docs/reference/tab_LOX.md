# tab_LOX

`tab_LOX` estimates the limit of detection (LOD) and quantification
(LOQ) and returns a data.frame.

## Usage

``` r
tab_LOX(
  x,
  cali_slope = 1,
  wf = c("ExtCal", "ExtGasCal", "IDMS", "oIDMS"),
  ExtCal_unit = c("pg", "ng", "µg"),
  ExtGasCal_unit = c("nL/min", "µL/min", "mL/min"),
  c_sp_unit = c("µg/L", "mg/L", "g/L"),
  mass_fraction2 = 1,
  sample_mass = 1
)
```

## Arguments

- x:

  Variable containing the numeric data to infer SD (either peak area
  (for ExtCal/ExtGasCal) or analyte mass (for IDMS/oIDMS)).

- cali_slope:

  A slope from the calibration data.

- wf:

  Calibration method/Workflow.

- ExtCal_unit:

  The measurement unit of the ExtCal workflow to correctly format the
  output.

- ExtGasCal_unit:

  The measurement unit of the ExtGasCal_unit workflow to correctly
  format the output.

- c_sp_unit:

  The measurement unit of the "IDMS", "oIDMS" workflow to correctly
  format the output.

- mass_fraction2:

  Mass fraction of the analyte element in the analyte component.

- sample_mass:

  Sample mass.

## Value

A data.frame.

## Details

The LOD and LOQ will be calculated as three and ten times the standard
deviation of at least three peak areas divided by the slope of a linear
calibration curve (for workflow ExtCal/ExtGasCal). Results are provided
as element, analyte and relative to a sample mass.

## Examples

``` r
tab_LOX(x = runif(2))
#> Warning: At least three finite values are needed for a statistical evaluation.
#>   LOD as element [pg] LOQ as element [pg] Sample mass [mg]
#> 1                  NA                  NA                1
#>   LOD per sample mass [ppb] LOQ per sample mass [ppb]
#> 1                        NA                        NA
tab_LOX(x = runif(5), wf = "IDMS")
#> At least ten blank values are recommended for estimating the LOD and LOQ.
#>   LOD as element [pg] LOQ as element [pg] Sample mass [mg]
#> 1           0.9673392            3.224464                1
#>   LOD per sample mass [ppb] LOQ per sample mass [ppb]
#> 1                 0.9673392                  3.224464
tab_LOX(x = runif(10), mass_fraction2 = 0.5, sample_mass = 3, ExtCal_unit = "ng")
#>   LOD as element [ng] LOQ as element [ng] Mass fraction Sample mass [mg]
#> 1           0.8660005            2.886668           0.5                3
#>   LOD as analyte [ng] LOQ as analyte [ng] LOD per sample mass [ppm]
#> 1            1.732001            5.773337                 0.2886668
#>   LOQ per sample mass [ppm]
#> 1                 0.9622228
```
