# Tabulate sample data

Computes result data from peak data or a data.frame containing at least
two numeric columns.

## Usage

``` r
tab_result(
  peak_data,
  wf = c("ExtCal", "ExtGasCal", "IDMS", "oIDMS"),
  ExtCal_unit = c("pg", "ng", "µg"),
  ExtGasCal_unit = c("nL/min", "µL/min", "mL/min"),
  c_sp_unit = c("µg/L", "mg/L", "g/L"),
  a = 0,
  b = 1,
  K = 1,
  amae = 1,
  mass_fraction2 = 1,
  sample_mass = 1
)
```

## Arguments

- peak_data:

  Data.frame containing peak information.

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

- a:

  Intercept from a calibration model (for ExtCal/ExtGasCal).

- b:

  Slope from a calibration model (for ExtCal/ExtGasCal).

- K:

  K (for IDMS).

- amae:

  Analyte mass as element (for IDMS/oIDMS).

- mass_fraction2:

  Mass fraction of the analyte element in the analyte component.

- sample_mass:

  Sample mass in \[mg\].

## Value

A data.frame.

## Details

Result data table. Depending on the selected method, the analyte mass
will be calculated from the peak area and calibration data which are
provided in data.frames. For the on-line IDMS workflow, values of the
intensity column from a data.frame containing at least two columns are
transferred into mass flow data using calibration data from a
data.frame. The analyte mass will be determined via peak integration
using the function `get_peakdata`. Result data will be provided for the
analyte component itself and as element, as well as content value
related to the sample mass.

## Examples

``` r
# wf "ExtCal", "ExtGasCal" use col = 4
pd <- data.frame("A"=1:3,"B"=1:3,"R_m"=1:3,"col4"=1:3)
tab_result(peak_data=pd, wf="ExtCal")
#>   A B R_m col4 Analyte mass as element [pg] Sample mass [mg]
#> 1 1 1   1    1                            1                1
#> 2 2 2   2    2                            2                1
#> 3 3 3   3    3                            3                1
#>   Content as element [ppb]
#> 1                        1
#> 2                        2
#> 3                        3
# wf "IDMS", "oIDMS" use col = "R_m"
tab_result(peak_data=pd, wf="IDMS", c_sp_unit="g/L")
#>   A B R_m col4 R_corr Analyte mass as element [µg] Sample mass [mg]
#> 1 1 1   1    1      1                            1                1
#> 2 2 2   2    2      2                            1                1
#> 3 3 3   3    3      3                            1                1
#>   Content as element [g/100 g]
#> 1                          0.1
#> 2                          0.1
#> 3                          0.1
tab_result(peak_data=pd, wf="IDMS", c_sp_unit="mg/L")
#>   A B R_m col4 R_corr Analyte mass as element [ng] Sample mass [mg]
#> 1 1 1   1    1      1                            1                1
#> 2 2 2   2    2      2                            1                1
#> 3 3 3   3    3      3                            1                1
#>   Content as element [ppm]
#> 1                        1
#> 2                        1
#> 3                        1
tab_result(peak_data=pd, wf="IDMS", c_sp_unit="mg/L", mass_fraction2 = 10^3)
#>   A B R_m col4 R_corr Analyte mass as element [ng] Analyte mass [ng]
#> 1 1 1   1    1      1                            1             0.001
#> 2 2 2   2    2      2                            1             0.001
#> 3 3 3   3    3      3                            1             0.001
#>   Mass fraction Sample mass [mg] Content as element [ppm]
#> 1          1000                1                        1
#> 2          1000                1                        1
#> 3          1000                1                        1
#>   Content as analyte [ppm]
#> 1                    0.001
#> 2                    0.001
#> 3                    0.001
tab_result(peak_data=pd, wf="IDMS", c_sp_unit="mg/L", mass_fraction2 = 10^4)
#>   A B R_m col4 R_corr Analyte mass as element [pg] Analyte mass [pg]
#> 1 1 1   1    1      1                         1000               0.1
#> 2 2 2   2    2      2                         1000               0.1
#> 3 3 3   3    3      3                         1000               0.1
#>   Mass fraction Sample mass [mg] Content as element [ppb]
#> 1         10000                1                     1000
#> 2         10000                1                     1000
#> 3         10000                1                     1000
#>   Content as analyte [ppb]
#> 1                      0.1
#> 2                      0.1
#> 3                      0.1
```
