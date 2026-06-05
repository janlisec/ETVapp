# Calculates the mass flow mf_sp

Calculates the mass flow mf_sp.

## Usage

``` r
calc_massflow(
  x,
  n_trans = 16.64236,
  As_iso1 = 7.68,
  As_iso2 = 4.36,
  Asp_iso1 = 91.06,
  Asp_iso2 = 0.08,
  V_fl = 0.0075,
  c_sp = 19581.71,
  DF = 20
)
```

## Arguments

- x:

  Input data.

- n_trans:

  Transport efficiency.

- As_iso1:

  Natural abundance of the spike isotope.

- As_iso2:

  Natural abundance of the sample isotope.

- Asp_iso1:

  Abundance of the spike isotope in the spike.

- Asp_iso2:

  Abundance of the sample isotope in the spike.

- V_fl:

  Sample inlet flow in mL/min.

- c_sp:

  Concentration of spike isotope in spike stock solution.

- DF:

  Dilution factor of spike solution.

## Value

A numeric vector.

## Examples

``` r
calc_massflow(1:10)
#>  [1] -55.815317 178.022989  34.255734  18.936273  13.077555   9.983341
#>  [7]   8.070455   6.770818   5.830270   5.118050
```
