# Calculates the mass bias correction factor K

A mass bias correction factor will be determined from measurements
without added spike.

## Usage

``` r
calc_massbias(R_m, As_iso1, As_iso2)
```

## Arguments

- R_m:

  Isotope ratio.

- As_iso1:

  Natural abundance of the spike isotope.

- As_iso2:

  Natural abundance of the sample isotope.

## Value

Numeric values K for each R_m.

## Details

Calculates the mass bias correction factor K.

Ratio of the spike to sample isotope peak area from measurements without
spike addition.

## Examples

``` r
mb_imp <- ETVapp::ETVapp_testdata[["oIDMS"]][["Massbias"]]

mb_peaks <- ldply_base(1:length(mb_imp), function(i) {
  get_isoratio(mb_imp[[i]], iso1_col = "117Sn", iso2_col = "122Sn",
    PPmethod = "Peak (manual)", peak_start = 70, peak_end = 105)
})
head(mb_peaks)
#>   Spike isotope Sample isotope      R_m
#> 1         117Sn          122Sn 1.471331
#> 2         117Sn          122Sn 1.450506
#> 3         117Sn          122Sn 1.464995
calc_massbias(mb_peaks[,"R_m"], As_iso1 = 7.68, As_iso2 = 4.63)
#> [1] 1.127378 1.143564 1.132254
```
