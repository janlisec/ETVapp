# Calculates the transport efficiency

The transport efficiency will be calculated based on the particle size
approach from single particle-ICP-MS data.

## Usage

``` r
calc_transeff(
  data,
  int_col,
  LFD = 100,
  cali_slope = 1,
  V_fl,
  part_mat = c("Au", "Ag"),
  dia_part
)
```

## Arguments

- data:

  A data.frame containing at least two columns.

- int_col:

  Intensity column.

- LFD:

  Intensity limit for the detection of particle signals.

- cali_slope:

  Calibration lm slope result for ionic standards.

- V_fl:

  Sample inlet flow in mL/min.

- part_mat:

  Particle material.

- dia_part:

  Particle diameter in nm.

## Value

A data.frame containing single particle data.

## Details

Calculates the transport efficiency.

Determination of the transport efficiency from measurements with gold or
silver nano particle standards of known size.

## Examples

``` r
spion_imp <- ETVapp::ETVapp_testdata[["oIDMS"]][["sp_ionic"]]

head(spion_imp[[1]])
#>    Time    197Au
#> 1 0.006 143249.1
#> 2 0.009 118398.2
#> 3 0.012 141904.9
#> 4 0.015 137537.0
#> 5 0.018 136865.1
#> 6 0.021 131826.8
names(spion_imp)
#> [1] "Au_ion_100_ppb_w-_CHF3_P16.csv" "Au_ion_20_ppb_w-_CHF3_P16.csv" 
#> [3] "Au_ion_200_ppb_w-_CHF3_P16.csv" "Au_ion_50_ppb_w-_CHF3_P16.csv" 
#> [5] "Au_ion_500_ppb_w-_CHF3_P16.csv"
sp_cali <- ldply_base(1:length(spion_imp), function(i) {
  get_peakdata(spion_imp[[i]], "197Au", PPmethod = "mean signal", peak_start = 0.003, peak_end = 60)
})
conc_ion <- c(20, 50, 100, 200, 500)
fac <- 1.661 * 0.01104347 * 12/44
sp_cali <- tab_cali(sp_cali, wf = "oIDMS", std_info = conc_ion, fac = fac)

(cali_slope <- calc_cali_mod(df = sp_cali[,c(5,4)], wf = "oIDMS")[,1])
#> [1] 1488.358

sp_data <- ETVapp::ETVapp_testdata[["oIDMS"]][["sp_particle"]][[1]]
# filter time if required
sp_data_flt <- sp_data[sp_data[,"Time"]<60,]

(n_trans <- calc_transeff(sp_data, int_col = "197Au", LFD = 20000, cali_slope,
  V_fl = 0.0075, part_mat = c("Au", "Ag"), dia_part = 60))
#>   Signal response [cps*L/µg] Detected particle number [/min]
#> 1                   1488.358                         1334.55
#>   Detected particle mass [fg] Calculated trans_eff [%]
#> 1                    15.47976                 14.10086
```
