# Calculate an isotope ratio based on peak areas.

Calculate an isotope ratio based on peak areas.

## Usage

``` r
get_isoratio(
  data,
  iso1_col,
  iso2_col,
  PPmethod = c("Peak (height)", "Peak (manual)"),
  peak_start = NULL,
  peak_end = NULL,
  minpeakheight = 1000,
  BLmethod = c("modpolyfit", "none"),
  deg = 1,
  cf = 50,
  fl = 5,
  simplify = TRUE
)
```

## Arguments

- data:

  A data.frame containing at least two columns or a list of such
  data.frames in which case the result will be a list of data frames as
  well.

- iso1_col:

  Spike isotope.

- iso2_col:

  Sample isotope.

- PPmethod:

  Peak picking method.

- peak_start:

  Value which is taken as peak start point, when manual peak picking is
  chosen.

- peak_end:

  Value which is taken as peak end point, when manual peak picking is
  chosen.

- minpeakheight:

  A threshold value for peak picking when "Peak (height)" is choosen as
  an option.

- BLmethod:

  Method for baseline correction.

- deg:

  Degree of polynomial for baseline correction.

- cf:

  A correction value for cutting the area around the detected peak.

- fl:

  Filter length of smoothing function, has to be odd integer \>=3.

- simplify:

  In case that data is a list: shall result table be combined to a
  data.frame?

## Value

A data.frame or a list of data.frames when data is a list itself.

## Details

Time-resolved ICP-MS data will be integrated and optionally baseline
corrected. Peak boundaries will be checked regarding the similarity
between the isotopes.

## Examples

``` r
# for sample measurements
td <- ETVapp::ETVapp_testdata[["IDMS"]][["Samples"]]
get_isoratio(
  td, iso1_col = "113Cd", iso2_col = "111Cd", PPmethod = "Peak (manual)",
  peak_start = 72, peak_end = 132
)
#>   Spike isotope Sample isotope      R_m
#> 1         113Cd          111Cd 1.349456
#> 2         113Cd          111Cd 1.660122
#> 3         113Cd          111Cd 1.653288
```
