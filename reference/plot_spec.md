# Spectra and peaks plot.

Generate an annotated plot of one to several spectra.

## Usage

``` r
plot_spec(
  opt = "",
  xrng = NULL,
  yrng = NULL,
  mi_spec = NULL,
  c1 = "13C",
  xlab = paste0("Time [", "s", "]"),
  ylab = "Intensity [cps]",
  T_prog = NULL,
  s_focus = NULL,
  pks = NULL,
  BLmethod = "none",
  sel_pk = NULL
)
```

## Arguments

- opt:

  Vector of keywords to hide/show specific elements of the plot.

- xrng:

  Numeric vector of length 2 specifying plotting range for x.

- yrng:

  Numeric vector of length 2 specifying plotting range for y.

- mi_spec:

  Spectra (list of data.frame objects).

- c1:

  Main isotope.

- xlab:

  Character to be used for bottom axis labeling.

- ylab:

  Character to be used for left axis labeling.

- T_prog:

  Temperature program (a data frame with columns Time and Temp).

- s_focus:

  Index of sample within focus.

- pks:

  Data frame of peaks with columns 'Sample', 'Peak ID', 'Scan start' and
  'Scan end'.

- BLmethod:

  Hide or show baseline.

- sel_pk:

  Selected peak as numeric index of length one.

## Value

An annotated plot of one to several spectra.

## Examples

``` r
if (interactive()) {
  wf <- "ExtGasCal"
  c1 <- "13C"
  td <- ETVapp::ETVapp_testdata[[wf]][["Samples"]]
  pro_data <- process_data(td, wf=wf, c1=c1, fl=9)
  plot_spec(mi_spec = pro_data, opt = "overlay_legend")
  peak_data <- cbind(
    "Sample"=1:length(pro_data),
    get_peakdata(pro_data, int_col = c1, minpeakheight = 10^6)
  )
  T_prog <- data.frame(
    "Time"=seq(min(pro_data[[1]][,"Time"]), max(pro_data[[1]][,"Time"]), length.out=7),
    "Temp"=c(20,20,50,50,150,170,170)
  )
  plot_spec(
    mi_spec = pro_data, opt=c("overlay_pb", "overlay_Temp"), BLmethod = "modpolyfit",
    T_prog = T_prog, pks = peak_data, s_focus = 2
  )
}
```
