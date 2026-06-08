# Plots the signal distribution.

Plots the signal distribution of a single particle-ICP-MS measurement.

## Usage

``` r
plot_signal_distribution(
  x,
  LFD = 20000,
  style = c("hist", "counts"),
  ylim = NULL
)
```

## Arguments

- x:

  A data.frame containing at least two columns.

- LFD:

  Intensity limit for the detection of particle signals.

- style:

  Choose plotting style.

- ylim:

  Specify ylim for style = counts.

## Value

A data.frame containing single particle data (invisible) and a plot.

## Details

Determination of the intensity limit for the differentiation between
particle and background signals.

## Examples

``` r
sp_data <- ETVapp::ETVapp_testdata[["oIDMS"]][["sp_particle"]][[1]]
plot_signal_distribution(x = sp_data[,2])

plot_signal_distribution(x = sp_data[,2], style="counts")
#> Warning: 1 x value <= 0 omitted from logarithmic plot
```
