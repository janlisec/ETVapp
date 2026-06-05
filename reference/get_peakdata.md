# Generate a table with peak data.

Generate a table with peak data.

## Usage

``` r
get_peakdata(
  pro_data,
  int_col,
  time_col = "Time",
  peak_start = NULL,
  peak_end = NULL,
  minpeakheight = 1000,
  PPmethod = c("Peak (height)", "Peak (manual)", "mean signal"),
  BLmethod = c("modpolyfit", "none"),
  deg = 1,
  cf = 50
)
```

## Arguments

- pro_data:

  Data.frame with at least two numeric columns.

- int_col:

  Column name of the intensity column to be used.

- time_col:

  Column name of the time column to be used.

- peak_start:

  Value which is taken as peak start point, when manual peak picking is
  chosen.

- peak_end:

  Value which is taken as peak end point, when manual peak picking is
  chosen.

- minpeakheight:

  A threshold value for peak picking via peak height.

- PPmethod:

  Peak picking method.

- BLmethod:

  Method for baseline correction.

- deg:

  Degree of polynomial for baseline correction.

- cf:

  A correction value for cutting the area around the detected peak.

## Value

A data.frame with the peak boundaries in seconds and the peak area in
cps.

## Details

This function provides a simple detection algorithm for peak boundaries
based on the peak height and alternatively allows for manual setting of
the peak boundaries or selecting a signal range. It has implemented a
method for baseline correction based on polynomial fitting and will
compute the peak area using the trapezoidal rule.

## Examples

``` r
raw_data <- ETVapp::ETVapp_testdata[["oIDMS"]][["Samples"]][[1]]
pro_data <- process_data(raw_data, c1 = "117Sn", c2 = "80Se", fl = 9)
peak_data <- get_peakdata(pro_data, int_col = "117Sn")
plot(x = pro_data[,1:2], type = "l")
abline(v = peak_data[1,2:3], col=grey(0.7))

# limiting peak detection using the 'minpeakheight' parameter
peak_data <- get_peakdata(pro_data, int_col = "117Sn", minpeakheight = 2*10^6)
abline(v = peak_data[1,2:3], col=5)

# limiting peak detection setting start and end manually
peak_data <- get_peakdata(pro_data, int_col = "117Sn", PPmethod = "Peak (manual)",
  peak_start = 80, peak_end = 130)
abline(v = peak_data[1,2:3], col=3)
```
