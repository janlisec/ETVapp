# Extract a number from a string.

Often numeric information is stored in character vectors like file
names. This function will extract such information, focusing on numeric
values which are different between files.

## Usage

``` r
extract_unique_number(x)
```

## Arguments

- x:

  Charcter vector, i.e. of file names.

## Examples

``` r
x <- paste("File123_conc", c(3, 21, 101), "endtext", sep="_")
extract_unique_number(x=x)
#> [1]   3  21 101
```
