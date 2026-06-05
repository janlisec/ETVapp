# str_sort_num

Sort strings by numeric

## Usage

``` r
str_sort_num(x)
```

## Arguments

- x:

  character vector.

## Details

A base R implementation to sort strings according to first number within
string.

## Examples

``` r
x <- paste0("File", c(20, 1, 9, 11, 32, 100))
str_sort_num(x)
#> [1] "File1"   "File9"   "File11"  "File20"  "File32"  "File100"
x <- c("Z1", x, "a", "Z10", "z1", "z2")
str_sort_num(x)
#>  [1] "File1"   "File9"   "File11"  "File20"  "File32"  "File100" "Z1"     
#>  [8] "Z10"     "a"       "z1"      "z2"     
x <- c("b1_3", "b2_1_3", x)
str_sort_num(x)
#>  [1] "File1"   "File9"   "File11"  "File20"  "File32"  "File100" "Z1"     
#>  [8] "Z10"     "a"       "b1_3"    "b2_1_3"  "z1"      "z2"     
```
