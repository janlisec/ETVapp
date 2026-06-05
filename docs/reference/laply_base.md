# Base-R laply

A base R only implementation of laply.

## Usage

``` r
laply_base(.data, .fun, ..., .drop = TRUE)
```

## Arguments

- .data:

  list.

- .fun:

  fun.

- ...:

  additional parameters to lapply.

- .drop:

  Avoid dropping dimensionality.

## Examples

``` r
lst <- list(a = 1:3, b = 4:6, c = 7:9)
laply_base(lst, function(x) x[1:2], .drop = FALSE)
#>   [,1] [,2]
#> a    1    2
#> b    4    5
#> c    7    8
#plyr::laply(lst, function(x) x[1:2], .drop = FALSE)
```
