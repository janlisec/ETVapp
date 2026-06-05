# Read vendor specific text files

Import time resolved ICP-MS data in various file formats from different
instrument types.

## Usage

``` r
import_data(file_path, simplify = TRUE)
```

## Arguments

- file_path:

  Valid file path.

- simplify:

  Use FALSE to ensure that names list is returned also for single file
  as input.

## Value

A data.frame containing numeric columns or a list of such data.frames in
case that file_path is a vector.

## Details

The function works for .csv, .txt, and .exp files. Codes are provided to
transform raw data from common ICP-MS instruments to a data.frame with a
time column and additional columns for the acquired intensities. You may
check why data import of your files fails in the app on this function
and potentially extend it to handle your files.

## Examples

``` r
pth <- system.file(package = "ETVapp", "extdata", 'fmt')
imp <- import_data(dir(pth, full.names = TRUE))
lapply(imp, head)
#> $Agilent_PTFE_0.23_mg_P19.csv
#>      Time     157
#> 1 0.05141 1177.92
#> 2 0.08191 1258.19
#> 3 0.11241 1244.14
#> 4 0.14292 1187.95
#> 5 0.17342 1165.88
#> 6 0.20392 1240.12
#> 
#> $`Multicollector_S-test7_006SMP.exp`
#>     Time      32S    32.9755       34S
#> 1  0.000 53.17213 0.02418420 0.1025601
#> 2  4.197 53.17203 0.02423485 0.1028556
#> 3  8.394 53.17189 0.02430093 0.1028909
#> 4 12.591 53.17188 0.02434563 0.1033503
#> 5 16.787 53.17186 0.02432855 0.1031190
#> 6 20.986 53.17174 0.02438896 0.1034052
#> 
#> $OES_Cl_Br_I_1000ng_P14.csv
#>   Time O130.603 Ar425.118 Cl134.724 Cl135.165 Cl136.345 Cl133.573 Cl139.653
#> 1  0.1    -7090   2182493      -911     -6976     -3598     -5230       119
#> 2  0.2    -3317   2229401     -1673       582     -1965     -5145     -3087
#> 3  0.3     4015   2145859     -1215     -1908      -260        40      -875
#> 4  0.4     1851   2123576     -5291       317     -1356     -3840      -173
#> 5  0.5     1476   2154909     -4374     -4374     -4113     -2421     -3390
#> 6  0.6     6586   2160724     -3291      -931      -338       429     -3257
#>   Br154.065 Br153.174 Br148.845 Br144.990 Br157.480 Br163.340 I178.276 I142.549
#> 1     -3191      2799     -5386      1040     -3361       211    -2408     -358
#> 2     -4843      4253     -2375      2369      -679      3207    -2402     -323
#> 3     -3730     -1492      2684     -1025      -298       613     -986    -1602
#> 4     -2314      1451     -1916       104      -457     -1155    -1521     -416
#> 5      -817      4235      -930     -1413      2300      -746      493    -1212
#> 6      1983     -2850     -1010      2038     -4536       167     2451     -481
#>   I161.760 I183.038 I179.909
#> 1      874     3231    -1254
#> 2     2056     1255     1169
#> 3     -792     1308     -961
#> 4     -735     -202      455
#> 5       74      -45     -282
#> 6     2381     -404     3727
#> 
#> $`iCAP_BCR-646_0.8_mg_P20.csv`
#>    Time     80Se    117Sn     122Sn    124Sn    121Sb    125Te
#> 1 0.032 949834.6 400.0046 100.00029 166.6675 500.0071   0.0000
#> 2 0.220 850913.6 166.6675  66.66679 133.3338 200.0011 133.3338
#> 3 0.408 912671.1 300.0026 166.66746 266.6687 400.0046 166.6675
#> 4 0.595 919409.6 366.6705  33.33337 133.3338 466.6729 166.6675
#> 5 0.783 834415.5 500.0071  33.33337 333.3365 333.3365 433.3387
#> 6 0.971 958803.3 400.0046   0.00000 166.6675 466.6729 333.3365
#> 

import_data("not_existent.file")
#> File 'not_existent.file' not found. Returning NULL.
#> NULL
```
