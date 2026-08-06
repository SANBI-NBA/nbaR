# Coloured table

A basic table with a coloured heading bar and coloured blocks around the
categories This data should be in the same format as for the plot
functions.

## Usage

``` r
nba_tbl_colr(
  DF,
  COL,
  HEADER = c("sanbi-green", "sanbi-orange", "sanbi-purple", "Freshwater", "Marine",
    "Coast", "Estuarine", "Terrestrial", "Genetics", "PEI")
)
```

## Arguments

- DF:

  The data frame that contains the data

- COL:

  the column containing the categories

- HEADER:

  the name to determine the colour of the header row

## Details

This table has the same styling as set out in nba_tbl_theme. If you
would like to change anything you can just use a pipe %\>% to add gt
styling onto the gt object this function creates and it will override
the styling set in the function.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(tidyr)

tbl <- NBA_example_pro_data %>%
 pivot_longer(2:5, names_to = "protection_level") %>%
 nba_tbl_colr(COL = protection_level, HEADER = "Coast")

tbl


  

OVERALL types
```
