# gt table theme

Function to apply gt styling to table

## Usage

``` r
nba_tbl_theme(
  GT_TBL,
  HEADER = c("sanbi-green", "sanbi-orange", "sanbi-purple", "Freshwater", "Marine",
    "Coast", "Estuarine", "Terrestrial", "Genetics", "PEI")
)
```

## Arguments

- GT_TBL:

  The data frame that contains the data

- HEADER:

  the name to determine the colour of the header row. Defaults to SANBI
  green

## Examples

``` r
library(gt)
library(dplyr)


gt_tbl <- NBA_example_thr_data %>%
 gt() %>%
nba_tbl_theme()

gt_tbl


  

OVERALL types
```
