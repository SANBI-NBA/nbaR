# Table of protection level of threatened ecosystems.

Function to style a table with threat status on the horizontal rows and
protection level on the vertical columns, with number of ecosystems or
taxa that share those categories, with total and percentage çolumns
added

## Usage

``` r
nba_tbl_comb(DF, GROUP, THR, PRO, FILE = c("spatial", "csv"))
```

## Arguments

- DF:

  The data frame that contains the data

- GROUP:

  The column that contains the name of the variable (the ecosystems or
  taxa names)

- THR:

  The column name of the threat statuses

- PRO:

  The column name of the protection levels

- FILE:

  An indication if the input file is a map (spatial file with a geom
  column) or a csv/ normal dataframe.

## Examples

``` r

thr_pro_tbl <- NBA_example_map_data |>
nba_tbl_comb(GROUP = P_EcosysType,
THR = threat_status,
PRO = protection_level,
FILE = "spatial")
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `dplyr::across(tidyselect::all_of(count_columns), round, 0)`.
#> Caused by warning:
#> ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
#> Supply arguments directly to `.fns` through an anonymous function instead.
#> 
#>   # Previously
#>   across(a:b, mean, na.rm = TRUE)
#> 
#>   # Now
#>   across(a:b, \(x) mean(x, na.rm = TRUE))
#> ℹ The deprecated feature was likely used in the nbaR package.
#>   Please report the issue at <https://github.com/TashBes/NBA.package/issues>.

thr_pro_tbl


  
```
