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

thr_pro_tbl


  
```

Not Protected

Poorly Protected

Moderately Protected

Well Protected

Total

Critically Endangered

0

1

1

1

3

Endangered

0

1

0

1

2

Vulnerable

1

0

1

1

3

Near Threatened

0

0

1

1

2

Least Concern

0

1

1

1

3

Total (n)

1

3

4

5

13
