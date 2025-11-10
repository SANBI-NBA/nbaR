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

protection_level

value

subAntarctic

Well Protected

15

subAntarctic

Moderately Protected

14

subAntarctic

Poorly Protected

1

subAntarctic

Not Protected

4

Coastal

Well Protected

65

Coastal

Moderately Protected

28

Coastal

Poorly Protected

52

Coastal

Not Protected

41

Estuarine

Well Protected

4

Estuarine

Moderately Protected

8

Estuarine

Poorly Protected

7

Estuarine

Not Protected

3

Marine

Well Protected

47

Marine

Moderately Protected

62

Marine

Poorly Protected

22

Marine

Not Protected

19

Wetland

Well Protected

8

Wetland

Moderately Protected

4

Wetland

Poorly Protected

41

Wetland

Not Protected

82

River

Well Protected

29

River

Moderately Protected

33

River

Poorly Protected

66

River

Not Protected

94

Inland Aquatic

Well Protected

37

Inland Aquatic

Moderately Protected

37

Inland Aquatic

Poorly Protected

107

Inland Aquatic

Not Protected

176

Terrestrial

Well Protected

118

Terrestrial

Moderately Protected

59

Terrestrial

Poorly Protected

166

Terrestrial

Not Protected

115
