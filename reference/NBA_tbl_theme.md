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

Critically Endangered

Endangered

Vulnerable

Least Concern

TOT

Sub-Antarctic

0

1

5

28

34

Coastal

21

37

54

74

186

Estuarine

2

10

7

3

22

Marine

2

22

51

75

150

Wetland

83

12

12

28

135

River

95

42

5

80

222

Inland Aquatic

178

54

17

108

357

Terrestrial

35

39

29

355

458
