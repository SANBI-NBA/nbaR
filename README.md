
# nbaR <a href="https://sanbi-nba.github.io/nbaR/"><img src="man/figures/logo.png" align="right" height="139" alt="nbaR website" /></a>

<!-- badges: start -->
<!-- badges: end -->

The nbaR houses functions that enable easy creation of standardised
graphs, plots and maps for SANBI’s National Biodiversity Assessment
(NBA) content.

## Installation

You can install the development version of nbaR from
[GitHub](https://github.com/) with:

``` r
# if (!require("devtools")) install.packages("devtools")
#devtools::install_github("SANBI-NBA/nbaR")
```

You can then do a tutorial on the package using:

``` r
#
# if (!require("learnr")) install.packages("learnr")
# learnr::run_tutorial("nba_package_tutorial", package = "nbaR")
```

## Usage

The package is intended for use in r and quarto by the NBA team to
assist with the creation of the NBA website pages and reports. The goal
of the functions is to simplify the processes of creating plots, tables,
and maps in the reports and to standardise the outputs of all realms.

## Data

The data used should be read in from csv’s, excel documents, or sql (or
created within r). The data structure accepted by the functions are
explained in the help files of each function. There are also a number of
example datasets to help users understand and visually see the required
data structure. Please refer to the help files and vignettes for these
example datasets. Having extra columns in not a problem for the
functions as they are mostly bulit with the ability to select the
columns you wish to use in the function.

Along with the example datasets there is a list of all possible names to
be used in the categories columns (**NBA_categories**), please pay
attention to spelling and cases as the functions are case sensitive. The
categories examples are ‘Well Protected’ or ‘Least Concern’, and are
used to assign the correct colours to the correct categories.

## Issues

If you find any issues with the package or functions, or if you have any
suggestions for improvements to the package please go to the git
repository and log an issue so it can be dealt with.

## How to cite this package

You can cite this package like this “tables and figures were produced
using the nbaR R package (Besseling et al., 2024)”. Here is the full
bibliographic reference to include in your reference list (don’t forget
to update the ‘last accessed’ date):

To cite package ‘nbaR’ in publications use:

Besseling N, Bull L, Monyeki M, Hendricks S, von Staden L (2024).
“Package for creation of the NBA.” *NA*, *1*(1), 1.
<doi:10.5281/zenodo.15310061> <https://doi.org/10.5281/zenodo.15310061>.

A BibTeX entry for LaTeX users is

@Article{, title = {Package for creation of the NBA}, author = {Natasha
Besseling and Lauryn Bull and Maphale Monyeki and Shae-Lynn Hendricks
and Lize {von Staden}}, journal = {NA}, year = {2024}, volume = {1},
number = {1}, pages = {1}, doi =
{<https://doi.org/10.5281/zenodo.15310061>}, }
