# NBA citation function

A function to create the citation string when given the author metadata
in a quarto file to be rendered to a website.

## Usage

``` r
nba_citation(META)
```

## Arguments

- META:

  The result of calling rmarkdown::metadata. Note that
  rmarkdown::metadata only runs when the .qmd file is rendered,
  therefore the outputs of this code will return no results if you try
  to run it within R - that does not mean it is not working.

## Value

Returns a citation string

## Examples

``` r
# meta <- knitr::opts_knit$get("rmarkdown.pandoc.to")  # forces knitr to load metadata
# meta <- rmarkdown::metadata  # full YAML is now in `meta`
#
# nba_citation(meta)
#
#

```
