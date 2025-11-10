# Create template quarto project to get an NBA html page up and running

Copies a project named templates

## Usage

``` r
nba_init_quarto_proj(
  path = "Path/To/My/Project/",
  overwrite = FALSE,
  rename = NULL
)
```

## Arguments

- path:

  The destination directory for the project, the project will
  automatically be named templates

- overwrite:

  Whether to overwrite existing files. Defaults to FALSE.

- rename:

  Named character vector to rename files on copy, e.g.,
  c("scientific.qmd" = "index.qmd")

## Value

Invisibly returns the paths of copied files.
