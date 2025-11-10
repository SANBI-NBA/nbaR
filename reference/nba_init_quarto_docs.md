# Create template quarto documents to get an NBA html page up and running

Copies a all template files into project directory.

## Usage

``` r
nba_init_quarto_docs(
  path = ".",
  overwrite = FALSE,
  files = c("_brand.yml", "_quarto.yml", "custom.scss", "sanbi.csl",
    "_title-meta-author.html", "title-metadata.html", "scientific.qmd", "basic.qmd",
    "references.bib"),
  rename = NULL
)
```

## Arguments

- path:

  The destination directory. Defaults to current working directory. Just
  add the name of any folder you want the documents to be cpoied into as
  long as it is in your project folder.

- overwrite:

  Whether to overwrite existing files. Defaults to FALSE.

- files:

  Character vector of files to copy. Defaults to all template files.

- rename:

  Named character vector to rename files on copy, e.g.,
  c("scientific.qmd" = "index.qmd")

## Value

Invisibly returns the paths of copied files.

## Details

main folder "\_brand.yml","\_quarto.yml", "custom.scss", "sanbi.csl"

partials "\_title-meta-author.html", "title-metadata.html",

quarto "scientific.qmd","basic.qmd","references.bib"
