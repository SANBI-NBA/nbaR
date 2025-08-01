% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quarto.R
\name{nba_init_quarto_proj}
\alias{nba_init_quarto_proj}
\title{Create template quarto project to get an NBA html page up and running}
\usage{
nba_init_quarto_proj(
  path = "Path/To/My/Project/",
  overwrite = FALSE,
  rename = NULL
)
}
\arguments{
\item{path}{The destination directory for the project, the project will automatically be names templates}

\item{overwrite}{Whether to overwrite existing files. Defaults to FALSE.}

\item{rename}{Named character vector to rename files on copy, e.g., c("scientific.qmd" = "index.qmd")}
}
\value{
Invisibly returns the paths of copied files.
}
\description{
Copies a project named templates
}
