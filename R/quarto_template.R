#' Create template quarto project to get an NBA html page up and running
#'
#' Copies a template.qmd, template.scss, and _quarto.yml into a Quarto project directory.
#'
#' @param path The destination directory for the project followed by the project name
#' @param overwrite Whether to overwrite existing files. Defaults to FALSE.
#' @param files Character vector of files to copy. Defaults to all template files.
#' @param rename Named character vector to rename files on copy, e.g., c("scientific.qmd" = "index.qmd")
#'
#' @return Invisibly returns the paths of copied files.
#'
#' @importFrom usethis create_project
#'
#' @export
nba_init_quarto_proj <- function(path = "PathToMyProject/MyNewProject",
                                 overwrite = FALSE,
                                 files = c("scientific.qmd","basic.qmd", "custom.scss", "_quarto.yml",
                                           "_brand.yml", "annals-of-the-new-york-academy-of-sciences.csl",
                                           "references.bib", "nba-banner.png",
                                           "sanbi-logo-small.png", "terrestrial-ecosystems.csv"),
                                 rename = NULL) {

  # Load required package
  if (!requireNamespace("usethis", quietly = TRUE)) {
    stop("The 'usethis' package is required. Please install it with install.packages('usethis').")
  }

  # Create a new RStudio project
  usethis::create_project(path = path, open = TRUE, rstudio = TRUE)


  # Template source directory
  template_dir <- system.file("templates", package = "nbaR")


  copied <- character()

  ##Create imgs/ subfolder if not already present
  imgs_dir <- file.path(path, "imgs")
  if (!dir.exists(imgs_dir)) {
    dir.create(imgs_dir, recursive = TRUE)
    message("Created folder: imgs/")
  }

  ##Create data/ subfolder if not already present
  data_dir <- file.path(path, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
    message("Created folder: data/")
  }

  for (file in files) {
    source_path <- file.path(template_dir, file)
    if (!file.exists(source_path)) {
      warning(paste("Template file", file, "not found."))
      next
    }

    # Determine file destination
    if (grepl("\\.png$", file)) {
      dest_dir <- imgs_dir
    } else if (grepl("\\.csv$", file)) {
      dest_dir <- data_dir
    } else {
      dest_dir <- path
    }

    # Rename if requested
    out_name <- if (!is.null(rename) && file %in% names(rename)) {
      rename[[file]]
    } else {
      file
    }

    target_path <- file.path(dest_dir, out_name)

    # Handle existing files
    if (file.exists(target_path) && !overwrite) {
      print(paste("Skipping", out_name, "(already exists)."))
      next
    }


    file.copy(source_path, target_path, overwrite = TRUE)
    print(paste("Copied", out_name, "to", target_path))
    copied <- c(copied, target_path)
  }

  invisible(copied)
}


#' Create template quarto documents to get an NBA html page up and running
#'
#' Copies a template.qmd, template.scss, and _quarto.yml into a Quarto project directory.
#'
#' @param path The destination directory. Defaults to current working directory. Just add the name of any folder you want the documents to be cpoied into as long as it is in your project folder.
#' @param overwrite Whether to overwrite existing files. Defaults to FALSE.
#' @param files Character vector of files to copy. Defaults to all template files.
#' @param rename Named character vector to rename files on copy, e.g., c("scientific.qmd" = "index.qmd")
#'
#' @return Invisibly returns the paths of copied files.
#'
#' @importFrom usethis create_project
#'
#' @export
nba_init_quarto_docs <- function(path = ".",
                                 overwrite = FALSE,
                                 files = c("scientific.qmd","basic.qmd", "custom.scss", "_quarto.yml",
                                           "_brand.yml", "annals-of-the-new-york-academy-of-sciences.csl"),
                                 rename = NULL) {


  ##create template quarto file and yaml file
  # Template source directory
  template_dir <- system.file("templates", package = "nbaR")


  copied <- character()

  for (file in files) {
    source_path <- file.path(template_dir, file)
    if (!file.exists(source_path)) {
      print(paste("Template file", file, "not found."))
      next
    }

    # Rename if requested
    out_name <- if (!is.null(rename) && file %in% names(rename)) {
      rename[[file]]
    } else {
      file
    }

    target_path <- file.path(path, out_name)

    # Handle existing files
    if (file.exists(target_path) && !overwrite) {
      print(paste("Skipping", out_name, "(already exists)."))
      next
    }

    file.copy(source_path, target_path, overwrite = TRUE)
    print(paste("Copied", out_name, "to", target_path))
    copied <- c(copied, target_path)
  }

  invisible(copied)
}
