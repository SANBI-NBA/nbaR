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
#' @export
nba_init_quarto_docs <- function(path = ".",
                                 overwrite = FALSE,
                                 files = c("scientific.qmd","basic.qmd", "custom.scss", "_quarto.yml"),
                                 rename = NULL) {

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
    print(paste("Copied", file, "to", out_name))
    copied <- c(copied, target_path)
  }

  invisible(copied)
}
