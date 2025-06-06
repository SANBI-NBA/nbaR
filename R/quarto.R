##nba_init_quarto_proj##################################################
#' Create template quarto project to get an NBA html page up and running
#'
#' Copies a template.qmd, template.scss, and _quarto.yml into a Quarto project directory.
#'
#' @param path The destination directory for the project, the project will automatically be names templates
#' @param overwrite Whether to overwrite existing files. Defaults to FALSE.
#' @param files Character vector of files to copy. Defaults to all template files.
#' @param rename Named character vector to rename files on copy, e.g., c("scientific.qmd" = "index.qmd")
#'
#' @return Invisibly returns the paths of copied files.
#'
#' @importFrom usethis create_project
#'
#' @export
nba_init_quarto_proj <- function(path = "Path/To/My/Project/",
                                 overwrite = FALSE, #outputs
                                 rename = NULL) {

  # Load required package
  if (!requireNamespace("usethis", quietly = TRUE)) {
    stop("The 'usethis' package is required. Please install it with install.packages('usethis').")
  }



  # Template source directory
  template_dir <- system.file("templates", package = "nbaR")


  # Copy everything from Template to new path
  file.copy(from = template_dir,
            to = path,
            recursive = TRUE)

  # Create a new RStudio project
    usethis::create_project(path = paste0(path, "/templates"), open = TRUE, rstudio = TRUE)



  # copied <- character()
  #
  # ##Create partials/ subfolder if not already present
  # partials_dir <- file.path(path, "partials")
  # if (!dir.exists(partials_dir)) {
  #   dir.create(partials_dir, recursive = TRUE)
  #   message("Created folder: partials/")
  # }
  #
  # ##Create quarto/ subfolder if not already present
  # quarto_dir <- file.path(path, "quarto")
  # if (!dir.exists(quarto_dir)) {
  #   dir.create(quarto_dir, recursive = TRUE)
  #   message("Created folder: quarto/")
  # }
  #
  # ##Create quarto/imgs/ subfolder if not already present
  # imgs_dir <- file.path(path, "quarto/imgs")
  # if (!dir.exists(imgs_dir)) {
  #   dir.create(imgs_dir, recursive = TRUE)
  #   message("Created folder: quarto/imgs/")
  # }
  #
  # ##Create quarto/data/ subfolder if not already present
  # data_dir <- file.path(path, "quarto/data")
  # if (!dir.exists(data_dir)) {
  #   dir.create(data_dir, recursive = TRUE)
  #   message("Created folder: quarto/data/")
  # }
  #
  # ##Create quarto/outputs subfolder if not already present
  # outputs_dir <- file.path(path, "quarto/outputs")
  # if (!dir.exists(outputs_dir)) {
  #   dir.create(outputs_dir, recursive = TRUE)
  #   message("Created folder: quarto/outputs")
  # }
  #
  # for (file in files) {
  #   source_path <- file.path(template_dir, file)
  #   if (!file.exists(source_path)) {
  #     warning(paste("Template file", file, "not found."))
  #     next
  #   }
  #
  #   # Determine file destination
  #   if (grepl("\\.png$", file)) {
  #     dest_dir <- imgs_dir
  #   } else if (grepl("\\.csv$", file)) {
  #     dest_dir <- data_dir
  #   } else {
  #     dest_dir <- path
  #   }
  #
  #   # Rename if requested
  #   out_name <- if (!is.null(rename) && file %in% names(rename)) {
  #     rename[[file]]
  #   } else {
  #     file
  #   }
  #
  #   target_path <- file.path(dest_dir, out_name)
  #
  #   # Handle existing files
  #   if (file.exists(target_path) && !overwrite) {
  #     print(paste("Skipping", out_name, "(already exists)."))
  #     next
  #   }
  #
  #
  #   file.copy(source_path, target_path, overwrite = TRUE)
  #   print(paste("Copied", out_name, "to", target_path))
  #   copied <- c(copied, target_path)
  # }
  #
  # invisible(copied)
}

##nba_init_quarto_docs##################################################
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
                                 files = c("_brand.yml","_quarto.yml", #main folder
                                           "custom.scss",  "sanbi.csl",

                                           "_title-meta-author.html", "title-metadata.html", #partials


                                           "scientific.qmd","basic.qmd","references.bib"#quarto
                                           ),
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

##nba_citation##############################################################
#' NBA citation  function
#'
#' A function to create the citation string when given the author metadata
#' in a quarto file to be rendered to a website.
#'
#'
#'
#' @param META The result of calling rmarkdown::metadata. Note that
#' rmarkdown::metadata only runs when the .qmd file is rendered,
#' therefore the outputs of this code will return no results
#' if you try to run it within R - that does not mean it is not working.
#'
#'
#' @return Returns a citation string
#'
#'
#' @importFrom WikidataR initials
#' @importFrom knitr current_input
#'
#' @export
#'
#' @examples
#' # meta <- knitr::opts_knit$get("rmarkdown.pandoc.to")  # forces knitr to load metadata
#' # meta <- rmarkdown::metadata  # full YAML is now in `meta`
#' #
#' # nba_citation(meta)
#' #
#'#
#'
#'

nba_citation <- function(META){

  # 1. ----------- Author string ----------------------------------------------
  # helper: turn "Jo M."  -> "J.M."
  nba_initials <- function(given) {
    # split on spaces or dots, drop empties, keep first letter, add a dot
    parts <- unlist(strsplit(given, "[[:space:].]+"))
    paste0(substring(parts, 1, 1), collapse = ".") |> paste0(".")
  }

  authors <- vapply(meta$author, function(a) {
    fam  <- a$name$family
    fam  <- sub("^(.)(.*)$", "\\U\\1\\E\\2", fam, perl = TRUE)  # cap first letter only
    inits <- nba_initials(a$name$given)
    paste0(fam, ", ", inits)
  }, FUN.VALUE = character(1))

  if (length(authors) == 1) {
    author_str <- authors
  } else if (length(authors) == 2) {
    author_str <- paste(authors, collapse = " & ")
  } else {
    author_str <- paste(
      paste(authors[-length(authors)], collapse = ", "),
      authors[length(authors)],
      sep = if (length(authors) > 2) ", & " else " & "
    )
  }

  #--- 2. year ---------------------------------------------------------
  if (identical(meta$date, "last-modified")) {
    # take the file’s modification time (UTC) and pull the year
    file_time <- file.info(knitr::current_input())$mtime
    yr <- format(as.Date(file_time), "%Y")
  } else {
    yr <- format(as.Date(meta$date), "%Y")
  }

  # 3. ----------- Remaining pieces -----------------------------------------
  title     <- meta$title
  container <- meta$citation$`container-title`
  publisher <- meta$citation$publisher
  url       <- meta$citation$url

  # 4. ----------- Stitch the citation together -----------------------------
  cat(paste0(
    author_str, " ", yr, ". ", title, ". ",
    container, ". ", publisher, ". ", url, ".\n\n"
  ))

}
