#' Basic table
#'
#' A basic table with a purple heading bar.
#'
#'
#' @param DF The data frame that contains the data
#'
#' @importFrom kableExtra  kable
#' @importFrom kableExtra  kable_styling
#' @importFrom kableExtra  row_spec
#' @importFrom kableExtra  column_spec
#' @importFrom kableExtra  add_header_above
#'
#'@examples
#'#test <- basic_tbl(bird_data)
#'
#' @export
#'
#'
basic_tbl <- function(DF){

  table <- kableExtra::kable(DF, "html", escape = FALSE) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      position = "center",
      font_size = 1) %>%
    kableExtra::row_spec(0, background = "#899be1", color = "black", bold = TRUE, extra_css = "border: 1px solid black") %>% # purple header with black text and black borders
    kableExtra::column_spec(1:ncol(DF), border_left = TRUE, border_right = TRUE, background = "white") %>% # black border around columns
    kableExtra::add_header_above(c(" " = ncol(DF)), line = TRUE, line_sep = 3, color = "black") # black border around header
  table
}

################################################################################
#' Protection level table
#'
#' A basic table with a purple heading bar and coloured blocks around the
#' Protection level categories
#'
#' @param DF The data frame that contains the data
#' @param COL the column containing the Protection level categories
#'
#' @importFrom kableExtra  kable
#' @importFrom kableExtra  kable_styling
#' @importFrom kableExtra  row_spec
#' @importFrom kableExtra  column_spec
#' @importFrom kableExtra  add_header_above
#' @importFrom dplyr  case_when
#' @importFrom dplyr  mutate
#'
#'@examples
#'#test <- thr_tbl(bird_data, Protection_level)
#'
#' @export
#'
#'
thr_tbl <- function(DF, COL) {

  color_cell <- function(COL) {
    color <- dplyr::case_when(
      COL == "Well Protected" ~ "#466a31",
      COL == "Moderately Protected" ~ "#80a952",
      COL == "Poorly Protected" ~ "#d5dec3",
      COL == "No Protection" ~ "#a4a3a3",
      TRUE ~ "white"
    )

    html <- paste0(
      '<div style="background-color:', color, '; color: black; padding: 5px;">',
      COL, '</div>'
    )

    return(html)
  }

  # Apply the HTML function to the Status column
  DF_col <- DF %>%
    dplyr::mutate(Status = sapply({{COL}}, color_cell))

  table <- kableExtra::kable(DF_col, "html", escape = FALSE) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      position = "center",
      font_size = 12
    ) %>%
    kableExtra::column_spec(1:ncol(DF_col), border_left = TRUE, border_right = TRUE, background = "white") %>%
    kableExtra::add_header_above(c(" " = ncol(DF_col)), line = TRUE, line_sep = 3, color = "black")%>%
    kableExtra::row_spec(0, background = "#899be1", color = "black", bold = TRUE, extra_css = "border: 1px solid black")
  table

}
