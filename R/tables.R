### Create the function to create tables for regular tables
#' Title
#'
#' @param DAT
#'
#' @return
#' @export
#'
#' @examples
basic_table <-function(DAT)

{
kable(DAT, "html", escape = FALSE) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    position = "center",
    font_size = 12
  ) %>%
  row_spec(0, background = "#899be1", color = "black", bold = TRUE, extra_css = "border: 1px solid black") %>% # purple header with black text and black borders
  column_spec(1:ncol(eco_mpa_df), border_left = TRUE, border_right = TRUE, background = "white") %>% # black border around columns
  kableExtra::add_header_above(c(" " = ncol(eco_mpa_df)), line = TRUE, line_sep = 3, color = "black") # black border around header
}

### Create the function to create tables for protection level tables
#' Title
#'
#' @param DAT
#'
#' @return
#' @export
#'
#' @examples
basic_table <-function(DATA)

{
  kable(DATA, "html", escape = FALSE) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      position = "center",
      font_size = 12
    ) %>%
    row_spec(0, background = "#899be1", color = "black", bold = TRUE, extra_css = "border: 1px solid black") %>% # purple header with black text and black borders
    column_spec(1:ncol(eco_mpa_df), border_left = TRUE, border_right = TRUE, background = "white") %>% # black border around columns
    kableExtra::add_header_above(c(" " = ncol(eco_mpa_df)), line = TRUE, line_sep = 3, color = "black") # black border around header
}

