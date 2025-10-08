#' Basic NBA table
#'
#' A basic table with a purple heading bar.
#' This data should be in the same format as for the plot functions
#'
#'
#' @param DF The data frame that contains the data
#'
#' @importFrom kableExtra  kable
#' @importFrom kableExtra  kable_styling
#' @importFrom kableExtra  row_spec
#' @importFrom kableExtra  column_spec
#' @importFrom kableExtra  add_header_above
#' @importFrom magrittr "%>%"
#'
#'@examples
#'tbl <- nba_tbl(NBA_example_pro_data)
#'
#'tbl
#'
#' @export nba_tbl
#'
#'
nba_tbl <- function(DF){

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
#' Coloured table
#'
#' A basic table with a coloured heading bar and coloured blocks around the
#' categories
#' This data should be in the same format as for the plot functions.
#'
#' This table has the same styling as set out in nba_tbl_theme. If you would like
#' to change anything you can just use a pipe %>%  to add gt styling onto the gt
#' object this function creates and it will override the styling set in the function.
#'
#' @param DF The data frame that contains the data
#' @param COL the column containing the categories
#' @param HEADER the name to determine the colour of the header row
#'
#' @importFrom gt gt
#' @importFrom gt tab_style
#' @importFrom gt cell_fill
#' @importFrom gt cell_text
#' @importFrom gt cells_column_labels
#' @importFrom gt tab_options
#' @importFrom gt cell_borders
#' @importFrom gt cells_body
#' @importFrom tidyselect everything
#' @importFrom scales col_factor
#' @importFrom tidyselect all_of
#' @importFrom rlang as_name
#' @importFrom dplyr  case_when
#' @importFrom dplyr  mutate
#' @importFrom magrittr "%>%"
#'
#'@examples
#'library(dplyr)
#'library(tidyr)
#'
#'tbl <- NBA_example_pro_data %>%
#'  pivot_longer(2:5, names_to = "protection_level") %>%
#'  nba_tbl_colr(COL = protection_level, HEADER = "Coast")
#'
#'tbl
#'
#' @export nba_tbl_colr
#'
#'


nba_tbl_colr <- function(DF, COL, HEADER = c("sanbi-green",
                                                      "sanbi-orange",
                                                      "sanbi-purple",
                                                      "Freshwater",
                                                      "Marine",
                                                      "Coast",
                                                      "Estuarine",
                                                      "Terrestrial",
                                                      "Genetics",
                                                      "PEI")) {

  # Capture column name as string
  col_name <- rlang::as_name(enquo(COL))

  # Extract header colors from nbaR palette
  header_col <- nbaR::NBA_colours[match(HEADER, names(nbaR::NBA_colours))]

  # Extract colors for cell values in the target column
  value_colors <- nbaR::NBA_colours[DF[[col_name]]]

  # last row
  last_row <- nrow(DF)

  # Build the gt table
  gt_tbl <- DF %>%
    gt::gt() %>%
    # Color the entire column cells by matching their values to the palette
    gt::data_color(
      columns = tidyselect::all_of(col_name),
      colors = scales::col_factor(
        palette = value_colors,
        domain = DF[[col_name]]
      )
    ) %>%
    # Style the header row background colors according to header_col vector
    gt::tab_style(
      style = list(
        gt::cell_fill(color = header_col[1]),
        gt::cell_text(weight = "bold", color = "black")
      ),
      locations = gt::cells_column_labels(columns = tidyselect::everything())
    ) %>%
    # Center the table
    gt::tab_options(table.align = "center") %>%
    # 1. Header shading and borders
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom"),
          color = "black",
          weight = gt::px(2),
          style = "solid"
        ),
        gt::cell_borders(
          sides = c("left", "right"),
          color = "lightgray"
        ),
        gt::cell_text(weight = "bold")
      ),
      locations = gt::cells_column_labels()
    ) %>%

    # 2. Data row borders (dotted top and bottom, no vertical)
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("top", "bottom"),
        color = "gray50",
        weight = gt::px(1),
        style = "dotted"
      ),
      locations = gt::cells_body()
    ) %>%

    # 3. Remove all vertical borders
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("left", "right"),
        color = "transparent"
      ),
      locations = gt::cells_body()
    ) %>%

    # 4. Bottom border for whole table
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom",
        color = "black",
        weight = gt::px(2),
        style = "solid"
      ),
      locations = gt::cells_body(rows = last_row)  # last row only
    )

  return(gt_tbl)
}



#########################################
# Function to apply gt styling to table #
#########################################
#' gt table theme
#'
#' Function to apply gt styling to table
#'
#' @param GT_TBL The data frame that contains the data
#' @param HEADER the name to determine the colour of the header row. Defaults to SANBI green
#'
#' @importFrom gt tab_style
#' @importFrom gt cell_fill
#' @importFrom gt cell_borders
#' @importFrom gt px
#' @importFrom gt cell_text
#' @importFrom gt cells_column_labels
#' @importFrom gt cells_body
#' @importFrom magrittr "%>%"
#'
#'@examples
#'library(gt)
#'library(dplyr)
#'
#'
#'gt_tbl <- NBA_example_thr_data %>%
#'  gt() %>%
#' nba_tbl_theme()
#'
#'gt_tbl
#'
#' @export nba_tbl_theme
#'
#'

nba_tbl_theme <- function(GT_TBL, HEADER = c("sanbi-green",
                                             "sanbi-orange",
                                             "sanbi-purple",
                                             "Freshwater",
                                             "Marine",
                                             "Coast",
                                             "Estuarine",
                                             "Terrestrial",
                                             "Genetics",
                                             "PEI")) {
  # Extract header colors from nbaR palette
  header_col <- nbaR::NBA_colours[match(HEADER, names(nbaR::NBA_colours))]

  #last row
  last_row <- nrow(GT_TBL[["_data"]])

  GT_TBL %>%

    # 1. Header shading and borders
    gt::tab_style(
      style = list(
        gt::cell_fill(color = header_col[1]),
        gt::cell_borders(
          sides = c("top", "bottom"),
          color = "black",
          weight = gt::px(2),
          style = "solid"
        ),
        gt::cell_borders(
          sides = c("left", "right"),
          color = "lightgray"
        ),
        gt::cell_text(weight = "bold")
      ),
      locations = gt::cells_column_labels()
    ) %>%

    # Left-align header and body of first column
    gt::tab_style(
      style = gt::cell_text(align = "left"),
      locations = list(
        gt::cells_column_labels(columns = 1),
        gt::cells_body(columns = 1)
      )
    ) %>%

    # Center-align header and body of all other columns
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = list(
        gt::cells_column_labels(columns = 2:last_col()),
        gt::cells_body(columns = 2:last_col())
      )
    ) %>%




    # 2. Data row borders (dotted top and bottom, no vertical)
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("top", "bottom"),
        color = "gray50",
        weight = gt::px(1),
        style = "dotted"
      ),
      locations = gt::cells_body()
    ) %>%

    # 3. Remove all vertical borders
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("left", "right"),
        color = "transparent"
      ),
      locations = gt::cells_body()
    ) %>%

    # 4. Bottom border for whole table
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom",
        color = "black",
        weight = gt::px(2),
        style = "solid"
      ),
      locations = gt::cells_body(rows = last_row)  # last row only
    ) %>%

    # gt::opt_table_font(
    #   font = list(
    #     gt::google_font(name = "Ariel"),
    #     gt::default_fonts()
    #   )
    # ) %>%
    gt::tab_options(
      table.font.size = gt::px(11.5)  # or "small", or px(10)
    )
}


#########################################
# Function to make a table of threat and protection level
#########################################
#' Table of protection level of threatened ecosystems.
#'
#' Function to style a table with threat status on the horizontal rows
#' and protection level on the vertical columns, with number of ecosystems
#' or taxa that share those categories, with total and percentage çolumns added
#'
#'
#'
#' @param DF The data frame that contains the data
#' @param GROUP The column that contains the name of the variable (the ecosystems or taxa names)
#' @param THR The column name of the threat statuses
#' @param PRO The column name of the protection levels
#' @param FILE An indication if the input file is a map (spatial file with a geom column) or a csv/ normal dataframe.
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr arrange
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr count
#' @importFrom dplyr bind_rows
#' @importFrom dplyr across
#' @importFrom dplyr slice
#' @importFrom tidyselect all_of
#' @importFrom dplyr select
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra row_spec
#' @importFrom kableExtra column_spec
#' @importFrom magrittr "%>%"
#'
#'@examples
#'
#'thr_pro_tbl <- NBA_example_map_data |>
#'nba_tbl_comb(GROUP = P_EcosysType,
#'THR = threat_status,
#'PRO = protection_level,
#'FILE = "spatial")
#'
#'thr_pro_tbl
#'
#' @export nba_tbl_comb
#'
#'


nba_tbl_comb <- function(DF, GROUP, THR, PRO, FILE = c("spatial", "csv")){
  ###make a table of protection level of threatened ecosystems
  ### define the levels and color of threat status and protection level categories

  ## define color mapping for threat statuses for use in the table display
  threat_color_mapping <- c("Critically Endangered" = rgb(216, 30, 5, maxColorValue = 255),
                            "Endangered" = rgb(252, 127, 63, maxColorValue = 255),
                            "Vulnerable" = rgb(249, 232, 20, maxColorValue = 255),
                            "Near Threatened" = rgb(204, 226, 38, maxColorValue = 255),
                            "Least Concern" = rgb(180, 215, 158, maxColorValue = 255),
                            "Data Deficient" = rgb(209, 209, 198, maxColorValue = 255),
                            "Rare" = rgb(193, 181, 165, maxColorValue = 255),
                            "Extinct" = rgb(0, 0, 0, maxColorValue = 255),
                            "Extinct in the Wild" = rgb(84, 35, 68, maxColorValue = 255))


  ## specify the order for threat statuses and protection levels for consistent ordering
  threat_order <- c("Critically Endangered", "Endangered", "Vulnerable", "Near Threatened", "Least Concern")
  protection_order <- c("Not Protected", "Poorly Protected", "Moderately Protected", "Well Protected")

  ############################################

  if(FILE == "spatial"){

    summary_table <- DF %>%
      sf::st_drop_geometry()
  } else{

    summary_table <- DF
    }

  summary_table <- summary_table %>%
      dplyr::distinct({{GROUP}}, {{THR}}, {{PRO}}) %>%
      dplyr::mutate(
        ## set factor levels for threat status based on the defined order
        {{THR}} := factor({{THR}}, levels = threat_order)
      )%>%
      ## Group by threat status and protection level
      dplyr::group_by({{THR}}, {{PRO}}) %>%
      ## count the occurrences within each group
      dplyr::summarise(Count = dplyr::n(), .groups = 'drop') %>%
      ## convert from long to wide format, filling missing values with zero
      tidyr::pivot_wider(names_from = {{PRO}}, values_from = Count, values_fill = list(Count = 0)) %>%
      ## calculate a total count for each row
      dplyr::mutate(Total = rowSums(dplyr::select(., -c({{THR}})), na.rm = TRUE)) %>%
      dplyr::arrange({{THR}})


    ## calculate total count for each column and add a total row
    total_row <- summary_table %>%
      dplyr::summarise(across(-c({{THR}}), \(x) sum(x, na.rm = TRUE))) %>%
      dplyr::mutate({{THR}} := "Total (n)")

    # ##calculate number of ecosystems
    # eco_num <- DF %>%
    #   dplyr::distinct({{GROUP}}) %>%
    #   dplyr::count() %>%
    #   as.data.frame()
    #
    # eco_num <- eco_num[,1]
    #
    # ## calculate the percentage
    # percentage_row <- total_row %>%
    #   dplyr::mutate(dplyr::across(-{{THR}}, ~ (.x/eco_num) * 100))%>%  ## calculate percentage for each column
    #   dplyr::mutate({{THR}} := "Percentage (%)")  ## set the row name as "Percentage %"

    ## combine summary, total
    final_table <- dplyr::bind_rows(summary_table, total_row)

    var <- deparse(substitute(THR))

    ## identify columns to round (exclude total and threat status columns)
    count_columns <- setdiff(names(final_table), c(var, "Total"))

    ## round counts to 0 decimal places for cleaner display
    final_table <- final_table %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(count_columns), round, 0))

    ## reorder protection level columns based on predefined order
    final_table <- final_table %>%
      dplyr::select({{THR}}, tidyselect::all_of(protection_order), Total)



  ######################################################################################
  ### use the function to produce the correct table

  ## create and format the table using kableExtra
  tbl_final <- knitr::kable(final_table, col.names = c("", colnames(final_table)[-1]), format = "html", escape = FALSE) %>%
    ## apply table styling
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      position = "center",
      font_size = 16
    ) %>%
    ## style the header row
    kableExtra::row_spec(0, background = "lightgrey", color = "black",
             extra_css = "border-top: 2px solid black; border-bottom: 2px solid black; text-align: left; font-weight: normal;") %>%
    ## set general column styling (no borders, white background)
    kableExtra::column_spec(1:ncol(final_table), border_left = FALSE, border_right = FALSE, background = "white") %>%
    # ## grey out and add borders for the Total row and make bold
    # row_spec(nrow(final_table) - 1, background = "lightgrey", color = "black",
    #          extra_css = "border-top: 2px solid black; border-bottom: 2px solid black",
    #          bold=T,hline_after = T) %>%
    ## grey out and add borders for the Percentage row and make bold
    kableExtra::row_spec(nrow(final_table), background = "lightgrey", color = "black",
             extra_css = "border-top: 2px solid black; border-bottom: 2px solid black;",
             bold=T,hline_after = T)

  # ## rotate the header text to fit and enhance readability
  # tbl_final <- tbl_final %>%
  #   row_spec(0, extra_css = "text-align: left; writing-mode: vertical-rl; transform: rotate(180deg); white-space: nowrap; height: 170px;")


  ## apply background colors for threat status rows
  for (i in 1:(nrow(final_table) - 1)) {
    thr_name <- final_table %>%
      dplyr::select({{THR}}) %>%
      dplyr::slice(i) %>%
      as.data.frame()
    thr_name <- thr_name[,1]
    if (!is.na(thr_name) && thr_name %in% names(threat_color_mapping)) {
      tbl_final <- tbl_final %>%
        kableExtra::row_spec(i, background = threat_color_mapping[thr_name])
    }
  }




  ## ensure the Total column has no background color
  tbl_final <- tbl_final %>%
    kableExtra::column_spec(1, background = "white")%>%
    kableExtra::column_spec(ncol(final_table), background = "white")

  ## apply background color to the total row
  tbl_final <- tbl_final %>%
    kableExtra::row_spec(nrow(final_table), background = "lightgrey")  ## change color for the total row as needed

  # apply bold to the heading
  tbl_final <- tbl_final %>%
    kableExtra::row_spec(0, bold=T,hline_after = T)  ## change color for the total row as needed


  ## display the final table
  tbl_final ## render the final formatted table
}
