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
#' A basic table with a purple heading bar and coloured blocks around the
#' categories
#' This data should be in the same format as for the plot functions
#'
#' @param DF The data frame that contains the data
#' @param COL the column containing the categories
#'
#' @importFrom kableExtra  kable
#' @importFrom kableExtra  kable_styling
#' @importFrom kableExtra  row_spec
#' @importFrom kableExtra  column_spec
#' @importFrom kableExtra  add_header_above
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
#'  nba_tbl_colr(COL = protection_level)
#'
#'tbl
#'
#' @export nba_tbl_colr
#'
#'
nba_tbl_colr <- function(DF, COL) {

  NBA_categories <-c("Natural",
                     "Natural/near-natural",
                     "Near-natural",
                     "Moderately modified",
                     "Heavily modified",
                     "Severely/critically modified",
                     "Well Protected",
                     "Moderately Protected",
                     "Poorly Protected",
                     "No Protection",
                     "Not Protected",
                     "Extinct",
                     "Critically Endangered",
                     "Endangered",
                     "Vulnerable",
                     "Near Threatened",
                     "Data Deficient",
                     "Rare",
                     "Least Concern",
                     "Cropland",
                     "Plantation",
                     "Built up",
                     "Mine",
                     "Artificial waterbody")

  NBA_colours <- c(

    ##Threat status
    "Critically Endangered" = rgb(216, 30, 5, maxColorValue = 255),
    "Endangered" = rgb(252, 127, 63, maxColorValue = 255),
    "Vulnerable" = rgb(249, 232, 20, maxColorValue = 255),
    "Near Threatened" = rgb(204, 226, 38, maxColorValue = 255),
    "Least Concern" = rgb(180, 215, 158, maxColorValue = 255),
    "Data Deficient" = rgb(209, 209, 198, maxColorValue = 255),
    "Rare" = rgb(193, 181, 165, maxColorValue = 255),
    "Extinct" = rgb(0, 0, 0, maxColorValue = 255),
    "Extinct in the Wild" = rgb(84, 35, 68, maxColorValue = 255),

    #Protection level
    "Not Protected" = rgb(166, 166, 166, maxColorValue = 255),
    "Poorly Protected" = rgb(213, 222, 196, maxColorValue = 255),
    "Moderately Protected" = rgb(132, 171, 92, maxColorValue = 255),
    "Well Protected" = rgb(75, 110, 0, maxColorValue = 255),

    #Pressures
    "Low" = rgb(223, 220, 199, maxColorValue = 255),
    "Medium" = rgb(175, 168, 117, maxColorValue = 255),
    "High" = rgb(122, 116, 70, maxColorValue = 255),
    "Very high" = rgb(88, 82, 50, maxColorValue = 255),


    "No threats" = rgb(48, 30, 6, maxColorValue = 255),
    "Pollution" = rgb(97, 65, 56, maxColorValue = 255),
    "Transportation & service corridors" = rgb(99, 76, 39, maxColorValue = 255),
    "Agriculture" = rgb(133, 76, 13, maxColorValue = 255),
    "Agriculture and aquaculture" = rgb(133, 76, 13, maxColorValue = 255),
    "Geological events" = rgb(153, 102, 0, maxColorValue = 255),
    "Biological resource use" = rgb(180, 121, 42, maxColorValue = 255),
    "Other threats" = rgb(231, 160, 54, maxColorValue = 255),
    "Human intrusions & disturbance" = rgb(159, 134, 9, maxColorValue = 255),
    "Human intrusions and disturbance" = rgb(159, 134, 9, maxColorValue = 255),
    "Climate change" = rgb(178, 149, 78, maxColorValue = 255),
    "Climate change & severe weather" = rgb(178, 149, 78, maxColorValue = 255),
    "Energy production & mining" = rgb(122, 116, 70, maxColorValue = 255),
    "Energy production and mining" = rgb(122, 116, 70, maxColorValue = 255),
    "Natural system modifications" = rgb(88, 82, 50, maxColorValue = 255),
    "Invasive and other problematic species, genes & diseases" = rgb(61, 69, 64, maxColorValue = 255),
    "Invasive and other problamatic species, genes and diseases" = rgb(61, 69, 64, maxColorValue = 255),
    "Residential & commercial development" = rgb(128, 128, 128, maxColorValue = 255),


    # Condition
    "Natural" = rgb(110, 159, 212, maxColorValue = 255),
    "Natural / near natural" = rgb(110, 159, 212, maxColorValue = 255),
    "Near natural" = rgb(110, 159, 212, maxColorValue = 255),
    "Moderately modified" = rgb(165, 197, 199, maxColorValue = 255),
    "Heavily / intensively modified" = rgb(129, 171, 167, maxColorValue = 255),
    "Permanently / irreversibly modified" = rgb(136, 129, 78, maxColorValue = 255),


    # Responses
    "No response" = rgb(91, 66, 114, maxColorValue = 255),
    "Some kind of response" = rgb(100, 103, 130, maxColorValue = 255),
    "Gazetted" = rgb(117, 164, 179, maxColorValue = 255),
    "Signed off" = rgb(117, 164, 179, maxColorValue = 255),


    # Priority areas
    "Land-based Protected Areas" = rgb(0, 60, 0, maxColorValue = 255),
    "Marine Protected Areas" = rgb(0, 38, 115, maxColorValue = 255),
    "Critical Biodiversity Areas" = rgb(67, 128, 0, maxColorValue = 255),
    "Ecologically Sensitive Areas" = rgb(168, 168, 0, maxColorValue = 255),

    # Built up areas
    "Cropland"= rgb(0, 0, 0, maxColorValue = 255),
    "Plantation"= rgb(0, 0, 0, maxColorValue = 255),
    "Built up"= rgb(0, 0, 0, maxColorValue = 255),
    "Mine"= rgb(0, 0, 0, maxColorValue = 255),
    "Artificial waterbody" = rgb(0, 0, 0, maxColorValue = 255)

  )

  color_cell <- function(COL) {

    color <- NBA_colours[match(COL, names(NBA_colours))]

    html <- paste0(
      '<div style="background-color:', color, '; color: black; padding: 5px;">',
      COL, '</div>'
    )

    return(html)
  }

  # Apply the HTML function to the Status column
  DF_col <- DF %>%
    dplyr::mutate({{COL}} := sapply({{COL}}, color_cell))

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


#########################################
# Function to apply gt styling to table #
#########################################
#' gt table theme
#'
#' Function to apply gt styling to table
#'
#' @param GT_TBL The data frame that contains the data
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

nba_tbl_theme <- function(GT_TBL) {
  last_row <- nrow(GT_TBL[["_data"]])

  GT_TBL %>%
    # 1. Header shading and borders
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "lightgray"),
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
#' @param DF The data frame that contains the data
#' @param GROUP The column that contains the name of the variable (the ecosystems or taxa names)
#' @param THR The column name of the threat statuses
#' @param PRO The column name of the protection levels
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
#'sf::st_drop_geometry() |>
#'nba_tbl_comb(GROUP = P_EcosysType,
#'THR = threat_status,
#'PRO = protection_level)
#'
#'thr_pro_tbl
#'
#' @export nba_tbl_comb
#'
#'

nba_tbl_comb <- function(DF, GROUP, THR, PRO){


  NBA_categories <-c("Natural",
                     "Natural/near-natural",
                     "Near-natural",
                     "Moderately modified",
                     "Heavily modified",
                     "Severely/critically modified",
                     "Well Protected",
                     "Moderately Protected",
                     "Poorly Protected",
                     "No Protection",
                     "Not Protected",
                     "Extinct",
                     "Critically Endangered",
                     "Endangered",
                     "Vulnerable",
                     "Near Threatened",
                     "Data Deficient",
                     "Rare",
                     "Least Concern",
                     "Cropland",
                     "Plantation",
                     "Built up",
                     "Mine",
                     "Artificial waterbody")

  NBA_colours <- c(

    ##Threat status
    "Critically Endangered" = rgb(216, 30, 5, maxColorValue = 255),
    "Endangered" = rgb(252, 127, 63, maxColorValue = 255),
    "Vulnerable" = rgb(249, 232, 20, maxColorValue = 255),
    "Near Threatened" = rgb(204, 226, 38, maxColorValue = 255),
    "Least Concern" = rgb(180, 215, 158, maxColorValue = 255),
    "Data Deficient" = rgb(209, 209, 198, maxColorValue = 255),
    "Rare" = rgb(193, 181, 165, maxColorValue = 255),
    "Extinct" = rgb(0, 0, 0, maxColorValue = 255),
    "Extinct in the Wild" = rgb(84, 35, 68, maxColorValue = 255),

    #Protection level
    "Not Protected" = rgb(166, 166, 166, maxColorValue = 255),
    "Poorly Protected" = rgb(213, 222, 196, maxColorValue = 255),
    "Moderately Protected" = rgb(132, 171, 92, maxColorValue = 255),
    "Well Protected" = rgb(75, 110, 0, maxColorValue = 255),

    #Pressures
    "Low" = rgb(223, 220, 199, maxColorValue = 255),
    "Medium" = rgb(175, 168, 117, maxColorValue = 255),
    "High" = rgb(122, 116, 70, maxColorValue = 255),
    "Very high" = rgb(88, 82, 50, maxColorValue = 255),


    "No threats" = rgb(48, 30, 6, maxColorValue = 255),
    "Pollution" = rgb(97, 65, 56, maxColorValue = 255),
    "Transportation & service corridors" = rgb(99, 76, 39, maxColorValue = 255),
    "Agriculture" = rgb(133, 76, 13, maxColorValue = 255),
    "Agriculture and aquaculture" = rgb(133, 76, 13, maxColorValue = 255),
    "Geological events" = rgb(153, 102, 0, maxColorValue = 255),
    "Biological resource use" = rgb(180, 121, 42, maxColorValue = 255),
    "Other threats" = rgb(231, 160, 54, maxColorValue = 255),
    "Human intrusions & disturbance" = rgb(159, 134, 9, maxColorValue = 255),
    "Human intrusions and disturbance" = rgb(159, 134, 9, maxColorValue = 255),
    "Climate change" = rgb(178, 149, 78, maxColorValue = 255),
    "Climate change & severe weather" = rgb(178, 149, 78, maxColorValue = 255),
    "Energy production & mining" = rgb(122, 116, 70, maxColorValue = 255),
    "Energy production and mining" = rgb(122, 116, 70, maxColorValue = 255),
    "Natural system modifications" = rgb(88, 82, 50, maxColorValue = 255),
    "Invasive and other problematic species, genes & diseases" = rgb(61, 69, 64, maxColorValue = 255),
    "Invasive and other problamatic species, genes and diseases" = rgb(61, 69, 64, maxColorValue = 255),
    "Residential & commercial development" = rgb(128, 128, 128, maxColorValue = 255),


    # Condition
    "Natural" = rgb(110, 159, 212, maxColorValue = 255),
    "Natural / near natural" = rgb(110, 159, 212, maxColorValue = 255),
    "Near natural" = rgb(110, 159, 212, maxColorValue = 255),
    "Moderately modified" = rgb(165, 197, 199, maxColorValue = 255),
    "Heavily / intensively modified" = rgb(129, 171, 167, maxColorValue = 255),
    "Permanently / irreversibly modified" = rgb(136, 129, 78, maxColorValue = 255),


    # Responses
    "No response" = rgb(91, 66, 114, maxColorValue = 255),
    "Some kind of response" = rgb(100, 103, 130, maxColorValue = 255),
    "Gazetted" = rgb(117, 164, 179, maxColorValue = 255),
    "Signed off" = rgb(117, 164, 179, maxColorValue = 255),


    # Priority areas
    "Land-based Protected Areas" = rgb(0, 60, 0, maxColorValue = 255),
    "Marine Protected Areas" = rgb(0, 38, 115, maxColorValue = 255),
    "Critical Biodiversity Areas" = rgb(67, 128, 0, maxColorValue = 255),
    "Ecologically Sensitive Areas" = rgb(168, 168, 0, maxColorValue = 255),

    # Built up areas
    "Cropland"= rgb(0, 0, 0, maxColorValue = 255),
    "Plantation"= rgb(0, 0, 0, maxColorValue = 255),
    "Built up"= rgb(0, 0, 0, maxColorValue = 255),
    "Mine"= rgb(0, 0, 0, maxColorValue = 255),
    "Artificial waterbody" = rgb(0, 0, 0, maxColorValue = 255)

  )

  ## specify the order for threat statuses and protection levels for consistent ordering
  threat_order <- c("Critically Endangered", "Endangered", "Vulnerable", "Near Threatened", "Least Concern")
  protection_order <- c("Not Protected", "Poorly Protected", "Moderately Protected", "Well Protected")




  summary_table <- DF %>%
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

  ##calculate number of ecosystems
  eco_num <- DF %>%
    dplyr::distinct({{GROUP}}) %>%
    dplyr::count() %>%
    as.data.frame()

  eco_num <- eco_num[,1]

  ## calculate the percentage
  percentage_row <- total_row %>%
    dplyr::mutate(dplyr::across(-{{THR}}, ~ (.x/eco_num) * 100))%>%  ## calculate percentage for each column
    dplyr::mutate({{THR}} := "Percentage (%)")  ## set the row name as "Percentage %"

  ## combine summary, total, and percentage rows
  final_table <- dplyr::bind_rows(summary_table, total_row, percentage_row)

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
    ## grey out and add borders for the Total row
    kableExtra::row_spec(nrow(final_table) - 1, background = "lightgrey", color = "black",
             extra_css = "border-top: 2px solid black; border-bottom: 2px solid black") %>%
    ## grey out and add borders for the Percentage row
    kableExtra::row_spec(nrow(final_table), background = "lightgrey", color = "black",
             extra_css = "border-top: 2px solid black; border-bottom: 2px solid black;")

  ## rotate the header text to fit and enhance readability
  tbl_final <- tbl_final %>%
    kableExtra::row_spec(0, extra_css = "text-align: left; writing-mode: vertical-rl; transform: rotate(180deg); white-space: nowrap; height: 170px;")


  ## apply background colors for threat status rows
  for (i in 1:(nrow(final_table) - 1)) {
    thr_name <- final_table %>%
      select({{THR}}) %>%
      dplyr::slice(i) %>%
      as.data.frame()
    thr_name <- thr_name[,1]
    if (!is.na(thr_name) && thr_name %in% names(NBA_colours)) {
      tbl_final <- tbl_final %>%
        kableExtra::row_spec(i, background = NBA_colours[thr_name])
    }
  }


  ## apply background colors for protection level columns
  for (j in 1:length(protection_order)) {
    protection_column <-   colnames(final_table)[[j + 1]] ## access each protection level column
    tbl_final <- tbl_final %>%
      kableExtra::column_spec(j+1 ,
                  background = ifelse(protection_column %in% names(NBA_colours),
                                      NBA_colours[as.character(protection_column)],
                                      "white"),
                  include_thead = TRUE,
                  bold = FALSE)
  }

  ## ensure the Total column has no background color
  tbl_final <- tbl_final %>%
    kableExtra::column_spec(2:ncol(final_table), background = "white")

  ## apply background color to the percentage row
  tbl_final <- tbl_final %>%
    kableExtra::row_spec(nrow(final_table), background = "lightgrey")  ## change color for the total row as needed

  ## apply background color to the total row
  tbl_final <- tbl_final %>%
    kableExtra::row_spec(nrow(final_table) - 1, background = "lightgrey")

  ## display the final table
  tbl_final ## render the final formatted table


}
