#######################################################################################################
###
#' NBA plot function
#'
#' A function to create horizontal bar plots and donut plots.
#' The function expects that there is a coloumn of groups (e.g. ecosystem function groups,
#' taxa etc), with one
#' group value per row, and then several columns of the protection level,
#' threat status, or condition of the groups with values representing either the number
#' or percentage/ extent of groups within each category. These columns should be named
#' according the to conventions in the nbaR::NBA_categories example list.
#'
#' Please look at the example datasets NBA_example_thr_plot and
#' NBA_example_pro_plot to see the correct
#' structure for the data. Please note that both of these datasets have the
#' same structure, whether it will be used to make a bar or donut plot is
#' irrelevant.
#' This function will plot the data as either a bar or donut plot
#' depending on what you require. You can also decide if you want the donut
#' plot to be split by ecosystem functional group or not (e.g one donut plot per
#' functional group) and choose ifyou want the number of ecosystems to be
#' displayed within the plot.
#'
#' The name of the groups column is irrelevant, but the categories must be
#' spelled correctly (there is a list of the standard spellings/ cases
#' of NBA categories named nbaR::NBA_categories in this package,
#' which can be accessed for reference).
#'
#'
#'
#' @param DF The data frame that contains the information
#' @param GROUPS The categorized variables (ecosystem functional group, taxa, etc)
#' @param COLS The categories to describe the variables (protection level, threat status, condition, etc). You can use any tidyselect method to select these columns e.g. 2:4/ Endangered:Vulnerable/ c(Well Protected,Moderately Protected,Poorly Protected,Not Protected) etc
#' @param CHRT A choice of either "bar" or "donut" plot
#' @param NUM A choice to show numbers in the plot, False to show no numbers
#' @param LAB The x axis label of the plot
#' @param GRP A choice of whether or not to plot the donut graphs by group, TRUE will plot a donut plot for each group.
#' @param SAVE The name of the output file that will be saved to the output folder. If you do not have an outputs folder you will be prompted to make one.
#' @param SCALE_TEXT scale the sizes of the plot text to fit your intended output. currently set at 1 as default. If you want to save it to 8 by 6 cm, set it to 0.5.
#'
#' @return Returns a plot
#'
#'
#' @importFrom ggplot2  ggplot
#' @importFrom ggplot2  geom_rect
#' @importFrom ggplot2  geom_text
#' @importFrom ggplot2  coord_polar
#' @importFrom ggplot2  xlim
#' @importFrom ggplot2  scale_fill_manual
#' @importFrom ggplot2  labs
#' @importFrom ggplot2  theme_void
#' @importFrom ggplot2  theme
#' @importFrom ggplot2  geom_bar
#' @importFrom ggplot2  ylab
#' @importFrom ggplot2  xlab
#' @importFrom ggplot2  guides
#' @importFrom ggplot2  labs
#' @importFrom ggplot2  scale_y_continuous
#' @importFrom ggplot2  theme_minimal
#' @importFrom ggplot2  theme
#' @importFrom ggplot2  coord_flip
#' @importFrom ggplot2  aes
#' @importFrom ggplot2  element_rect
#' @importFrom ggplot2  vars
#' @importFrom ggplot2  position_stack
#' @importFrom ggplot2  guide_legend
#' @importFrom ggplot2  element_blank
#' @importFrom ggplot2  element_text
#' @importFrom ggplot2  margin
#' @importFrom ggplot2  ggsave
#' @importFrom dplyr count
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom magrittr "%>%"
#' @importFrom dplyr na_if
#'
#'
#' @export
#'
#' @examples
#'
#' bar_plot <- nba_plot(NBA_example_thr_data,
#'`OVERALL types`,
#'2:5,
#'CHRT = "bar",
#'NUM = TRUE,
#'LAB = "Percentage of ecosystem types",
#'SAVE = NULL)
#'
#'bar_plot
#'
#'donut_plot <- nba_plot(NBA_example_pro_data,
#'`OVERALL types`,
#'2:5,
#'CHRT = "donut",
#'NUM = TRUE,
#'LAB = "Percentage of ecosystem types",
#'GRP = FALSE,
#'SAVE = NULL)
#'
#'donut_plot
#'

nba_plot <- function(DF, GROUPS, COLS, CHRT = c("bar", "donut"), NUM = FALSE, LAB, GRP = FALSE, SAVE = NULL,
                            SCALE_TEXT = 1){


    if(CHRT == "donut"){

      if(GRP == FALSE) {

        ## Prepare the data frame by arranging and setting colors
        dat <- DF %>%
          tidyr::pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
          dplyr::summarise(COUNT = sum(COUNT, na.rm = T), .by = FILL)  %>%
          dplyr::mutate(FILL = factor(FILL, levels = nbaR::NBA_categories))%>%
          dplyr::mutate(ymax = cumsum(COUNT)) %>%
          dplyr::mutate(ymin = ymax -COUNT) %>%
          dplyr::ungroup()

        if(NUM == FALSE){


          plot <- ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
            ggplot2::geom_rect() +
            #ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 5) +  ## Add this line to include count values
            ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
            ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
            ggplot2::scale_fill_manual(values = nbaR::NBA_colours) +
            #ggplot2::ggtitle(LAB)+
            ggplot2::labs(fill = "", title = LAB) + #this is the legend label
            ggplot2::theme_void() + ## removes the lines around chart and grey background
            ggplot2::theme(
              panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
              plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
              title = ggplot2::element_text(size = 10* SCALE_TEXT),
              strip.text = ggplot2::element_blank()
            )

        }

        #if NUm is true
        else{

          plot <- ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
            ggplot2::geom_rect() +
            ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 5* SCALE_TEXT) +  ## Add this line to include count values
            ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
            ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
            ggplot2::scale_fill_manual(values = nbaR::NBA_colours) +
            ggplot2::labs(fill = "", title = LAB)+
            #ggplot2::xlab(LAB)+
            ggplot2::theme_void() + ## removes the lines around chart and grey background
            ggplot2::theme(
              panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
              plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
              title = ggplot2::element_text(size = 10* SCALE_TEXT),
              strip.text = ggplot2::element_blank()
            )

        }
      }

      #if grp is true
      else {

        ## Prepare the data frame by arranging and setting colors
        dat <- DF %>%
          tidyr::pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
          dplyr::mutate(TOT = sum(COUNT, na.rm = T), .by = {{GROUPS}} )%>%
          dplyr::mutate(PERCENTAGE = (COUNT/TOT)*100)%>%
          dplyr::mutate(FILL = factor(FILL, levels = nbaR::NBA_categories))%>%
          dplyr::mutate(ymax = cumsum(PERCENTAGE), .by = {{GROUPS}}) %>%
          dplyr::mutate(ymin = ymax -PERCENTAGE)

        if(NUM == FALSE){


          plot <-ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
            ggplot2::geom_rect() +
            ggplot2::facet_wrap(vars({{GROUPS}}))+
            #ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 3) +  ## Add this line to include count values
            ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
            ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
            ggplot2::scale_fill_manual(values = nbaR::NBA_colours) +
            ggplot2::labs(fill = "", title = LAB)+
            #ggplot2::xlab(LAB)+
            ggplot2::theme_void() + ## removes the lines around chart and grey background
            ggplot2::theme(
              panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
              plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
              title = ggplot2::element_text(size = 10* SCALE_TEXT),
              strip.text = ggplot2::element_blank()
            )
        }

        #if Num is true
        else{

          plot <-ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
            ggplot2::geom_rect() +
            ggplot2::facet_wrap(vars({{GROUPS}}))+
            ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 3* SCALE_TEXT) +  ## Add this line to include count values
            ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
            ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
            ggplot2::scale_fill_manual(values = nbaR::NBA_colours) +
            ggplot2::labs(fill = "", title = LAB)+
            #ggplot2::xlab(LAB)+
            ggplot2::theme_void() + ## removes the lines around chart and grey background
            ggplot2::theme(
              panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
              plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
              title = ggplot2::element_text(size = 10* SCALE_TEXT),
              strip.text = ggplot2::element_blank()
            )

        }

      }
    }


    ## if chart is bar:
    else {

      ord <-   DF %>%
        dplyr::pull({{GROUPS}})

      dat <- DF %>%
        tidyr::pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
        dplyr::mutate(TOT = sum(COUNT, na.rm = T), .by = {{GROUPS}} )%>%
        dplyr::mutate(PERCENTAGE = (COUNT/TOT)*100)%>%
        dplyr::mutate(dplyr::across(COUNT, ~ dplyr::na_if(., 0))) %>%
        dplyr::mutate(FILL = factor(FILL, levels = nbaR::NBA_categories))

      if(NUM == TRUE){



        plot <-ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{GROUPS}}, level = ord), fill = FILL)) +
          ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
          ggplot2::geom_text(aes(label = COUNT),
                             position = position_stack(vjust = 0.5, reverse = TRUE), # add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
                             size = 3* SCALE_TEXT,
                             color = "black",
                             show.legend = FALSE) + # adjust size of labels with no legend being shown
          ggplot2::scale_fill_manual(values = nbaR::NBA_colours)+  # order the colours of the bars in the reversed order
          ggplot2::ylab({{LAB}}) +
          ggplot2::xlab("") + ## remove the heading for the y-axis
          ggplot2::guides(fill = guide_legend(reverse = F, nrow = 1, size = 0.5* SCALE_TEXT)) +  # display legend in 2 rows
          ggplot2::labs(fill = "") + ## change the legend title here
          ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = c(0, 50, 100)) + # set the y-axis to show 0%, 50%, and 100%
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "bottom", # position legend to the bottom
                         panel.grid.minor = element_blank(), # remove grid lines on every second x-axis value
                         axis.line = element_blank(), # remove all x-axis grid lines
                         panel.grid.major.y = element_blank(), # remove the horizontal lines only on 1st , 3rd and 5 ... x-axis
                         legend.text = element_text(size = 8* SCALE_TEXT), # change legend text size
                        # plot.background = element_rect(color = "black", fill = NA),  # add border around the entire plot include legend
                        plot.margin=grid::unit(c(4,4,4,4), "pt"),
                         axis.text.x = element_text(size = 10 * SCALE_TEXT),
                         axis.text.y = element_text(size = 8 * SCALE_TEXT),
                         axis.title.x = element_text(size = 10 * SCALE_TEXT),
                         axis.title.y = element_text(size = 10 * SCALE_TEXT),
                         legend.key.size = unit(1 * SCALE_TEXT, "lines"),
                         legend.box.margin = margin()) +   # extend plot margins to accommodate the border)
          ggplot2::coord_flip()  # flip the orientation of the chart
      }

      ## if NUM == FALSE
      else {

        plot <- ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{GROUPS}}, level = ord), fill = FILL)) +
          ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
          ggplot2::scale_fill_manual(values = nbaR::NBA_colours) +  # order the colours of the bars in the reversed order
          ggplot2::ylab({{LAB}}) +
          ggplot2::xlab("") + ## remove the heading for the y-axis
          ggplot2::guides(fill = guide_legend(reverse = F, nrow = 1, size = 0.5* SCALE_TEXT)) +  # display legend in 2 rows
          ggplot2::labs(fill = "") + ## change the legend title here
          ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = c(0, 50, 100)) + # set the y-axis to show 0%, 50%, and 100%
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "bottom", # position legend to the bottom
                         panel.grid.minor = element_blank(), # remove grid lines on every second x-axis value
                         axis.line = element_blank(), # remove all x-axis grid lines
                         panel.grid.major.y = element_blank(), # remove the horizontal lines only on 1st , 3rd and 5 ... x-axis
                         legend.text = element_text(size = 8* SCALE_TEXT), # change legend text size
                        # plot.background = element_rect(color = "black", fill = NA),  # add border around the entire plot include legend
                        plot.margin=grid::unit(c(4,4,4,4), "pt"),
                         axis.text.x = element_text(size = 10 * SCALE_TEXT),
                         axis.text.y = element_text(size = 8 * SCALE_TEXT),
                         axis.title.x = element_text(size = 10 * SCALE_TEXT),
                         axis.title.y = element_text(size = 10 * SCALE_TEXT),
                         legend.key.size = unit(1 * SCALE_TEXT, "lines"),
                         legend.box.margin = margin()) +   # extend plot margins to accommodate the border)
          ggplot2::coord_flip()  # flip the orientation of the chart


      }
    }

    if (!is.null(SAVE)) {

      plot_save <- plot +
        theme(legend.justification='right')

      ggsave(paste0("outputs/", SAVE, ".jpeg"),
             plot = plot_save,
             height = 8, width = 6, units = 'cm', dpi = 300, create.dir = TRUE)

    }




    plot

  }


#######################################################################################################
###
#' NBA index plot function
#'
#' Generates the Red List Index (RLI) plots for species and ecosystems, or the Ecosystem  Protection Level Index (EPLI).
#' Supports multiple index types: RLIs (species), RLIe (ecosystems), and EPLI (protection level).
#'
#' @param TYPE Type of index to plot. Options: `"RLIs"`, `"RLIe"`, or `"EPLI"`.
#' @param DF  Input dataset containing index data (depending on TYPE).
#' @param YEAR Column name for year variable.
#' @param RLI Column name for RLI (for TYPE = "RLIs").
#' @param MIN Column name for lower bound (for TYPE = "RLIs").
#' @param MAX Column name for upper bound (for TYPE = "RLIs").
#' @param ASSESSMENT_YEAR Year to highlight assessment points (optional).
#' @param GROUP Column name for taxon or biome grouping (for TYPE = "RLIs").
#' @param RLIE Column name for ecosystem RLI (for TYPE = "RLIe").
#' @param BIOME Column name for biome (for TYPE = "RLIe" or "EPLI").
#' @param EPLI_list List of EPLI datasets keyed by year (for TYPE = "EPLI").
#' @param PALETTE Choose color palette `"taxon"` or `"biome"`.
#' @param AGGREGATE Whether to include an aggregate line. Default = TRUE.
#' @param SAVE Optional filename to save the plot PNG in `outputs/`.
#'
#'
#' @return Returns a multiple line plot with points
#'
#'
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr ungroup
#' @importFrom dplyr pull
#' @importFrom dplyr distinct
#' @importFrom dplyr rename
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_linetype_manual
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 ggsave
#' @importFrom RColorBrewer brewer.pal
#' @importFrom purrr map2_dfr
#' @importFrom rlang sym
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom rlang as_name
#' @importFrom rlang :=
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @examples
#'
#'
#' # Example 1: RLIs grouped by Taxon
#' nba_index_plot(
#'   TYPE = "RLIs",
#'   DF = NBA_example_rlis_data,
#'   YEAR = Year,
#'   RLI = RLI,
#'   ASSESSMENT_YEAR = Assessment_Year,
#'   GROUP = Taxon,
#'   PALETTE = "taxon",
#'   AGGREGATE = TRUE,
#'   SAVE = NULL
#' )
#'
#' # Example 2: RLIe
#' nba_index_plot(
#'   TYPE = "RLIe",
#'   DF = NBA_example_rlie_data,
#'   YEAR = "Year",
#'   RLIE = "RLIE",
#'   BIOME = "Biome",
#'   PALETTE = "biome",
#'   SAVE = NULL
#' )
#'
#' # Example 3: EPLI
#' nba_index_plot(
#'   TYPE = "EPLI",
#'   EPLI_list = list(
#'     "2018" = NBA_example_epli2018_data,
#'     "2024" = NBA_example_epli2024_data
#'   ),
#'   PALETTE = "biome",
#'   SAVE = NULL
#' )
#'
#'
nba_index_plot <- function(
    TYPE = c("RLIs", "RLIe", "EPLI"),
    DF = NULL,
    YEAR = NULL,
    RLI = NULL, MIN = NULL, MAX = NULL, ASSESSMENT_YEAR = NULL, GROUP = NULL,
    RLIE = NULL, BIOME = NULL,
    EPLI_list = NULL,
    PALETTE = "taxon",
    AGGREGATE = TRUE,
    SAVE = NULL
) {

  TYPE <- match.arg(TYPE)

  # Biome Color Map
  BIOME_COLORS <- c(
    "Albany Thicket" = "#A6CEE3",
    "Azonal Vegetation" = "#1F78B4",
    "Desert" = "#B2DF8A",
    "Forests" = "#33A02C",
    "Fynbos" = "#FB9A99",
    "Grassland" = "#E31A1C",
    "Indian Ocean Coastal Belt" = "#FDBF6F",
    "Nama-Karoo" = "#FF7F00",
    "Savanna" = "#CAB2D6",
    "Succulent Karoo" = "#6A3D9A"
  )

  # RLIs
  if (TYPE == "RLIs") {

    YEAR <- enquo(YEAR)
    RLI <- enquo(RLI)
    MIN <- enquo(MIN)
    MAX <- enquo(MAX)
    GROUP <- enquo(GROUP)
    ASSESSMENT_YEAR <- enquo(ASSESSMENT_YEAR)

    BIOME_MODE <- "Biome" %in% names(DF)

    TAXON_COLORS <- c(
      "Amphibians"="#17becf", "Anostraca"="#a6cee3", "Birds"="#ffcc66",
      "Butterflies"="#1f78b4", "Coral"="#De1234", "Dragonflies & Damselflies"="#7570b3",
      "Freshwater Crabs"="#66cc99", "Freshwater Fishes"="#e7298a", "Mammals"="#B15928",
      "Plants*"="#66a61e", "Reptiles"="#fb9a99", "Spiders"="yellow2",
      "Sparids"="#666666", "Sharks (incl. Rays & Chimaeras)"="#6a3d9a"
    )

    COLS <- switch(
      PALETTE,
      "taxon" = TAXON_COLORS,
      "biome" = BIOME_COLORS %||% TAXON_COLORS,
      stop("Invalid palette. Choose 'taxon' or 'biome'")
    )

    # Aggregate line
    if (BIOME_MODE) {
      if (AGGREGATE && "Aggregate" %in% DF$Biome) {
        AGGREGATE_DF <- DF %>%
          filter(Biome == "Aggregate") %>%
          group_by(!!YEAR) %>%
          summarise(RLI = mean(!!RLI, na.rm = TRUE), .groups = "drop") %>%
          mutate(LineType = factor("Aggregate", levels = "Aggregate"))
      } else {
        AGGREGATE_DF <- NULL
      }

      SUMMARY_DF <- DF %>%
        filter(Biome != "Aggregate") %>%
        group_by(!!GROUP, !!YEAR) %>%
        summarise(
          RLI = mean(!!RLI, na.rm = TRUE),
          MIN = mean(!!MIN, na.rm = TRUE),
          MAX = mean(!!MAX, na.rm = TRUE),
          .groups = "drop"
        )

    } else {
      if (AGGREGATE && "Aggregate" %in% DF$Taxon) {
        AGGREGATE_DF <- DF %>%
          filter(Taxon == "Aggregate") %>%
          group_by(!!YEAR) %>%
          summarise(RLI = mean(!!RLI, na.rm = TRUE), .groups = "drop") %>%
          mutate(LineType = factor("Aggregate", levels = "Aggregate"))
      } else {
        AGGREGATE_DF <- NULL
      }

      SUMMARY_DF <- DF %>%
        filter(Taxon != "Aggregate") %>%
        group_by(!!GROUP, !!YEAR) %>%
        summarise(
          RLI = mean(!!RLI, na.rm = TRUE),
          MIN = mean(!!MIN, na.rm = TRUE),
          MAX = mean(!!MAX, na.rm = TRUE),
          .groups = "drop"
        )
    }

    ASSESSMENT_POINTS <- NULL
    if (!is.null(rlang::as_name(ASSESSMENT_YEAR))) {
      ASSESSMENT_POINTS <- DF %>%
        filter(!!YEAR == !!ASSESSMENT_YEAR) %>%
        distinct(!!GROUP, !!YEAR, .keep_all = TRUE)
    }

    y_min <- ifelse(PALETTE == "biome", 0.8, 0.7)
    y_max <- 1
    padding <- 0.02 * (y_max - y_min)
    y_min <- y_min - padding
    y_max <- y_max + padding

    P <- ggplot() +
      geom_line(data = SUMMARY_DF, aes(x = !!YEAR, y = RLI, group = interaction(!!GROUP), color = !!GROUP), linewidth = 0.4)

    if (!is.null(ASSESSMENT_POINTS)) {
      P <- P + geom_point(
        data = ASSESSMENT_POINTS,
        aes(x = !!YEAR, y = !!RLI, group = !!GROUP, color = !!GROUP),
        size = 2, shape = 20, fill = NA, stroke = 1.2, inherit.aes = FALSE
      )
    }

    if (!is.null(AGGREGATE_DF)) {
      P <- P +
        geom_line(data = AGGREGATE_DF, aes(x = !!YEAR, y = RLI, linetype = LineType), color = "black", linewidth = 0.4) +
        scale_linetype_manual(values = c("Aggregate" = "dashed"), breaks = "Aggregate", labels = "Aggregate", name = NULL)
    }

    P <- P +
      scale_color_manual(values = COLS) +
      coord_cartesian(ylim = c(y_min, y_max)) +
      labs(x = "Year", y = "Red List Index of Species", color = ifelse(PALETTE == "taxon", "Taxon", "Biome")) +
      theme_classic() +
      theme(
        axis.title.x = element_text(size = 10, margin = margin(t = 12)),
        axis.title.y = element_text(size = 10, margin = margin(r = 12)),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.spacing.y = unit(0.5, "cm"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "right"
      ) +
      guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2))

    if (!is.null(SAVE)) {
      ggsave(filename = paste0("outputs/", SAVE, ".png"), plot = P, height = 15, width = 20, units = "cm", dpi = 300)
    }

    return(P)

  }

  # RLIe
  else if (TYPE == "RLIe") {

    YEAR_sym <- rlang::sym(YEAR)
    RLIE_sym <- rlang::sym(RLIE)
    BIOME_sym <- rlang::sym(BIOME)

    # ---- Data Prep ----
    biomes <- DF %>%
      filter(!is.na(!!BIOME_sym)) %>%
      pull(!!BIOME_sym) %>%
      factor() %>%
      droplevels() %>%
      levels()

    biome_data   <- DF %>% filter(!is.na(!!BIOME_sym))
    overall_data <- DF %>% filter(is.na(!!BIOME_sym))

    # Color Palette
    biome_colors_used <- BIOME_COLORS[names(BIOME_COLORS) %in% biomes]

    # Plot
    P <- ggplot() +

      # Biomes
      geom_line(data = biome_data, aes(x = !!YEAR_sym, y = !!RLIE_sym, color = !!BIOME_sym), size = 1.0) +
      geom_point(data = biome_data, aes(x = !!YEAR_sym, y = !!RLIE_sym, color = !!BIOME_sym), size = 4)

    # Aggregate line if enabled
    if (AGGREGATE & nrow(overall_data) > 0) {
      P <- P +
        geom_line(data = overall_data, aes(x = !!YEAR_sym, y = !!RLIE_sym, linetype = "Aggregate", shape = "Aggregate"),
                  color = "black", size = 1.2) +
        geom_point(data = overall_data, aes(x = !!YEAR_sym, y = !!RLIE_sym, shape = "Aggregate"),
                   color = "black", size = 5.5) +
        scale_linetype_manual(name = "", values = c("Aggregate" = "dotted"), guide = guide_legend(order = 2)) +
        scale_shape_manual(name = "", values = c("Aggregate" = 20), guide = guide_legend(order = 2))
    }

    P <- P +
      scale_color_manual(name = "Biome", values = biome_colors_used) +
      scale_x_continuous(breaks = unique(DF[[rlang::as_name(YEAR_sym)]])) +
      scale_y_continuous(limits = c(0.4, 1), breaks = seq(0.5, 1, 0.1)) +
      labs(x = "Year", y = "Red List Index of ecosystems") +
      theme_classic() +
      theme(
        legend.position    = "right",
        legend.box         = "vertical",
        legend.text        = element_text(size = 12),
        legend.title       = element_text(size = 14),
        legend.key.size    = unit(0.6, "cm"),
        legend.key.height  = unit(0.8, "cm"),
        legend.key.width   = unit(1.0, "cm"),
        legend.spacing.x   = unit(0.5, "cm"),
        legend.box.spacing = unit(1.0, "cm"),
        axis.title.x       = element_text(size = 14, margin = margin(t = 25)),
        axis.title.y       = element_text(size = 14, margin = margin(r = 25)),
        axis.text          = element_text(size = 12)
      )

    if (!is.null(SAVE)) {
      ggsave(filename = paste0("outputs/", SAVE, ".png"), plot = P, height = 15, width = 20, units = "cm", dpi = 300)
    }

    return(P)
  }


  # EPLI
  else if (TYPE == "EPLI") {

    if (is.null(EPLI_list) || !is.list(EPLI_list)) {
      stop("Please provide a list of EPLI datasets (EPLI_list = list('2018' = df1, '2024' = df2, ...))")
    }

    EPLI_ALL <- purrr::map2_dfr(EPLI_list, names(EPLI_list) %||% seq_along(EPLI_list),
                                ~ mutate(.x, Year = as.numeric(.y))) %>%
      rename(Biome = T_BIOME) %>%
      filter(!is.na(Biome))

    BIOME_DATA <- EPLI_ALL %>% filter(Biome != "Total")

    AGGREGATE_DF <- NULL
    if (AGGREGATE) {
      AGGREGATE_DF <- EPLI_ALL %>% filter(Biome == "Total") %>%
        mutate(LineType = "Aggregate")
    }

    # Color palette
    BIOME_COLORS_USED <- setNames(RColorBrewer::brewer.pal(length(unique(BIOME_DATA$Biome)), "Paired"), unique(BIOME_DATA$Biome))

    # Plot
    P <- ggplot() +
      geom_line(data = BIOME_DATA, aes(x = Year, y = EPLI, color = Biome), size = 1) +
      geom_point(data = BIOME_DATA, aes(x = Year, y = EPLI, color = Biome), size = 4)

    if (!is.null(AGGREGATE_DF)) {
      P <- P +
        geom_line(data = AGGREGATE_DF, aes(x = Year, y = EPLI, linetype = LineType), color = "black", size = 1.2) +
        geom_point(data = AGGREGATE_DF, aes(x = Year, y = EPLI, shape = LineType), color = "black", size = 5.5) +
        scale_linetype_manual(values = c("Aggregate" = "dotted")) +
        scale_shape_manual(values = c("Aggregate" = 20))
    }

    P <- P +
      scale_color_manual(values = BIOME_COLORS_USED) +
      labs(x = "Year", y = "Ecosystem Protection Level Index") +
      theme_classic() +
      theme(legend.position    = "right",
            legend.box         = "vertical",
            legend.text        = element_text(size = 10),
            legend.title       = element_text(size = 12),
            legend.key.size    = unit(0.6, "cm"),
            legend.key.height  = unit(0.8, "cm"),
            legend.key.width   = unit(1.0, "cm"),
            legend.spacing.x   = unit(0.5, "cm"),
            legend.box.spacing = unit(1.0, "cm"),
            axis.title.x       = element_text(size = 12, margin = margin(t = 15)),
            axis.title.y       = element_text(size = 12, margin = margin(r = 15)),
            axis.text          = element_text(size = 12))

    if (!is.null(SAVE)) {
      ggsave(filename = paste0("outputs/", SAVE, ".png"), plot = P, height = 15, width = 20, units = "cm", dpi = 300)
    }

    return(P)

  }

}

#######################################################################
#' NBA multi plot function
#'
#' A function to plot multiple plots created by the NBA_plot function in one grid.
#'
#' The function expects that the groups are in a column, with one
#' group per row, and the protection level, threat status, or condition
#' categories are the headings of each column as well as a column to identify which
#' rows should be plotted together (i.e which rows represent extent and which represent
#' number). Please look at the example data NBA_example_comb_data to see the correct structure for the data.
#' Please note that whether it will be used to make a bar or donut plot is
#' irrelevant.
#'
#' The name of the groups column is irrelevant, but the categories must be
#' spelled correctly (there is a list of the standard spellings/ cases
#' of NBA categories named nbaR::NBA_categories in this package,
#' which can be accessed for reference).
#'
#' The nameing of the plots is automatically alphabetical and lower case.
#' The ordering of the plots is also automatic.
#' The legend is assumed to be the same and added to the bottom
#' of the plots.
#'
#'
#'
#' @param DF The data frame that contains the information
#' @param GROUPS The categorized variables (ecosystem functional group, taxa, etc)
#' @param METRIC_COL The column that differentiates which rows belong to which plots
#' @param METRICS a list of the identifiers for each plot
#' @param COLS The categories to describe the variables (protection level, threat status, condition, etc). You can use any tidyselect method to select these columns e.g. 2:4/ Endangered:Vulnerable/ c(Well Protected,Moderately Protected,Poorly Protected,Not Protected) etc
#' @param CHRT A choice of either "bar" or "donut" plot
#' @param NUM A choice to show numbers in the plot, False to show no numbers
#' @param LAB The x axis label of the plot
#' @param GRP A choice of whether or not to plot the donut graphs by group, TRUE will plot a donut plot for each group.
#' @param SAVE The name of the output file that will be saved to the output folder. If you do not have an outputs folder you will be prompted to make one.
#'@param SCALE_TEXT scale the sizes of the plot text to fit your intended output. currently set at 1 as default. If you want to save it to 8 by 6 cm, set it to 0.5.
#'
#' @return Returns a plot
#'
#'
#' @importFrom ggpubr ggarrange
#' @importFrom ggplot2  ggsave
#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @examples
#' bar_plot_comb <- nba_plot_comb(
#'NBA_example_comb_data,
#'GROUPS= `OVERALL types`,
#'METRIC_COL = metric,
#'METRICS = c("protection_level", "threat_status"),
#'COLS = 3:10,
#'CHRT = "bar",
#'NUM = FALSE,
#'LAB = "Percentage of ecosystem",
#'SAVE=NULL)
#'
#'bar_plot_comb
#'#
#'
#'

nba_plot_comb <- function(DF,
                          GROUPS,
                          METRIC_COL,
                          METRICS,
                          COLS,
                          CHRT = c("bar", "donut"),
                          NUM = FALSE,
                          LAB,
                          GRP = FALSE,
                          SAVE = NULL,
                          SCALE_TEXT = 0.75) {


  plot_list2 <- list()
  i <- 1

  for (m in METRICS) {

    if(m == "count"){
       figure <- nba_plot(DF = DF %>%
                         filter({{METRIC_COL}}== m),
                       GROUPS= {{GROUPS}},
                       COLS = COLS,
                       CHRT = CHRT,
                       NUM = T,
                       GRP = GRP,
                       LAB = paste(LAB, m),
                       SAVE=NULL,
                       SCALE_TEXT = SCALE_TEXT)
    } else

      if(m == "extent"){

      figure <- nba_plot(DF = DF %>%
                           filter({{METRIC_COL}}== m),
                         GROUPS= {{GROUPS}},
                         COLS = COLS,
                         CHRT = CHRT,
                         NUM = F,
                         GRP = GRP,
                         LAB = paste(LAB, m),
                         SAVE=NULL,
                         SCALE_TEXT = SCALE_TEXT)

      } else {

        figure <- nba_plot(DF = DF %>%
                             filter({{METRIC_COL}}== m),
                           GROUPS= {{GROUPS}},
                           COLS = COLS,
                           CHRT = CHRT,
                           NUM = NUM,
                           GRP = GRP,
                           LAB = paste(LAB, m),
                           SAVE=NULL,
                           SCALE_TEXT = SCALE_TEXT)

    }

    plot_list2[[i]] <- figure
    i <- i + 1

  }

  plot <- ggpubr::ggarrange(plotlist =  plot_list2,
                    labels = c("AUTO"),
                    common.legend = T,
                    legend = "bottom")

  if (!is.null(SAVE)) {

    ggplot2::ggsave(paste0("outputs/", SAVE, ".png"), height = 10, width = 16, units = 'cm', bg="white" , dpi = 300, create.dir = TRUE)

  }



  plot
}

#######################################################################################################
###
#' NBA theme for a ggplot object
#'
#'This function will create a theme for all ggplot graphs to align them to the
#'NBA "look". All NBA functions already use this theme.
#'
#'
#' @return Returns a ggplot object with the NBA_theme styling
#'
#'
#' @importFrom ggplot2  ggplot
#' @importFrom ggplot2  theme
#' @importFrom ggplot2  element_rect
#' @importFrom ggplot2  element_blank
#' @importFrom ggplot2  element_text
#' @importFrom ggplot2  element_line
#' @importFrom ggplot2  margin
#'
#'
#' @export
#'
#' @examples
#'#library(ggplot2)
#'
#'#gg_plot <- ggplot(NBA_example_pro_data, aes(x = `OVERALL types`, y = `Well Protected`))+
#'#  ggplot2::geom_point()+
#'#  nba_plot_theme()
#'
#'#gg_plot
#'

nba_plot_theme <- function() {
  ggplot2::theme(
    # add border 1)
    panel.border = ggplot2::element_rect(colour = "grey", fill = NA, linetype = 2),
    # color background 2)
    panel.background = ggplot2::element_blank(),
    # modify grid 3)
    panel.grid.minor = ggplot2::element_blank(), # remove grid lines on every second x-axis value
    panel.grid.major.y = ggplot2::element_blank(), # remove the horizontal lines only on 1st , 3rd and 5 ... x-axis
    # modify text, axis and colour
    axis.text = ggplot2::element_text(colour = "black", face = "italic", family = "Times New Roman"),
    axis.title = ggplot2::element_text(colour = "black", face = "bold",family = "Times New Roman"),
    axis.ticks = ggplot2::element_line(colour = "black"),
    axis.line = ggplot2::element_blank(), # remove all x-axis grid lines
    # legend at the bottom 6)
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 8), # change legend text size
    #plot margin
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )
}


#######################################################################################################
###
#' Bubble plot
#'
#'This function will create bubble plot intended for creation of the
#'percentage of taxa of ecological concern impacted by various pressures
#'
#'Please use the example dataset NBA_bubble_plot_example_data to see how
#'the data should be formatted.
#'
#' @param DF The data frame that contains the information
#' @param GROUP The grouping variables (taxa group, etc)
#' @param CAT The overall pressure category
#' @param SUB_CAT The sub pressure category
#' @param VALUE The percentage of taxa of ecological concern impacted by the pressure
#' @param SAVE The name of the output file that will be saved to the output folder. If you do not have an outputs folder you will be prompted to make one.
#'
#' @return Returns a bubble plot
#'
#'
#' @importFrom ggplot2  ggplot
#' @importFrom ggplot2  theme
#' @importFrom ggplot2  element_rect
#' @importFrom ggplot2  element_blank
#' @importFrom ggplot2  element_text
#' @importFrom ggplot2  element_line
#' @importFrom ggplot2  aes
#' @importFrom ggplot2  geom_point
#' @importFrom ggplot2  geom_text
#' @importFrom ggplot2  ggsave
#' @importFrom ggplot2  labeller
#' @importFrom ggplot2  label_wrap_gen
#' @importFrom ggplot2  scale_size
#' @importFrom ggplot2  scale_x_discrete
#' @importFrom ggplot2  scale_fill_brewer
#' @importFrom ggplot2  scale_colour_brewer
#' @importFrom ggplot2  ylab
#' @importFrom ggplot2  xlab
#' @importFrom ggplot2  unit
#' @importFrom ggplot2  guide_axis
#' @importFrom ggh4x facet_grid2
#' @importFrom ggh4x strip_themed
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#'
#'
#' @export
#'
#' @examples
#' bubble_plot <- nba_plot_bubble(DF = NBA_example_bubble_data,
#'                         GROUP = taxon_group,
#'                        CAT = pressure,
#'                        SUB_CAT = sub_pressure,
#'                        VALUE = perc_concern_under_press,
#'                         SAVE = NULL)
#'
#' bubble_plot
#'


nba_plot_bubble <- function(DF, GROUP, CAT, SUB_CAT, VALUE, SAVE = NULL){




  # Create a named color palette for pressures
  cat <- DF %>%
    dplyr::select({{CAT}}) %>%
    unique() %>%
    as.data.frame()
  cat <- cat[,1]

  # Subset nbaR::NBA_colours using names that match the values in pressures
  subset_colours <- nbaR::NBA_colours[match(cat, names(nbaR::NBA_colours))]

  # Ensure pressure is a factor with correct levels
  DF  <- DF %>%
    mutate(pressure = factor(pressure, levels = names(subset_colours)))



  # Create strip background and text style lists
  strip_bg <- lapply(subset_colours, function(col) element_rect(fill = col, colour = col))
  strip_text <- lapply(subset_colours, function(col) element_text(colour = "white"))  # or custom color


  # Build strip object
  my_strips <- ggh4x::strip_themed(
    background_y = strip_bg,
    text_y = strip_text
  )


  p <- DF %>%
    ggplot2::ggplot(ggplot2::aes({{GROUP}}, {{SUB_CAT}}, size = {{VALUE}},
                                 fill = {{CAT}}, colour = {{CAT}})) +
    ggplot2::geom_point(shape = 21) +
    ggplot2::geom_text(ggplot2::aes(label = {{VALUE}}),
                       parse = TRUE,
                       size = 2,
                       colour = "white") +
    ggh4x::facet_grid2(
      pressure ~ ., scales = "free", space = "free",
      labeller = ggplot2::labeller(pressure = ggplot2::label_wrap_gen(width = 20)),
      strip = my_strips
    ) +
    ggplot2::scale_size(range = c(3, 15)) +
    ggplot2::scale_x_discrete(position = "top", guide = ggplot2::guide_axis(n.dodge = 2)) +
    ggplot2::scale_fill_manual(values = nbaR::NBA_colours) +
    ggplot2::scale_colour_manual(values = nbaR::NBA_colours) +
    ggplot2::ylab("") +
    ggplot2::xlab("") +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0, "cm"),
      strip.text.y = ggplot2::element_text(angle = 0),
      panel.grid.major.y = ggplot2::element_line(colour = "lightgrey"),
      panel.grid.major.x = ggplot2::element_line(colour = "lightgrey"),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_line(size = 0.5, linetype = "solid", colour = "black"),
      axis.line.x = ggplot2::element_line(size = 0.5, linetype = "solid", colour = "black"))



  if(!is.null(SAVE)){

    ggplot2::ggsave(paste0("outputs/", SAVE, ".png"), plot = p, height = 10, width = 16, units = 'cm', dpi = 300, create.dir = TRUE)


  }
  p
}


###################################################################################################
###
#' horizontal bar plot of pressures
#'
#' This function generates a horizontal bar plot showing the percentage of pressures
#' affecting taxa.
#'
#' @param DF A data frame containing at least the columns `Taxon`, `pressure`, and `percentage`.
#' @param TAXON The name of the taxon to filter for.
#' @param SAVE The name of the output file that will be saved to the output folder. If you do not have an outputs folder you will be prompted to make one.
#'
#' @return A horizontal bar plot.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 margin
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom stringr str_wrap
#' @importFrom stats setNames
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @examples
#'
#' birds_press_bar_plot <- nba_pressure_bar_plot(
#'                           DF = NBA_example_press_bar_data,
#'                           TAXON = "Birds")
#'
#' birds_press_bar_plot
#'
nba_pressure_bar_plot <- function(DF, TAXON = NULL, SAVE = NULL) {

  # Filter by TAXON if provided
  if (!is.null(TAXON)) {
    DF <- DF %>% filter(Taxon == TAXON)
  }

  # Filter 0% pressures
  plot_data <- DF %>% filter(percentage > 0)

  if (nrow(plot_data) == 0) stop("No data to plot after filtering by TAXON and percentage > 0.")

  # Subset colours for pressures present
  valid_pressure_colours <- nbaR::NBA_colours[names(nbaR::NBA_colours) %in% plot_data$pressure]

  # --- Fix for ascending order per facet ---
  if (is.null(TAXON)) {
    # Create unique factor per Taxon to order bars ascending within each facet
    plot_data <- plot_data %>%
      group_by(Taxon) %>%
      arrange(percentage) %>%
      mutate(pressure_ordered = factor(paste0(Taxon, "_", pressure), levels = paste0(Taxon, "_", pressure))) %>%
      ungroup()
  } else {
    # Single taxon plot, ascending order
    plot_data <- plot_data %>%
      arrange(percentage) %>%
      mutate(pressure_ordered = factor(pressure, levels = unique(pressure)))
  }

  # Create plot
  p <- ggplot(plot_data, aes(x = pressure_ordered, y = percentage, fill = pressure)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = valid_pressure_colours) +
    scale_x_discrete(labels = ~ stringr::str_wrap(.x, width = 18)) +
    scale_y_continuous(breaks = seq(0, 100, by = 10)) +
    labs(x = "Pressure", y = "Percentage (%)") +
    theme_classic() +
    coord_flip()

  # Facet & theme adjustments
  if (is.null(TAXON)) {
    p <- ggplot(plot_data, aes(x = pressure_ordered, y = percentage, fill = pressure)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = valid_pressure_colours) +
      scale_y_continuous(breaks = seq(0, 100, by = 20)) +
      labs(x = "Pressure", y = "Percentage (%)") +
      theme_classic() +
      coord_flip()
    p <- p +
      facet_wrap(~Taxon, scales = "free_y") +
      theme(
        axis.text.y = element_blank(),        # remove y-axis labels for facets
        axis.text.x = element_text(size = 8, angle = 0, hjust = 1, vjust = 0.5 ),
        axis.title.y = element_text(margin = margin(r = 15), size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 5),
        panel.grid.minor = element_blank(),   # remove minor grid lines
        axis.ticks.x = element_line(color = "black", size = 0.5),
        axis.ticks.y = element_blank()# remove tick marks
      ) +
      guides(fill = guide_legend(ncol = 3))
  } else {
    p <- p +
      theme(
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(margin = margin(r = 15), size = 10),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()
      )
  }

  # Save the plot if SAVE is provided
  if (!is.null(SAVE)) {
    if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)
    ggplot2::ggsave(
      filename = file.path("outputs", paste0(SAVE, ".png")),
      plot = p,
      height = 16,
      width = 20,
      units = "cm",
      dpi = 300
    )
  }

  return(p)
}
##############################################################################
