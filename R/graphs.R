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
              panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
              plot.background = element_rect(fill = "white", color = NA),
              title = element_text(size = 10* SCALE_TEXT),
              strip.text = element_blank(),## set plot background to white
              axis.text.x = element_text(size = 8 * SCALE_TEXT),
              axis.text.y = element_text(size = 8 * SCALE_TEXT),
              axis.title.x = element_text(size = 10 * SCALE_TEXT),
              axis.title.y = element_text(size = 10 * SCALE_TEXT),
              legend.key.size = unit(1 * SCALE_TEXT, "lines"),
              legend.box.margin = margin()
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
              panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
              plot.background = element_rect(fill = "white", color = NA),
              title = element_text(size = 10),
              strip.text = element_blank(),## set plot background to white
              axis.text.x = element_text(size = 8 * SCALE_TEXT),
              axis.text.y = element_text(size = 8 * SCALE_TEXT),
              axis.title.x = element_text(size = 10 * SCALE_TEXT),
              axis.title.y = element_text(size = 10 * SCALE_TEXT),
              legend.key.size = unit(1 * SCALE_TEXT, "lines"),
              legend.box.margin = margin()
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
              panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
              plot.background = element_rect(fill = "white", color = NA),
              title = element_text(size = 10* SCALE_TEXT),
              strip.text = element_blank(), ## set plot background to white
              axis.text.x = element_text(size = 8 * SCALE_TEXT),
              axis.text.y = element_text(size = 8 * SCALE_TEXT),
              axis.title.x = element_text(size = 10 * SCALE_TEXT),
              axis.title.y = element_text(size = 10 * SCALE_TEXT),
              legend.key.size = unit(1 * SCALE_TEXT, "lines"),
              legend.box.margin = margin()
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
              panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
              plot.background = element_rect(fill = "white", color = NA),
              title = element_text(size = 10* SCALE_TEXT),
              strip.text = element_blank(),## set plot background to white
              axis.text.x = element_text(size = 8 * SCALE_TEXT),
              axis.text.y = element_text(size = 8 * SCALE_TEXT),
              axis.title.x = element_text(size = 10 * SCALE_TEXT),
              axis.title.y = element_text(size = 10 * SCALE_TEXT),
              legend.key.size = unit(1 * SCALE_TEXT, "lines"),
              legend.box.margin = margin()
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
                         plot.background = element_rect(color = "black", fill = NA),  # add border around the entire plot include legend
                         plot.margin = margin(10, 10, 10, 10),
                         axis.text.x = element_text(size = 8 * SCALE_TEXT),
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
                         plot.background = element_rect(color = "black", fill = NA),  # add border around the entire plot include legend
                         plot.margin = margin(10, 10, 10, 10),
                         axis.text.x = element_text(size = 8 * SCALE_TEXT),
                         axis.text.y = element_text(size = 8 * SCALE_TEXT),
                         axis.title.x = element_text(size = 10 * SCALE_TEXT),
                         axis.title.y = element_text(size = 10 * SCALE_TEXT),
                         legend.key.size = unit(1 * SCALE_TEXT, "lines"),
                         legend.box.margin = margin()) +   # extend plot margins to accommodate the border)
          ggplot2::coord_flip()  # flip the orientation of the chart


      }
    }

    if (!is.null(SAVE)) {

      plot_save <- plot+
        theme(legend.justification='right')

      ggsave(paste0("outputs/", SAVE, ".jpeg"),
             plot = plot_save,
             height = 8, width = 6, units = 'cm', dpi = 300, create.dir = TRUE)

    }




    plot

  }


#######################################################################################################
###
#' NBA Redlist index plot function
#'
#'This function will create a line graph indicating the Red list index per period/year.
#'Additionally, depending on the graph requirements it further creates a multiple line graph
#'grouping the plot according to species, and creates a buffer parallel to the RLI
#'line using the minimum and maximum values.
#'
#'Note: The graph can be used on other datasets as well, as long as it contains the same
#'data structure as the Red List Index.
#'There is an example dataset (NBA_example_RLI_data), available for users
#'to reference if needed.
#'
#' @param DF The data frame that contains the information on the Red List Index
#' @param YEAR The years
#' @param RLI The Red List Index
#' @param MIN The minimum values
#' @param MAX The maximum values
#' @param GROUP A choice to group the plot, if a column name is supplied will groupd, if left NULL will not group
#' @param summarise_by_year **must be added**
#' @param SAVE The name of the output file that will be saved to the output folder. If you do not have an outputs folder you will be prompted to make one.
#'
#'
#' @return Returns a RLI plot
#'
#'
#' @importFrom ggplot2  ggplot
#' @importFrom ggplot2  geom_line
#' @importFrom ggplot2  geom_ribbon
#' @importFrom ggplot2  theme_classic
#' @importFrom ggplot2  ylim
#' @importFrom ggplot2  scale_fill_manual
#' @importFrom ggplot2  labs
#' @importFrom ggplot2  theme_void
#' @importFrom ggplot2  theme
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom rlang enquo
#' @importFrom rlang quo_is_null
#' @importFrom rlang !!
#'
#'
#' @export
#'
#' @examples
#' #RLI_plot <- nba_plot_RLI(NBA_example_RLI_data,
#' #Years,
#' #RLI,
#' #MIN,
#' #MAX)
#'
#' RLI_plot
#'
#'
nba_plot_RLI <- function(DF, YEAR, RLI, MIN, MAX, GROUP = NULL, summarise_by_year = TRUE, SAVE = NULL) {
  # Convert column names to symbols (quosures)
  YEAR <- enquo(YEAR)
  RLI <- enquo(RLI)
  MIN <- enquo(MIN)
  MAX <- enquo(MAX)
  GROUP <- enquo(GROUP)

  # Summarisation
  if (summarise_by_year && rlang::quo_is_null(GROUP)) {
    DF <- DF %>%
      group_by(!!YEAR) %>%
      summarise(
        !!MIN := mean(!!MIN, na.rm = TRUE),
        !!MAX := mean(!!MAX, na.rm = TRUE),
        !!RLI := mean(!!RLI, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Base plot
  p <- ggplot(DF, aes(x = !!YEAR, y = !!RLI))

  # Add layers based on grouping
  if (rlang::quo_is_null(GROUP)) {
    p <- p +
      geom_line() +
      geom_ribbon(aes(ymin = !!MIN, ymax = !!MAX), alpha = 0.3, colour = NA)
  } else {
    p <- p +
      geom_line(aes(group = !!GROUP, color = !!GROUP), linetype = "dashed") +
      geom_ribbon(aes(ymin = !!MIN, ymax = !!MAX, group = !!GROUP), fill = "grey", alpha = 0.2, colour = NA)
  }

  # Finalize plot
  p <- p + theme_classic() + ylim(0.7, 1)

  # Optional: save to file if SAVE is provided
  if (!is.null(SAVE)) {
    ggsave(filename = paste0("outputs/", SAVE, ".png"),
           plot = p,
           height = 10, width = 16, units = "cm", dpi = 300)
  }

  p
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
                          SAVE = NULL) {


  plot_list2 <- list()
  i <- 1

  for (m in METRICS) {

    figure <- nba_plot(DF = DF %>%
                         filter({{METRIC_COL}}== m),
                       GROUPS= {{GROUPS}},
                       COLS = COLS,
                       CHRT = CHRT,
                       NUM = NUM,
                       GRP = GRP,
                       LAB = paste(LAB, m),
                       SAVE=NULL)

    plot_list2[[i]] <- figure
    i <- i + 1
  }

  plot <- ggpubr::ggarrange(plotlist =  plot_list2,
                    labels = c("auto"),
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
