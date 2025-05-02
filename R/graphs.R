#######################################################################################################
###
#' NBA plot function
#'
#' NBA plots for protection level, threat status, and condition.
#' The function expects that the groups are in a column, with one
#' group per row, and the protection level, threat status, or condition
#' categories are the headings of each column. Please look at the example
#' data NBA_example_bar_plot and NBA_example_donut_plot to see the correct
#' structure for the data. Please note that both of these datasets have the
#' same structure, whether it will be used to make a bar or donut plot is
#' irrelevant.
#' This function will plot the data as either a bar or donut plot
#' depending on what you require. You can also decide if you want the donut
#' plot to be split by ecosystem functional group or not and choose if
#' you want the number of ecosystems to be displayed within the plot.
#'
#' The name of the groups column is irrelevant, but the categories must be
#' spelled correctly (there is a list of the standard spellings/ cases
#' of NBA categories named NBA_categories in this package,
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
#' bar_plot <- NBA_plot(NBA_example_thr_data,
#'`OVERALL types`,
#'2:5,
#'CHRT = "bar",
#'NUM = TRUE,
#'LAB = "Percentage of ecosystem types",
#'SAVE = NULL)
#'
#'bar_plot
#'
#'donut_plot <- NBA_plot(NBA_example_pro_data,
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



NBA_plot <- function(DF, GROUPS, COLS, CHRT = c("bar", "donut"), NUM = FALSE, LAB, GRP = TRUE, SAVE = NULL){

  cols <- c("#6e9fd4",
            "#6e9fd4",
            "#a5c5c7",
            "#81aba7",
            "#88814e",
            "#88812e",
            "#466a31",
            "#80a952",
            "#d5dec3",
            "#a4a3a3",
            "#a4a3a3",
            "black",
            "#e9302c",
            "#f97835",
            "#fff02a",
            "#eeeea3",
            "brown",
            "grey" ,
            "#b1d798",
            "#DB7D15",
            "#B36611",
            "#808080",
            "#F5C592",
            "#0071C0")

  breaks <- c("Natural",
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

  if(CHRT == "donut"){

    if(GRP == FALSE) {

 ## Prepare the data frame by arranging and setting colors
      dat <- DF %>%
        tidyr::pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
        dplyr::summarise(COUNT = sum(COUNT, na.rm = T), .by = FILL)  %>%
        dplyr::mutate(FILL = factor(FILL, levels = breaks))%>%
        dplyr::mutate(ymax = cumsum(COUNT)) %>%
        dplyr::mutate(ymin = ymax -COUNT) %>%
        dplyr::ungroup()

      if(NUM == FALSE){


      plot <- ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
        ggplot2::geom_rect() +
        #ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 5) +  ## Add this line to include count values
        ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
        ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
        ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
        #ggplot2::ggtitle(LAB)+
        ggplot2::labs(fill = "", title = LAB) + #this is the legend label
        ggplot2::theme_void() + ## removes the lines around chart and grey background
        ggplot2::theme(
          panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
          plot.background = element_rect(fill = "white", color = NA),
          title = element_text(size = 10),
          strip.text = element_blank()## set plot background to white
        )

      }

      #if NUm is true
      else{

        plot <- ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
          ggplot2::geom_rect() +
          ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 5) +  ## Add this line to include count values
          ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
          ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
          ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
          ggplot2::labs(fill = "", title = LAB)+
          #ggplot2::xlab(LAB)+
          ggplot2::theme_void() + ## removes the lines around chart and grey background
          ggplot2::theme(
            panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
            plot.background = element_rect(fill = "white", color = NA),
            title = element_text(size = 10),
            strip.text = element_blank()  ## set plot background to white
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
        dplyr::mutate(FILL = factor(FILL, levels = breaks))%>%
        dplyr::mutate(ymax = cumsum(PERCENTAGE), .by = {{GROUPS}}) %>%
        dplyr::mutate(ymin = ymax -PERCENTAGE)

      if(NUM == FALSE){


      plot <-ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
        ggplot2::geom_rect() +
        ggplot2::facet_wrap(vars({{GROUPS}}))+
        #ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 3) +  ## Add this line to include count values
        ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
        ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
        ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
        ggplot2::labs(fill = "", title = LAB)+
        #ggplot2::xlab(LAB)+
        ggplot2::theme_void() + ## removes the lines around chart and grey background
        ggplot2::theme(
          panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
          plot.background = element_rect(fill = "white", color = NA),
          title = element_text(size = 10),
          strip.text = element_blank()  ## set plot background to white
        )
      }

      #if Num is true
      else{

        plot <-ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
          ggplot2::geom_rect() +
          ggplot2::facet_wrap(vars({{GROUPS}}))+
          ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 3) +  ## Add this line to include count values
          ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
          ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
          ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
          ggplot2::labs(fill = "", title = LAB)+
          #ggplot2::xlab(LAB)+
          ggplot2::theme_void() + ## removes the lines around chart and grey background
          ggplot2::theme(
            panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
            plot.background = element_rect(fill = "white", color = NA),
            title = element_text(size = 10),
            strip.text = element_blank()  ## set plot background to white
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
      dplyr::mutate(FILL = factor(FILL, levels = breaks))

    if(NUM == TRUE){



      plot <-ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{GROUPS}}, level = ord), fill = FILL)) +
        ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
        ggplot2::geom_text(aes(label = COUNT),
                           position = position_stack(vjust = 0.5, reverse = TRUE), # add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
                           size = 3,
                           color = "black",
                           show.legend = FALSE) + # adjust size of labels with no legend being shown
        ggplot2::scale_fill_manual(values = cols, breaks = breaks)+  # order the colours of the bars in the reversed order
        ggplot2::ylab({{LAB}}) +
        ggplot2::xlab("") + ## remove the heading for the y-axis
        ggplot2::guides(fill = guide_legend(reverse = F, nrow = 1, size = 0.5)) +  # display legend in 2 rows
        ggplot2::labs(fill = "") + ## change the legend title here
        ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = c(0, 50, 100)) + # set the y-axis to show 0%, 50%, and 100%
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom", # position legend to the bottom
                       panel.grid.minor = element_blank(), # remove grid lines on every second x-axis value
                       axis.line = element_blank(), # remove all x-axis grid lines
                       panel.grid.major.y = element_blank(), # remove the horizontal lines only on 1st , 3rd and 5 ... x-axis
                       legend.text = element_text(size = 8), # change legend text size
                       plot.background = element_rect(color = "black", fill = NA),  # add border around the entire plot include legend
                       plot.margin = margin(10, 10, 10, 10)) +   # extend plot margins to accommodate the border)
        ggplot2::coord_flip()  # flip the orientation of the chart
    }

    ## if NUM == FALSE
    else {

      plot <- ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{GROUPS}}, level = ord), fill = FILL)) +
        ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
        ggplot2::scale_fill_manual(values = cols, breaks = breaks) +  # order the colours of the bars in the reversed order
        ggplot2::ylab({{LAB}}) +
        ggplot2::xlab("") + ## remove the heading for the y-axis
        ggplot2::guides(fill = guide_legend(reverse = F, nrow = 1, size = 0.5)) +  # display legend in 2 rows
        ggplot2::labs(fill = "") + ## change the legend title here
        ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = c(0, 50, 100)) + # set the y-axis to show 0%, 50%, and 100%
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom", # position legend to the bottom
                       panel.grid.minor = element_blank(), # remove grid lines on every second x-axis value
                       axis.line = element_blank(), # remove all x-axis grid lines
                       panel.grid.major.y = element_blank(), # remove the horizontal lines only on 1st , 3rd and 5 ... x-axis
                       legend.text = element_text(size = 8), # change legend text size
                       plot.background = element_rect(color = "black", fill = NA),  # add border around the entire plot include legend
                       plot.margin = margin(10, 10, 10, 10)) +   # extend plot margins to accommodate the border)
        ggplot2::coord_flip()  # flip the orientation of the chart


    }
  }

  if (!is.null(SAVE)) {

    ggsave(paste0("outputs/", SAVE, ".png"), height = 10, width = 16, units = 'cm')

    }




  plot

}


#######################################################################################################
###
#' NBA_plot_RLI
#'
#'This function will create a line graph indicating the Red list index per period/year.
#'Additionally, depending on the graph requirements it further creates a multiple line graph
#'grouping the plot according to species,
#'and creates a buffer parallel to the RLI line using the minimum and maximum values.
#'Note: The graph can be used on other datasets as well, as long as it contains the same
#'data structure as the Red List Index.
#'There is an example dataset (NBA_example_RLI_plot), available for users
#'to reference if needed.
#'
#' @param DF The data frame that contains the information on the Red List Index
#' @param YEAR The years
#' @param RLI The Red List Index
#' @param min The minimum values
#' @param max The maximum values
#' @param GRP A choice to group the plot, TRUE will group if, FALSE will not.
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
#' @importFrom dplyr count
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#'
#'
#' @export
#'
#' @examples
#' RLI_plot <- NBA_plot_RLI(NBA_example_RLI_data,
#' Years,
#' RLI,
#' min,
#' max)
#'
#' RLI_plot
#'
#'
NBA_plot_RLI <- function(DF,YEAR, RLI, min, max, GRP = FALSE){

if(GRP == TRUE){

  ggplot2::ggplot(DF, aes(x = {{YEAR}}, y = {{RLI}}, group = {{GROUP}}, color = {{GROUP}})) +
    ggplot2::geom_line(linetype="dashed") +
    ggplot2::geom_ribbon(aes(ymin = {{min}}, ymax = {{max}}), fill = "grey", alpha = .2, colour = NA)+
    ggplot2::theme_classic()+
    ggplot2::ylim(0.7,1)

}
else {

  ggplot2::ggplot(DF, aes(x = {{YEAR}}, y = {{RLI}})) +
    ggplot2::geom_line(aes(y = {{RLI}})) +
    ggplot2::geom_ribbon(aes(ymin = {{min}}, ymax = {{max}}),alpha = .3, colour = NA)+
    ggplot2::theme_classic()+
    ggplot2::ylim(0.7,1)


}
}

#######################################################################
#' NBA multi plot function
#'
#' NBA plots for multiple plots of protection level, threat status,
#' and condition on the same grid.
#' The function expects that the groups are in a column, with one
#' group per row, and the protection level, threat status, or condition
#' categories are the headings of each column. Please look at the example
#' data NBA_example_bar_plot and NBA_example_donut_plot to see the correct
#' structure for the data. Please note that both of these datasets have the
#' same structure, whether it will be used to make a bar or donut plot is
#' irrelevant.
#' This function will plot the data as either a bar or donut plot
#' depending on what you require. You can also decide if you want the donut
#' plot to be split by ecosystem functional group or not and choose if
#' you want the number of ecosystems to be displayed within the plot.
#'
#' The name of the groups column is irrelevant, but the categories must be
#' spelled correctly (there is a list of the standard spellings/ cases
#' of NBA categories named NBA_categories in this package,
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
#' bar_plot_comb <- NBA_plot_comb(
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

NBA_plot_comb <- function(DF,
                          GROUPS,
                          METRIC_COL,
                          METRICS,
                          COLS,
                          CHRT = c("bar", "donut"),
                          NUM = FALSE,
                          LAB,
                          GRP = TRUE,
                          SAVE = NULL) {


  plot_list2 <- list()
  i <- 1

  for (m in METRICS) {

    figure <- NBA_plot(DF = DF %>%
                         filter({{METRIC_COL}}== m),
                       GROUPS= {{GROUPS}},
                       COLS = COLS,
                       CHRT = CHRT,
                       NUM = NUM,
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

    ggplot2::ggsave(paste0("outputs/", SAVE, ".png"), height = 10, width = 16, units = 'cm', bg="white")

  }



  plot
}

#######################################################################################################
###
#' NBA_plot_theme
#'
#'This function will create a theme for all ggplot graphs to align them to the
#'NBA "look". All NBA functions already use this theme.
#'

#' @return Returns a NBA_theme
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
#'#  NBA_plot_theme()
#'
#'#gg_plot
#'

NBA_plot_theme <- function() {
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








###################################################################################################
