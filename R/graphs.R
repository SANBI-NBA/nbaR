#' Protection level bar plot
#'
#' Horizontal barplots for the protection level of ecosystem function groups (efgs)
#'
#' @param DF The data frame that contains the information on protection level
#' @param X The groups
#' @param Y The protection level percentages
#' @param FILL The protection level categories
#' @param COUNT The frequency counts of the number of ecosystems within each protection level
#'
#' @return Returns a bar graph of protection level
#'
#' @importFrom ggplot2  ggplot
#' @importFrom ggplot2  geom_bar
#' @importFrom ggplot2  geom_text
#' @importFrom ggplot2  scale_fill_manual
#' @importFrom ggplot2  ylab
#' @importFrom ggplot2  xlab
#' @importFrom ggplot2  guides
#' @importFrom ggplot2  labs
#' @importFrom ggplot2  scale_y_continuous
#' @importFrom ggplot2  theme_minimal
#' @importFrom ggplot2  theme
#' @importFrom ggplot2  coord_flip
#'
#'@examples
#'#test <- prot_efg(mydata, ecosystem_functional_grps, percentages, threat_status)
#'
#' @export
#'
#'
prot_efg <-function(DF, X, Y, FILL, COUNT) {


  ggplot2::ggplot(DF, aes(y = {{Y}}, x = {{X}}, fill = {{FILL}})) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) + ## change width of bars
    ggplot2::geom_text(aes(label = {{COUNT}}), position = position_stack(vjust = 0.5), # add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
              size = 3, color = "black", show.legend = FALSE) + # adjust size of labels with no legend being shown
    ggplot2::scale_fill_manual(values = c("#a4a3a3", "#d5dec3", "#80a952", "#466a31")) +  # order the colours of the bars in the reversed order
    ggplot2::ylab("Percentage of ecosystem functional types") +
    ggplot2::xlab("") +
    ggplot2::guides(fill = guide_legend(reverse = TRUE, nrow = 2)) +  # display legend in 2 rows
    ggplot2::labs(fill = "") + # change the legend title
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = c(0, 50, 100)) + # set the y-axis to show 0%, 50%, and 100%
    ggplot2::theme_minimal() + # create a black bounding box around the plot
    ggplot2::theme(legend.position = "bottom", # position legend to the bottom
          panel.grid.minor = element_blank(), # remove grid lines on every second x-axis value
          axis.line = element_blank(), # remove all x-axis grid lines from
          panel.grid.major.y = element_blank(), # include the horizontal grid line on 1st , 3rd and 5 ... x-axis
          legend.text = element_text(size = 8),
          plot.background = element_rect(color = "black", fill = NA),  # add black border around the entire plot
          plot.margin = margin(10, 10, 10, 10)) +   # extend plot margins to accommodate the border)
    ggplot2::coord_flip()
}

#######################################################################################################
###
#' Threat status bar plot
#'
#' Horizontal barplots for the threat status of ecosystem function groups (efgs)
#'
#' @param DF The data frame that contains the information on threat status
#' @param X The groups
#' @param Y The threat status percentages
#' @param FILL The threat status categories
#' @param COUNT The frequency counts of the number of ecosystems within each protection level
#'
#' @return Returns a bar graph of threat status
#'
#'
#' @importFrom ggplot2  ggplot
#' @importFrom ggplot2  geom_bar
#' @importFrom ggplot2  geom_text
#' @importFrom ggplot2  scale_fill_manual
#' @importFrom ggplot2  ylab
#' @importFrom ggplot2  xlab
#' @importFrom ggplot2  guides
#' @importFrom ggplot2  labs
#' @importFrom ggplot2  scale_y_continuous
#' @importFrom ggplot2  theme_minimal
#' @importFrom ggplot2  theme
#' @importFrom ggplot2  coord_flip
#'
#' @examples
#' #test <- thr_efg(mydata, ecosystem_functional_grps, percentages, threat_status)
#' @export
thr_efg <-function(DF, X, Y, FILL, COUNT)

{
  ggplot2::ggplot(DF, aes(y = {{Y}}, x = {{X}}, fill = {{FILL}})) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) + # change width of bars
    ggplot2::geom_text(aes(label = {{COUNT}}), position = position_stack(vjust = 0.5), # add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
              size = 3, color = "black", show.legend = FALSE) + # adjust size of labels with no legend being shown
    ggplot2::scale_fill_manual(values = c("#b1d798", "#eeeea3", "#fff02a", "#f97835", "#e9302c")) +  # order the colours of the bars in the reversed order
    ggplot2::ylab("Percentage of ecosystem functional types") +
    ggplot2::xlab("") + ## remove the heading for the y-axis
    ggplot2::guides(fill = guide_legend(reverse = TRUE, nrow = 2)) +  # display legend in 2 rows
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

#######################################################################################################
###
#' Threat status donut plot
#'
#' Donut plots for the threat status of the ecosystem types
#'
#' @param DF The data frame that contains the information on threat status
#' @param YMIN The groups
#'
#' @return Returns a bar donut plot of threat status
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
#'
#' @export
#'
#' @examples
#' #test <- thr_donut_plot(mydata, ecosystem_functional_grps, percentages, threat_status)
#'
thr_donut_plot_test <-function(DF, FILL)
{

  ### generate a frequency table for the categorical data
  df <- dplyr::count(DF, {{FILL}})

  ### define the order of the protection levels
  ord <- c("Critically Endangered", "Endangered", "Vulnerable", "Near Threatened", "Least Concern")

  ## Prepare the data frame by arranging and setting colors
  df <- df %>%
    dplyr::arrange(factor({{FILL}}, levels = ord))

  df <- df %>%
    dplyr::mutate(ymax = cumsum(n)) %>%
    dplyr::mutate(ymin = ymax -n)

  cols <- c("#e9302c","#f97835","#fff02a","#eeeea3","#b1d798")
  breaks <- c("Critically Endangered", "Endangered", "Vulnerable", "Near Threatened", "Least Concern")

  ggplot2::ggplot(df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = {{FILL}})) +
    ggplot2::geom_rect() +
    ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = n), color = "black", size = 5) +  ## Add this line to include count values
    ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
    ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
    ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
    ggplot2::labs(fill = "Threat Status") +
    ggplot2::theme_void() + ## removes the lines around chart and grey background
    ggplot2::theme(
      panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
      plot.background = element_rect(fill = "white", color = NA)  ## set plot background to white
    )
}

#######################################################################################################
###
#' Protection level donut plot
#'
#' Donut plot for the protection level of the ecosystem types
#'
#' @param DF The data frame that contains the information on protection level
#' @param FILL The protection level categories
#'
#' @return Returns a bar donut plot of protection level
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
#' @importFrom dplyr count
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#'
#'
#' @export
#'
#' @examples
#' #test <- pro_donut_plot(mydata, ecosystem_functional_grps)
#'
pro_donut_plot_test <-function(DF, FILL)
{

  ### generate a frequency table for the categorical data
  df <- dplyr::count(DF, {{FILL}})


  ### define the order of the protection levels
  ord <- c("Well Protected", "Moderately Protected", "Poorly Protected", "No Protection")

  # Prepare the data frame by arranging and setting colors
  df <- df %>%
    dplyr::arrange(factor({{FILL}}, levels = ord))

  df <- df %>%
    dplyr::mutate(ymax = cumsum(n)) %>%
    dplyr::mutate(ymin = ymax -n)


  cols <- c("#466a31","#80a952","#d5dec3","#a4a3a3")
  breaks <- c("Well Protected","Moderately Protected","Poorly Protected","No Protection")

  ggplot2::ggplot(df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = {{FILL}})) +
    ggplot2::geom_rect() +
    ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = n), color = "black", size = 5) +  # Add this line to include values
    ggplot2::coord_polar(theta = "y") + # convert to polar coordinates
    ggplot2::xlim(c(2, 4)) + # limit x-axis to create a donut chart
    ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
    ggplot2::labs(fill = "Protection Levels") +
    ggplot2::theme_void() + # removes the lines around chart and grey background
    ggplot2::theme(
      panel.background = element_rect(fill = "white", color = NA),  # set panel background to white
      plot.background = element_rect(fill = "white", color = NA)  # set plot background to white
    )
}


#######################################################################################################
