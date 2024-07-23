



#' Protection level
#'
#' Horizontal barplots for the protection level of ecosystem function groups (efgs)
#'
#' @param DF The data frame that contains the information on protection level
#' @param X The groups
#' @param Y The protection level percentages
#' @param FILL The protection level categories
#' @param LABEL The frequency counts of the number of ecosystems within each protection level
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
#' @export
#'
#'
prot_efg <-function(DF, X, Y, FILL, LABEL) {


  ggplot2::ggplot(DF, aes(y = Y, x = X, fill = FILL)) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) + ## change width of bars
    ggplot2::geom_text(aes(label = LABEL), position = position_stack(vjust = 0.5), # add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
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
#' Threat status
#'
#' Horizontal barplots for the threat status of ecosystem function groups (efgs)
#'
#' @param DF The data frame that contains the information on threat status
#' @param X The groups
#' @param Y The threat status percentages
#' @param FILL The threat status categories
#' @param LABEL The frequency counts of the number of ecosystems within each protection level
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
thr_efg <-function(DAT, X, Y, FILL, LABEL)

{
  ggplot2::ggplot(DATA, aes(y = Y, x = X, fill = FILL)) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) + # change width of bars
    ggplot2::geom_text(aes(label = LABEL), position = position_stack(vjust = 0.5), # add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
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
### Create the function for the donut plots for the threat status of the ecosystem types
#' Title
#'
#' @param DF The data frame that contains the information on threat status
#' @param YMIN The groups
#' @param YMAX The threat status percentages
#' @param FILL The threat status categories
#' @param COLOUR colour for the threat status
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
thr_donut_plot <-function(DF, YMAX, YMIN, FILL, COLOUR)
{
  ggplot2::ggplot(DF, aes(ymax=YMAX, ymin=YMIN, xmax=4, xmin=3, fill=FILL)) +
    ggplot2::geom_rect() +
    ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = Frequency), color = "black", size = 5)+  ## Add this line to include values
    ggplot2::coord_polar(theta="y") + ## try to remove that to understand how the chart is built initially
    ggplot2::xlim(c(2, 4)) + ## try to remove that to see how to make a pie chart
    ggplot2::scale_fill_manual(values = COLOUR, breaks = FILL) +
    ggplot2::labs(fill = "Threat Status") +
    ggplot2::theme_void() # removes the chart grid lines and  and grey background
    ggplot2::theme(
    panel.background = element_rect(fill = "white", color = NA),  # set panel background to white
    plot.background = element_rect(fill = "white", color = NA)  # set plot background to white
    )
}

#######################################################################################################
### Create the function for the donut plot for the protection level of the ecosystem types
#' Title
#'
#' @param DF The data frame that contains the information on threat status
#' @param YMIN The groups
#' @param YMAX The threat status percentages
#' @param FILL The threat status categories
#' @param COLOUR colour for the threat status
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
#'
#' @export
#'
#' @examples
pro_donut_plot <-function(DF, YMAX, YMIN, FILL, COLOUR, LABEL)
{
  ggplot2::ggplot(DF, aes(ymax = YMAX, ymin = YMIN, xmax = 4, xmin = 3, fill = FILL)) +
    ggplot2::geom_rect() +
    ggplot2::geom_text(aes(x = 3.5, y = (YMIN + YMAX) / 2, label = LABEL), color = "black", size = 5) +  # Add this line to include values
    ggplot2::coord_polar(theta = "y") + # convert to polar coordinates
    ggplot2::xlim(c(2, 4)) + # limit x-axis to create a donut chart
    ggplot2::scale_fill_manual(values = COLOUR, breaks = FILL) +
    ggplot2::labs(fill = "Protection Levels") +
    ggplot2::theme_void() + # removes the lines around chart and grey background
    ggplot2::theme(
      ggplot2::panel.background = element_rect(fill = "white", color = NA),  # set panel background to white
      ggplot2::plot.background = element_rect(fill = "white", color = NA)  # set plot background to white
  )
}

#######################################################################################################
