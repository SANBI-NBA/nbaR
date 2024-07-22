### Create the function to create horizontal barplots for the protection of ecosystem function groups (efgs)
#' Title
#'
#' @param DAT
#' @param X
#' @param Y
#' @param FILL
#'
#' @return
#' @export
#'
#' @examples
prot_efg <-function(DAT, X, Y, FILL)

{
  ggplot(DAT, aes(y = Y, x = X, fill = FILL)) +
    geom_bar(stat = "identity", width = 0.5) + ## change width of bars
    geom_text(aes(label = count), position = position_stack(vjust = 0.5), # add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
              size = 3, color = "black", show.legend = FALSE) + # adjust size of labels with no legend being shown
    scale_fill_manual(values = c("#a4a3a3", "#d5dec3", "#80a952", "#466a31")) +  # order the colours of the bars in the reversed order
    ylab("Percentage of ecosystem functional types") +
    xlab("") +
    guides(fill = guide_legend(reverse = TRUE, nrow = 2)) +  # display legend in 2 rows
    labs(fill = "") + # change the legend title
    scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = c(0, 50, 100)) + # set the y-axis to show 0%, 50%, and 100%
    theme_minimal() + # create a black bounding box around the plot
    theme(legend.position = "bottom", # position legend to the bottom
          panel.grid.minor = element_blank(), # remove grid lines on every second x-axis value
          axis.line = element_blank(), # remove all x-axis grid lines from
          panel.grid.major.y = element_blank(), # include the horizontal grid line on 1st , 3rd and 5 ... x-axis
          legend.text = element_text(size = 8),
          plot.background = element_rect(color = "black", fill = NA),  # add black border around the entire plot
          plot.margin = margin(10, 10, 10, 10)) +   # extend plot margins to accommodate the border)
    coord_flip()
}

#######################################################################################################
### Create the function to create horizontal barplots for the protection of ecosystem function groups (efgs)
#' Title
#'
#' @param DAT
#' @param X
#' @param Y
#' @param FILL
#'
#' @return
#' @export
#'
#' @examples
thr_efg <-function(DAT, X, Y, FILL)

{
  ggplot(DATA, aes(y = Y, x = X, fill = FILL)) +
    geom_bar(stat = "identity", width = 0.5) + # change width of bars
    geom_text(aes(label = count), position = position_stack(vjust = 0.5), # add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
              size = 3, color = "black", show.legend = FALSE) + # adjust size of labels with no legend being shown
    scale_fill_manual(values = c("#b1d798", "#eeeea3", "#fff02a", "#f97835", "#e9302c")) +  # order the colours of the bars in the reversed order
    ylab("Percentage of ecosystem functional types") +
    xlab("") + ## remove the heading for the y-axis
    guides(fill = guide_legend(reverse = TRUE, nrow = 2)) +  # display legend in 2 rows
    labs(fill = "") + ## change the legend title here
    scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = c(0, 50, 100)) + # set the y-axis to show 0%, 50%, and 100%
    theme_minimal() +
    theme(legend.position = "bottom", # position legend to the bottom
          panel.grid.minor = element_blank(), # remove grid lines on every second x-axis value
          axis.line = element_blank(), # remove all x-axis grid lines
          panel.grid.major.y = element_blank(), # remove the horizontal lines only on 1st , 3rd and 5 ... x-axis
          legend.text = element_text(size = 8), # change legend text size
          plot.background = element_rect(color = "black", fill = NA),  # add border around the entire plot include legend
          plot.margin = margin(10, 10, 10, 10)) +   # extend plot margins to accommodate the border)
    coord_flip()  # flip the orientation of the chart
}

#######################################################################################################
### Create the function for the donut plots for the threat status of the ecosystem types
#' Title
#'
#' @param DAT
#' @param X
#' @param Y
#' @param FILL
#'
#' @return
#' @export
#'
#' @examples
thr_donut_plot <-function(DAT, YMAX, YMIN, FILL)
{
  ggplot(DAT, aes(ymax=YMAX, ymin=YMIN, xmax=4, xmin=3, fill=FILL)) +
    geom_rect() +
    geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = Frequency), color = "black", size = 5)+  ## Add this line to include values
    coord_polar(theta="y") + ## try to remove that to understand how the chart is built initially
    xlim(c(2, 4)) + ## try to remove that to see how to make a pie chart
    scale_fill_manual(values = DAT$Cols, breaks = DAT$Threat_status_2023) +
    labs(fill = "Threat Status") +
    theme_void() # removes the chart grid lines and  and grey background
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # set panel background to white
    plot.background = element_rect(fill = "white", color = NA)  # set plot background to white
    )
}

#######################################################################################################
### Create the function for the donut plot for the protection level of the ecosystem types
#' Title
#'
#' @param DAT
#' @param X
#' @param Y
#' @param FILL
#'
#' @return
#' @export
#'
#' @examples
pro_donut_plot <-function(DAT, YMAX, YMIN, FILL)
{
  ggplot(DAT, aes(ymax = YMAX, ymin = YMIN, xmax = 4, xmin = 3, fill = FILL)) +
  geom_rect() +
  geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = Frequency), color = "black", size = 5) +  # Add this line to include values
  coord_polar(theta = "y") + # convert to polar coordinates
  xlim(c(2, 4)) + # limit x-axis to create a donut chart
  scale_fill_manual(values = freq_df2$Cols, breaks = freq_df2$Protection_level_2023) +
  labs(fill = "Protection Levels") +
  theme_void() + # removes the lines around chart and grey background
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # set panel background to white
    plot.background = element_rect(fill = "white", color = NA)  # set plot background to white
  )
}

#######################################################################################################
