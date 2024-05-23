### Create the function to create horizontal barplots for the threat status of ecosystem function groups (efgs)
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
thr_efg_plot <-function(DAT, X, Y, FILL )

{
  ggplot(DAT, aes(y = Y, x = X, fill = FILL)) +
    geom_bar(stat = "identity", width = 0.5) + ## change width of bars
    geom_text(aes(label = count), position = position_stack(vjust = 0.5), ## add count labels to the stacked bars and adjust the "vjust" value to place text at the beginning, centre or end of bars
              size = 3, color = "black", show.legend = FALSE) + ## adjust size of labels with no legend being shown
    scale_fill_manual(values = c("#b1d798", "#eeeea3", "#fff02a", "#f97835", "#e9302c")) +  ## order the colours of the bars in the reversed order
    ylab("Percent protected (%)") +
    xlab("") + ## remove the heading for the y-axis
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(fill = "Threat Status") + ## change the legend title here
    theme_minimal() + ## create a black bounding box around the plot
    theme(panel.grid = element_blank(),
          panel.border = element_rect(fill= NA),
          panel.grid.major.y = element_line(colour = "grey", size = 0.005))+
    theme(legend.position = "bottom") + ## position the legend to beneath the plot
    coord_flip()  ## flip the orientation of the chart
}

### Create the function for the donut plots for the threat status of the ecosystem types
#' Title
#'
#' @param data
#' @param ymax
#' @param ymin
#' @param fill
#'
#' @return
#' @export
#'
#' @examples
thr_donut_plot <-function(data, ymax, ymin, fill)

{
  ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Threat_status_2023)) +
    geom_rect() +
    geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = Frequency), color = "black", size = 5)+  ## Add this line to include values
    coord_polar(theta="y") + ## try to remove that to understand how the chart is built initially
    xlim(c(2, 4)) + ## try to remove that to see how to make a pie chart
    scale_fill_manual(values = freq_df$Cols, breaks = freq_df$Threat_status_2023) +
    labs(fill = "Threat Status") +
    theme_void() ## removes the lines around chart and grey background

}

### Create the function to create horizontal barplots for the threat status of ecosystem function groups (efgs)
#' Title
#'
#' @param data
#' @param ymax
#' @param ymin
#' @param fill
#'
#' @return
#' @export
#'
#' @examples
prot_donut_plt <-function(data, ymax, ymin, fill)

{

  ggplot(freq_df2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Protection_level_2023)) +
    geom_rect() +
    geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = Frequency), color = "black", size = 5)+  ## Add this line to include values
    coord_polar(theta="y") + ## try to remove that to understand how the chart is built initially
    xlim(c(2, 4)) + ## try to remove that to see how to make a pie chart
    scale_fill_manual(values = freq_df2$Cols, breaks = freq_df2$Protection_level_2023) +
    labs(fill = "Protection Levels") +
    theme_void() ## removes the lines around chart and grey background

}

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
prot_efg_plot <-function(DAT, X, Y, FILL )

{
  ggplot(DAT, aes(y = Y, x = X, fill = FILL)) +
    geom_bar(stat = "identity", width = 0.5) + ## change width of bars
    geom_text(aes(label = count), position = position_stack(vjust = 0.5),## add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
              size = 3, color = "black", show.legend = FALSE) + ## adjust size of labels with no legend being shown
    scale_fill_manual(values = c("#a4a3a3", "#d5dec3", "#80a952", "#466a31")) +  ## order the colours of the bars in the reversed order
    ylab("Percent protected (%)") +
    xlab("") +
    guides(fill = guide_legend(reverse = TRUE)) + ## reverse the order of the legend to match the plots order of categories
    labs(fill = "Protection Levels") + ## change the legend title
    theme_bw() + ## create a black bounding box around the plot
    theme(legend.position = "bottom") + ## position the legend to the beneath the plot
    coord_flip()  ## flip the orientation of the chart
}
