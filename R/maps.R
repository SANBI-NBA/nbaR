######################################################################
###
#' NBA map function
#'
#' NBA maps for protection level, threat status, and condition.
#' The function can take a dataset with either one polygon or
#' multipolygon per ecosystem (each row has a unique ecosystem) or
#' multiple rows per ecosystem type.
#'
#' The function expects an sf object.
#'
#' Please look at the example data NBA_example_map_data and NBA_example_donut_plot to see the correct
#' structure for the data.
#'
#' The name of the fill column is irrelevant, but the categories must be
#' spelled correctly (there is a list of the standard spellings/ cases
#' of NBA categories named NBA_categories in this package,
#' which can be accessed for reference).
#'
#'
#' @param DF The data frame that contains the information
#' @param COLS The variables to group together with the ecosystem types if the data isn't already in the form of one row per ecosystem type.
#' @param GEOM The name of the geometry column
#' @param CAP The caption for the map (will be place at the bottom left of the plot)
#' @param FILL The column that contains the categories to colour the ecosystems by (e.g. protection level, threat status, or condition)
#'
#' @return Returns a plot
#'
#'
#' @importFrom ggplot2  ggplot
#' @importFrom ggplot2  scale_fill_manual
#' @importFrom ggplot2  labs
#' @importFrom ggplot2  theme
#' @importFrom ggplot2  aes
#' @importFrom ggplot2  element_rect
#' @importFrom ggplot2  element_blank
#' @importFrom ggplot2  element_text
#' @importFrom ggspatial annotation_scale
#' @importFrom ggspatial annotation_north_arrow
#' @importFrom ggspatial north_arrow_orienteering
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr pick
#' @importFrom magrittr "%>%"
#' @importFrom sf st_union
#' @importFrom sf st_geometry_type
#'
#'
#'
#' @export
#'
#' @examples
#'
#'#map <- NBA_map(
#'#DF = mem_2023,
#'#COLS = c(ecosystem_type, protection_level_30, threat_status, eco_poly_colour),
#'#GEOM = geometry,
#'#CAP = "Figure 1. Map of the distribution of the ecosystems threat status",
#'#FILL = threat_status)
#'
#'#map
#'
#'


NBA_map <- function(DF, COLS, GEOM, CAP, FILL){



  dat <- DF %>%
    dplyr::group_by(dplyr::pick({{COLS}}))%>%
    dplyr::summarise(geometry = sf::st_union({{GEOM}})) %>%
    dplyr::filter(sf::st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
    dplyr::ungroup()



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




  ## plot the 30% protection threshold map
  map <- ggplot2::ggplot() +

    ggplot2::geom_sf(data = dat,
                     ggplot2::aes(fill = {{FILL}}),
            color = "grey",
            lwd = 0.1) +  # plot protection level and separate each protectipn level category in grey boundaries

    ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
    # theme_void() +
    ggplot2::labs(title = "",
         fill = "", ## legend title
         x = "",
         y = "",
         caption = CAP) +

    ggspatial::annotation_scale(location = "bl",            #location of the scale bar (br = bottom right)
                                width_hint = 0.1,
                                style= "bar") +         #proportion of plot that scalebar occupies

    ggspatial::annotation_north_arrow(location = "bl",                 #location of arrow (br = bottom right)
                                      which_north = "true",            #points to the north pole
                                      height = unit(0.8, "cm"),
                                      width = unit(0.8, "cm"),
                                      pad_x = unit(0.1, "in"),        #margin between arrow and map edge
                                      pad_y = unit(0.3, "in"),         #margin between arrow and map edge
                                      style = ggspatial::north_arrow_orienteering(text_size = 8)) +

    ggplot2::theme(legend.key.size = ggplot2::unit(0.5,"line"),
          legend.position = "inside",
          # legend.position.inside = c(.95, .95),
          legend.justification = c("right", "bottom"),
          plot.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_rect(colour = "black", fill = NA),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          plot.caption.position = "plot",
          plot.caption = ggplot2::element_text(hjust = 0))

}
