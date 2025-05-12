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
#' Please look at the example data NBA_example_map_data to see the correct
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
#' @return Returns a map
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
#'map <- nba_map(DF = NBA_example_map_data,
#'               GEOM = geometry,
#'               CAP = "Figure 1. A map of the protection level of marine pelagic ecosystem types in South Africa",
#'               FILL = protection_level)
#'
#'map
#'
#'
#'
#'


nba_map <- function(DF, COLS = NULL, GEOM, CAP, FILL){

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

  if (!is.null(COLS)) {

    dat <- DF %>%
      dplyr::group_by(dplyr::pick({{COLS}}))%>%
      dplyr::summarise(geometry = sf::st_union({{GEOM}})) %>%
      dplyr::filter(sf::st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
      dplyr::ungroup()

  }
  else {

    dat <- DF
  }



  dat <- dat %>%
    mutate({{FILL}} := factor({{FILL}}, levels = NBA_categories))


  ## plot the 30% protection threshold map
  map <- ggplot2::ggplot() +

    ggplot2::geom_sf(data = dat,
                     ggplot2::aes(fill = {{FILL}}),
            color = "grey",
            lwd = 0.1) +  # plot protection level and separate each protectipn level category in grey boundaries

    ggplot2::scale_fill_manual(values = NBA_colours) +
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
                                      height = ggplot2::unit(0.8, "cm"),
                                      width = ggplot2::unit(0.8, "cm"),
                                      pad_x = ggplot2::unit(0.1, "in"),        #margin between arrow and map edge
                                      pad_y = ggplot2::unit(0.3, "in"),         #margin between arrow and map edge
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


