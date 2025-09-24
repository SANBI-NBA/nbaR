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
#' of NBA categories named nbaR::NBA_categories in this package,
#' which can be accessed for reference).
#'
#'
#' @param DF The data frame that contains the information
#' @param GEOM Whether the layer is a vector or a raster. Vectors must be
#' sf objects and rasters must be terra (spatraster) objects
#' @param FILL The column that contains the categories to colour the ecosystems by
#'  (e.g. protection level, threat status, or condition)
#' @param LEGEND True to include the legend in the plot, False to exclude it
#' @param MOE True to include the Map orientation elements (MOE) of a scale bar and north arrow, False to exclude
#' @param LWD Line width to use in the plot
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
#' @importFrom tidyterra geom_spatraster
#'
#'
#' @export
#'
#' @examples
#'
#'map <- nba_map(DF = NBA_example_map_data,
#'GEOM = "vector",
#'FILL = protection_level,
#'LEGEND = TRUE,
#'MOE = TRUE)
#'
#'map
#'
#'
#'
#'


nba_map <- function(DF, GEOM = c("vector", "raster"), FILL, LEGEND = FALSE, MOE = FALSE,
                    LWD = 0.1){


if(GEOM == "vector"){


  dat <- DF

  # %>%
  #   mutate({{FILL}} := factor({{FILL}}, levels = nbaR::NBA_categories))


  ## plot map
  map <- ggplot2::ggplot() +

    ggplot2::geom_sf(data = dat,
                     ggplot2::aes(fill = {{FILL}},
                                  colour = {{FILL}}),
            lwd = LWD) +

    ggplot2::scale_fill_manual(values = nbaR::NBA_colours)+
    ggplot2::scale_colour_manual(values = nbaR::NBA_colours)

} else {



  map <- ggplot2::ggplot()+
    tidyterra::geom_spatraster(data=DF)+
    ggplot2::scale_fill_manual(values = nbaR::NBA_colours, na.value = "transparent")

}





if(LEGEND == TRUE){

  map_leg <- map +
    ggplot2::theme(legend.key.size = ggplot2::unit(1,"line"),
                   legend.position = "inside",
                   legend.justification = c("right", "bottom"),
                   legend.title = element_blank(),
                   plot.background = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())

} else {

  map_leg <- map +
    ggplot2::theme(legend.position = "none",
                   plot.background = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())

}

  if(MOE == TRUE){

    map_moe <- map_leg +
    ggspatial::annotation_scale(location = "bl",            #location of the scale bar (br = bottom right)
                                width_hint = 0.1,
                                style= "bar") +         #proportion of plot that scalebar occupies

      ggspatial::annotation_north_arrow(location = "bl",                 #location of arrow (br = bottom right)
                                        which_north = "true",            #points to the north pole
                                        height = ggplot2::unit(0.8, "cm"),
                                        width = ggplot2::unit(0.8, "cm"),
                                        pad_x = ggplot2::unit(0.1, "in"),        #margin between arrow and map edge
                                        pad_y = ggplot2::unit(0.3, "in"),         #margin between arrow and map edge
                                        style = ggspatial::north_arrow_orienteering(text_size = 8))


  }else {


    map_moe <- map_leg

  }

  map_moe
}


