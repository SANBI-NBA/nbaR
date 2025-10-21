#####################################################################################
##
## Script name: data
##
## Purpose of script:document the packages example data
##
## Author: Natasha Besseling
##
## Date Created: 2024-10-23
##
##
## Notes:
##
##
#####################################################################################
#' NBA ecosystem type threat status data
#'
#'This data is used to make a bar plot of threat status of ecosystems within a functional group.
#'This dataset has a totals column, but this is not necessary.
#'
#' @format ## `NBA_example_thr_data`
#' A data frame with 8 rows and 6 columns:
#' \describe{
#'   \item{OVERALL types}{Ecosystem functional group name}
#'   \item{Critically Endangered,Endangered,Vulnerable,Least Concern}{Threat status}
#'   \item{TOT}{Total number of ecosystems}
#'   ...
#' }
#' @source SANBI
"NBA_example_thr_data"

#' NBA ecosystem type protection level data
#'
#'This data is used to make a donut plot of protection level of ecosystems
#'within a functional group.
#'
#' @format ## `NBA_example_pro_data`
#' A data frame with 8 rows and 5 columns:
#' \describe{
#'   \item{OVERALL types}{Ecosystem functional group name}
#'   \item{Well Protected,Moderately Protected,Poorly Protected,Not Protected}{Protection levels}
#'   ...
#' }
#' @source SANBI
"NBA_example_pro_data"


#' NBA index data (RLIs biome)
#'
#'This data is used to make a line plot of the red list index across a number of years
#'
#' @format A data frame with 63 rows and 6 variables:
#' \describe{
#'   \item{Year}{assessment year}
#'   \item{RLI}{Red List Index value}
#'   \item{min}{minimum value}
#'   \item{max}{maximum value}
#'   \item{Assessment_Year}{year of assessment}
#'   \item{Biome}{biome name}
#'   ...
#' }
#' @source SANBI
"NBA_rlis_biome_example_data"

#' NBA index data (RLIs taxon)
#'
#'This data is used to make a line plot of the red list index across a number of years
#'
#' @format A data frame with 63 rows and 6 variables:
#' \describe{
#'   \item{Year}{assessment year}
#'   \item{RLI}{Red List Index value}
#'   \item{min}{minimum value}
#'   \item{max}{maximum value}
#'   \item{Assessment_Year}{year of assessment}
#'   \item{Taxon}{taxon name}
#'   ...
#' }
#' @source SANBI
"NBA_rlis_example_data"

#' NBA index data (RLIe)
#'
#'This data is used to make a line plot of the red list index across a number of years
#'
#' @format A data frame with 44 rows and 8 variables:
#' \describe{
#'   \item{Year}{assessment year}
#'   \item{RLIE}{Red List Index value}
#'   \item{lower}{minimum value}
#'   \item{upper}{maximum value}
#'   \item{criteria}{Overall}
#'   \item{total_weight}{weight}
#'   \item{total_count}{count}
#'   \item{Biome}{biome name}
#'   ...
#' }
#' @source SANBI
"NBA_rlie_example_data"

#' NBA index data (EPLI 2018)
#'
#'This data is used to make a line plot of the ecosystem protection level index across a number of years
#'
#' @format A data frame with 11 rows and 4 variables:
#' \describe{
#'   \item{X}{row number}
#'   \item{T_BIOME}{biome name}
#'   \item{Total}{total number}
#'   \item{EPLI}{EPLI}
#'   ...
#' }
#' @source SANBI
"NBA_epli2018_example_data"

#' NBA index data (EPLI 2024)
#'
#'This data is used to make a line plot of the ecosystem protection level index across a number of years
#'
#' @format A data frame with 11 rows and 4 variables:
#' \describe{
#'   \item{X}{row number}
#'   \item{T_BIOME}{biome name}
#'   \item{Total}{total number}
#'   \item{EPLI}{EPLI}
#'   ...
#' }
#' @source SANBI
"NBA_epli2024_example_data"

#' NBA category words/ phrases used in threat status/ protection level/ and condition
#' categorisation
#'
#'This data is used to enable users to verify that they are using the
#'standard format for their headings.
#'
#' @format ## `NBA_categories`
#' A vector of category names:

#' @source SANBI
"NBA_categories"

#' NBA colours for every category
#'
#'This data is used to enable users to use the standard colours for their figures
#'
#' @format ## `NBA_colours`
#' A vector of category colours:

#' @source SANBI
"NBA_colours"

#' NBA combined ecosystem type protection level and threat level example data
#'
#'This data is used to make a combined plot of protection level and threat status of ecosystems
#'within a functional group.
#'
#' @format ## `NBA_example_comb_data`
#' A data frame with 16 rows and 11 columns:
#' \describe{
#'   \item{OVERALL types}{Ecosystem functional group name}
#'   \item{Well Protected,Moderately Protected,Poorly Protected,Not Protected}{Protection levels}
#'   \item{Critically Endangered,Endangered,Vulnerable,Least Concern}{Threat status}
#'   \item{TOT}{Total number of ecosystems}
#'   \item{metric}{Column to differentiate rows into groups}
#'   ...
#' }
#' @source SANBI
"NBA_example_comb_data"

#' NBA map example data, mem (marine ecosystem map) 2023
#'
#'This data is used to make a map of marine pelagic ecosystems and their protection level
#'
#' @format ## `NBA_example_map_data`
#' A data frame with 13 rows and 3 columns:
#' \describe{
#'   \item{P_EcosysType}{Ecosystem name}
#'   \item{geometry}{polygons}
#'   \item{protection_level}{protection level}
#'   \item{threat_status}{threat statis}
#'   ...
#' }
#' @source SANBI
"NBA_example_map_data"

#' NBA bubble example data
#'
#'This data is used to make a bubble plot of the percentage of species of ecological concern
#'in a group that are affected by a pressure
#'
#' @format ## `NBA_example_bubble_data`
#' A data frame with 13 rows and 3 columns:
#' \describe{
#'   \item{taxon_group}{The names of the taxon groups the species fall into}
#'   \item{pressure}{The pressures that could impact the taxa}
#'   \item{sub_pressure}{The specific sub pressures within the pressure categories}
#'   \item{perc_concern_under_press}{The percentage of species of ecological concern that are impacted by the pressure}
#'   ...
#' }
#' @source SANBI
"NBA_example_bubble_data"

#' NBA pressure bar plot example data
#'
#' A small example dataset used to demonstrate the `nba_pressure_bar_plot()` function.
#'
#' @format `NBA_press_bar_example_data`
#' A data frame with 3 variables:
#' \describe{
#'   \item{Taxon}{Taxonomic group (e.g., Birds, Mammals, Plants)}
#'   \item{pressure}{Name of the pressure category}
#'   \item{percentage}{Percentage value for each pressure}
#'  ...
#' }
#' @source SANBI
"NBA_press_bar_example_data"
