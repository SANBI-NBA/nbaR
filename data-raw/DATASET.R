## code to prepare `DATASET` dataset goes here

####################################################################################
##
## Script name: example data
##
## Purpose of script: to make example data to be used for reference by package users
##
## Author: Natasha Besseling
##
## Date Created: 2024-10-32
##
#####################################################################################
###libraries
library(tidyverse)
library(readxl)
library(sf)

### load data


## threat status graph
NBA_example_thr_data <- read_excel(
  dir("data-raw",
      "Fig1a_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =8) %>%
  mutate(across(2:6, as.numeric))

## Index graph
NBA_example_rlis_data <- read_excel(
  dir("data-raw",
      "NBA_example_rlis_data.xlsx",
      full.names = T,
      recursive = T))

NBA_example_rlie_data <- read_excel(
  dir("data-raw",
      "NBA_example_rlie_data.xlsx",
      full.names = T,
      recursive = T))

NBA_example_epli2018_data <- read_excel(
  dir("data-raw",
      "NBA_example_epli2018_data.xlsx",
      full.names = T,
      recursive = T))

NBA_example_epli2024_data <- read_excel(
  dir("data-raw",
      "NBA_example_epli2024_data.xlsx",
      full.names = T,
      recursive = T))


## protection level
NBA_example_pro_data <- read_excel(
  dir("data-raw",
      "Fig61mapinset_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =8)%>%
  mutate(across(2:5, as.numeric)) %>%
  dplyr::select(1:5)

##combined plots
NBA_example_comb_data <- NBA_example_pro_data %>%
  mutate(metric = "protection_level") %>%
  dplyr::bind_rows(NBA_example_thr_data %>%
                     mutate(metric = "threat_status")) %>%
  dplyr::select(`OVERALL types`, metric, dplyr::everything())

## pressure bar plot
NBA_example_press_bar_data <- read_excel(
  dir("data-raw",
      "nba_example_press_bar_data.xlsx",
      full.names = T,
      recursive = T))

##threat status/ protection level categories

NBA_categories <- c(
                    #Threat staus
                    "Critically Endangered",
                    "Endangered" ,
                    "Vulnerable",
                    "Near Threatened" ,
                    "Least Concern" ,
                    "Data Deficient" ,
                    "Rare" ,
                    "Extinct" ,
                    "Extinct in the Wild" ,
                    "Regionally Extinct",
                    "Critically Endangered (Possibly Extinct)",

                    #Protection level
                    "Well Protected" ,
                    "Moderately Protected" ,
                    "Poorly Protected" ,
                    "Not Protected" ,

                    #Pressures
                    "Low" ,
                    "Medium" ,
                    "High" ,
                    "Very high",


                    "No threats" ,
                    "Pollution",
                    "Transportation & service corridors" ,
                    "Agriculture" ,
                    "Agriculture and aquaculture",
                    "Geological events",
                    "Biological resource use" ,
                    "Other threats",
                    "Human intrusions & disturbance" ,
                    "Human intrusions and disturbance" ,
                    "Climate change" ,
                    "Climate change & severe weather",
                    "Energy production & mining" ,
                    "Energy production and mining" ,
                    "Natural system modifications" ,
                    "Invasive and other problematic species, genes & diseases",
                    "Invasive and other problematic species, genes and diseases",
                    "Residential & commercial development" ,


                    # Condition
                    "Natural" ,
                    "Natural / near-natural" ,
                    "Natural / near-natural ", #space for anti-rule
                    "Near-natural" ,
                    "Near-natural / moderately modified" ,
                    "Near-natural / moderately modified " , #space for anti-rule
                    "Moderately modified",
                    "Moderately / heavily modified" ,
                    "Moderately / heavily modified " , #space for anti-rule
                    "Heavily modified" ,
                    "Heavily / severely modified" ,
                    "Heavily / severely modified " , #space for anti-rule
                    "Severely modified" ,
                    "Severely / critically modified" ,
                    "Severely / critically modified " ,#space for anti-rule
                    "Critically modified" ,


                    # Responses
                    "No response" ,
                    "Some kind of response" ,
                    "Gazetted" ,
                    "Signed off" ,


                    # Priority areas
                    "Land-based Protected Areas",
                    "Marine Protected Areas",
                    "Critical Biodiversity Areas",
                    "Ecologically Sensitive Areas" ,

                    #priority ecosystems

                     "Critically Endangered & Not Protected",
                    "Critically Endangered & Poorly Protected",
                     "Endangered & Not Protected",
                     "Endangered & Poorly Protected",

                    # Built up areas
                    "Cropland",
                    "Plantation",
                    "Built up",
                    "Mine",
                    "Artificial waterbody",
                    "Landcover Natural")


##colour mapping
NBA_colours <- c(

  ##Threat status
  "Critically Endangered" = rgb(216, 30, 5, maxColorValue = 255),
  "Endangered" = rgb(252, 127, 63, maxColorValue = 255),
  "Vulnerable" = rgb(249, 232, 20, maxColorValue = 255),
  "Near Threatened" = "#FFFFBE",
  "Least Concern" = rgb(180, 215, 158, maxColorValue = 255),
  "Data Deficient" = rgb(209, 209, 198, maxColorValue = 255),
  "Rare" = rgb(193, 181, 165, maxColorValue = 255),
  "Extinct" = rgb(0, 0, 0, maxColorValue = 255),
  "Extinct in the Wild" = rgb(84, 35, 68, maxColorValue = 255),
  "Regionally Extinct" = "#482B4D",
  "Critically Endangered (Possibly Extinct)" = "#9B0411",

  #Protection level
  "Well Protected" = rgb(75, 110, 0, maxColorValue = 255),
  "Moderately Protected" = rgb(132, 171, 92, maxColorValue = 255),
  "Poorly Protected" = rgb(213, 222, 196, maxColorValue = 255),
  "Not Protected" = rgb(166, 166, 166, maxColorValue = 255),

  #unluckies
  "Critically Endangered & Not Protected" = "#A93800",
  "Critically Endangered & Poorly Protected" = "#A87001",
  "Endangered & Poorly Protected" = "#E69800",
  "Endangered & Not Protected" = "#FFEBB0",

  #Pressures
  "Low" = "#EDC31C", #rgb(223, 220, 199, maxColorValue = 255),
  "Medium" = "#F58026", #rgb(175, 168, 117, maxColorValue = 255),
  "High" = "#FE0C0C", #rgb(122, 116, 70, maxColorValue = 255),
  "Very high" = "#BC0906", #rgb(88, 82, 50, maxColorValue = 255),


  "No threats" = rgb(48, 30, 6, maxColorValue = 255),
  "Pollution" = rgb(97, 65, 56, maxColorValue = 255),
  "Transportation & service corridors" = rgb(99, 76, 39, maxColorValue = 255),
  "Transportation and service corridors" = rgb(99, 76, 39, maxColorValue = 255),
  "Agriculture" = rgb(133, 76, 13, maxColorValue = 255),
  "Agriculture and aquaculture" = rgb(133, 76, 13, maxColorValue = 255),
  "Agriculture & aquaculture" = rgb(133, 76, 13, maxColorValue = 255),
  "Geological events" = rgb(153, 102, 0, maxColorValue = 255),
  "Biological resource use" = rgb(180, 121, 42, maxColorValue = 255),
  "Other threats" = rgb(231, 160, 54, maxColorValue = 255),
  "Other threat" = rgb(231, 160, 54, maxColorValue = 255),
  "Human intrusions & disturbance" = rgb(159, 134, 9, maxColorValue = 255),
  "Human intrusions and disturbance" = rgb(159, 134, 9, maxColorValue = 255),
  "Climate change" = rgb(178, 149, 78, maxColorValue = 255),
  "Climate change & severe weather" = rgb(178, 149, 78, maxColorValue = 255),
  "Energy production & mining" = rgb(122, 116, 70, maxColorValue = 255),
  "Energy production and mining" = rgb(122, 116, 70, maxColorValue = 255),
  "Natural system modifications" = rgb(88, 82, 50, maxColorValue = 255),
  "Invasive and other problematic species, genes & diseases" = rgb(61, 69, 64, maxColorValue = 255),
  "Invasive and other problematic species, genes and diseases" = rgb(61, 69, 64, maxColorValue = 255),
  "Residential & commercial development" = rgb(128, 128, 128, maxColorValue = 255),


  # Condition
  "Natural" = "#345F91",
  "Natural / near-natural" = "#345F91",
  "Natural / near-natural " = "#6D9FD4", #space for anti-rule
  "Near-natural" = "#6D9FD4",
  "Near-natural / moderately modified" = "#6D9FD4",
  "Near-natural / moderately modified " = "#A5C5C7", #space for anti-rule
  "Moderately modified" = "#A5C5C7",
  "Moderately / heavily modified" = "#A5C5C7",
  "Moderately / heavily modified " = "#A7AB81", #space for anti-rule
  "Heavily modified" = "#A7AB81",
  "Heavily / severely modified" = "#A7AB81",
  "Heavily / severely modified " = "#88814D", #space for anti-rule
  "Severely modified" = "#88814D",
  "Severely / critically modified" = "#88814D",
  "Severely / critically modified " = "#736D41",#space for anti-rule
  "Critically modified" = "#736D41",


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
  "Cropland"= "#DB7D15",
  "Plantation"= "#B36611",
  "Built up"= "#808080",
  "Mine"= "#F5C592",
  "Artificial waterbody" ="#0071C0",
  "Landcover Natural" = "#B9B386",

  # Realms
  "sanbi-green" = "#70B276",
  "sanbi-orange" =  "#FAA755",
  "sanbi-purple" = "#9D85BE",
  "Freshwater" = "#4097C2",
  "Marine" = "#02268A",
  "Coast" = "#DEA004",
  "Estuarine" = "#028A85",
  "Terrestrial"=  "#617016",
  "Genetics" =  "#DE2104",
  "PEI" =  "#481C66"

)

NBA_colours <- NBA_colours[!duplicated(names(NBA_colours))]
##map

NBA_example_map_data <- sf::st_read(dir("data-raw",
                   "Marine_Ecosystem_Map_2023_final_pelagic_only.gpkg",
                   full.names = T,
                   recursive = T))%>%
  group_by(P_EcosysType)%>%
  summarise(geometry = st_union(geom)) %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  ungroup() %>%
  mutate(protection_level = case_when(
    P_EcosysType== "Agulhas Retroflection and South Ocean Transitional Water" ~ "Well Protected",
    P_EcosysType== "Cold Southeast Atlantic Water" ~ "Well Protected",
    P_EcosysType== "Indian Ocean Frontal Margin Water" ~ "Moderately Protected",
    P_EcosysType== "Indian Ocean Frontal Water" ~ "Moderately Protected",
    P_EcosysType== "South Atlantic Productive Margin Water" ~ "Poorly Protected",
    P_EcosysType== "South Atlantic-Benguela Transitional Waters" ~ "Well Protected",
    P_EcosysType== "Stable Agulhas Current Water" ~ "Moderately Protected",
    P_EcosysType== "Stable Indian Ocean Water" ~ "Poorly Protected",
    P_EcosysType== "Stable Southeast Atlantic Water" ~ "Poorly Protected",
    P_EcosysType== "Upwelled Agulhas Current Margin Water" ~ "Not Protected",
    P_EcosysType== "Variable Agulhas current core" ~ "Moderately Protected",
    P_EcosysType== "Variable Indo-Atlantic Water" ~ "Well Protected",
    P_EcosysType== "Warm Stable Indian Ocean Water" ~ "Well Protected"

  ))%>%
  dplyr::mutate(threat_status = dplyr::case_when(
    P_EcosysType== "Agulhas Retroflection and South Ocean Transitional Water" ~ "Critically Endangered",
    P_EcosysType== "Cold Southeast Atlantic Water" ~ "Endangered",
    P_EcosysType== "Indian Ocean Frontal Margin Water" ~ "Vulnerable",
    P_EcosysType== "Indian Ocean Frontal Water" ~ "Near Threatened",
    P_EcosysType== "South Atlantic Productive Margin Water" ~ "Least Concern",
    P_EcosysType== "South Atlantic-Benguela Transitional Waters" ~ "Vulnerable",
    P_EcosysType== "Stable Agulhas Current Water" ~ "Critically Endangered",
    P_EcosysType== "Stable Indian Ocean Water" ~ "Critically Endangered",
    P_EcosysType== "Stable Southeast Atlantic Water" ~ "Endangered",
    P_EcosysType== "Upwelled Agulhas Current Margin Water" ~ "Vulnerable",
    P_EcosysType== "Variable Agulhas current core" ~ "Least Concern",
    P_EcosysType== "Variable Indo-Atlantic Water" ~ "Least Concern",
    P_EcosysType== "Warm Stable Indian Ocean Water" ~ "Near Threatened"

  ))



##bubble plot

NBA_example_bubble_data <- read.csv(
  dir("data-raw",
      "nba_bubble_plot_example_data.csv",
      full.names = T,
      recursive = T))



### turn into correct format
usethis::use_data(NBA_example_thr_data)
usethis::use_data(NBA_example_pro_data)
usethis::use_data(NBA_example_comb_data)
usethis::use_data(NBA_categories, overwrite = TRUE)
usethis::use_data(NBA_example_map_data, overwrite = TRUE)
usethis::use_data(NBA_example_bubble_data, overwrite = TRUE)
usethis::use_data(NBA_colours, overwrite = TRUE)
usethis::use_data(NBA_example_press_bar_data)
usethis::use_data(NBA_example_rlis_data)
usethis::use_data(NBA_example_rlie_data)
usethis::use_data(NBA_example_epli2018_data)
usethis::use_data(NBA_example_epli2024_data)



##create a folder for this script
#usethis::use_data_raw()

#####################################################################################
### unload packages

# detach("package:xxx", unload=TRUE)

