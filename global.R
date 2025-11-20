
# Load and install packages
pacman::p_load(
  dplyr, tidyr, ggplot2, lubridate, ggthemes, cowplot, readr, rlang,
  classInt, tidycensus, sf, here, stringr, purrr, svglite, rmapshaper,
  scales, ggrepel, viridis, RColorBrewer, maps, ggfx, glue, rlang, tidyselect,
  forcats, ggnewscale, sandwich
)



color_scale <- c("#95c6ea", "#3e5c58","#fcd008", "#a8b09d")

# Load global functions 
global_function_files <- list.files("_functions/", 
                                    full.names = TRUE, 
                                    pattern = "\\.R$",
                                    recursive = TRUE)
walk(global_function_files, ~ source(.x))




# Load in shapefile list object
all_sf <- bring_in_shapefile_list()



# Bring in and clean demographic data subset
dem_data <- read.csv(file = here("_data/demographics.csv")) 
#data_cleaned <- clean_raw_data(dem_data)
#write.csv(data_cleaned, "_data/data_cleaned.csv", row.names = TRUE)
data_cleaned <- read.csv(file = here("_data/data_cleaned.csv"))
