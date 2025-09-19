mt <- all_sf$MT_boundary
counties <- all_sf$MT_counties

regions <- all_sf$ampho_regions %>%
  st_make_valid() %>%
  mutate(
    region_cat = 
    case_when(
      NAME %in% c("Region 1", "Region 2") ~ "Region 1 and 2", 
      NAME %in% c("Region 3") ~ "Region 3", 
      NAME %in% c("Region 4", "Region 5") ~ "Region 4 and 5", 
      TRUE ~ NA_character_
    )
  )

custom_colors1 <- c("#1d376c", "#4e79d2", "#8db7e1")
custom_colors2 <- c("#cee6ff", "#b6d4f2", "#8db7e1")


# uses function saved as: 
### _functions/09_make_region_map.R
##### saves plots in: 
######## _www/map_exports/region_X_map.png
#################################################################
###############################
# individual region maps region 
region_1_plot <- make_region_map(regions, counties, "Region 1")
region_2_plot <- make_region_map(regions, counties, "Region 2")
region_3_plot <- make_region_map(regions, counties, "Region 3")
region_4_plot <- make_region_map(regions, counties, "Region 4")
region_5_plot <- make_region_map(regions, counties, "Region 5")

#################################### 
#################################################################
