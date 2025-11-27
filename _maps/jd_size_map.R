


##-- prep for health dept. points
mt_hd_pnts <- all_sf$mt_hd_pnts %>%
  mutate(
    hd_lbl = str_replace(
      hd,
      "(County|City[-–]County|Health|Public|Tribal|Board).*",
      ""
    ) %>% str_squish(),
    
    hd_lbl = case_when(
      
      hd == "Central MT Health District Public Health" ~ "Central MT Health District",
      hd == "Custer County Public Health Department One Health" ~ "Custer / One Health", 
      type == "Tribal Health Department" ~ paste0(hd_lbl, " THD"), 
      TRUE ~ hd_lbl
    ),
    
    hd_lbl = str_wrap(hd_lbl, width = 10),
    
    hd_cat = factor(hd_cat, levels = c("Frontier", "Small", "Medium", "Large"))

  )

st_write(mt_hd_pnts, "_data/shapefiles/mt_hd_pnts/mt_hd_pnts.shp", append = FALSE)


##-- simplify ampho region geometry
regions <- all_sf$ampho_regions %>%
  st_make_valid()%>%
  mutate(region = 1:5) %>%
  st_simplify(dTolerance = 1000) 

st_write(regions, "_data/shapefiles/AMPHO_Regions/AMPHO_Regions.shp", append = FALSE)









  


 
  

regions <- all_sf$ampho_regions %>%
  st_make_valid() %>%
  mutate(region = 1:5) %>%
  left_join(region_responses, by = "region") %>%
  mutate(region = factor(region))

region_points <- st_centroid(regions)

custom_colors1 <- c("#1d376c", "#4e79d2", "#8db7e1")
custom_colors2 <- c("#cee6ff", "#b6d4f2", "#8db7e1")


custom_colors3 <- c(
  "#86b2d3",
  "#95c6ea", 
  "#aad1ee",
  "#bfddf2",
  "#d5e8f7"
)