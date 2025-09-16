mt <- all_sf$MT_boundary
counties <- all_sf$MT_counties

regions <- all_sf$ampho_regions %>%
  st_make_valid() 

custom_colors1 <- c("#1d376c", "#4e79d2", "#8db7e1")
custom_colors2 <- c("#cee6ff", "#b6d4f2", "#8db7e1")




region_map1 <- 
  
  ggplot() + 
    geom_sf(data = regions, aes(fill = region_color_cat), color = NA) + 
      scale_fill_manual(values = custom_colors2) +
    geom_sf(data = counties, fill = NA, color = alpha("black", 0.25))+
    geom_sf(data = regions, fill = NA, color = "black", linewidth = 0.65) + 
    
    theme_void() + 
    theme(
      legend.position = "none"
    )



ggsave(
  filename = here("www/map_exports/region_map1.png"),
  plot = region_map1,
  width = 12, 
  height = 8,
  units = "in",
  dpi = 900
)
