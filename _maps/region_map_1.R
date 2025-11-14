
region_responses <- read.csv(file = "_www/df_exports/responses_by_region_filtered.csv")
mt <- all_sf$MT_boundary
counties <- all_sf$MT_counties

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


region_map <- 
  
  ggplot() + 
  geom_sf(data = regions,  color = NA, fill = "#bfddf2") + 
  geom_sf(data = counties, fill = NA, color = alpha("#688ba4", 0.25))+
  geom_sf(data = regions, fill = NA, color = "#86b2d3", linewidth = 1.2) + 
  geom_sf(data = regions, fill = NA, color = "#688ba4", linewidth = 0.6) + 
  
  theme_void() + 
  theme(
    legend.position = "none"
  )


region_map_pnts <- 
  
  ggplot() + 
    geom_sf(data = regions,  color = NA, fill = "#bfddf2") + 
    geom_sf(data = counties, fill = NA, color = alpha("#688ba4", 0.25))+
  geom_sf(data = regions, fill = NA, color = "#86b2d3", linewidth = 1.2) + 
    geom_sf(data = regions, fill = NA, color = "#688ba4", linewidth = 0.6) + 
  
  geom_sf(data = region_points, aes(size = pct_respon), color = "#1d376c")+
  scale_size_continuous(range = c(10, 30), trans = "sqrt") +
    
    theme_void() + 
    theme(
      legend.position = "none"
    )




ggsave(
  filename = here("_www/map_exports/region_map_pnts.png"),
  plot = region_map_pnts,
  width = 12, 
  height = 8,
  units = "in",
  dpi = 900
)
