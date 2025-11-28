mt <- all_sf$MT_boundary
mt_cntys <- all_sf$MT_counties
tribes  <- all_sf$tribal_nations %>% select(NAME, NAMELSAD)
mt_hd_pnts <- all_sf$mt_hd_pnts
regions <- all_sf$ampho_regions %>% mutate(region = factor(region))

train_locs <- read.csv(file = "_data/df_exports/train_locs_rgn.csv")

train_locs_pnts <- train_locs %>%
  st_as_sf(
    coords = c("lng", "lat"),  
    crs    = 4326               
  ) %>%
  st_transform(crs = st_crs(mt))   


m_train_locs <- 
ggplot() + 
  
  geom_sf(data = mt, fill = NA, color = "lightgrey") + 
  geom_sf(data = train_locs_pnts, size = 6) + 
  theme_void()+
  theme(
    plot.background  = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )




##################
ggsave(
  filename = here("_www/map_exports/m_train_locs.png"),
  plot = m_train_locs,
  width = 12, 
  height = 8,
  units = "in",
  dpi = 300
)
