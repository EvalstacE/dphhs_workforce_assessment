

# Single-region map function -------------------------------
make_region_map <- function(regions_sf,
                            counties_sf,
                            region_name,
                            out_dir = here("_www/map_exports"),
                            width = 8, height = 8, dpi = 900,
                            fill_cnty = "#8db7e1",
                            line_cnty = "#1d376c",
                            region_fill = "#8db7e1",
                            region_line = "#5283b4",
                            save = TRUE) {
  # filter to selected region
  region_sf <- regions_sf %>%
    dplyr::filter(.data$NAME == region_name) %>%
    sf::st_make_valid()
  
  # clip counties to region
  region_counties <- counties_sf %>%
    sf::st_make_valid() %>%
    sf::st_intersection(region_sf)
  
  # build plot
  p <- ggplot() +
    geom_sf(data = region_counties,
            fill  = alpha(fill_cnty, 1),
            color = alpha(line_cnty, 0)) +
    theme_void() +
    theme(legend.position = "none")
  
  if (save) {
    # safe file name from region name
    safe <- str_to_lower(region_name) |>
      str_replace_all("[^a-z0-9]+", "_") |>
      str_replace("^_|_$", "")
    out_path <- glue("{out_dir}/{safe}_map.png")
    
    ggsave(filename = out_path, plot = p,
           width = width, height = height, units = "in", dpi = dpi)
  }
  
  return(p)
}


