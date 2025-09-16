# === Bring in shapefiles === #

bring_in_shapefile_list <- function(folder = "_data/shapefiles") {
  
  shapefiles <- c(
    MT_counties             = "MT_counties/MT_counties.shp",
    MT_boundary             = "MT_boundary/MT_boundary.shp",
    inverse_MT              = "inverse_MT/inverse_MT.shp",
    MT_counties_centroids   = "MT_counties_centroids/MT_counties_centroids.shp",
    ampho_regions           = "AMPHO_Regions/AMPHO_Regions.shp"
  )
  
  purrr::imap(shapefiles, function(file_path, obj_name) {
    st_read(here(folder, file_path), quiet = TRUE) %>%
      st_transform(crs = 32100)
  })
}
