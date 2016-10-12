template_raster <- function() {
  library(raster)
  library(sp)

  template_raster <- read.table(system.file("data-raw", "eid08_drivers_19OCT11.txt", package = "hotspots2"), header=TRUE, sep="\t")
  template_raster <- template_raster[, c("lon", "lat", "GridID")]
  coordinates(template_raster) <- c("lon", "lat")
  gridded(template_raster) <- TRUE
  proj4string(template_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  template_raster <- raster(template_raster)
  return(template_raster)
}

.template_df <- function(save_to_data = FALSE) {
  template_df <- read.table(system.file("data-raw", "eid08_drivers_19OCT11.txt", package = "hotspots2"), header=TRUE, sep="\t") %>%
    select(gridid = GridID, lon, lat)
  if (save_to_data) {
    save(template_df, file = file.path(data_dir(), "template_df.RData"))
  } else {
    return(template_df)
  }
}