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