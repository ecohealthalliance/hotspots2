load_all()

library(raster)
library(rgdal)
library(dplyr)
library(foreach)
library(doParallel)

registerDoParallel()

# This script loads the shapefiles for EID events and overlays them on the grid.

template_raster <- template_raster()

shapefiles <- list.files(system.file("data-raw", "Maps_Unprojected", package = "hotspots2"), pattern = "HED_[0-9]+.shp", full.names = TRUE)



event_coverage <- foreach(shapefile = shapefiles, .combine = rbind) %dopar% {
  layer <- strsplit(basename(shapefile), ".", fixed = TRUE)[[1]][1]
  print(layer)
  event_polygon <- readOGR(dsn = shapefile, layer = layer, verbose = FALSE)
  coverage <- raster::extract(template_raster, event_polygon,
                              weights = TRUE, df = TRUE)
  if (nrow(coverage) > 0) {
    coverage <- coverage %>%
      mutate(event_name = layer) %>%
      select(gridid = GridID, event_name, coverage = weight)
  }
  print(coverage)
  coverage
}

save(event_coverage, file = file.path(data_dir(), "event_coverage.RData"))
