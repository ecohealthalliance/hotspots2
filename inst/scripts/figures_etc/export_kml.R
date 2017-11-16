load_all()

library(dismo)
library(gbm)
library(purrr)
library(viridis)
# library(plotKML)

data(predictions)
predictions <- select(predictions, gridid, lon, lat, bsm_weight_pop)

clip_at_sd <- function(x, multiple = 1) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE) * multiple
  y <- x %>%
    pmin(m + s) %>%
    pmax(m - s)
  return(y)
}

predictions$bsm_weight_pop_clipped <- clip_at_sd(predictions$bsm_weight_pop)

bsm_weight_pop <- predictions %>%
  select(x = lon, y = lat, z = bsm_weight_pop_clipped) %>%
  rasterFromXYZ(crs = crs(template_raster()))
names(bsm_weight_pop) <- "bsm_weight_pop"



KML(bsm_weight_pop,
    filename = "inst/out/raster/bsm_weight_pop.kml",
    col = viridis(n_distinct(predictions$bsm_weight_pop_clipped)))



plotkml

# Rescale so they sum to 1 over the study area, then export to raster.
predictions_resc <- predictions

predictions_resc$bsm_weight_pop <- predictions_resc$bsm_weight_pop * 1 / sum(predictions_resc$bsm_weight_pop, na.rm = TRUE)

bsm_weight_pop <- predictions_resc %>%
  select(x = lon, y = lat, z = bsm_weight_pop_clipped) %>%
  rasterFromXYZ(crs = crs(template_raster()))
names(bsm_weight_pop) <- "bsm_weight_pop"
writeRaster(bsm_weight_pop, filename = "inst/out/raster/bsm_weight_pop.tif")

