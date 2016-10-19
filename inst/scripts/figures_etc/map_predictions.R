load_all()

library(dismo)
library(gbm)
library(purrr)

data(drivers_full)

model_name <- "bsm_1000_iter"

load("cache/bsm_1000_iter/bsm_1000_iter.RData")

drivers_full$bsm_response <- predict_multibrt(bsm, drivers_full, type = "response", value = "mean")

predictions <- drivers_full %>%
  mutate(weight_pubs = pubs_fit / sum(pubs_fit, na.rm = TRUE),
         weight_pop = pop / sum(pop, na.rm = TRUE),
         bsm_weight_pubs = bsm_response * weight_pubs,
         bsm_weight_pop = bsm_response * weight_pop)

quickmap(predictions, bsm_response)
quickmap(predictions, weight_pubs)
quickmap(predictions, weight_pop)
quickmap(predictions, bsm_weight_pubs)
quickmap(predictions, bsm_weight_pop)

save(predictions, file = file.path(data_dir(), "predictions.RData"))
write.csv(predictions, file = "inst/out/predictions.csv", row.names = FALSE)
data(predictions)

quickmap(predictions, quantvar(weight_pubs - weight_pop))

quickmap(predictions, log(bsm_weight_pubs))
quickmap(predictions, log(bsm_weight_pop))

quickmap(predictions, quantvar(bsm_weight_pubs))
quickmap(predictions, quantvar(bsm_weight_pop))

map.world <- map_data(map = "world") %>%
  rename(lon = long)

ggplot(map.world, aes(x = long, y = lat, group = group, colour = region))

predictions <- select(predictions, gridid, lon, lat,
                      bsm_response, weight_pubs, weight_pop, bsm_weight_pubs, bsm_weight_pop)

bsm_weight_pubs <- predictions %>%
  select(x = lon, y = lat, z = bsm_weight_pubs) %>%
  rasterFromXYZ(crs = crs(template_raster()))
names(bsm_weight_pubs) <- "bsm_weight_pubs"
writeRaster(bsm_weight_pubs, filename = "inst/out/raster/bsm_weight_pubs.tif")

bsm_weight_pop <- predictions %>%
  select(x = lon, y = lat, z = bsm_weight_pop) %>%
  rasterFromXYZ(crs = crs(template_raster()))
names(bsm_weight_pop) <- "bsm_weight_pop"
writeRaster(bsm_weight_pop, filename = "inst/out/raster/bsm_weight_pop.tif")

# To make this map really work, I should convert to a raster, upscale, and clip.
quickmap(drivers_full, pop) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world, inherit.aes = FALSE) +
  theme_black()

country_outlines <- readOGR("data-raw/shapes_simplified_low/shapes_simplified_low.json", layer = "OGRGeoJSON")
r <- predictions %>%
  select(x = lon, y = lat, z = bsm_weight_pubs) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  disaggregate(4, method = "bilinear") %>%
  mask(mask = country_outlines) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

quickmap(r, log(z), pal_fun = "inferno") +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world, inherit.aes = FALSE,
            color = "white", size = 0.1) +
  theme_black_nothing()


# Try out raster_plot
r <- predictions %>%
  select(x = lon, y = lat, z = bsm_weight_pubs) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  # disaggregate(4, method = "bilinear") %>%
  mask(mask = country_outlines)

raster_plot(log(r), pal_fun = inferno)



