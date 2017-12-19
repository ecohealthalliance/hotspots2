load_all()

library(dismo)
library(gbm)
library(purrr)
library(viridis)


data(drivers_full)

model_name <- "bsm_1000_iter"
current_out_dir <- file.path(out_dir(), "additional_figures")

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
predictions <- select(predictions, gridid, lon, lat,
                      bsm_response, weight_pubs, weight_pop, bsm_weight_pubs, bsm_weight_pop)

quickmap(predictions, quantvar(weight_pubs - weight_pop))


quickmap(predictions, log(bsm_weight_pop))

quickmap(predictions, quantvar(bsm_weight_pubs))
quickmap(predictions, quantvar(bsm_weight_pop))


# Rescale so they sum to 1 over the study area, then export to raster.
predictions_resc <- predictions

predictions_resc$bsm_response <- predictions_resc$bsm_response * 1 / sum(predictions_resc$bsm_response, na.rm = TRUE)
predictions_resc$bsm_weight_pubs <- predictions_resc$bsm_weight_pubs * 1 / sum(predictions_resc$bsm_weight_pubs, na.rm = TRUE)
predictions_resc$bsm_weight_pop <- predictions_resc$bsm_weight_pop * 1 / sum(predictions_resc$bsm_weight_pop, na.rm = TRUE)

bsm_response <- predictions_resc %>%
  select(x = lon, y = lat, z = bsm_response) %>%
  rasterFromXYZ(crs = crs(template_raster()))
names(bsm_response) <- "bsm_response"
writeRaster(bsm_response, filename = "inst/out/raster/bsm_response.tif", overwrite = TRUE)

bsm_weight_pubs <- predictions_resc %>%
  select(x = lon, y = lat, z = bsm_weight_pubs) %>%
  rasterFromXYZ(crs = crs(template_raster()))
names(bsm_weight_pubs) <- "bsm_weight_pubs"
writeRaster(bsm_weight_pubs, filename = "inst/out/raster/bsm_weight_pubs.tif", overwrite = TRUE)

bsm_weight_pop <- predictions_resc %>%
  select(x = lon, y = lat, z = bsm_weight_pop) %>%
  rasterFromXYZ(crs = crs(template_raster()))
names(bsm_weight_pop) <- "bsm_weight_pop"
writeRaster(bsm_weight_pop, filename = "inst/out/raster/bsm_weight_pop.tif", overwrite = TRUE)

library(readr)
write_csv(predictions_resc, path = "inst/out/raster/hotspots2_predictions.csv")

# To make this map really work, I should convert to a raster, upscale, and clip.
quickmap(drivers_full, pop) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE) +
  theme_black_nothing()



# Nice maps!
# This whole workflow does a nice plot of a variable.

##### Can just come here run #####

clip_at_sd <- function(x, multiple = 1) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE) * multiple
  y <- x %>%
    pmin(m + s) %>%
    pmax(m - s)
  return(y)
}

data(predictions)
predictions <- select(predictions, gridid, lon, lat,
                    bsm_response, weight_pubs, weight_pop, bsm_weight_pubs, bsm_weight_pop)

# Output for bsm_weight_pubs
pretty <- predictions %>%
  select(x = lon, y = lat, z = bsm_weight_pubs) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(2, method = "bilinear") %>%
  mask(mask = country_outlines) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(pretty[["z"]]))

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "white", size = 0.15) +
  coord_fixed() +
  scale_fill_gradientn(colours = viridis(numcolors),
                       guide = guide_colorbar()) +
  theme_black_legend() +
  theme(legend.title = element_blank(),
        legend.title.align = 0,
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.975, 0.5)) +
  labs(x = NULL, y = NULL)

ggsave(file.path(current_out_dir, "map_bsm_weight_pubs.pdf"), height = 4.25, width = 9)
ggsave(file.path(current_out_dir, "map_bsm_weight_pubs.png"), height = 4.25, width = 9)



# Output for bsm_weight_pop
pretty <- predictions %>%
  select(x = lon, y = lat, z = bsm_weight_pop) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(2, method = "bilinear") %>%
  mask(mask = country_outlines) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(pretty[["z"]]))

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "white", size = 0.15) +
  coord_fixed() +
  scale_fill_gradientn(colours = viridis(numcolors),
                       guide = guide_colorbar()) +
  theme_black_legend() +
  theme(legend.title = element_blank(),
        legend.title.align = 0,
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.975, 0.5)) +
  labs(x = NULL, y = NULL)

ggsave(file.path(current_out_dir, "map_bsm_weight_pop.pdf"), height = 4.25, width = 9)
ggsave(file.path(current_out_dir, "map_bsm_weight_pop.png"), height = 4.25, width = 9)




# Output for map_bsm_response
# Version with no labels
pretty <- predictions %>%
  select(x = lon, y = lat, z = bsm_response) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(2, method = "bilinear") %>%
  mask(mask = country_outlines) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(pretty[["z"]]))

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "white", size = 0.15) +
  coord_fixed() +
  ylim(-65, 90) +
  scale_fill_gradientn(colours = viridis(numcolors),
                       guide = guide_colorbar()) +
  theme_black_legend() +
  theme(legend.title = element_blank(),
        legend.title.align = 0,
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.975, 0.5)) +
  labs(x = NULL, y = NULL)

ggsave(file.path(current_out_dir, "map_bsm_response.pdf"), height = 4.25, width = 9)
ggsave(file.path(current_out_dir, "map_bsm_response.png"), height = 4.25, width = 9)



# Output for map_bsm_response_labeled
##### Version with labels #####
pretty <- predictions %>%
  select(x = lon, y = lat, z = bsm_response) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(2, method = "bilinear") %>%
  mask(mask = country_outlines) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(pretty[["z"]]))

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "white", size = 0.15) +
  coord_fixed() +
  ylim(-65, 90) +
  scale_fill_gradientn(colours = viridis(numcolors),
                       guide = guide_colorbar(label = TRUE,
                                              label.position = "right",
                                              title = "Event probability\n(relative to\nreporting effort)")) +
  theme_black_legend() +
  theme(legend.title = element_text(color = "white", size = 8),
        legend.text = element_text(color = "white", size = 8),
        legend.title.align = 0,
        legend.background = element_blank(),
        legend.position = c(0.11, 0.45)) +
  labs(x = NULL, y = NULL)

ggsave(file.path(current_out_dir, "map_bsm_response_labeled.pdf"), height = 4.25, width = 9)
ggsave(file.path(current_out_dir, "map_bsm_response_labeled.png"), height = 4.25, width = 9)






# Output for bsm_weight_pop_large
# Large version for EHA gala
pretty <- predictions %>%
  select(x = lon, y = lat, z = bsm_weight_pop) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(16, method = "bilinear") %>%
  mask(mask = country_outlines) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(pretty[["z"]]))

big_map <- ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "white", size = 0.15) +
  coord_fixed() +
  scale_fill_gradientn(colours = viridis(numcolors),
                       guide = guide_colorbar()) +
  theme_black_legend() +
  theme(legend.title = element_blank(),
        legend.title.align = 0,
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.975, 0.5)) +
  labs(x = NULL, y = NULL)

ggsave(file.path(current_out_dir, "map_bsm_weight_pop_large.pdf"), plot = big_map, height = 4.25, width = 9)
ggsave(file.path(current_out_dir, "map_bsm_weight_pop_large.png"), plot = big_map, height = 4.25, width = 9, dpi = 1600)


