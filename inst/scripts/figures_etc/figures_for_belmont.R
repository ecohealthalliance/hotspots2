load_all()
library(viridis)
library(RColorBrewer)

data(predictions)

predictions <- select(predictions, gridid, lon, lat,
                      bsm_response, weight_pubs, weight_pop, bsm_weight_pubs, bsm_weight_pop)


spectral <- function(numcolors) {
  rev(colorRampPalette(brewer.pal(11, "Spectral"))(numcolors))
}

belmont_theme <- function(base_size = 10, base_family = "Helvetica",
                        ocean = c("blue", "clear")) {
  if (ocean == "blue") {
    bg = element_rect(fill = "#76afd6", color  =  NA)
  } else if (ocean == "clear") {
    bg = element_blank()
  }
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      line = element_blank(),
      # text = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = bg,
      legend.position = "clear",
      # legend.text = element_text(size = 8, color = "white")
      # axis.ticks = element_text
    )
}

out_dir <- "inst/out/maps_for_belmont"

# Output for bsm_weight_pop_large
# Large version for EHA gala
map_data <- predictions %>%
  select(x = lon, y = lat, z = bsm_response) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(4, method = "bilinear") %>%
  mask(mask = country_outlines) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(map_data[["z"]]))


# Inferno

inferno_blue <- ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = inferno(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = map_data) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "white", size = 0.25) +
  coord_fixed(expand = FALSE) +
  scale_fill_gradientn(colours = inferno(numcolors),
                       guide = guide_colorbar()) +
  belmont_theme(ocean = "blue") +
  labs(x = NULL, y = NULL)

ggsave(file.path(out_dir, "hotspots_inferno_blue.pdf"), height = 6.5, width = 9)

ggsave(file.path(out_dir, "hotspots_inferno_blue.png"), height = 6.5, width = 9, dpi = 300)

inferno_clear <- ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = inferno(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = map_data) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "black", size = 0.25) +
  coord_fixed(expand = FALSE) +
  scale_fill_gradientn(colours = inferno(numcolors),
                       guide = guide_colorbar()) +
  belmont_theme(ocean = "clear") +
  labs(x = NULL, y = NULL)

ggsave(file.path(out_dir, "hotspots_inferno_clear.pdf"), height = 6.5, width = 9)

ggsave(file.path(out_dir, "hotspots_inferno_clear.png"), height = 6.5, width = 9, dpi = 300)


# Viridis

viridis_blue <- ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = map_data) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "white", size = 0.25) +
  coord_fixed(expand = FALSE) +
  scale_fill_gradientn(colours = viridis(numcolors),
                       guide = guide_colorbar()) +
  belmont_theme(ocean = "blue") +
  labs(x = NULL, y = NULL)

ggsave(file.path(out_dir, "hotspots_viridis_blue.pdf"), height = 6.5, width = 9)

ggsave(file.path(out_dir, "hotspots_viridis_blue.png"), height = 6.5, width = 9, dpi = 300)

viridis_clear <- ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = map_data) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "black", size = 0.25) +
  coord_fixed(expand = FALSE) +
  scale_fill_gradientn(colours = viridis(numcolors),
                       guide = guide_colorbar()) +
  belmont_theme(ocean = "clear") +
  labs(x = NULL, y = NULL)

ggsave(file.path(out_dir, "hotspots_viridis_clear.pdf"), height = 6.5, width = 9)

ggsave(file.path(out_dir, "hotspots_viridis_clear.png"), height = 6.5, width = 9, dpi = 300)


# Spectral

spectral_blue <- ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = spectral(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = map_data) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "white", size = 0.25) +
  coord_fixed(expand = FALSE) +
  scale_fill_gradientn(colours = spectral(numcolors),
                       guide = guide_colorbar()) +
  belmont_theme(ocean = "blue") +
  labs(x = NULL, y = NULL)

ggsave(file.path(out_dir, "hotspots_spectral_blue.pdf"), height = 6.5, width = 9)

ggsave(file.path(out_dir, "hotspots_spectral_blue.png"), height = 6.5, width = 9, dpi = 300)

spectral_clear <- ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = spectral(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = map_data) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "black", size = 0.25) +
  coord_fixed(expand = FALSE) +
  scale_fill_gradientn(colours = spectral(numcolors),
                       guide = guide_colorbar()) +
  belmont_theme(ocean = "clear") +
  labs(x = NULL, y = NULL)

ggsave(file.path(out_dir, "hotspots_spectral_clear.pdf"), height = 6.5, width = 9)

ggsave(file.path(out_dir, "hotspots_spectral_clear.png"), height = 6.5, width = 9, dpi = 300)