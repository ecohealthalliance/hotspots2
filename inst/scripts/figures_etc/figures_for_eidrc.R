load_all()
library(viridis)
library(RColorBrewer)

data(predictions)

predictions <- select(predictions, gridid, lon, lat,
                      bsm_response, weight_pubs, weight_pop, bsm_weight_pubs, bsm_weight_pop)


clip_at_sd <- function(x, multiple = 1) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE) * multiple
  y <- x %>%
    pmin(m + s) %>%
    pmax(m - s)
  return(y)
}

spectral <- function(numcolors) {
  rev(colorRampPalette(brewer.pal(11, "Spectral"))(numcolors))
}

eidrc_theme <- function(base_size = 10, base_family = "Helvetica") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      line = element_blank(),
      # text = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "#76afd6", color  =  NA),
      legend.position = "none",
      # legend.text = element_text(size = 8, color = "white")
      # axis.ticks = element_text
    )
}

out_dir <- "inst/out/eidrc_maps"

xmin <- 79
xmax <- 139
ymin <- -12
ymax <- 28

# Output for bsm_weight_pop_large
# Large version for EHA gala
pretty <- predictions %>%
  select(x = lon, y = lat, z = bsm_response) %>%
  filter(x > xmin, x < xmax, y > ymin, y < ymax) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(8, method = "bilinear") %>%
  mask(mask = country_outlines) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(pretty[["z"]]))


# Inferno

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = inferno(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "white", size = 0.5) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  scale_fill_gradientn(colours = inferno(numcolors),
                       guide = guide_colorbar()) +
  eidrc_theme() +
  labs(x = NULL, y = NULL)

ggsave(file.path(out_dir, "eidrc_bsm_response_inferno.pdf"), height = 4, width = 6)
ggsave(file.path(out_dir, "eidrc_bsm_response_inferno.png"), height = 4, width = 6, dpi = 800)


# Viridis

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "white", size = 0.5) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  scale_fill_gradientn(colours = viridis(numcolors),
                       guide = guide_colorbar()) +
  eidrc_theme() +
  labs(x = NULL, y = NULL)

ggsave(file.path(out_dir, "eidrc_bsm_response_viridis.pdf"), height = 4, width = 6)
ggsave(file.path(out_dir, "eidrc_bsm_response_viridis.png"), height = 4, width = 6, dpi = 800)



# Spectral

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = spectral(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "black", size = 0.5) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  scale_fill_gradientn(colours = spectral(numcolors),
                       guide = guide_colorbar()) +
  eidrc_theme() +
  labs(x = NULL, y = NULL)

ggsave(file.path(out_dir, "eidrc_bsm_response_spectral.pdf"), height = 4, width = 6)
ggsave(file.path(out_dir, "eidrc_bsm_response_spectral.png"), height = 4, width = 6, dpi = 800)
