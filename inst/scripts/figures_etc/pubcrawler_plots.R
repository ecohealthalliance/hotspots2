load_all()

current_out_dir <- file.path(out_dir(), "additional_figures")
dir.create(current_out_dir, showWarnings = FALSE)

library(ggplot2)
library(GGally)
library(dplyr)
library(viridis)


library(pubcrawler2hotspots)

data(pubs_hires)
data(pubs_superhires)

prettypubs <- pubs_superhires %>%
  select(x = lon, y = lat, z = w1b) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(2, method = "bilinear") %>%
  mask(mask = country_outlines) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(prettypubs[["z"]]))

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = "#440154FF") +
  geom_raster(aes(x = x, y = y, fill = quantvar(z)), data = prettypubs) + 
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "white", size = 0.15) +
  coord_fixed() +
  scale_fill_gradientn(colours = viridis(numcolors),
                       guide = guide_colorbar(title = "Predicted\nPublications")) +
  theme_black_legend() +
  labs(x = NULL, y = NULL)

ggsave(file.path(current_out_dir, "pubcrawler_hires.pdf"), height = 4.5, width = 9)
ggsave(file.path(current_out_dir, "pubcrawler_hires.png"), height = 4.5, width = 9)

quickmap(prettypubs, quantvar(z), pal_fun = "viridis") +
  geom_polygon(aes(x = long, y = lat, group = group), data = map_data("world"), inherit.aes = FALSE, color = "white", fill = "#440154FF", size = 0.1) +
  theme_black_nothing()

  viridis(1)