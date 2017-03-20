load_all()

current_out_dir <- file.path(out_dir(), "additional_figures")
dir.create(current_out_dir, showWarnings = FALSE)

library(ggplot2)
library(GGally)
library(dplyr)
library(viridis)

load_all()

load("cache/drivers_orig.RData")
data(drivers)

pubs <- drivers %>%
  select(gridid, lon, lat, iso3, pubs_fit) %>%
  left_join(select(drivers_orig, gridid, pubs_jones))

quickmap(pubs, pubs_jones)
quickmap(pubs, pubs_fit)



clip_at_sd <- function(x, multiple = 1) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE) * multiple
  y <- x %>%
    pmin(m + s) %>%
    pmax(m - s)
  return(y)
}

pretty_jones <- pubs %>%
  select(x = lon, y = lat, z = pubs_jones) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(2, method = "bilinear") %>%
  mask(mask = country_outlines) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(pretty_jones[["z"]]))

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = "#440154FF") +
  geom_raster(aes(x = x, y = y, fill = log(z)), data = pretty_jones) + 
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "white", size = 0.15) +
  coord_fixed() +
  scale_fill_gradientn(colours = viridis(numcolors),
                       guide = guide_colorbar(title = "Publications")) +
  theme_black_nothing() +
  labs(x = NULL, y = NULL)

ggsave(file.path(current_out_dir, "pubs_jones.pdf"), height = 4.5, width = 9)
ggsave(file.path(current_out_dir, "pubs_jones.png"), height = 4.5, width = 9)


pretty_pubcrawler <- pubs %>%
  select(x = lon, y = lat, z = pubs_fit) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(2, method = "bilinear") %>%
  mask(mask = country_outlines) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(pretty_pubcrawler[["z"]]))

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = "#440154FF") +
  geom_raster(aes(x = x, y = y, fill = log(z)), data = pretty_pubcrawler) + 
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "white", size = 0.15) +
  coord_fixed() +
  scale_fill_gradientn(colours = viridis(numcolors),
                       guide = guide_colorbar(title = "Publications")) +
  theme_black_nothing() +
  labs(x = NULL, y = NULL)



# New attempt with pasted code

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = z), data = pretty_pubcrawler) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "white", size = 0.15) +
  coord_fixed() +
  ylim(-65, 90) +
  scale_fill_gradientn(colours = viridis(numcolors),
                       guide = guide_colorbar(label = TRUE,
                                              label.position = "right",
                                              title = "Predicted number of publications")) +
  theme_black_legend() +
  theme(legend.title = element_text(color = "white", size = 8),
        legend.text = element_text(color = "white", size = 8),
        legend.title.align = 0,
        legend.background = element_blank(),
        legend.position = c(0.11, 0.45)) +
  labs(x = NULL, y = NULL)

ggsave(file.path(current_out_dir, "pubs_fit.pdf"), height = 4.5, width = 9)
ggsave(file.path(current_out_dir, "pubs_fit.png"), height = 4.5, width = 9)