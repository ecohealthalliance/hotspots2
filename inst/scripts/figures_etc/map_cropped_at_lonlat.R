load_all()
library(viridis)
library(RColorBrewer)

# Variables
#-----------

# Edit these to change the output of the function.

OUT_DIR <- "inst/out/maps_2019-11-06"
OUT_FILENAME <- "armenia-azerbaijan-georgia-map" # Without file extension

xmin <- 41 - 2.5
xmax <- 50 + 1.5
ymin <- 38 - 1
ymax <- 43 + 2

# Setting this to 1 will output the map at its native resolution. A higher
# factor will increase the map that size, smoothing with bilinear filtering.
upscaling_factor <- 8

# Names of countries you want to plot. Set to NA to plot everything
# filter_countries <- NA
filter_countries <- c("Azerbaijan", "Georgia", "Armenia")

legend_position <- "left"


# Script
#--------

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

data(predictions)
data(country_outlines)

 # Variables

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
      legend.position = legend_position,
      # legend.text = element_text(size = 8, color = "white")
      # axis.ticks = element_text
    )
}

pretty <- predictions %>%
  select(x = lon, y = lat, z = bsm_response) %>%
  filter(x > xmin, x < xmax, y > ymin, y < ymax) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(upscaling_factor, method = "bilinear") %>%
  mask(mask = country_outlines)

if (is_character(filter_countries)) {
  filter_outlines <- country_outlines[country_outlines$country %in% filter_countries, ]
  pretty <- mask(pretty, mask = filter_outlines, updatevalue = NA)
}

pretty %<>%
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
                       guide = guide_colorbar(), name = "EID Risk Index") +
  eidrc_theme() +
  labs(x = NULL, y = NULL)

ggsave(file.path(OUT_DIR, paste0(OUT_FILENAME, "_inferno.pdf")), height = 4, width = 6)
ggsave(file.path(OUT_DIR, paste0(OUT_FILENAME, "_inferno.png")), height = 4, width = 6, dpi = 800)


# Viridis

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "white", size = 0.5) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  scale_fill_gradientn(colours = viridis(numcolors),
                       guide = guide_colorbar(), name = "EID Risk Index") +
  eidrc_theme() +
  labs(x = NULL, y = NULL)

ggsave(file.path(OUT_DIR, paste0(OUT_FILENAME, "_viridis.pdf")), height = 4, width = 6)
ggsave(file.path(OUT_DIR, paste0(OUT_FILENAME, "_viridis.png")), height = 4, width = 6, dpi = 800)



# Spectral

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, fill = spectral(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world(), inherit.aes = FALSE, color = "black", size = 0.5) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  scale_fill_gradientn(colours = spectral(numcolors),
                       guide = guide_colorbar(), name = "EID Risk Index") +
  eidrc_theme() +
  labs(x = NULL, y = NULL)

ggsave(file.path(OUT_DIR, paste0(OUT_FILENAME, "_spectral.pdf")), height = 4, width = 6)
ggsave(file.path(OUT_DIR, paste0(OUT_FILENAME, "_spectral.png")), height = 4, width = 6, dpi = 800)
