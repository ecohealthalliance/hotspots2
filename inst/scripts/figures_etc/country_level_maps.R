load_all()

library(dismo)
library(gbm)
library(purrr)
library(viridis)
library(readr)

current_out_dir <- "inst/out/subset_maps"

options(stringsAsFactors = FALSE)
country_outlines <- readOGR("data-raw/shapes_simplified_low/shapes_simplified_low.json", layer = "OGRGeoJSON")
country_info <- read_tsv("data-raw/GeoNames/countryInfo.txt", col_types = cols_only(Country = "c", Continent = "c", geonameid = "c"))
plotting_outlines <- map.world()


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



# Ethiopia
countries_to_plot <- "Ethiopia"

geonameids_to_plot <- country_info %>%
  filter(Country == countries_to_plot) %>%
  select(geonameid) %>%
  as.character()

plotting_mask <- country_outlines[country_outlines$geoNameId %in% geonameids_to_plot, ]

outlines_to_plot <- plotting_outlines[plotting_outlines$region %in% countries_to_plot, ]

# Output for bsm_weight_pop
pretty <- predictions %>%
  select(x = lon, y = lat, z = bsm_weight_pop) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(2, method = "bilinear") %>%
  mask(mask = plotting_mask) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(pretty[["z"]]))

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = outlines_to_plot, inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = outlines_to_plot, inherit.aes = FALSE, color = "white") +
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

ggsave(file.path(current_out_dir, paste0(countries_to_plot, "_reweighted.pdf")), height = 7, width = 7)
ggsave(file.path(current_out_dir, paste0(countries_to_plot, "_reweighted.png")), height = 7, width = 7)



# Burkina Faso
countries_to_plot <- "Burkina Faso"

geonameids_to_plot <- country_info %>%
  filter(Country == countries_to_plot) %>%
  select(geonameid) %>%
  as.character()

plotting_mask <- country_outlines[country_outlines$geoNameId %in% geonameids_to_plot, ]

outlines_to_plot <- plotting_outlines[plotting_outlines$region %in% countries_to_plot, ]

# Output for bsm_weight_pop
pretty <- predictions %>%
  select(x = lon, y = lat, z = bsm_weight_pop) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(2, method = "bilinear") %>%
  mask(mask = plotting_mask) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(pretty[["z"]]))

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = outlines_to_plot, inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = outlines_to_plot, inherit.aes = FALSE, color = "white") +
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

ggsave(file.path(current_out_dir, paste0(countries_to_plot, "_reweighted.pdf")), height = 7, width = 7)
ggsave(file.path(current_out_dir, paste0(countries_to_plot, "_reweighted.png")), height = 7, width = 7)



# Egypt
countries_to_plot <- "Egypt"

geonameids_to_plot <- country_info %>%
  filter(Country == countries_to_plot) %>%
  select(geonameid) %>%
  as.character()

plotting_mask <- country_outlines[country_outlines$geoNameId %in% geonameids_to_plot, ]

outlines_to_plot <- plotting_outlines[plotting_outlines$region %in% countries_to_plot, ]

# Output for bsm_weight_pop
pretty <- predictions %>%
  select(x = lon, y = lat, z = bsm_weight_pop) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(2, method = "bilinear") %>%
  mask(mask = plotting_mask) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(pretty[["z"]]))



ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = outlines_to_plot, inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = outlines_to_plot, inherit.aes = FALSE, color = "white") +
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

ggsave(file.path(current_out_dir, paste0(countries_to_plot, "_reweighted.pdf")), height = 7, width = 7)
ggsave(file.path(current_out_dir, paste0(countries_to_plot, "_reweighted.png")), height = 7, width = 7)



# Uganda
countries_to_plot <- "Uganda"

geonameids_to_plot <- country_info %>%
  filter(Country == countries_to_plot) %>%
  select(geonameid) %>%
  as.character()

plotting_mask <- country_outlines[country_outlines$geoNameId %in% geonameids_to_plot, ]

outlines_to_plot <- plotting_outlines[plotting_outlines$region %in% countries_to_plot, ]

# Output for bsm_weight_pop
pretty <- predictions %>%
  select(x = lon, y = lat, z = bsm_weight_pop) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(2, method = "bilinear") %>%
  mask(mask = plotting_mask) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(pretty[["z"]]))



ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = outlines_to_plot, inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = outlines_to_plot, inherit.aes = FALSE, color = "white") +
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

ggsave(file.path(current_out_dir, paste0(countries_to_plot, "_reweighted.pdf")), height = 7, width = 7)
ggsave(file.path(current_out_dir, paste0(countries_to_plot, "_reweighted.png")), height = 7, width = 7)



# Nigeria
countries_to_plot <- "Nigeria"

geonameids_to_plot <- country_info %>%
  filter(Country == countries_to_plot) %>%
  select(geonameid) %>%
  as.character()

plotting_mask <- country_outlines[country_outlines$geoNameId %in% geonameids_to_plot, ]

outlines_to_plot <- plotting_outlines[plotting_outlines$region %in% countries_to_plot, ]

# Output for bsm_weight_pop
pretty <- predictions %>%
  select(x = lon, y = lat, z = bsm_weight_pop) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(2, method = "bilinear") %>%
  mask(mask = plotting_mask) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(pretty[["z"]]))



ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = outlines_to_plot, inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = outlines_to_plot, inherit.aes = FALSE, color = "white") +
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

ggsave(file.path(current_out_dir, paste0(countries_to_plot, "_reweighted.pdf")), height = 7, width = 7)
ggsave(file.path(current_out_dir, paste0(countries_to_plot, "_reweighted.png")), height = 7, width = 7)



###################################



# Africa
african_countries <- country_info %>%
  filter(Continent == "AF")
geonameids_to_plot <- african_countries$geonameid
countries_to_plot <- african_countries$Country

plotting_mask <- country_outlines[country_outlines$geoNameId %in% geonameids_to_plot, ]

outlines_to_plot <- plotting_outlines[plotting_outlines$region %in% countries_to_plot, ]

# Output for bsm_weight_pop
pretty <- predictions %>%
  select(x = lon, y = lat, z = bsm_weight_pop) %>%
  rasterFromXYZ(crs = crs(template_raster())) %>%
  raster::disaggregate(2, method = "bilinear") %>%
  mask(mask = plotting_mask) %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

numcolors <- length(unique(pretty[["z"]]))

ggplot() +
  geom_polygon(aes(x = lon, y = lat, group = group), data = outlines_to_plot, inherit.aes = FALSE, fill = viridis(1)) +
  geom_raster(aes(x = x, y = y, fill = clip_at_sd(z, 2)), data = pretty) +
  geom_path(aes(x = lon, y = lat, group = group), data = outlines_to_plot, inherit.aes = FALSE, color = "white") +
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

ggsave(file.path(current_out_dir, "Africa_reweighted.pdf"), height = 7, width = 7)
ggsave(file.path(current_out_dir, "Africa_reweighted.png"), height = 7, width = 7)



