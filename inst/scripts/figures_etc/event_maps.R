load_all()

current_out_dir <- file.path(out_dir(), "additional_figures")
dir.create(current_out_dir, showWarnings = FALSE)

library(ggplot2)
library(GGally)
library(dplyr)

data(drivers)
data(eid_metadata)
data(event_coverage)

# Set up some things for plotting
# country_outlines <- readOGR("data-raw/shapes_simplified_low/shapes_simplified_low.json", layer = "OGRGeoJSON")
# save(country_outlines, file = "data/country_outlines.RData")
map.world <- map_data(map = "world") %>%
  rename(lon = long)

# Quick and dirty function to weight
make_weight <- function(df, weighting_varname) {
  event_coverage %>%
    filter(event_name %in% df$name) %>%
    left_join(select_(drivers, "gridid", "lon", "lat", weighting_var = weighting_varname)) %>%
    group_by(event_name) %>%
    # only_if()(mutate)(weight = 1) %>%
    mutate(weight = coverage * weighting_var / sum(coverage * weighting_var, na.rm = TRUE),
           # We do this part to provide any weights where the publication value is NA.
           total_weight = sum(weight, na.rm = TRUE),
           weight = ifelse(total_weight == 0, coverage, weight)) %>%
    ungroup() %>%
    replace_na(replace = list(weight = 0, weighting_var = 0)) %>%
    group_by(gridid, lon, lat) %>%
    summarize(weight = sum(weight)) %>%
    ungroup()  
}

# # Define a function to upscale to raster
# prettyquick <- function(data, fill, method = "") {
#   data %>%
#     select_(x = "lon", y = "lat", z = fill) %>%
#     rasterFromXYZ(crs = crs(template_raster())) %>%
#     disaggregate(4, method = method) %>%
#     mask(mask = country_outlines) %>%
#     as.data.frame(xy = TRUE) %>%
#     na.omit()
# }


wz <- eid_metadata %>%
    filter(wildlife_zoonoses == 1) %>%
    select(name = eid_name,
           year = event_year)

wzpre70 <- eid_metadata %>%
    filter(wildlife_zoonoses == 1,
           event_year < 1970) %>%
    select(name = eid_name,
           year = event_year)

wzpst70 <- eid_metadata %>%
    filter(wildlife_zoonoses == 1,
           event_year >= 1970) %>%
    select(name = eid_name,
           year = event_year)


# Upscale them all
wz_upsc <- wz %>%
  make_weight("land_area") %>%
  prettyquick("weight", method = "")

wzpre70_upsc <- wzpre70 %>%
  make_weight("land_area") %>%
  prettyquick("weight", method = "")

wzpst70_upsc <- wzpst70 %>%
  make_weight("land_area") %>%
  prettyquick("weight", method = "")


# Plot and save them all
quickmap(wz_upsc, z, pal_fun = "inferno") +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world, inherit.aes = FALSE,
            color = "white", size = 0.1) +
  theme_black_nothing()
# ggsave(file.path(current_out_dir, "wildlife_zoonoses_unweighted.png"), height = 4.5, width = 9)
ggsave(file.path(current_out_dir, "wildlife_zoonoses_unweighted.pdf"), height = 4.5, width = 9)


quickmap(wzpre70_upsc, z, pal_fun = "inferno") +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world, inherit.aes = FALSE,
            color = "white", size = 0.1) +
  theme_black_nothing()
# ggsave(file.path(current_out_dir, "wildlife_zoonoses_pre-70_unweighted.png"), height = 4.5, width = 9)
ggsave(file.path(current_out_dir, "wildlife_zoonoses_pre-70_unweighted.pdf"), height = 4.5, width = 9)

quickmap(wzpst70_upsc, z, pal_fun = "inferno") +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world, inherit.aes = FALSE,
            color = "white", size = 0.1) +
  theme_black_nothing()
# ggsave(file.path(current_out_dir, "wildlife_zoonoses_post-70_unweighted.png"), height = 4.5, width = 9)
ggsave(file.path(current_out_dir, "wildlife_zoonoses_post-70_unweighted.pdf"), height = 4.5, width = 9)




# Save log-transformed versions so that we can better see what this looks like.
quickmap(wz_upsc, log(z), pal_fun = "inferno") +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world, inherit.aes = FALSE,
            color = "white", size = 0.1) +
  theme_black_nothing()
# ggsave(file.path(current_out_dir, "wildlife_zoonoses_unweighted_log.png"), height = 4.5, width = 9)
ggsave(file.path(current_out_dir, "wildlife_zoonoses_unweighted_log.pdf"), height = 4.5, width = 9)


quickmap(wzpre70_upsc, log(z), pal_fun = "inferno") +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world, inherit.aes = FALSE,
            color = "white", size = 0.1) +
  theme_black_nothing()
# ggsave(file.path(current_out_dir, "wildlife_zoonoses_pre-70_unweighted_log.png"), height = 4.5, width = 9)
ggsave(file.path(current_out_dir, "wildlife_zoonoses_pre-70_unweighted_log.pdf"), height = 4.5, width = 9)

quickmap(wzpst70_upsc, log(z), pal_fun = "inferno") +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world, inherit.aes = FALSE,
            color = "white", size = 0.1) +
  theme_black_nothing()
# ggsave(file.path(current_out_dir, "wildlife_zoonoses_post-70_unweighted_log.png"), height = 4.5, width = 9)
ggsave(file.path(current_out_dir, "wildlife_zoonoses_post-70_unweighted_log.pdf"), height = 4.5, width = 9)




# Now we will demonstrate presence weights and absence weights.

wz <- eid_metadata %>%
    filter(wildlife_zoonoses == 1,
           event_year >= 1970) %>%
    select(name = eid_name,
           year = event_year)


w8_land_area_pres <- wz %>%
  make_weight("land_area") %>%
  prettyquick("weight", method = "")

w8_pubs_fit_pres <- wz %>%
  make_weight("pubs_fit") %>%
  prettyquick("weight", method = "")

quickmap(w8_land_area_pres, log(z), pal_fun = "magma") +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world, inherit.aes = FALSE,
            color = "white", size = 0.1) +
  theme_black_nothing()
# ggsave(file.path(current_out_dir, "wildlife_zoonoses_unweighted_log.png"), height = 4.5, width = 9)
ggsave(file.path(current_out_dir, "weighted_land_area_presences.pdf"), height = 4.5, width = 9)

quickmap(w8_pubs_fit_pres, log(z), pal_fun = "magma") +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world, inherit.aes = FALSE,
            color = "white", size = 0.1) +
  theme_black_nothing()
# ggsave(file.path(current_out_dir, "wildlife_zoonoses_unweighted_log.png"), height = 4.5, width = 9)
ggsave(file.path(current_out_dir, "weighted_pubs_fit_presences.pdf"), height = 4.5, width = 9)


w8_land_area_abs <- drivers %>%
  prettyquick("land_area", method = "")

w8_pubs_fit_abs <- drivers %>%
  prettyquick("pubs_fit", method = "")

quickmap(w8_land_area_abs, log(z), pal_fun = "magma") +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world, inherit.aes = FALSE,
            color = "white", size = 0.1) +
  theme_black_nothing()
# ggsave(file.path(current_out_dir, "wildlife_zoonoses_unweighted_log.png"), height = 4.5, width = 9)
ggsave(file.path(current_out_dir, "weighted_land_area_absences.pdf"), height = 4.5, width = 9)

quickmap(w8_pubs_fit_abs, log(z), pal_fun = "magma") +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world, inherit.aes = FALSE,
            color = "white", size = 0.1) +
  theme_black_nothing()
# ggsave(file.path(current_out_dir, "wildlife_zoonoses_unweighted_log.png"), height = 4.5, width = 9)
ggsave(file.path(current_out_dir, "weighted_pubs_fit_absences.pdf"), height = 4.5, width = 9)