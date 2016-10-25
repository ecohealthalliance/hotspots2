library(raster)
library(ggplot2)

# Define a function to upscale to raster
prettyquick <- function(data, fill, method = "") {
  data %>%
    select_(x = "lon", y = "lat", z = fill) %>%
    rasterFromXYZ(crs = crs(template_raster())) %>%
    raster::disaggregate(4, method = method) %>%
    mask(mask = country_outlines) %>%
    as.data.frame(xy = TRUE) %>%
    na.omit()
}


prettyquickmap <- function(data, fill, ...) {
  map.world <- map_data(map = "world") %>%
    rename(lon = long)
  arglist <- as.list(match.call())
  arglist[[1]] <- substitute(quickmap)
  quickmap_call <- as.call(arglist)
  eval(quickmap_call, envir = parent.frame()) +
  geom_path(aes(x = lon, y = lat, group = group), data = map.world, inherit.aes = FALSE,
            color = "white", size = 0.1) +
  theme_black_nothing()
}

map.world <- function(map = "world") {
  map_data(map) %>% rename(lon = long)
}