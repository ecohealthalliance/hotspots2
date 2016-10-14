load_all()
library(raster)

template_raster <- template_raster()
gens_hires <- raster(system.file("data-raw", "GEnSv3_25012013", "gens_v3", package = "hotspots2"))

gens_hs <- aggregate(gens_hires, fact = res(template_raster)/res(gens_hires))
gens_df <- as.data.frame(gens_hs, xy = TRUE)
names(gens_df) <- c("lon", "lat", "gens")

# We have to do one extra step because for some reason this output lon and lat values with very slightly off decimal places.
# mean((round(gens_df$lon + 0.5) - 0.5) - gens_df$lon)
gens_df$lon <- round(gens_df$lon + 0.5) - 0.5
gens_df$lat <- round(gens_df$lat + 0.5) - 0.5

cache_dir <- system.file("cache", package = "hotspots2")
save(gens_df, file = file.path(cache_dir(), "gens_df.RData"))
