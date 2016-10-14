load_all()
library(sp)
library(dplyr)

mamdiv <- readGDAL(system.file("data-raw", "rich_1deg", "rich_mammals_1deg.asc", package = "hotspots2")) %>%
  as.data.frame() %>%
  rename(mamdiv = band1, lon = x, lat = y)

cache_dir <- system.file("cache", package = "hotspots2")
save(mamdiv, file = file.path(cache_dir(), "mamdiv.RData"))