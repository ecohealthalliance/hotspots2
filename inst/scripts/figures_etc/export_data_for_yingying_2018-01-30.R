load_all()
library(tidyverse)

out_dir = "/Users/toph/Dropbox (EHA)/repositories/hotspots2/inst/out/data for yingying 2018-01-30"

data(predictions)

bsm_response <- predictions %>%
  select(lon, lat, bsm_response)

write_csv(bsm_response, path = file.path(out_dir, "bsm_response.csv"))

bsm_response_raster <- bsm_response %>%
  rename(x = lon, y = lat) %>%
  rasterFromXYZ()

writeRaster(bsm_response_raster, filename = file.path(out_dir, "bsm_response.asc"))
writeRaster(bsm_response_raster, filename = file.path(out_dir, "bsm_response.tif"))
