load_all()
library(tidyverse)

out_dir = "/Users/toph/Dropbox (EHA)/repositories/hotspots2/inst/out/data_for_nicholas_ray_2018-08-13"

dir.create(out_dir, showWarnings = FALSE)

data(predictions)

bsm_response <- predictions %>%
  select(lon, lat, bsm_response)

write_csv(bsm_response, path = file.path(out_dir, "bsm_response.csv"))

bsm_response_raster <- bsm_response %>%
  rename(x = lon, y = lat) %>%
  rasterFromXYZ()

writeRaster(bsm_response_raster, filename = file.path(out_dir, "bsm_response.asc"))
writeRaster(bsm_response_raster, filename = file.path(out_dir, "bsm_response.tif"))




bsm_weight_pop <- predictions %>%
  select(lon, lat, bsm_weight_pop)

write_csv(bsm_weight_pop, path = file.path(out_dir, "bsm_weight_pop.csv"))

bsm_weight_pop_raster <- bsm_weight_pop %>%
  rename(x = lon, y = lat) %>%
  rasterFromXYZ()

writeRaster(bsm_weight_pop_raster, filename = file.path(out_dir, "bsm_weight_pop.asc"))
writeRaster(bsm_weight_pop_raster, filename = file.path(out_dir, "bsm_weight_pop.tif"))



data(eid_metadata)

eid_metadata %>%
  filter(wildlife_zoonoses == 1)
  filter(eidid, eid_name, wildlife_zoonoses)