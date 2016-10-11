load_all()
library(dplyr)
library(purrr)

list.files(cache_dir(), full.names = TRUE) %>%
  walk(load, envir = globalenv())

names(drivers_orig)

drivers <- drivers_orig %>%
  select(gridid,
         lon,
         lat,
         iso3,
         land_area,
         gdp,
         pop_2000,
         crop_2000,
         past_2000,
         poultry,
         livestock_mam) %>%
  left_join(earthenv) %>%
  left_join(gens_df) %>%
  left_join(mamdiv) %>%
  left_join(pubs_fit)

save(drivers, file = file.path(data_dir(), "drivers.RData"))