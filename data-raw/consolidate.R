load_all()
library(dplyr)
library(purrr)

list(file.path(cache_dir(), "drivers_orig.RData"),
     file.path(cache_dir(), "earthenv.RData"),
     file.path(cache_dir(), "gens_df.RData"),
     file.path(cache_dir(), "mamdiv.RData"),
     file.path(cache_dir(), "pubs_fit.RData")) %>%
  walk(load, envir = globalenv())

drivers <- drivers_orig %>%
  select(gridid,
         lon,
         lat,
         iso3,
         land_area,
         gdp,
         poultry,
         livestock_mam) %>%
  left_join(earthenv) %>%
  left_join(gens_df) %>%
  left_join(mamdiv) %>%
  left_join(pubs_fit)


# Create a "drivers_full" data frame, which includes variables for decadal and
# change variables. This is useful for plotting the output of models etc.
data(decadal)
data(change)

drivers_full <- drivers %>%
  left_join(filter(decadal, year == 2000)) %>%
  select(-year) %>%
  left_join(filter(change, year == 1995)) %>%
  select(-year)


save(drivers, file = file.path(data_dir(), "drivers.RData"))
save(drivers_full, file = file.path(data_dir(), "drivers_full.RData"))
