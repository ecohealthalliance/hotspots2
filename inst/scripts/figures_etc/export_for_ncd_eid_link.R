load_all()
library(tidyverse)

data(drivers)
data(predictions)
drivers <- as.tibble(drivers)
predictions <- as.tibble(predictions)

# Load the original drivers dataset for its country code grid cell.
og_drivers <- read_tsv("data-raw/eid08_drivers_19OCT11.txt") %>%
  select(gridid = GridID, country) %>%
  mutate(country = recode(country, Mauretania = "Mauritania"))

# Pull the pubs model variable from the drivers data.
pubs <- drivers %>%
  select(gridid, pubs_fit)

hs2_grid <- predictions %>%
  select(gridid, lon, lat, pop, starts_with("weight"), starts_with("bsm")) %>%
  left_join(pubs) %>%
  left_join(og_drivers)

# In Noam's GitHub Issues comment, he suggested computing:
# sum(corrected_hotspot_risk[pixel] * population[pixel]) /
# sum(population[country])

# The variable bsm_weight_pop has been multiplied by weight_pop — which is
# population normalized to sum to 1 over the entire grid. However, we'll
# create a new temporary version to do this version of normalization, and
# we'll start with bsm_response.

hs2_country <- hs2_grid %>%
  group_by(country) %>%
  select(-gridid, -lon, -lat) %>%
  mutate(bsm_weight_pop_2 = bsm_response * pop) %>%
  summarize_all(~ sum(., na.rm = TRUE)) %>%
  mutate(bsm_over_pop = bsm_weight_pop_2 / pop)

# Load grid with country regions and create an aggregation using those regions.
regions <- read_csv("data-raw/country_region_grid.csv") %>%
  select(lon, lat, gridid, region)

hs2_region <- hs2_grid %>%
  select(-country) %>%
  left_join(regions, by = c("lon", "lat", "gridid")) %>%
  group_by(region) %>%
  mutate(bsm_weight_pop_2 = bsm_response * pop) %>%
  summarize_all(~ sum(., na.rm = TRUE)) %>%
  mutate(bsm_over_pop = bsm_weight_pop_2 / pop)


dir.create("inst/out/ncd-eid-link", showWarnings = FALSE)

write_csv(hs2_grid, "inst/out/ncd-eid-link/hs2_grid.csv")
write_csv(hs2_country, "inst/out/ncd-eid-link/hs2_country.csv")
write_csv(hs2_region, "inst/out/ncd-eid-link/hs2_region.csv")
