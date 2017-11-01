load_all()

library(dismo)
library(gbm)
library(purrr)
library(viridis)
library(tidyverse)

data(drivers_full)
data(predictions)

bsm_weight_pop_landarea <- bsm_


clip_at_sd <- function(x, multiple = 1) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE) * multiple
  y <- x %>%
    pmin(m + s) %>%
    pmax(m - s)
  return(y)
}

predictions_country <- predictions %>%
  group_by(iso3) %>%
  summarize(bsm_weight_pop = sum(bsm_weight_pop, na.rm = TRUE),
            bsm_weight_pubs = sum(bsm_weight_pubs, na.rm = TRUE)) %>%
  mutate(bsm_weight_pop = bsm_weight_pop * (1 / sum(bsm_weight_pop)),
         bsm_weight_pubs = bsm_weight_pubs * (1 / sum(bsm_weight_pubs)))

write_csv(predictions_country, path = "inst/out/predictions_summed_to_country.csv")