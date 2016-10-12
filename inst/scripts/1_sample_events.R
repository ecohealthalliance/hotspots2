## ----load----------------------------------------------------------------
load_all()
library(hotspots2)
library(raster)
library(dplyr)
library(tidyr)
library(purrr)
library(foreach)
library(doParallel)

registerDoParallel(4)

# First, we'll load all datasets.

data(decadal)
data(change)
data(drivers)
data(eid_metadata)
data(event_coverage)


## ----select-events-------------------------------------------------------
selected_events <- eid_metadata %>%
  filter(wildlife_zoonoses == 1,
         event_year >= 1970) %>%
  select(name = eid_name,
         year = event_year)


## ----create-sampling-weights---------------------------------------------
set.seed(20140605)

presence_weights <- event_coverage %>%
  filter(event_name %in% selected_events$name) %>%
  left_join(select(drivers, gridid, pubs_identity)) %>%
  group_by(event_name) %>%
  # only_if()(mutate)(weight = 1) %>%
  mutate(weight = coverage * pubs_identity / sum(coverage * pubs_identity, na.rm = TRUE),
         # We do this part to provide any weights where the publication value is NA.
         total_weight = sum(weight, na.rm = TRUE),
         weight = ifelse(total_weight == 0, coverage, weight)) %>%
  ungroup() %>%
  replace_na(replace = list(weight = 0, pubs_identity = 0))


# We now deal with this by replacing pasture and crop NAs with 0s.
# There are a few locations where our presence weights don't overlap the drivers data frame. Because of this, we will omit those.
# quickmap(semi_join(presence_weights, decadal), weight)

# presence_weights <- semi_join(presence_weights, decadal)
# This removes all grid cells belonging to HED_166



absence_weights <- drivers %>%
  select(gridid, pubs_identity) %>%
  replace_na(replace = list(pubs_identity = 0))

# There are two polygons which did not produce a presence weight.
selected_events <- selected_events %>%
  filter(name %in% presence_weights$event_name)




## ----sample-events-and-gridids-------------------------------------------

sample_iter <- 100

# First, we sample randomly among events.
sampled_events <- foreach(i = 1:sample_iter) %do% {
  sample_n(selected_events, size = 149, replace = TRUE)
}

# Next, for each event, we select a presence and absence as described above.
sample_gridids <- function(to_sample) {
  # print(to_sample$name)
  presence <- presence_weights %>%
    filter(event_name == to_sample$name) %>%
    sample_n(size = 1, weight = weight) %>%
    select(gridid) %>%
    data.frame(presence = 1)

  absence <- absence_weights %>%
    sample_n(size = 1, weight = pubs_identity) %>%
    select(gridid) %>%
    data.frame(presence = 0)

  sampled <- rbind(presence, absence)
  # sampled$name <- to_sample$name
  # sampled$year <- to_sample$year
  return(sampled)
}

# # There is a way to do this with purrr, but it's much faster with foreach in parallel, so we'll use that.
# bsm_events1 <- sampled_events %>%
#   map(~ by_row(., sample_gridids, .collate = "row"))

system.time(
bsm_gridids <- foreach(i = sampled_events) %dopar% {
  by_row(i, sample_gridids, .collate = "row") %>%
    select(-.row)
}
)

save(bsm_gridids, file = file.path(cache_dir(), "bsm_gridids.RData"))


