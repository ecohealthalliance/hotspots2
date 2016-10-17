library(purrr)
library(tidyr)


sample_bsm_events <- function(drivers, model_name, sample_iter = 100) {
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
    left_join(select(drivers, gridid, pubs_fit)) %>%
    group_by(event_name) %>%
    # only_if()(mutate)(weight = 1) %>%
    mutate(weight = coverage * pubs_fit / sum(coverage * pubs_fit, na.rm = TRUE),
           # We do this part to provide any weights where the publication value is NA.
           total_weight = sum(weight, na.rm = TRUE),
           weight = ifelse(total_weight == 0, coverage, weight)) %>%
    ungroup() %>%
    replace_na(replace = list(weight = 0, pubs_fit = 0))


  # We now deal with this by replacing pasture and crop NAs with 0s. There are a
  # few locations where our presence weights don't overlap the drivers data frame.
  # Because of this, we will omit those. quickmap(semi_join(presence_weights,
  # decadal), weight)

  # presence_weights <- semi_join(presence_weights, decadal)
  # This removes all grid cells belonging to HED_166



  absence_weights <- drivers %>%
    select(gridid, pubs_fit) %>%
    replace_na(replace = list(pubs_fit = 0))

  # There are two polygons which did not produce a presence weight.
  selected_events <- selected_events %>%
    filter(name %in% presence_weights$event_name)




  ## ----sample-events-and-gridids-------------------------------------------

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
      sample_n(size = 1, weight = pubs_fit) %>%
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

  save(bsm_gridids, file = file.path(current_cache_dir, paste0(model_name, "_gridids.RData")))
}



join_predictors <- function(model_name) {
  load(file.path(current_cache_dir, paste0(model_name, "_gridids.RData")))


  ## ----prepare-------------------------------------------------------------
  # Remove some other variables that we were keeping for one reason or another.
  drivers <- drivers %>%
    select(-gdp, -land_area, -iso3)

  predictor_names <- c("pop",
                       "crop",
                       "past",
                       "pop_change",
                       "crop_change",
                       "past_change",
                       "earth1_trees_needl",
                       "earth2_trees_everg",
                       "earth3_trees_decid",
                       "earth4_trees_other",
                       "earth5_shrubs",
                       "earth6_veg_herba",
                       "earth7_veg_manag",
                       "earth8_veg_flood",
                       "earth9_urban",
                       "earth10_snowice",
                       "earth11_barren",
                       "earth12_water",
                       "gens",
                       "mamdiv",
                       "poultry",
                       "livestock_mam")

  # from_decadal <- predictor_names[predictor_names %in% names(decadal)]
  # from_change <- predictor_names[predictor_names %in% names(change)]
  # from_drivers <- predictor_names[!(predictor_names %in% c(from_decadal, from_change))]

  # decade <- function(x) floor(x / 10) * 10
  # middec <- function(x) round(x - 4.5, -1) + 5

  nearest <- function(x, choices) {
    choices[which.min(abs(choices - x))]
  }

  time_slice_vars <- function(event) {
    decadal_vars <- decadal %>%
      filter(year == nearest(event$year, choices = unique(.$year)),
             gridid == event$gridid) %>%
      select(crop, past, pop)

    change_vars <- change %>%
      filter(year == nearest(event$year, choices = unique(.$year)),
             gridid == event$gridid) %>%
      select(crop_change, past_change, pop_change)

    return(cbind(decadal_vars, change_vars))
  }

  system.time(
  bsm_events_dec <- foreach(i = bsm_gridids) %dopar% {
    by_row(i, time_slice_vars, .collate = "row") %>%
      select(-.row)
  }
  )

  remaining_names <- predictor_names[which(!predictor_names %in% names(bsm_events_dec[[1]]))]

  system.time(
  bsm_events <- foreach(i = bsm_events_dec) %dopar% {
    left_join(i, drivers[, c("gridid", "lon", "lat", remaining_names)])
  }
  )

  save(bsm_events, file = file.path(current_cache_dir, paste0(model_name, "_events.RData")))
}
