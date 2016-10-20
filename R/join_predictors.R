join_predictors <- function(gridids, predictor_names) {
  # load(file.path(current_cache_dir, paste0(model_name, "_gridids.RData")))


  ## ----prepare-------------------------------------------------------------
  # Remove some other variables that we were keeping for one reason or another.
  drivers <- drivers %>%
    select(-gdp, -land_area, -iso3)



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
  events_dec <- foreach(i = gridids) %dopar% {
    by_row(i, time_slice_vars, .collate = "row") %>%
      select(-.row)
  }
  )

  remaining_names <- predictor_names[which(!predictor_names %in% names(events_dec[[1]]))]

  system.time(
  events <- foreach(i = events_dec) %dopar% {
    left_join(i, drivers[, c("gridid", "lon", "lat", remaining_names)])
  }
  )

  # save(events, file = file.path(current_cache_dir, paste0(model_name, "_events.RData")))
  return(events)
}
