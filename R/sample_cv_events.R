library(purrr)
library(tidyr)


sample_cv_events <- function(drivers, sample_iter = 500, weighting_var = "pubs_fit") {
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
    left_join(select_(drivers, "gridid", weighting_variable)) %>%
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


  # This is where we diverge from the bootstrap sampling approach. Instead of randomly sampling events with replacement, we will partition the list of events, 75:25, into training and testing datasets.
  sampled_events <- foreach(i = 1:sample_iter) %do% {
    mutate(selected_events, k = kfold(x = selected_events, k = 4),
                            group = ifelse(k == 1, "test", "train"))
  }

  # Here, we are applying dplyr `filter` functions to the entire list.
  training_events <- sampled_events %>%
    map(~ filter(., group == "train")) %>%
    map(~ select(., -k, -group))
  testing_events <- sampled_events %>%
    map(~ filter(., group == "test")) %>%
    map(~ select(., -k, -group))

  # This is the sampling function from the bootstrap workflow.
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


  # We will create two data frames this time.
  training_gridids <- foreach(i = training_events) %dopar% {
    by_row(i, sample_gridids, .collate = "row") %>%
      select(-.row)
  }

  testing_gridids <- foreach(i = testing_events) %dopar% {
    by_row(i, sample_gridids, .collate = "row") %>%
      select(-.row)
  }

  cv_gridids <- list("training_gridids" = training_gridids,
                     "testing_gridids" = testing_gridids)
  return(cv_gridids)

  # save(training_gridids, file = file.path(current_cache_dir, paste0(model_name, "_training_gridids.RData")))
  # save(testing_gridids, file = file.path(current_cache_dir, paste0(model_name, "_testing_gridids.RData")))
}
