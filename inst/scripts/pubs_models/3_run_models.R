library(devtools)
load_all()
source("inst/scripts/pubs_models/2_define_functions.R")

registerDoParallel(20)
sample_iter <- 100

library(dplyr)

load(file.path(cache_dir(), "pubs_df_all.RData"))


drivers %>%
  select(-pubs_fit) %>%
  left_join(pubs_df_all) %>%
  rename(pubs_fit = old.response) %>%
  sample_events("old", sample_iter = sample_iter)
join_predictors("old")
run_models("old")
relative_influence_plots("old")
partial_dependence_plots("old")


drivers %>%
  select(-pubs_fit) %>%
  left_join(pubs_df_all) %>%
  rename(pubs_fit = w1a.response) %>%
  sample_events("w1a", sample_iter = 4)
join_predictors("w1a")
run_models("w1a")
relative_influence_plots("w1a")
partial_dependence_plots("w1a")


drivers %>%
  select(-pubs_fit) %>%
  left_join(pubs_df_all) %>%
  rename(pubs_fit = w1b.response) %>%
  sample_events("w1b", sample_iter = sample_iter)
join_predictors("w1b")
run_models("w1b")
relative_influence_plots("w1b")
partial_dependence_plots("w1b")


drivers %>%
  select(-pubs_fit) %>%
  left_join(pubs_df_all) %>%
  rename(pubs_fit = w2a.response) %>%
  sample_events("w2a", sample_iter = sample_iter)
join_predictors("w2a")
run_models("w2a")
relative_influence_plots("w2a")
partial_dependence_plots("w2a")


drivers %>%
  select(-pubs_fit) %>%
  left_join(pubs_df_all) %>%
  rename(pubs_fit = w2b.response) %>%
  sample_events("w2b", sample_iter = sample_iter)
join_predictors("w2b")
run_models("w2b")
relative_influence_plots("w2b")
partial_dependence_plots("w2b")


drivers %>%
  select(-pubs_fit) %>%
  left_join(pubs_df_all) %>%
  rename(pubs_fit = w3.response) %>%
  sample_events("w3", sample_iter = sample_iter)
join_predictors("w3")
run_models("w3")
relative_influence_plots("w3")
partial_dependence_plots("w3")


drivers %>%
  select(-pubs_fit) %>%
  left_join(pubs_df_all) %>%
  rename(pubs_fit = w4.response) %>%
  sample_events("w4", sample_iter = sample_iter)
join_predictors("w4")
run_models("w4")
relative_influence_plots("w4")
partial_dependence_plots("w4")
