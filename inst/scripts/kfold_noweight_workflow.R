# library(hotspots2) # Having issues with cache_dir()
load_all()

library(foreach)
library(doParallel)
registerDoParallel(20)

# First, we'll load all datasets.

data(decadal)
data(change)
data(eid_metadata)
data(event_coverage)

# Set our directory name and the number of sample iterations we want to conduct.
model_name <- "kfold_noweight_10x100_iter"
k <- 10
sample_iter <- 100 # Here, sample_iter controls how many times folding we fold
weighting_varname <- "land_area"
bootstrap <- FALSE
brt_params <- list(tree.complexity = 3,
                   learning.rate = 0.0035,
                   n.trees = 35)
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
                     # "earth10_snowice",
                     # "earth11_barren",
                     # "earth12_water",
                     "gens",
                     "mamdiv",
                     "poultry",
                     "livestock_mam")

# Create output and cache directories.
current_cache_dir <- file.path(cache_dir(), model_name)
current_out_dir <- file.path(out_dir(), model_name)
dir.create(current_cache_dir, showWarnings = FALSE)
dir.create(current_out_dir, showWarnings = FALSE)

sink(file.path(current_out_dir, "info"))
cat("Model Run Parameters\n")
print(Sys.time())
print(model_name)
print(sample_iter)
print(brt_params)
sink()

# Sample grid cells according to weighting and join to predictors.
# This will return a list of two data frames, which will be unpacked.
kfold_gridids <- sample_kfold_events(drivers, k, sample_iter, weighting_varname)
save(kfold_gridids, file = file.path(current_cache_dir, paste0(model_name, "_gridids.RData")))

##### Do some extra work  to unfurl the DF into a longer one of stuff to be fit. #####
unfurl_training_gridids <- function(x) {
  foreach(i = sort(unique(x$fold))) %do% {
    filter(x, !fold == i)
  }
}
unfurl_testing_gridids <- function(x) {
  foreach(i = sort(unique(x$fold))) %do% {
    filter(x, fold == i)
  }
}
# load(file.path(current_cache_dir, paste0(model_name, "_gridids.RData")))
training_gridids <- map(kfold_gridids, unfurl_training_gridids)
testing_gridids <- map(kfold_gridids, unfurl_testing_gridids)
save(training_gridids, file = file.path(current_cache_dir, paste0(model_name, "_training_gridids.RData")))
save(testing_gridids, file = file.path(current_cache_dir, paste0(model_name, "_testing_gridids.RData")))

##### We're doing this a different way, keeping each iteration at greater list depth #####
# load(file.path(current_cache_dir, paste0(model_name, "_training_gridids.RData")))
# load(file.path(current_cache_dir, paste0(model_name, "_testing_gridids.RData")))
training_events <- map(training_gridids, join_predictors, predictor_names)
testing_events <- map(testing_gridids, join_predictors, predictor_names)
# training_events <- join_predictors(training_gridids, predictor_names)
# testing_events <- join_predictors(testing_gridids, predictor_names)
save(training_events, file = file.path(current_cache_dir, paste0(model_name, "_training_events.RData")))
save(testing_events, file = file.path(current_cache_dir, paste0(model_name, "_testing_events.RData")))

# You can pick up here if you want to re-fit the model.
# load(file.path(current_cache_dir, paste0(model_name, "_training_events.RData")))
# load(file.path(current_cache_dir, paste0(model_name, "_testing_events.RData")))
# cvm <- fit_brts_to_events(training_events, brt_params, predictor_names) # Need to refactor
##### Because we're one level deep in the list, we use `map()` #####

kfm <- map(training_events, fit_brts_to_events, brt_params, predictor_names, null_behavior = "rerun")
save(kfm, file = file.path(current_cache_dir, paste0(model_name, ".RData")))

kfm_flat <- flatten(kfm)

# You can start here if you want to just output the plots again.
# load(file.path(current_cache_dir, paste0(model_name, ".RData")))
relative_influence_plots(kfm_flat, model_name)
partial_dependence_plots(kfm_flat, flatten(training_events), model_name)

# Output interactions and summary to text file
sink(file.path(current_out_dir, "summary_interactions"))
cat("Summary\n")
summarize_multibrt(kfm_flat, .parallel = TRUE)
cat("\nInteractions\n")
interactions_multibrt(kfm_flat, .parallel = TRUE)
sink()

