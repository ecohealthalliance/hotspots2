# library(hotspots2) # Having issues with cache_dir()
load_all()

library(foreach)
library(doParallel)
registerDoParallel(4)

# First, we'll load all datasets.

data(decadal)
data(change)
data(eid_metadata)
data(event_coverage)

# Set our directory name and the number of sample iterations we want to conduct.
model_name <- "bsm_1000_iter"
sample_iter <- 1000
brt_params <- list(tree.complexity = 3,
                   learning.rate = 0.005,
                   n.trees = 50)

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

# Make sure you name the thing "pubs_fit".

# Sample grid cells according to weighting and join to predictors.
# Skip these steps if you just want to refit.
bsm_gridids <- sample_bsm_events(drivers, model_name, sample_iter)
save(bsm_gridids, file = file.path(current_cache_dir, paste0(model_name, "_gridids.RData")))

# load(file.path(current_cache_dir, paste0(model_name, "_gridids.RData")))
bsm_events <- join_predictors(bsm_gridids)
save(bsm_events, file = file.path(current_cache_dir, paste0(model_name, "_events.RData")))

# You can pick up here if you want to re-fit the model.
# load(file.path(current_cache_dir, paste0(model_name, "_events.RData")))
bsm <- fit_brts_to_events(bsm_events, brt_params)
save(bsm, file = file.path(current_cache_dir, paste0(model_name, ".RData")))

# You can start here if you want to just output the plots again.
# load(file.path(current_cache_dir, paste0(model_name, ".RData")))
relative_influence_plots(bsm, model_name)
partial_dependence_plots(bsm, bsm_events, model_name)

