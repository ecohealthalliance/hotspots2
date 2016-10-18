# library(hotspots2) # Having issues with cache_dir()
load_all()

library(foreach)
library(doParallel)
registerDoParallel(16)

# First, we'll load all datasets.

data(decadal)
data(change)
data(eid_metadata)
data(event_coverage)

# Set our directory name and the number of sample iterations we want to conduct.
model_name <- "loocvm_5_iter"
sample_iter <- 5
weighting_varname <- "pubs_fit"
bootstrap <- FALSE
brt_params <- list(tree.complexity = 3,
                   learning.rate = 0.0025,
                   n.trees = 40)

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


# The variable named "pubs_fit" will be used for weighting.

# Sample grid cells according to weighting and join to predictors.
# This will return a list of two data frames, which will be unpacked.
cv_gridids <- sample_loocv_events(drivers, sample_iter, weighting_varname, bootstrap)
list2env(cv_gridids, globalenv())
save(training_gridids, file = file.path(current_cache_dir, paste0(model_name, "_training_gridids.RData")))
save(testing_gridids, file = file.path(current_cache_dir, paste0(model_name, "_testing_gridids.RData")))


# load(file.path(current_cache_dir, paste0(model_name, "_training_gridids.RData")))
# load(file.path(current_cache_dir, paste0(model_name, "_testing_gridids.RData")))
training_events <- join_predictors(training_gridids)
testing_events <- join_predictors(testing_gridids)
save(training_events, file = file.path(current_cache_dir, paste0(model_name, "_training_events.RData")))
save(testing_events, file = file.path(current_cache_dir, paste0(model_name, "_testing_events.RData")))

# You can pick up here if you want to re-fit the model.
# load(file.path(current_cache_dir, paste0(model_name, "_training_events.RData")))
# load(file.path(current_cache_dir, paste0(model_name, "_testing_events.RData")))
cvm <- fit_brts_to_events(training_events, brt_params) # Need to refactor
save(cvm, file = file.path(current_cache_dir, paste0(model_name, ".RData")))

# You can start here if you want to just output the plots again.
# load(file.path(current_cache_dir, paste0(model_name, ".RData")))
relative_influence_plots(cvm, model_name)
partial_dependence_plots(cvm, training_events, model_name)
