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
model_name <- "bsm_1000_iter"
sample_iter <- 1000

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
sink()

# Make sure you name the thing "pubs_fit". cache_name should be the name of the file you wanna save.

# Sample grid cells according to weighting and join to predictors.
# Skip these steps if you just want to refit.
sample_bsm_events(drivers, model_name, sample_iter)
join_bsm_predictors(model_name)

# You can pick up here if you want to re-fit the model.
fit_brts_to_events(model_name)
relative_influence_plots(model_name)
partial_dependence_plots(model_name)

