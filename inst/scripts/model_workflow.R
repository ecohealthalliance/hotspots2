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
model_name <- "2016-10-15_18-38_500"
sample_iter <- 500

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

sample_events(drivers, model_name, sample_iter)
join_predictors(model_name)
run_models(model_name)
relative_influence_plots(model_name)
partial_dependence_plots(model_name)

