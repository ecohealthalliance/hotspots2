# library(hotspots2) # Having issues with cache_dir()
load_all()

library(foreach)
library(doParallel)
registerDoParallel(1)

# First, we'll load all datasets.

data(decadal)
data(change)
data(eid_metadata)
data(event_coverage)

# Set our directory name and the number of sample iterations we want to conduct.
model_name <- "test_1_iteration"
sample_iter <- 1
weighting_varname <- "pubs_fit"
brt_params <- list(tree.complexity = 3,
                   learning.rate = 0.0035,
                   n.trees = 35)

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
# Skip these steps if you just want to refit.
bsm_gridids <- sample_bsm_events(drivers, sample_iter, weighting_varname)
save(bsm_gridids, file = file.path(current_cache_dir, paste0(model_name, "_gridids.RData")))

# load(file.path(current_cache_dir, paste0(model_name, "_gridids.RData")))
bsm_events <- join_predictors(bsm_gridids)
save(bsm_events, file = file.path(current_cache_dir, paste0(model_name, "_events.RData")))

# You can pick up here if you want to re-fit the model.
# load(file.path(current_cache_dir, paste0(model_name, "_events.RData")))

### RUN A TEST MODEL ON ONE SAMPLE ITERATION ###

# load(file.path(current_cache_dir, paste0(model_name, "_events.RData")))

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


i <- 1


# gbm.step() doesn't like tibbles.
bsm_data <- as.data.frame(bsm_events[[i]])

x_ind <- which(colnames(bsm_data) %in% predictor_names)
y_ind <- which(colnames(bsm_data) %in% "presence")

# This code allows us to pass in brt_params,
# And run the model as below.
model <- do.call(gbm.step,
                 c(list(data = bsm_data,
                        gbm.x = x_ind,
                        gbm.y = y_ind,
                        family = "bernoulli",
                        plot.main = TRUE,
                        verbose = TRUE),
                   brt_params))

# model
# summary(model)
# gbm.plot(model)
