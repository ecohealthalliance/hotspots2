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

# Create output and cache directories.
current_cache_dir <- file.path(cache_dir(), model_name)
current_out_dir <- file.path(out_dir(), model_name)
dir.create(current_cache_dir, showWarnings = FALSE)
dir.create(current_out_dir, showWarnings = FALSE)


### RUN A TEST MODEL ON ONE SAMPLE ITERATION ###

load(file.path(current_cache_dir, paste0(model_name, "_events.RData")))

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

bsm_data <- as.data.frame(bsm_events[[i]])

x_ind <- which(colnames(bsm_data) %in% predictor_names)
y_ind <- which(colnames(bsm_data) %in% "presence")

model <- gbm.step(data = bsm_data,
                  gbm.x = x_ind,
                  gbm.y = y_ind,
                  family = "bernoulli",
                  tree.complexity = 3,
                  learning.rate = 0.005,
                  n.trees = 50,
                  plot.main = TRUE,
                  verbose = TRUE)

model
summary(model)
gbm.plot(model)
