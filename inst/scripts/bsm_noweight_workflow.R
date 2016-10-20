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
model_name <- "bsm_1000_noweight"
sample_iter <- 1000
weighting_varname <- "land_area"  # We actually weight by land area b/c lonlat bias.
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
# Skip these steps if you just want to refit.
bsm_gridids <- sample_bsm_events(drivers, sample_iter, weighting_varname = weighting_varname)
save(bsm_gridids, file = file.path(current_cache_dir, paste0(model_name, "_gridids.RData")))

# load(file.path(current_cache_dir, paste0(model_name, "_gridids.RData")))
bsm_events <- join_predictors(bsm_gridids, predictor_names)
save(bsm_events, file = file.path(current_cache_dir, paste0(model_name, "_events.RData")))

# You can pick up here if you want to re-fit the model.
# load(file.path(current_cache_dir, paste0(model_name, "_events.RData")))
bsm <- fit_brts_to_events(bsm_events, brt_params, predictor_names)
save(bsm, file = file.path(current_cache_dir, paste0(model_name, ".RData")))

# You can start here if you want to just output the plots again.
# load(file.path(current_cache_dir, paste0(model_name, ".RData")))
relative_influence_plots(bsm, model_name)
partial_dependence_plots(bsm, bsm_events, model_name)
partial_dependence_plot_truncated(bsm, bsm_events, model_name)



# Optional maps stuff

# quickmap(sum_presences(bsm_events), log(n))
# quickmap(sum_absences(bsm_events), log(n))

sink(file.path(current_out_dir, "summary_interactions"))
cat("Summary\n")
summarize_multibrt(bsm, .parallel = TRUE)
cat("\nInteractions\n")
interactions_multibrt(bsm, .parallel = TRUE)
sink()
