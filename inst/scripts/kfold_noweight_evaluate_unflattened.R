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

# Create output and cache directories.
current_cache_dir <- file.path(cache_dir(), model_name)
current_out_dir <- file.path(out_dir(), model_name)
dir.create(current_cache_dir, showWarnings = FALSE)
dir.create(current_out_dir, showWarnings = FALSE)


# Here begins the testing workflow.

library(dismo)
library(ggplot2)
library(purrr)

# Load our testing events and models if we don't have them.
load(file.path(current_cache_dir, paste0(model_name, ".RData")))
load(file.path(current_cache_dir, paste0(model_name, "_testing_events.RData")))


##### This part of the script deals with the probably only one null model we have #####
##### Although as of 2017-03-08 8:13 PM the models are fitting, 100% of the time #####

nulls <- map(kfm, ~ map_lgl(.x, ~ is.null(.x)))
keeps <- map(kfm, ~ map_lgl(.x, ~ !is.null(.x)))

kfm <- map2(kfm, keeps, ~ keep(.x, .y))
testing_events <- map2(testing_events, keeps, ~ keep(.x, .y))

##### The actual meat of the evaluation #####
predictions <- foreach(i = 1:length(kfm)) %do% {
  iter_models <- kfm[[i]]
  iter_events <- testing_events[[i]]
  iter_predictions <- foreach(i = 1:length(iter_models), .combine = "rbind") %do% {
    print(paste0("Working on ", i, "..."))
    events_to_test <- as.data.frame(iter_events[[i]])
    model_to_test <- iter_models[[i]]

    print("Running predictions...")
    holdout_reference <- events_to_test$presence
    holdout_prediction <- predict(model_to_test, events_to_test, n.trees = model_to_test$n.trees)

    predictions <- data.frame(reference = holdout_reference, prediction = holdout_prediction)
  }
}

# Either use dismo evaluate or caret confusionMatrix
e_fixed <- predictions %>%
  map(~ dismo::evaluate(p = .x[as.logical(.x$reference), "prediction"],
                        a = .x[!.x$reference, "prediction"],
                        tr = 0.5))
e_free <- predictions %>%
  map(~ dismo::evaluate(p = .x[as.logical(.x$reference), "prediction"],
                        a = .x[!.x$reference, "prediction"]))


tss <- function(e) {
  tss <- e@TPR + e@TNR - 1
  return(unname(tss))
}
auc <- function(e) {
  auc <- e@auc
  return(auc)
}

kfm_auc <- e_fixed %>% map_dbl(auc)
kfm_tss <- e_fixed %>% map_dbl(tss)


# Output interactions and summary to text file
sink(file.path(current_out_dir, "cv_summary_unflattened"))
cat("AUC (vector, mean, sd, quantiles)\n")
kfm_auc
mean(kfm_auc)
sd(kfm_auc)
quantile(kfm_auc, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
cat("\nTSS(vector, mean, sd, quantiles)\n")
kfm_tss
mean(kfm_tss)
sd(kfm_tss)
quantile(kfm_tss, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
sink()

# qplot(factor(reference), prediction, data = predictions, geom = "boxplot")
# qplot(e_free@t, tss(e_free))
#
# boxplot(e_fixed)
# boxplot(e_free)
# density(e_fixed)
# density(e_free)
#
#
# plot(e_free, "ROC")
# plot(e_free, "TPR")
#
