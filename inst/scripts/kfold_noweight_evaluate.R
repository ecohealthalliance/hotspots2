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

# Maybe everything will be super compatible if I just flatten it all.
kfm <- flatten(kfm)
testing_events <- flatten(testing_events)


##### ----- #####

# I don't think we're going to worry about this.
# Occasionally, models fail to fit.
# These functions help us figure out which are the holdout events for the models.
# name_of_holdout <- function(event) unique(event$name)
# name_of_holdout_for_model <- function(model) {
#   hn <- map(testing_events, name_of_holdout) %>% unique()
#   unique(hn[!hn %in% model$gbm.call$dataframe$name])
# }

# # Get a flat list of all holdout names from the testing events.
# holdout_names <- map(testing_events, name_of_holdout)
# model_names <- map(kfm, name_of_holdout_for_model)

# i <- unique(holdout_names)

##### ----- #####

# The loop will go here.
predictions <- foreach(i = 1:length(kfm), .combine = "rbind") %do% {
  print(paste0("Working on ", i, "..."))
  events_to_test <- as.data.frame(testing_events[[i]])
  model_to_test <- kfm[[i]]

  print("Running predictions...")
  holdout_reference <- events_to_test$presence
  holdout_prediction <- predict(model_to_test, events_to_test, n.trees = model_to_test$n.trees)

  predictions <- data.frame(reference = holdout_reference, prediction = holdout_prediction)
}

# Either use dismo evaluate or caret confusionMatrix
e_fixed <- dismo::evaluate(p = predictions[as.logical(predictions$reference), "prediction"],
                           a = predictions[!predictions$reference, "prediction"],
                           tr = 0.5)

e_free <- dismo::evaluate(p = predictions[as.logical(predictions$reference), "prediction"],
                          a = predictions[!predictions$reference, "prediction"])

plot(e_free, "ROC")
plot(e_free, "TPR")

tss <- function(e) {
  tss <- e@TPR + e@TNR - 1
  return(unname(tss))
}

tss(e_free)
tss(e_fixed)

e_free@auc
e_fixed@auc

qplot(factor(reference), prediction, data = predictions, geom = "boxplot")
qplot(e_free@t, tss(e_free))

boxplot(e_fixed)
boxplot(e_free)
density(e_fixed)
density(e_free)







##### NON-FLATTENED VERSION #####

# The loop will go here.



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

plot(e_free, "ROC")
plot(e_free, "TPR")

tss <- function(e) {
  tss <- e@TPR + e@TNR - 1
  return(unname(tss))
}

e_free %>% map(tss)
e_fixed %>% map(tss)

e_free@auc
e_fixed@auc

qplot(factor(reference), prediction, data = predictions, geom = "boxplot")
qplot(e_free@t, tss(e_free))

boxplot(e_fixed)
boxplot(e_free)
density(e_fixed)
density(e_free)

