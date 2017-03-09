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
model_name <- "kfold_10x100_iter"
k <- 10
sample_iter <- 100 # Here, sample_iter controls how many times folding we fold
weighting_varname <- "pubs_fit"
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

##### This workflow runs with the flattened version #####

kfm <- flatten(kfm)
testing_events <- flatten(testing_events)



# The loop will go here.
predictions <- foreach(i = 1:length(kfm), .combine = "rbind") %do% {
  print(paste0("Working on ", i, "..."))
  events_to_test <- as.data.frame(testing_events[[i]])
  model_to_test <- kfm[[i]]

  print("Running predictions...")
  holdout_reference <- events_to_test$presence
  holdout_prediction <- predict(model_to_test, events_to_test, n.trees = model_to_test$n.trees, type = "response")

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
