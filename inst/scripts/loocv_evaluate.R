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
model_name <- "loocvm_10_iter"
sample_iter <- 10
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

# Load our testing events and models if we don't have them.
load(file.path(current_cache_dir, paste0(model_name, ".RData")))
load(file.path(current_cache_dir, paste0(model_name, "_testing_events.RData")))

# Occasionally, models fail to fit.
# These functions help us figure out which are the holdout events for the models.
name_of_holdout <- function(event) unique(event$name)
name_of_holdout_for_model <- function(model) {
  hn <- map_chr(testing_events, name_of_holdout) %>% unique()
  unique(hn[!hn %in% model$gbm.call$dataframe$name])
}

# Get a flat list of all holdout names from the testing events.
holdout_names <- map_chr(testing_events, name_of_holdout)
model_names <- map_chr(cvm, name_of_holdout_for_model)

i <- unique(holdout_names)

# The loop will go here.
predictions <- foreach(i = unique(holdout_names), .combine = rbind) %dopar% {
  print(paste0("Working on ", i, "..."))
  events_to_test <- testing_events %>%
    keep(~ name_of_holdout(.x) %in% i) %>%
    do.call(rbind, .)
  models_to_test <- cvm %>%
    keep(model_names %in% i)

  print("Running predictions...")
  holdout_reference <- events_to_test$presence
  holdout_prediction <- predict_multibrt(models_to_test, events_to_test)

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

