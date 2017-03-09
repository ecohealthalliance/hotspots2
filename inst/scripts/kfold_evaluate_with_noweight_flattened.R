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
load(file.path(cache_dir(),"kfold_noweight_10x100_iter", paste0("kfold_noweight_10x100_iter", "_testing_events.RData")))


##### This part of the script deals with the probably only one null model we have #####
##### Although as of 2017-03-08 8:13 PM the models are fitting, 100% of the time #####

nulls <- map(kfm, ~ map_lgl(.x, ~ is.null(.x)))
keeps <- map(kfm, ~ map_lgl(.x, ~ !is.null(.x)))

kfm <- map2(kfm, keeps, ~ keep(.x, .y))
testing_events <- map2(testing_events, keeps, ~ keep(.x, .y))


##### This workflow runs with the flattened version #####

kfm <- flatten(kfm)
testing_events <- flatten(testing_events)

# I think I need to create the pubs_weight column first.
pubs_weight <- drivers_full %>%
  select(gridid, pubs_fit) %>%
  mutate(pubs_weight = pubs_fit / sum(pubs_fit, na.rm = TRUE))
pubs_weight[is.na(pubs_weight)] <- 0

##### The actual meat of the evaluation #####
predictions <- foreach(i = 1:length(kfm), .combine = "rbind") %dopar% {
  print(paste0("Working on ", i, "..."))
  events_to_test <- as.data.frame(testing_events[[i]])
  model_to_test <- kfm[[i]]

  print("Running predictions...")
  response <- predict(model_to_test, events_to_test, n.trees = model_to_test$n.trees, type = "response")

  predictions <- events_to_test %>%
    select(gridid, reference = presence) %>%
    cbind(response) %>%
    left_join(pubs_weight) %>%
    mutate(prediction = response * pubs_weight,
           prediction2 = response * pubs_weight * 0.5 / mean(response * pubs_weight, na.rm = TRUE)) %>%
    select(reference, response, pubs_weight, prediction, prediction2)
  predictions
}

##### The variable named 'prediction' is the weighted response. #####

# Either use dismo evaluate or caret confusionMatrix
e_free <- dismo::evaluate(p = predictions[as.logical(predictions$reference), "prediction"],
                          a = predictions[!predictions$reference, "prediction"])

# We will set here the treshold to that which maximizes kappa.
e_fixed <- dismo::evaluate(p = predictions[as.logical(predictions$reference), "prediction"],
                           a = predictions[!predictions$reference, "prediction"],
                           tr = threshold(e_free)$kappa)



tss <- function(e) {
  tss <- e@TPR + e@TNR - 1
  return(unname(tss))
}
auc <- function(e) {
  auc <- e@auc
  return(auc)
}

# Output interactions and summary to text file
sink(file.path(current_out_dir, "cv_with_noweight_summary_flattened"))
cat("AUC\n")
auc(e_fixed)
cat("\nTSS\n")
tss(e_fixed)
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
