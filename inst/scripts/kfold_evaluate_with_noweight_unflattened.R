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

# I think I need to create the pubs_weight column first.
pubs_weight <- drivers_full %>%
  select(gridid, pubs_fit) %>%
  mutate(pubs_weight = pubs_fit / sum(pubs_fit, na.rm = TRUE))
pubs_weight[is.na(pubs_weight)] <- 0





##### The actual meat of the evaluation #####
predictions <- foreach(i = 1:length(kfm)) %dopar% {
  print(paste0("Working on ", i, "..."))
  iter_models <- kfm[[i]]
  iter_events <- testing_events[[i]]
  iter_predictions <- foreach(j = 1:length(iter_models), .combine = "rbind") %do% {
    # print(paste0("Working on ", j, "..."))
    events_to_test <- as.data.frame(iter_events[[j]])
    model_to_test <- iter_models[[j]]

    # print("Running predictions...")
    response <- predict(model_to_test, events_to_test, n.trees = model_to_test$n.trees, type = "response")

    predictions <- events_to_test %>%
      select(gridid, reference = presence) %>%
      cbind(response) %>%
      left_join(pubs_weight, by = "gridid") %>% # The "by" is there to shut the function up.
      mutate(prediction = response * pubs_weight,
             prediction2 = prediction * 0.5 / median(prediction)) %>% 
      select(reference, response, pubs_weight, prediction, prediction2)
    predictions
  }
}

##### The variable`prediction` is the weighted response. `prediction2` is a
##### way of a priori getting a proper TSS score at a reasonable threshold. We
##### know our prevalence is fixed at 0.5, so we set the median value of the
##### prediction to be 0.5, meaning that half of its predictions will be above
##### and half will be below that. In other words, it's not guessing
##### *absolutely*, it's guessing relatively.

# Either use dismo evaluate or caret confusionMatrix
e_all <- predictions %>%
  map(~ dismo::evaluate(p = .x[as.logical(.x$reference), "prediction"],
                        a = .x[!.x$reference, "prediction"]))
e_fixed <- predictions %>%
  map(~ dismo::evaluate(p = .x[as.logical(.x$reference), "prediction2"],
                        a = .x[!.x$reference, "prediction2"],
                        tr = 0.5))
e_free <- map2(predictions,
               e_all, ~ dismo::evaluate(p = .x[as.logical(.x$reference), "prediction"],
                                        a = .x[!.x$reference, "prediction"],
                                        tr = threshold(.y)$kappa))

map(e_all, ~ threshold(.x)$kappa) %>% simplify() %>% mean()




auc <- function(e) {
  auc <- e@auc
  return(auc)
}
tss <- function(e) {
  tss <- e@TPR + e@TNR - 1
  return(unname(tss))
}

kfm_auc <- e_all %>% map_dbl(auc)
kfm_tss_free <- e_free %>% map_dbl(tss)
kfm_tss_fixed <- e_fixed %>% map_dbl(tss)



# Output interactions and summary to text file
sink(file.path(current_out_dir, "cv_with_noweight_summary_unflattened"))
cat(paste0("Model Name: ", model_name, "\n"))
cat("\n\nAUC, which is threshold-free (vector, mean, sd, quantiles)\n")
kfm_auc
mean(kfm_auc)
sd(kfm_auc)
quantile(kfm_auc, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
cat("\n\nTSS, threshold-dependent (vector, mean, sd, quantiles)\n")
cat("\n# Scaling median to 0.5\n")
kfm_tss_fixed
mean(kfm_tss_fixed)
sd(kfm_tss_fixed)
quantile(kfm_tss_fixed, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
cat("\n# Allowing threshold to vary per CV set\n")
kfm_tss_free
mean(kfm_tss_free)
sd(kfm_tss_free)
quantile(kfm_tss_free, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
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
