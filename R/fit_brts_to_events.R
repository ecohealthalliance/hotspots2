fit_brts_to_events <- function(events, brt_params) {
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


  brts <- foreach(i = 1:length(events), .verbose = TRUE) %dopar% {
    print(paste0("About to work on model ", i, "..."))
    # gbm.step() doesn't like tibbles.
    bsm_data <- as.data.frame(events[[i]])

    x_ind <- which(colnames(bsm_data) %in% predictor_names)
    y_ind <- which(colnames(bsm_data) %in% "presence")

    # This code allows us to pass in brt_params,
    # And run the model as below.
    model <- do.call(gbm.step,
                     c(list(data = bsm_data,
                            gbm.x = x_ind,
                            gbm.y = y_ind,
                            family = "bernoulli",
                            plot.main = FALSE,
                            verbose = FALSE),
                     brt_params))

    # model <- gbm.step(data = bsm_data,
    #                   gbm.x = x_ind,
    #                   gbm.y = y_ind,
    #                   family = "bernoulli",
    #                   tree.complexity = 3,
    #                   learning.rate = 0.005,
    #                   n.trees = 50,
    #                   plot.main = FALSE,
    #                   verbose = FALSE)

    return(model)
  }

  brts <- brts[!sapply(brts, is.null)]

  return(brts)
  # save(brts, file = file.path(current_cache_dir, paste0(model_name, ".RData")))
}
