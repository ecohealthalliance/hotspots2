run_bsm <- function(model_name) {
  load(file.path(cache_dir(), paste0("bsm_events.RData")))

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


  bsm <- foreach(i = 1:length(bsm_events), .verbose = TRUE) %dopar% {
    # gbm.step() doesn't like tibbles.
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
                      plot.main = FALSE,
                      verbose = TRUE)

    return(model)
  }

  bsm <- bsm[!sapply(bsm, is.null)]

  save(bsm, file = file.path(current_cache_dir, paste0(model_name, ".RData")))
}