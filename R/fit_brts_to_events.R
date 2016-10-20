fit_brts_to_events <- function(events, brt_params, predictor_names) {
  # load(file.path(current_cache_dir, paste0(model_name, "_events.RData")))


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
