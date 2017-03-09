fit_brts_to_events <- function(events, brt_params, predictor_names, null_behavior = "drop") {
  # load(file.path(current_cache_dir, paste0(model_name, "_events.RData")))


  brts <- foreach(i = 1:length(events), .verbose = FALSE) %dopar% {
    cat(paste0("About to work on model ", i, "...\n"))
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
                            verbose = FALSE,
                            silent = TRUE),
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

  if (null_behavior == "keep") {
    return(brts)
  }
  if (null_behavior == "drop") {
    brts <- brts[!sapply(brts, is.null)]
  } else if (null_behavior == "rerun") {
    reruns <- 0
    max_reruns <- 200
    while(any(sapply(brts, is.null)) == TRUE & reruns < max_reruns) {
      nulls <- which(sapply(brts, is.null))
      cat(paste0("Rerunning following models (try ", reruns + 1, " of ", max_reruns, "): "))
      cat(nulls)
      cat("\n")
      rerun_brts <- foreach(i = nulls, .verbose = FALSE) %dopar% {
        cat(paste0("About to rerun model ", i, "...\n"))
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
                                verbose = FALSE,
                                silent = TRUE),
                                brt_params))
      }
      brts[nulls] <- rerun_brts
      reruns <- reruns + 1
    }
  }

  return(brts)
  # save(brts, file = file.path(current_cache_dir, paste0(model_name, ".RData")))
}
