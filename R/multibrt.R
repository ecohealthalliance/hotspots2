percent_deviance_explained <- function(object) {
  deviances <- laply(bsm, function(x) (x$self.statistics$mean.null - x$self.statistics$mean.resid) / x$self.statistics$mean.null)
  pde <- data.frame("mean" = mean(deviances), "sd" = sd(deviances))
  return(pde)
}


get_melted_rel_inf <- function(object, .parallel = FALSE) {
    rel_inf <- ldply(1:length(object), .fun = function(i) {
    y <- summary(object[[i]], plotit = FALSE)
    y$i <- i
    return(y)
    }, .parallel = .parallel)
    return(rel_inf)
}


summarize_multibrt <- function(object, .parallel = FALSE) {
  x <- get_melted_rel_inf(object, .parallel)

  brtsum <- ddply(x, c("var"), summarize, rel.inf.med = median(rel.inf),
                                          rel.inf.q1 = quantile(rel.inf, probs = 0.25),
                                          rel.inf.q3 = quantile(rel.inf, probs = 0.75),
                                          rel.inf.mean = mean(rel.inf),
                                          rel.inf.sd = sd(rel.inf))

  brtsum$var <- as.character(brtsum$var)
  brtsum <- brtsum[order(brtsum$rel.inf.med, decreasing = TRUE), ]

  return(brtsum)
}


interactions_multibrt <- function(object, .parallel = FALSE) {
  capture.output(intlist <- llply(object, function(x) gbm.interactions(x)[[2]], .parallel = .parallel))
  intlist <- intlist[!sapply(intlist, is.null)]

  intsum <- array(dim = c(nrow(intlist[[1]]), ncol(intlist[[1]]), length(intlist)))
  colnames(intsum) <- colnames(intlist[[1]])
  rownames(intsum) <- rownames(intlist[[1]])

  for (i in 1:length(intlist)) {
    intsum[, , i] <- as.matrix(intlist[[i]])
  }

  intsum <- data.frame(aaply(.data = intsum, .margins = c(1, 2), .fun = mean))
  return(intsum)
}


interaction_summary_multibrt <- function(object, .parallel = FALSE) {
  intlist <- llply(object, function(x) gbm.interactions(x)[[1]], .parallel = .parallel)
  intlist <- intlist[!sapply(intlist, is.null)]
  intsum <- intlist %>%
    map(.x = intlist, .f = ~ select(.x, -dplyr::contains("index"))) %>%
    bind_rows() %>%
    group_by(var1.names, var2.names) %>%
    summarize(int.mean = mean(int.size, na.rm = TRUE),
              int.min = min(int.size, na.rm = TRUE),
              int.q25 = quantile(int.size, probs = 0.25, na.rm = TRUE),
              int.med = median(int.size, na.rm = TRUE),
              int.q75 = quantile(int.size, probs = 0.75, na.rm = TRUE),
              int.max = max(int.size, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(-int.med)

  return(intsum)
}

predict_multibrt <- function(multibrt, newdata, type = "response", value = "mean") {
  library(matrixStats)
  library(purrr)
  prediction_matrix <- multibrt %>%
    map(~ predict(.x, newdata, n.trees = .x$n.trees, type = type)) %>%
    flatten_dbl() %>%
    matrix(nrow = nrow(newdata))
  if (value == "mean") {
    return(rowMeans(prediction_matrix))
  } else if (value == "sd") {
    return(rowSds(prediction_matrix))
  } else if (value == "matrix") {
    return(prediction_matrix)
  }
}
