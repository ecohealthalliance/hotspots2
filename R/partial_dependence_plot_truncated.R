partial_dependence_plot_truncated <- function(model, events, model_name) {
  library(ggplot2)
  library(boot)

  data(drivers)
  # load(file.path(current_cache_dir, paste0(model_name, ".RData")))


  # While I'm getting this working, do it with a small subset of models.
  # model <- model[1:5]


  to_plot <- model[[1]]$gbm.call$predictor.names

  partial_dependence_raw <- list()

  for(v in 1:length(to_plot)) {
    cat(paste0("Working on ", to_plot[v], "...\n"))
    pdvar <- foreach(i = 1:length(model), .verbose = FALSE, .combine = rbind) %do% {
      p <- plot.gbm(model[[i]],
                    i.var = to_plot[v],
                    continuous.resolution = 300, # Because we'll be subsetting
                    return.grid = TRUE,
                    type = "response")
      p <- data.frame(p, i)
    }
    pdvar$name <- to_plot[v]
    names(pdvar) <- c("x", "y", "i", "name")
    partial_dependence_raw[[v]] <- pdvar
  }

  pdq <- list()

  for(v in 1:length(partial_dependence_raw)) {
    var <- partial_dependence_raw[[v]]

    name <- var$name[1]
    cat(paste0("Working on ", name, "...\n"))

    s <- seq(min(var$x), max(var$x), length.out = 101)
    a <- array(c(s[1:length(s)-1], s[2:length(s)]), dim = c(length(s) - 1, 2))

    var2 <- adply(a, 1, function(i) {
      y <- var[var$x >= i[1] & var$x < i[2], "y"]
      x <- mean(c(i[1], i[2]))
      q <- (quantile(y, c(0.05, 0.5, 0.95), names = FALSE, na.rm = TRUE))
      df <- data.frame(t(c(x, q)))
      names(df) <- c("x", "q05", "q50", "q95")
      df$name <- name
      return(df)
    })
    pdq[[v]] <- var2[2:length(var2)]
  }
  pdq <- do.call(rbind, pdq)


  # This is so that we plot the response, not the function NO, NOT ANY MORE, YOU CAN DO 'TYPE'
  # pdq[, 2:4] <- colwise(inv.logit)(pdq[, 2:4])

  bsmsum <- summarize_multibrt(model)
  # This is for variable ordering on the plot
  pdq$name <- factor(pdq$name, levels = bsmsum$var)


  names <- c("past_change" = "Pasture Change",
             "earth6_veg_herba" = "Herbaceous Veg.",
             "earth9_urban" = "Urban/Built-up",
             "crop" = "Cropland",
             "mamdiv" = "Mammal Biodiversity",
             "pop_change" = "Population Change",
             "earth5_shrubs" = "Shrubs",
             "earth7_veg_manag" = "Cultivated/Managed\nVeg.",
             "earth12_water" = "Water",
             "earth10_snowice" = "Snow/Ice",
             "poultry" = "Poultry",
             "earth11_barren" = "Barren",
             "earth1_trees_needl" = "Evergreen/Deciduous\nNeedleleaf Trees",
             "earth8_veg_flood" = "Regularly Flooded Veg.",
             "earth3_trees_decid" = "Deciduous Broadleaf\nTrees",
             "pop" = "Population",
             "crop_change" = "Cropland Change",
             "gens" = "Global Envir. Strat.",
             "earth4_trees_other" = "Mixed/Other Trees",
             "past" = "Pasture",
             "earth2_trees_everg" = "Evergreen Broadleaf\nTrees",
             "livestock_mam" = "Livestock Mammal\nHeadcount",
             "pubs_fit" = "Reporting Effort")

  groups <- list("Human Activity" = "pop",
                 "Human Activity" = "pop_change",
                 "Human Activity" = "crop",
                 "Human Activity" = "past",
                 "Human Activity" = "past_change",
                 "Human Activity" = "crop_change",
                 "Human Activity" = "earth9_urban",
                 "Human Activity" = "earth7_veg_manag",
                 "Human Activity" = "pubs_fit",
                 "Animals" = "mamdiv",
                 "Animals" = "livestock_mam",
                 "Animals" = "poultry",
                 "Environment" = "gens",
                 "Environment" = "earth1_trees_needl",
                 "Environment" = "earth2_trees_everg",
                 "Environment" = "earth3_trees_decid",
                 "Environment" = "earth4_trees_other",
                 "Environment" = "earth5_shrubs",
                 "Environment" = "earth6_veg_herba",
                 "Environment" = "earth8_veg_flood",
                 "Environment" = "earth10_snowice",
                 "Environment" = "earth11_barren",
                 "Environment" = "earth12_water")

  pdq$Group <- factor(pdq$name)
  levels(pdq$Group) <- groups

  pdq$name <- revalue(pdq$name, replace = names)

  # Take parallel minimum of 0.02 so we don't see clipping in plot

  ymin <- 0.4 # This is only used in this next plot for now.
  pdq[, 2:4] <- colwise(pmax, ... = ymin)(pdq[, 2:4])

  ymax <- 0.6 # We will use this value in two places.
  pdq[, 2:4] <- colwise(pmin, ... = ymax)(pdq[, 2:4])

  # ALTERNATE FUTURE VERSION
  # In this version, we only plot the (0.05, 0.95) range of the x axis.

  # Set the quantile cutoffs for the x axes.
  x_cutoff <- c(0.1, 0.9)


  # load(file.path(current_cache_dir, paste0(model_name, "_events.RData")))
  bsm_hist_data <- do.call(rbind, events)

  # Make sure that the factor of names is exactly the same as the above plot.
  bsm_hist_data <- bsm_hist_data[, names(bsm_hist_data) %in% c("gridid", names(names))]
  bsm_hist_data <- reshape2::melt(bsm_hist_data, id.vars = "gridid")
  bsm_hist_data$variable <- revalue(bsm_hist_data$variable, replace = names)
  names(bsm_hist_data) <- c("gridid", "name", "x")

  # This is the part where I rescale the axes.
  # I'm going to try creating the histogram *after* we subset.
  hist_final <- foreach(name = unique(pdq$name), .combine = rbind) %do% {
    bsm_hist_data_sub <- bsm_hist_data[bsm_hist_data$name == name, ]

    xmin <- quantile(bsm_hist_data_sub$x, probs = x_cutoff, na.rm = TRUE)[1]
    xmax <- quantile(bsm_hist_data_sub$x, probs = x_cutoff, na.rm = TRUE)[2]
    # ymax <- max(pdq_subset$q95, na.rm = TRUE)

    bsm_hist_data_sub <- bsm_hist_data_sub[bsm_hist_data_sub$x >= xmin & bsm_hist_data_sub$x <= xmax, ]

    p <- ggplot(mapping = aes(x = x)) +
      facet_wrap(~ name, scales = "free", ncol = 4) +
      geom_histogram(data = bsm_hist_data_sub)

    plotted_data <- ggplot_build(p)$data[[1]]
    plotted_data$name <- name
    plotted_data <- plotted_data[, c("name", "x", "y")]

    # We don't do this because we don't actually WANT to add removed Y to the thing.
    # yminrm <- sum(plotted_data[plotted_data$x < xmin, "y"])
    # ymaxrm <- sum(plotted_data[plotted_data$x > xmax, "y"])


    # We use >= and <= here else Snow/Ice throws an error.
    plotted_data <- plotted_data[plotted_data$x >= xmin & plotted_data$x <= xmax, ]
    # plotted_data[1, "y"] <- plotted_data[1, "y"] + yminrm
    # plotted_data[nrow(plotted_data), "y"] <- plotted_data[nrow(plotted_data), "y"] + ymaxrm

    plotted_data$y <-  (ymax - ymin) / (max(plotted_data$y) - min(plotted_data$y)) *
      (plotted_data$y - max(plotted_data$y)) + ymax
    plotted_data
  }


  hist_final$name <- factor(hist_final$name, levels = levels(pdq$name))


  # We also have to do this to pdq now
  pdq_final <- foreach(name = unique(pdq$name), .combine = rbind) %do% {
    pdq_subset <- pdq[pdq$name == name, ]
    plotted_data_sub <- plotted_data[plotted_data$name == name, ]
    bsm_hist_data_sub <- bsm_hist_data[bsm_hist_data$name == name, ]

    xmin <- quantile(bsm_hist_data_sub$x, probs = x_cutoff, na.rm = TRUE)[1]
    xmax <- quantile(bsm_hist_data_sub$x, probs = x_cutoff, na.rm = TRUE)[2]
    # ymax <- max(pdq_subset$q95, na.rm = TRUE)

    pdq_subset <- pdq_subset[pdq_subset$x >= xmin & pdq_subset$x <= xmax, ]
    pdq_subset
  }


  # One-ioff tweak to rescale the x axes where numbers are overlapping.
  pdq_final2 <- pdq_final
  pdq_final2[pdq_final2$name == "Population", "x"] <- pdq_final2[pdq_final2$name == "Population", "x"] / 1000000
  pdq_final2[pdq_final2$name == "Livestock Mammal\nHeadcount", "x"] <- pdq_final2[pdq_final2$name == "Livestock Mammal\nHeadcount", "x"] / 100000
  pdq_final2$name <- fct_recode(pdq_final2$name, c("Livestock Mammal\nHeadcount (/100,000)" = "Livestock Mammal\nHeadcount"), c("Population\n(/1,000,000)" = "Population"))

  hist_final2 <- hist_final
  hist_final2[hist_final2$name == "Population", "x"] <- hist_final2[hist_final2$name == "Population", "x"] / 1000000
  hist_final2[hist_final2$name == "Livestock Mammal\nHeadcount", "x"] <- hist_final2[hist_final2$name == "Livestock Mammal\nHeadcount", "x"] / 100000
  hist_final2$name <- fct_recode(hist_final2$name, c("Livestock Mammal\nHeadcount (/100,000)" = "Livestock Mammal\nHeadcount"), c("Population\n(/1,000,000)" = "Population"))



  ggplot(mapping = aes(x = x)) +
    facet_wrap(~ name, scales = "free_x", ncol = 4) +
    ylim(ymin, ymax) + # Fix y axes for rigor :)
    geom_segment(data = hist_final2, mapping = aes(y = ymin, yend = y, xend = x),
                 color = "#999999") +
    geom_ribbon(data = pdq_final2, mapping = aes(ymin = q05, ymax = q95, fill = Group), alpha = 0.75) +
    geom_line(data = pdq_final2, mapping = aes(y = q50)) +
    theme_bw(base_size = 11, base_family = "") +
    labs(x = "Value of Predictor",
         y = "EID Event Risk Index (and 90% CI)",
         title = NULL)

  ggsave(file.path(current_out_dir, paste0(model_name, "_partial_dependence_hist_truncated.pdf")),
         height = 9, width = 8.5)
  ggsave(file.path(current_out_dir, paste0(model_name, "_partial_dependence_hist_truncated.png")),
         height = 9, width = 8.5)
}
