## ----load----------------------------------------------------------------
load_all()
library(hotspots2)
library(raster)
library(dplyr)
library(tidyr)
library(purrr)
library(foreach)
library(doParallel)
library(ggplot2)
library(boot)

library(dismo)
library(gbm)

# First, we'll load all datasets.

data(decadal)
data(change)
data(eid_metadata)
data(event_coverage)

library(dismo)
library(gbm)

# Make sure you name the thing "pubs_fit". cache_name should be the name of the file you wanna save.
sample_events <- function(drivers, cache_name, sample_iter = 100) {
  ## ----select-events-------------------------------------------------------
  selected_events <- eid_metadata %>%
    filter(wildlife_zoonoses == 1,
           event_year >= 1970) %>%
    select(name = eid_name,
           year = event_year)


  ## ----create-sampling-weights---------------------------------------------
  set.seed(20140605)

  presence_weights <- event_coverage %>%
    filter(event_name %in% selected_events$name) %>%
    left_join(select(drivers, gridid, pubs_fit)) %>%
    group_by(event_name) %>%
    # only_if()(mutate)(weight = 1) %>%
    mutate(weight = coverage * pubs_fit / sum(coverage * pubs_fit, na.rm = TRUE),
           # We do this part to provide any weights where the publication value is NA.
           total_weight = sum(weight, na.rm = TRUE),
           weight = ifelse(total_weight == 0, coverage, weight)) %>%
    ungroup() %>%
    replace_na(replace = list(weight = 0, pubs_fit = 0))


  # We now deal with this by replacing pasture and crop NAs with 0s. There are a
  # few locations where our presence weights don't overlap the drivers data frame.
  # Because of this, we will omit those. quickmap(semi_join(presence_weights,
  # decadal), weight)

  # presence_weights <- semi_join(presence_weights, decadal)
  # This removes all grid cells belonging to HED_166



  absence_weights <- drivers %>%
    select(gridid, pubs_fit) %>%
    replace_na(replace = list(pubs_fit = 0))

  # There are two polygons which did not produce a presence weight.
  selected_events <- selected_events %>%
    filter(name %in% presence_weights$event_name)




  ## ----sample-events-and-gridids-------------------------------------------

  # First, we sample randomly among events.
  sampled_events <- foreach(i = 1:sample_iter) %do% {
    sample_n(selected_events, size = 149, replace = TRUE)
  }

  # Next, for each event, we select a presence and absence as described above.
  sample_gridids <- function(to_sample) {
    # print(to_sample$name)
    presence <- presence_weights %>%
      filter(event_name == to_sample$name) %>%
      sample_n(size = 1, weight = weight) %>%
      select(gridid) %>%
      data.frame(presence = 1)

    absence <- absence_weights %>%
      sample_n(size = 1, weight = pubs_fit) %>%
      select(gridid) %>%
      data.frame(presence = 0)

    sampled <- rbind(presence, absence)
    # sampled$name <- to_sample$name
    # sampled$year <- to_sample$year
    return(sampled)
  }

  # # There is a way to do this with purrr, but it's much faster with foreach in parallel, so we'll use that.
  # bsm_events1 <- sampled_events %>%
  #   map(~ by_row(., sample_gridids, .collate = "row"))

  system.time(
  bsm_gridids <- foreach(i = sampled_events) %dopar% {
    by_row(i, sample_gridids, .collate = "row") %>%
      select(-.row)
  }
  )

  save(bsm_gridids, file = file.path(cache_dir(), "test_runs", paste0(cache_name, "_gridids.RData")))
}



join_predictors <- function(cache_name) {
  load(file.path(cache_dir(), "test_runs", paste0(cache_name, "_gridids.RData")))


  ## ----prepare-------------------------------------------------------------
  # Remove some other variables that we were keeping for one reason or another.
  drivers <- drivers %>%
    select(-gdp, -land_area, -iso3)

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

  # from_decadal <- predictor_names[predictor_names %in% names(decadal)]
  # from_change <- predictor_names[predictor_names %in% names(change)]
  # from_drivers <- predictor_names[!(predictor_names %in% c(from_decadal, from_change))]

  # decade <- function(x) floor(x / 10) * 10
  # middec <- function(x) round(x - 4.5, -1) + 5

  nearest <- function(x, choices) {
    choices[which.min(abs(choices - x))]
  }

  time_slice_vars <- function(event) {
    decadal_vars <- decadal %>%
      filter(year == nearest(event$year, choices = unique(.$year)),
             gridid == event$gridid) %>%
      select(crop, past, pop)

    change_vars <- change %>%
      filter(year == nearest(event$year, choices = unique(.$year)),
             gridid == event$gridid) %>%
      select(crop_change, past_change, pop_change)

    return(cbind(decadal_vars, change_vars))
  }

  system.time(
  bsm_events_dec <- foreach(i = bsm_gridids) %dopar% {
    by_row(i, time_slice_vars, .collate = "row") %>%
      select(-.row)
  }
  )

  remaining_names <- predictor_names[which(!predictor_names %in% names(bsm_events_dec[[1]]))]

  system.time(
  bsm_events <- foreach(i = bsm_events_dec) %dopar% {
    left_join(i, drivers[, c("gridid", "lon", "lat", remaining_names)])
  }
  )

  save(bsm_events, file = file.path(cache_dir(), "test_runs", paste0(cache_name, "_events.RData")))
}








run_models <- function(cache_name) {
  load(file.path(cache_dir(), "test_runs", paste0(cache_name, "_events.RData")))

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
                      learning.rate = 0.001,
                      n.trees = 50,
                      plot.main = FALSE,
                      verbose = TRUE)

    return(model)
  }

  save(bsm, file = file.path(cache_dir(), "test_runs", paste0(cache_name, "_bsm.RData")))
}







relative_influence_plots <- function(cache_name) {
  data(drivers)
  load(file.path(cache_dir(), "test_runs", paste0(cache_name, "_bsm.RData")))

  # Relative Influence Plot
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
             "livestock_mam" = "Livestock Mammal\nHeadcount")

  groups <- list("Human Activity" = "pop",
                 "Human Activity" = "pop_change",
                 "Human Activity" = "crop",
                 "Human Activity" = "past",
                 "Human Activity" = "past_change",
                 "Human Activity" = "crop_change",
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
                 "Human Activity" = "earth7_veg_manag",
                 "Environment" = "earth8_veg_flood",
                 "Human Activity" = "earth9_urban",
                 "Environment" = "earth10_snowice",
                 "Environment" = "earth11_barren",
                 "Environment" = "earth12_water")

  bsm_scatter <- ldply(1:length(bsm), .fun = function(i) {
    y <- summary(bsm[[i]], plotit = FALSE)
    y$i <- i
    return(y)
  }, .parallel = TRUE)

  bsm_rel_inf <- ddply(bsm_scatter, c("var"), summarize, rel.inf.med = median(rel.inf))
  bsm_rel_inf$var <- as.character(bsm_rel_inf$var)
  bsm_rel_inf <- bsm_rel_inf[order(bsm_rel_inf$rel.inf.med), ]
  bsm_scatter$var <- factor(bsm_scatter$var, levels = bsm_rel_inf$var)
  bsm_scatter$group <- factor(bsm_scatter$var)
  levels(bsm_scatter$group) <- groups

  ggplot(bsm_scatter, aes(x = var, y = rel.inf, fill = group)) + geom_boxplot() + coord_flip() + scale_x_discrete(labels = names) + labs(y = "Relative Influence (%)", x = "Variable", title = "Relative influence of drivers on\n zoonotic EID event risk index") + theme_bw(base_size = 11)

  ggsave(file.path(out_dir(), "test_runs", paste0(cache_name, "_relative_influence.png")),
         height = 6, width = 6.5)
  ggsave(file.path(out_dir(), "test_runs", paste0(cache_name, "_relative_influence.pdf")),
         height = 6, width = 6.5)
}







partial_dependence_plots <- function(cache_name) {
  data(drivers)
  load(file.path(cache_dir(), "test_runs", paste0(cache_name, "_bsm.RData")))


  # While I'm getting this working, do it with a small subset of models.
  # bsm <- bsm[1:5]


  to_plot <- bsm[[1]]$gbm.call$predictor.names

  partial_dependence_raw <- list()

  for(v in 1:length(to_plot)) {
    cat(paste0("Working on ", to_plot[v], "...\n"))
    pdvar <- foreach(i = 1:length(bsm), .verbose = FALSE, .combine = rbind) %do% {
      p <- plot.gbm(bsm[[i]], i.var = to_plot[v], return.grid = TRUE)
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


  # This is so that we plot the response, not the function
  pdq[, 2:4] <- colwise(inv.logit)(pdq[, 2:4])

  bsmsum <- summarize_multibrt(bsm)
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
             "livestock_mam" = "Livestock Mammal\nHeadcount")

  groups <- list("Human Activity" = "pop",
                 "Human Activity" = "pop_change",
                 "Human Activity" = "crop",
                 "Human Activity" = "past",
                 "Human Activity" = "past_change",
                 "Human Activity" = "crop_change",
                 "Human Activity" = "earth9_urban",
                 "Human Activity" = "earth7_veg_manag",
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

  # Now we can make a partial dependence plot
  ggplot(pdq, aes(x = x)) +
    facet_wrap(~ name, scales = "free", ncol = 4) +
    ylim(ymin, ymax) + # Fix y axes for rigor :)
    geom_ribbon(aes(ymin = q05, ymax = q95, fill = Group), alpha = 0.75) +
    geom_line(aes(y = q50)) + theme_bw(base_size = 11, base_family = "") +
    labs(x = "Value of driver",
         y = "Relative probability of EID event occurrence (and 95% CI)",
         title = "Partial dependence plot for zoonotic EID event occurrence")







  # Making the histogram

  load(file.path(cache_dir(), "test_runs", paste0(cache_name, "_events.RData")))
  bsm_hist_data <- do.call(rbind, bsm_events)

  # Make sure that the factor of names is exactly the same as the above plot.
  bsm_hist_data <- bsm_hist_data[, names(bsm_hist_data) %in% c("gridid", names(names))]
  bsm_hist_data <- reshape2::melt(bsm_hist_data, id.vars = "gridid")
  bsm_hist_data$variable <- revalue(bsm_hist_data$variable, replace = names)
  names(bsm_hist_data) <- c("gridid", "name", "x")

  p <- ggplot(mapping = aes(x = x)) +
    facet_wrap(~ name, scales = "free", ncol = 4) +
    geom_histogram(data = bsm_hist_data)


  plotted_data <- ggplot_build(p)$data[[1]]
  plotted_data$name <- unique(bsm_hist_data$name)[plotted_data$PANEL]
  plotted_data <- plotted_data[, c("name", "x", "y")]

  # This is the part where I rescale the axes.
  hist_final <- foreach(name = unique(pdq$name), .combine = rbind) %do% {
    pdq_subset <- pdq[pdq$name == name, ]
    plotted_data_sub <- plotted_data[plotted_data$name == name, ]

    xmin <- min(pdq_subset$x)
    xmax <- max(pdq_subset$x)
    # ymax <- max(pdq_subset$q95, na.rm = TRUE)

    yminrm <- sum(plotted_data_sub[plotted_data_sub$x < xmin, "y"])
    ymaxrm <- sum(plotted_data_sub[plotted_data_sub$x > xmax, "y"])

    plotted_data_sub <- plotted_data_sub[plotted_data_sub$x > xmin & plotted_data_sub$x < xmax, ]
    plotted_data_sub[1, "y"] <- plotted_data_sub[1, "y"] + yminrm
    plotted_data_sub[nrow(plotted_data_sub), "y"] <- plotted_data_sub[nrow(plotted_data_sub), "y"] + ymaxrm

    plotted_data_sub$y <-  plotted_data_sub$y * (ymax / max(plotted_data_sub$y))
    plotted_data_sub
  }

  hist_final$name <- factor(hist_final$name, levels = levels(pdq$name))


  ggplot(mapping = aes(x = x)) +
    facet_wrap(~ name, scales = "free_x", ncol = 4) +
    ylim(0, ymax) + # Fix y axes for rigor :)
    geom_segment(data = hist_final, mapping = aes(y = 0, yend = y, xend = x),
                 color = "#999999") +
    geom_ribbon(data = pdq, mapping = aes(ymin = q05, ymax = q95, fill = Group), alpha = 0.75) +
    geom_line(data = pdq, mapping = aes(y = q50)) +
    theme_bw(base_size = 11, base_family = "") +
    labs(x = "Value of driver",
         y = "Relative probability of EID event occurrence (and 95% CI)",
         title = "Partial dependence plot for zoonotic EID event occurrence")


  ggsave(file.path(out_dir(), "test_runs", paste0(cache_name, "_partial_dependence_hist.png")),
         height = 9, width = 8.5)
  ggsave(file.path(out_dir(), "test_runs", paste0(cache_name, "_partial_dependence_hist.pdf")),
         height = 9, width = 8.5)














  # ALTERNATE FUTURE VERSION
  # In this version, we only plot the (0.05, 0.95) range of the x axis.

  x_cutoff <- c(0.1, 0.9)


  load(file.path(cache_dir(), "test_runs", paste0(cache_name, "_events.RData")))
  bsm_hist_data <- do.call(rbind, bsm_events)

  # Make sure that the factor of names is exactly the same as the above plot.
  bsm_hist_data <- bsm_hist_data[, names(bsm_hist_data) %in% c("gridid", names(names))]
  bsm_hist_data <- reshape2::melt(bsm_hist_data, id.vars = "gridid")
  bsm_hist_data$variable <- revalue(bsm_hist_data$variable, replace = names)
  names(bsm_hist_data) <- c("gridid", "name", "x")

  # This is the part where I rescale the axes.
  # I'm going to try creating the histogram *after* we subset.
  hist_final <- foreach(name = unique(pdq$name), .combine = rbind) %do% {
    pdq_subset <- pdq[pdq$name == name, ]
    bsm_hist_data_sub <- bsm_hist_data[bsm_hist_data$name == name, ]

    # In this version, we 
    xmin <- quantile(bsm_hist_data_sub$x, probs = x_cutoff)[1]
    xmax <- quantile(bsm_hist_data_sub$x, probs = x_cutoff)[2]
    # ymax <- max(pdq_subset$q95, na.rm = TRUE)

    bsm_hist_data_sub <- bsm_hist_data_sub[bsm_hist_data_sub$x >= xmin & bsm_hist_data_sub$x <= xmax, ]

    p <- ggplot(mapping = aes(x = x)) +
      facet_wrap(~ name, scales = "free", ncol = 4) +
      geom_histogram(data = bsm_hist_data_sub)

    plotted_data <- ggplot_build(p)$data[[1]]
    plotted_data$name <- name
    plotted_data <- plotted_data[, c("name", "x", "y")]

    # Here is where I'd rescale the y axis.
    yminrm <- sum(plotted_data[plotted_data$x < xmin, "y"])
    ymaxrm <- sum(plotted_data[plotted_data$x > xmax, "y"])


    # We use >= and <= here else Snow/Ice throws an error.
    plotted_data <- plotted_data[plotted_data$x >= xmin & plotted_data$x <= xmax, ]
    plotted_data[1, "y"] <- plotted_data[1, "y"] + yminrm
    plotted_data[nrow(plotted_data), "y"] <- plotted_data[nrow(plotted_data), "y"] + ymaxrm

    plotted_data$y <-  plotted_data$y * (ymax / max(plotted_data$y))
    plotted_data
  }

  hist_final$name <- factor(hist_final$name, levels = levels(pdq$name))


  # We also have to do this to pdq now
  pdq_final <- foreach(name = unique(pdq$name), .combine = rbind) %do% {
    pdq_subset <- pdq[pdq$name == name, ]
    plotted_data_sub <- plotted_data[plotted_data$name == name, ]
    bsm_hist_data_sub <- bsm_hist_data[bsm_hist_data$name == name, ]

    # In this version, we 
    xmin <- quantile(bsm_hist_data_sub$x, probs = x_cutoff)[1]
    xmax <- quantile(bsm_hist_data_sub$x, probs = x_cutoff)[2]
    # ymax <- max(pdq_subset$q95, na.rm = TRUE)

    pdq_subset <- pdq_subset[pdq_subset$x >= xmin & pdq_subset$x <= xmax, ]
    pdq_subset
  }


  ggplot(mapping = aes(x = x)) +
    facet_wrap(~ name, scales = "free_x", ncol = 4) +
    ylim(0, ymax) + # Fix y axes for rigor :)
    geom_segment(data = hist_final, mapping = aes(y = 0, yend = y, xend = x),
                 color = "#999999") +
    geom_ribbon(data = pdq_final, mapping = aes(ymin = q05, ymax = q95, fill = Group), alpha = 0.75) +
    geom_line(data = pdq_final, mapping = aes(y = q50)) +
    theme_bw(base_size = 11, base_family = "") +
    labs(x = "Value of driver",
         y = "Relative probability of EID event occurrence (and 95% CI)",
         title = "Partial dependence plot for zoonotic EID event occurrence")

  ggsave(file.path(out_dir(), "test_runs", paste0(cache_name, "_partial_dependence_hist_truncated.pdf")),
         height = 9, width = 8.5)
  ggsave(file.path(out_dir(), "test_runs", paste0(cache_name, "_partial_dependence_hist_truncated.png")),
         height = 9, width = 8.5)
}