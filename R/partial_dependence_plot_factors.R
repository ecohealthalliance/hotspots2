partial_dependence_plot_factors <- function(model, events, model_name) {
  library(ggplot2)
  library(tidyverse)
  library(magrittr)
  # library(boot)


  bsmsum <- summarize_multibrt(model)
  # This is for variable ordering on the plot

  to_plot <- model[[1]]$gbm.call$dataframe %>%
    select_if(is.factor) %>%
    names()

  pd_values_raw <- list()

  for(v in 1:length(to_plot)) {
    cat(paste0("Working on ", to_plot[v], "...\n"))
    pdvar <- foreach(i = 1:length(model), .verbose = FALSE, .combine = rbind) %do% {
      p <- plot.gbm(model[[i]], i.var = to_plot[v], return.grid = TRUE, type = "response")
      p <- data.frame(p, i)
    }
    pdvar$name <- to_plot[v]
    names(pdvar) <- c("x", "y", "i", "name")
    if (is.factor(pdvar[["x"]])) {
      pd_values_raw[[v]] <- pdvar
    }
  }

  pd_values <- as_tibble(bind_rows(pd_values_raw))

  # This is so that we plot the response, not the function
  # pd_values[, 2:4] <- colwise(inv.logit)(pd_values[, 2:4])

  pd_values$name <- factor(pd_values$name, levels = bsmsum$var)


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
             "pubs_fit" = "Reporting Effort",
             "continent" = "Continent")

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
                 "Environment" = "earth12_water",
                 "Geography" = "continent")

  pd_values$Group <- factor(pd_values$name)
  levels(pd_values$Group) <- groups

  pd_values$name <- revalue(pd_values$name, replace = names)

  lim_dist <- pd_values %>%
    summarize(mindist = 0.5 - min(y), maxdist = max(y) - 0.5) %>%
    transpose() %>%
    flatten() %>%
    flatten_dbl %>%
    max %>%
    add(0.005)

  ymin <- 0.5 - lim_dist
  ymax <- 0.5 + lim_dist

  pd_values %<>%
    group_by(name, x) %>%
    arrange(y) %>%
    mutate(ecdf_fun = list(ecdf(y)),
           ecdf_val = ecdf_fun[[1]](y)) %>%
    select(-ecdf_fun) %>%
    ungroup() %>%
    mutate(confint_90 = case_when((ecdf_val <= 0.05) | (ecdf_val >= 0.95) ~ "Outside",
                                    TRUE ~ "Inside"),
          confint_90 = factor(confint_90, levels = c("Outside", "Inside")))

  pd_summary <- pd_values %>%
    group_by(name, x) %>%
    summarize(mean = mean(y))


  ggplot(mapping = aes(x = x)) +
    facet_wrap(~ name, scales = "free_x", ncol = 4) +
    ylim(ymin, ymax) + # Fix y axes for rigor :)
    geom_jitter(data = pd_values, mapping = aes(x = x, y = y, color = confint_90), alpha = 0.95, size = 0.5) +
    geom_boxplot(data = pd_values, mapping = aes(x = x, y = y), outlier.alpha = 0, alpha = 0) +
    # geom_segment(data = pd_summary, mapping = aes(x = x, yintercept = mean)) +
    theme_bw(base_size = 11, base_family = "") +
    labs(x = "Value of Predictor",
         y = "EID Event Risk Index (and 90% CI)",
         title = "Relative EID Risk Index by Continent (0.5 = No Effect)",
         color = "90% Conf. Int.")


  ggsave(file.path(current_out_dir, paste0(model_name, "_partial_dependence_box_scatter.png")),
         height = 7, width = 7)
  ggsave(file.path(current_out_dir, paste0(model_name, "_partial_dependence_box_scatter.pdf")),
         height = 7, width = 7)


  ggplot(mapping = aes(x = x)) +
    facet_wrap(~ name, scales = "free_x", ncol = 4) +
    ylim(ymin, ymax) + # Fix y axes for rigor :)
    geom_jitter(data = pd_values, mapping = aes(x = x, y = y, color = confint_90), alpha = 0.33, size = 1) +
    # geom_boxplot(data = pd_values, mapping = aes(x = x, y = y), outlier.alpha = 0, alpha = 0, size = 0.5) +
    # geom_segment(data = pd_summary, mapping = aes(x = x, yintercept = mean)) +
    theme_bw(base_size = 11, base_family = "") +
    labs(x = "Value of Predictor",
         y = "EID Event Risk Index (and 90% CI)",
         title = "Relative EID Risk Index by Continent (0.5 = No Effect)",
         color = "90% Conf. Int.")

  ggsave(file.path(current_out_dir, paste0(model_name, "_partial_dependence_scatterplot.png")),
         height = 7, width = 7)
  ggsave(file.path(current_out_dir, paste0(model_name, "_partial_dependence_scatterplot.pdf")),
         height = 7, width = 7)


  ggplot(mapping = aes(x = x)) +
    facet_wrap(~ name, scales = "free_x", ncol = 4) +
    ylim(ymin, ymax) + # Fix y axes for rigor :)
    # geom_jitter(data = pd_values, mapping = aes(x = x, y = y, color = confint_90), alpha = 1, size = 0.5) +
    geom_boxplot(data = pd_values, mapping = aes(x = x, y = y, fill = x)) +
    # geom_segment(data = pd_summary, mapping = aes(x = x, yintercept = mean)) +
    theme_bw(base_size = 11, base_family = "") +
    labs(x = "Value of Predictor",
         y = "EID Event Risk Index (and 90% CI)",
         title = "Relative EID Risk Index by Continent (0.5 = No Effect)",
         color = "90% Conf. Int.")


  ggsave(file.path(current_out_dir, paste0(model_name, "_partial_dependence_boxplot.png")),
         height = 7, width = 7)
  ggsave(file.path(current_out_dir, paste0(model_name, "_partial_dependence_boxplot.pdf")),
         height = 7, width = 7)
}
