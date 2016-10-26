relative_influence_plots <- function(model, model_name) {
  library(ggplot2)

  data(drivers)
  # load(file.path(current_cache_dir, paste0(model_name, ".RData")))

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

  bsm_scatter <- ldply(1:length(model), .fun = function(i) {
    y <- summary(model[[i]], plotit = FALSE)
    y$i <- i
    return(y)
  }, .parallel = TRUE)

  bsm_rel_inf <- ddply(bsm_scatter, c("var"), summarize, rel.inf.med = median(rel.inf))
  bsm_rel_inf$var <- as.character(bsm_rel_inf$var)
  bsm_rel_inf <- bsm_rel_inf[order(bsm_rel_inf$rel.inf.med), ]
  bsm_scatter$var <- factor(bsm_scatter$var, levels = bsm_rel_inf$var)
  bsm_scatter$group <- factor(bsm_scatter$var)
  levels(bsm_scatter$group) <- groups

  ggplot(bsm_scatter, aes(x = var, y = rel.inf, fill = group)) +
    geom_boxplot() +
    coord_flip() +
    scale_x_discrete(labels = names) +
    labs(y = "Relative Influence (%)", x = "Predictor", title = NULL) +
    theme_bw(base_size = 11)

  ggsave(file.path(current_out_dir, paste0(model_name, "_relative_influence.png")),
         height = 6, width = 6.5)
  ggsave(file.path(current_out_dir, paste0(model_name, "_relative_influence.pdf")),
         height = 6, width = 6.5)
}
