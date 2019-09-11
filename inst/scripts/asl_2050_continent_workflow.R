# library(hotspots2) # Having issues with cache_dir()
load_all()

library(purrrlyr)
library(foreach)
library(doParallel)
registerDoParallel(20)

# First, we'll load all datasets.

data(decadal)
data(change)
data(eid_metadata)
data(event_coverage)
data(continents)

drivers <- left_join(drivers, continents, by = "iso3")

# Set our directory name and the number of sample iterations we want to conduct.
model_name <- "asl_2050_continent"
sample_iter <- 1000
weighting_varname <- "pubs_fit"
brt_params <- list(tree.complexity = 3,
                   learning.rate = 0.0035,
                   n.trees = 35)
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
                     # "earth10_snowice",
                     # "earth11_barren",
                     # "earth12_water",
                     "gens",
                     "mamdiv",
                     "poultry",
                     "livestock_mam",
                     "continent")

# Create output and cache directories.
current_cache_dir <- file.path(cache_dir(), model_name)
current_out_dir <- file.path(out_dir(), model_name)
dir.create(current_cache_dir, showWarnings = FALSE)
dir.create(current_out_dir, showWarnings = FALSE)

sink(file.path(current_out_dir, "info"))
cat("Model Run Parameters\n")
print(Sys.time())
print(model_name)
print(sample_iter)
print(brt_params)
sink()

# Sample grid cells according to weighting and join to predictors.
# Skip these steps if you just want to refit.
bsm_gridids <- sample_bsm_events(drivers, sample_iter, weighting_varname)
save(bsm_gridids, file = file.path(current_cache_dir, paste0(model_name, "_gridids.RData")))

# load(file.path(current_cache_dir, paste0(model_name, "_gridids.RData")))
bsm_events <- join_predictors(bsm_gridids, predictor_names)
save(bsm_events, file = file.path(current_cache_dir, paste0(model_name, "_events.RData")))

# You can pick up here if you want to re-fit the model.
# load(file.path(current_cache_dir, paste0(model_name, "_events.RData")))
bsm <- fit_brts_to_events(bsm_events, brt_params, predictor_names)
save(bsm, file = file.path(current_cache_dir, paste0(model_name, ".RData")))

# You can start here if you want to just output the plots again.
# load(file.path(current_cache_dir, paste0(model_name, "_events.RData")))
# load(file.path(current_cache_dir, paste0(model_name, ".RData")))
relative_influence_plots(bsm, model_name)
partial_dependence_plots(bsm, bsm_events, model_name)
partial_dependence_plot_truncated(bsm, bsm_events, model_name)
partial_dependence_plot_factors(bsm, bsm_events, model_name)



# Optional maps stuff

# quickmap(sum_presences(bsm_events), log(n))
# quickmap(sum_absences(bsm_events), log(n))

sink(file.path(current_out_dir, "summary"))
cat("Summary\n")
summarize_multibrt(bsm, .parallel = TRUE)
sink()

intsum <- interaction_summary_multibrt(bsm, .parallel = TRUE)
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
intsum$var1.names <- revalue(intsum$var1.names, replace = names)
intsum$var2.names <- revalue(intsum$var2.names, replace = names)

write.csv(intsum, file = file.path(current_out_dir, "interactions.csv"), row.names = FALSE)

# Write percent deviance explained to file

pde <- percent_deviance_explained(bsm)
sink(file.path(current_out_dir, "percent_deviance_explained"))
cat("Percent Deviance Explained\n")
mean(pde)
sd(pde)
pde %>%
  quantile(c(0.05, 0.25, 0.5, 0.75, 0.95))
sink()



# Livestock mammal interaction plots

bsm[[1]]$gbm.call$predictor.names

inter_list <- map(bsm, plot.gbm,
                  i.var = c("livestock_mam", "continent"),
                  return.grid = TRUE,
                  type = "response")

inter_tbl <- bind_rows(inter_list, .id = "model") %>%
  as_tibble()

seq1 <- seq(min(inter_tbl$livestock_mam), max(inter_tbl$livestock_mam), length.out = 101)
group_width = seq1[2]/2
seq2 <- seq1[-1] - group_width

to_plot <- inter_tbl %>%
  mutate(mam_group = cut(livestock_mam, breaks = seq1, include.lowest = TRUE)) %>%
  group_by(mam_group) %>%
  nest() %>%
  mutate(mam_midpoint = seq2) %>%
  unnest(cols = "data") %>%
  group_by(continent, mam_midpoint) %>%
  summarize(mam_mean = mean(livestock_mam),
            q05 = quantile(y, 0.05, names = FALSE, na.rm = TRUE),
            q50 = quantile(y, 0.5, names = FALSE, na.rm = TRUE),
            q95 = quantile(y, 0.95, names = FALSE, na.rm = TRUE))


ggplot(data = to_plot, aes(x = mam_mean)) +
  geom_line(mapping = aes(y = q50, color = continent)) +
  theme_bw(base_size = 11, base_family = "") +
  labs(x = "Livestock Mammal Headcount",
       y = "EID Event Risk Index",
       title = "Livestock Mammal Headcount by Continent",
       color = "Continent")

ggsave(file.path(current_out_dir, paste0(model_name, "_livestock_by_continent.png")),
       height = 7, width = 7)
ggsave(file.path(current_out_dir, paste0(model_name, "_livestock_by_continent.pdf")),
       height = 7, width = 7)


ggplot(data = to_plot, aes(x = mam_mean)) +
  facet_wrap(~ continent, ncol = 3) +

  geom_ribbon(mapping = aes(ymin = q05, ymax = q95, fill = continent), alpha = 0.5) +
  geom_line(mapping = aes(y = q50, color = continent)) +
  theme_bw(base_size = 11, base_family = "") +
  labs(x = "Livestock Mammal Headcount",
       y = "EID Event Risk Index (and 90% CI)",
       title = "Livestock Mammal Headcount by Continent",
       color = "Continent",
       fill = "Continent")

ggsave(file.path(current_out_dir, paste0(model_name, "_livestock_by_continent_faceted.png")),
       height = 7, width = 7)
ggsave(file.path(current_out_dir, paste0(model_name, "_livestock_by_continent_faceted.pdf")),
       height = 7, width = 7)


to_plot_centered <- to_plot %>%
  group_by(continent) %>%
  mutate_at(vars(starts_with("q")), ~ . - mean(.))

ggplot(data = to_plot_centered, aes(x = mam_mean)) +
  geom_line(mapping = aes(y = q50, color = continent)) +
  theme_bw(base_size = 11, base_family = "") +
  labs(x = "Livestock Mammal Headcount",
       y = "EID Event Risk Index, Centered by Continent",
       title = "Livestock Mammal Headcount by Continent",
       color = "Continent")

ggsave(file.path(current_out_dir, paste0(model_name, "_livestock_by_continent_centered.png")),
       height = 7, width = 7)
ggsave(file.path(current_out_dir, paste0(model_name, "_livestock_by_continent_centered.pdf")),
       height = 7, width = 7)
