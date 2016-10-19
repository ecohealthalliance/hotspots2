load_all()

dir.create(file.path(out_dir(), "additional_figures"), showWarnings = FALSE)

library(ggplot2)
library(GGally)
library(dplyr)

# Correlation Matrix

data(drivers_full)
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
                     "livestock_mam",
                     "pubs_fit")

# It's pointless to examine this many drivers in a scatterplot matrix, so we'll
# examine a correlation matrix. We'll manually pick variables of interest to
# look for correlations.

drivers_full %>%
  select(one_of(predictor_names)) %>%
  ggcorr(label = TRUE)

to_examine <- c("pop",
                "pop_change",
                "crop",
                "earth7_veg_manag",
                "past",
                "earth6_veg_herba",
                "earth2_trees_everg",
                "mamdiv",
                "poultry",
                "livestock_mam",
                "earth9_urban",
                "pubs_fit",
                "gens")

drivers_full %>%
  select(one_of(to_examine)) %>%
  ggcorr(label = TRUE)

drivers_full %>%
  select(one_of(to_examine)) %>%
  sample_n(500) %>%
  ggpairs() + theme_bw()

# And with log transforms
drivers_full %>%
  select(one_of(predictor_names)) %>%
  log() %>%
  mutate_each(funs(replace(., is.infinite(.), NA))) %>%
  ggcorr(label = TRUE)

drivers_full %>%
  select(one_of(to_examine)) %>%
  log() %>%
  mutate_each(funs(replace(., is.infinite(.), NA))) %>%
  sample_n(500) %>%
  ggpairs() + theme_bw()

ggpairs(drivers_full[, predictor_names])
