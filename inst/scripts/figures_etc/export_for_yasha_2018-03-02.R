load_all()

library(dismo)
library(gbm)
library(matrixStats)
library(purrr)
library(viridis)
library(readr)

data(drivers_full)

model_name <- "bsm_1000_iter"
current_out_dir <- file.path(out_dir(), "additional_figures")

#------------------------------#

# Here, we run a prediction for each of the replicate models and save the results into a matrix.
# If you've already done this, start from the next section.

load("cache/bsm_1000_iter/bsm_1000_iter.RData")

response_matrix <- bsm %>%
  map(~ predict(.x, drivers_full, n.trees = .x$n.trees, type = "response")) %>%
  flatten_dbl() %>%
  matrix(nrow = nrow(drivers_full))

save(response_matrix, file = file.path(cache_dir(), "bsm_1000_iter", "response_matrix.RData"))

#------------------------------#

load(file.path(cache_dir(), "bsm_1000_iter", "response_matrix.RData"))

response_tbl <- response_matrix %>% as.data.frame() %>% as.tbl()

bsm_response_mean <- drivers_full %>%
	select(gridid, lon, lat) %>%
	mutate(response_mean = rowMeans(response_matrix),
	       response_sd = rowSds(response_matrix))

bsm_response_quantiles = response_matrix %>%
	rowQuantiles(probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) %>%
	as.data.frame()
names(bsm_response_quantiles) <- c("Q05", "Q25", "Q5", "Q75", "Q95")

bsm_response <- bind_cols(bsm_response_mean, bsm_response_quantiles) %>%
	as.tbl()

# This section needs to be adapted to compute the statistics for the actual layers. Or I'll ask Yasha.
to_export <- drivers_full %>%
  mutate(weight_pubs = pubs_fit / sum(pubs_fit, na.rm = TRUE),
         weight_pop = pop / sum(pop, na.rm = TRUE)) %>%
  select(weight_pubs, weight_pop) %>%
  bind_cols(bsm_response)



write_csv(to_export, path = file.path("inst", "out", "export_for_yasha_2018-03-05","bsm_output.csv"))
write_csv(response_tbl, path = file.path("inst", "out", "export_for_yasha_2018-03-05","response_tbl.csv"))
