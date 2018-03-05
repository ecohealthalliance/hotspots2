load_all()

library(dismo)
library(gbm)
library(matrixStats)
library(purrr)
library(viridis)

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

bsm_response <- drivers_full %>%
	select(gridid) %>%
	mutate(response_mean = rowMeans(response_matrix),
	       response_sd = rowSds(response_matrix),
	       response_median = rowMedians(response_matrix),
	       response_median = rowMedians(response_matrix)

response_mean <- rowMeans(response_matrix)
rowSds(response_matrix)



predictions <- drivers_full %>%
  mutate(weight_pubs = pubs_fit / sum(pubs_fit, na.rm = TRUE),
         weight_pop = pop / sum(pop, na.rm = TRUE),
         bsm_weight_pubs = bsm_response * weight_pubs,
         bsm_weight_pop = bsm_response * weight_pop)

