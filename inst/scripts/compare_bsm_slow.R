load_all()
library(ggplot2)
library(dplyr)

load("cache/bsm_1000_iter_slow/bsm_1000_iter_slow.RData")
bsm_slow <- bsm
load("cache/bsm_1000_iter/bsm_1000_iter.RData")

bsm %>% map(~ .x$n.trees) %>% simplify() %>% summary()
bsm_slow %>% map(~ .x$n.trees) %>% simplify() %>% summary()

drivers_full$bsm_response <- predict_multibrt(bsm, drivers_full, type = "response", value = "mean")
drivers_full$bsm_slow_response <- predict_multibrt(bsm_slow, drivers_full, type = "response", value = "mean")

drivers_full <- mutate(weight_pubs = pubs_fit / sum(pubs_fit, na.rm = TRUE),
       weight_pop = pop / sum(pop, na.rm = TRUE),
       bsm_weight_pubs = bsm_response * weight_pubs,
       bsm_weight_pop = bsm_response * weight_pop,
       bsm_slow_weight_pubs = bsm_slow_response * weight_pubs,
       bsm_slow_weight_pop = bsm_slow_response * weight_pop,
       bsm_diff = bsm_slow_response - bsm_response,
       bsm_percent_diff = bsm_diff / bsm_response)



cor(drivers_full$bsm_response, drivers_full$bsm_slow_response)

qplot(bsm_response, land_area, data = drivers_full) %>%
  ggsave(file = "inst/out/bsm_vs_slow.pdf")
ggsave(quickmap(drivers_full, bsm_response - bsm_slow_response), file = "inst/out/bsm_vs_slow_map.pdf")


bsm_prediction
bsm_slow_prediction

predictions <- drivers_full %>%
  mutate(weight_pubs = pubs_fit / sum(pubs_fit, na.rm = TRUE),
         weight_pop = pop / sum(pop, na.rm = TRUE),
         bsm_weight_pubs = bsm_response * weight_pubs,
         bsm_weight_pop = bsm_response * weight_pop)
