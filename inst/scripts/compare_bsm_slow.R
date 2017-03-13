load_all()
library(ggplot2)
library(dplyr)

load("cache/bsm_1000_iter_slow/bsm_1000_iter_slow.RData")
bsm_slow <- bsm
load("cache/bsm_1000_iter/bsm_1000_iter.RData")

bsm %>% map(~ .x$n.trees) %>% simplify() %>% summary()
bsm_slow %>% map(~ .x$n.trees) %>% simplify() %>% summary()

load("cache/bsm_1000_iter_slow/predictions.RData")
drivers_full$bsm_response <- predict_multibrt(bsm, drivers_full, type = "response", value = "mean")
drivers_full$bsm_slow_response <- predict_multibrt(bsm_slow, drivers_full, type = "response", value = "mean")

predictions <- drivers_full %>%
  mutate(weight_pubs = pubs_fit / sum(pubs_fit, na.rm = TRUE),
         weight_pop = pop / sum(pop, na.rm = TRUE),
         bsm_weight_pubs = bsm_response * weight_pubs,
         bsm_weight_pop = bsm_response * weight_pop,
         bsm_slow_weight_pubs = bsm_slow_response * weight_pubs,
         bsm_slow_weight_pop = bsm_slow_response * weight_pop,
         bsm_diff = bsm_slow_response - bsm_response,
         bsm_percent_diff = bsm_diff / bsm_response,
         bsm_pubs_diff = bsm_slow_weight_pubs - bsm_weight_pubs,
         bsm_pubs_pct_diff = bsm_pubs_diff / bsm_weight_pubs) %>%
  na.omit()


save(predictions, file = "cache/bsm_1000_iter_slow/predictions.RData")


cor(predictions$bsm_response, predictions$bsm_slow_response)
cor(predictions$bsm_weight_pubs, predictions$bsm_slow_weight_pubs)

m1 <- lm(bsm_slow_response ~ bsm_response, data = predictions)
m1$r.squared



qplot(bsm_response, bsm_slow_response, data = predictions) %>%
  ggsave(file = "inst/out/bsm_slow_xy.pdf")
qplot(bsm_response, bsm_percent_diff, data = predictions) %>%
  ggsave(file = "inst/out/bsm_slow_diff_xy.pdf")
quickmap(predictions, bsm_percent_diff) %>%
  ggsave(file = "inst/out/bsm_slow_percent_diff_map.pdf")

qplot(bsm_weight_pubs, bsm_slow_weight_pubs, data = predictions) %>%
  ggsave(file = "inst/out/bsm_pubs_slow_xy.pdf")
qplot(bsm_weight_pubs, bsm_pubs_diff, data = predictions) %>%
  ggsave(file = "inst/out/bsm_pubs_slow_diff_xy.pdf")
quickmap(predictions, bsm_pubs_pct_diff) %>%
  ggsave(file = "inst/out/bsm_pubs_slow_percent_diff_map.pdf")


bsm_prediction
bsm_slow_prediction


drivers_full <- select(drivers_full, -bsm_weight_pubs, -bsm_weight_pop, -bsm_slow_weight_pubs, -bsm_slow_weight_pop, -bsm_diff, -bsm_percent_diff)
