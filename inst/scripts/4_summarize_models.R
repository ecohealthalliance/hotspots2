load_all()

library(dismo)
library(gbm)
library(ggplot2)

require(foreach)
require(doParallel)
registerDoParallel(4)

data(drivers)
load("cache/bsm.RData")


bsm_summary <- summarize_multibrt(bsm)
pcd <- percent_deviance_explained(bsm)

bsm_interactions <- interactions_multibrt(bsm, .parallel = TRUE)
i