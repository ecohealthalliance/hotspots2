load_all()
library(dplyr)

if (!require(pubcrawler2hotspots)) {
    install_github("ecohealthalliance/pubcrawler2hotspots")
}

library(pubcrawler2hotspots)

data(pubs_fit)

pubs_fit <- mutate(pubs_fit, pubs_identity = exp(pubs_fit))

cache_dir <- system.file("cache", package = "hotspots2")
save(pubs_fit, file = file.path(cache_dir, "pubs_fit.RData"))