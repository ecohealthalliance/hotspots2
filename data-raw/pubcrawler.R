load_all()
library(dplyr)

if (!require(pubcrawler2hotspots)) {
    install_github("ecohealthalliance/pubcrawler2hotspots")
}

library(pubcrawler2hotspots)

data(pubs_fit)
save(pubs_fit, file = file.path(cache_dir(), "pubs_fit.RData"))
