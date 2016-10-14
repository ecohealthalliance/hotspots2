load_all()
library(dplyr)

if (!require(pubcrawler2hotspots)) {
  install_github("ecohealthalliance/pubcrawler2hotspots")
}

library(pubcrawler2hotspots)

data(pubs_df_all)

save(pubs_df_all, file = file.path(cache_dir(), "pubs_df_all.RData"))