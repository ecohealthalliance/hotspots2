library(dplyr)

options(stringsAsFactors = FALSE)

eid_metadata <- read.csv(system.file("data-raw", "CCM_EIDDatabase_2015-05-26.csv", package = "hotspots2"))

save(eid_metadata, file = file.path(data_dir(), "eid_metadata.RData"))