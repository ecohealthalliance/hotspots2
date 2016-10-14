load_all()
library(raster)
library(plyr)
library(foreach)
library(doParallel)

registerDoParallel(4)

template_raster <- template_raster()

earthenv_files <- list.files(system.file("data-raw", "EarthEnv", package = "hotspots2"), full.names = TRUE)

earthenv_list <- foreach(f = earthenv_files) %dopar% {
  x <- raster(f)
  y <- aggregate(x, fact = res(template_raster)[1]/res(x)[1])
  df <- as.data.frame(y, xy = TRUE)
  names(df) <- c("lon", "lat", names(df[3]))
  df
}
earthenv <- join_all(earthenv_list)

earthenvnames <- c("consensus_full_class_1"  = "earth1_trees_needl",
                   "consensus_full_class_2"  = "earth2_trees_everg",
                   "consensus_full_class_3"  = "earth3_trees_decid",
                   "consensus_full_class_4"  = "earth4_trees_other",
                   "consensus_full_class_5"  = "earth5_shrubs",
                   "consensus_full_class_6"  = "earth6_veg_herba",
                   "consensus_full_class_7"  = "earth7_veg_manag",
                   "consensus_full_class_8"  = "earth8_veg_flood",
                   "consensus_full_class_9"  = "earth9_urban",
                   "consensus_full_class_10" = "earth10_snowice",
                   "consensus_full_class_11" = "earth11_barren",
                   "consensus_full_class_12" = "earth12_water")
earthenv <- plyr::rename(earthenv, replace = earthenvnames)

cache_dir <- system.file("cache", package = "hotspots2")
save(earthenv, file = file.path(cache_dir(), "earthenv.RData"))
