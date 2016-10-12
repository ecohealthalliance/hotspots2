load_all()
library(dplyr)
library(reshape2)
library(tidyr)
library(stringr)

options(stringsAsFactors = FALSE)

EIDdata.v4 <- read.csv(system.file("data-raw", "EIDdata.v4.csv", package = "hotspots2"))
eid08_drivers_19OCT11 <- read.table(system.file("data-raw", "eid08_drivers_19OCT11.txt", package = "hotspots2"), header=TRUE, sep="\t")
drivers_orig <- merge(x=eid08_drivers_19OCT11, y=EIDdata.v4, by.x=c("lon", "lat", "GridID"), by.y=c("Longitude", "Latitude", "GridID")) %>%
  select(gridid = GridID,
         lon,
         lat,
         iso3 = ISO3v10,
         pubs_jones = cntrypubs7,
         land_area = landarea.x,
         gdp = GDPcap,
         pop_1970 = pop.1970,
         pop_1975 = pop.1975,
         pop_1980 = pop.1980,
         pop_1985 = pop.1985,
         pop_1990 = pop.1990,
         pop_1995 = pop.1995,
         pop_2000 = pop.2000,
         crop_1900 = crop1900,
         crop_1910 = crop1910,
         crop_1920 = crop1920,
         crop_1930 = crop1930,
         crop_1940 = crop1940,
         crop_1950 = crop1950,
         crop_1960 = crop1960,
         crop_1970 = crop1970,
         crop_1980 = crop1980,
         crop_1990 = crop1990,
         crop_2000 = crop2000,
         past_1900 = pastur1900,
         past_1910 = pastur1910,
         past_1920 = pastur1920,
         past_1930 = pastur1930,
         past_1940 = pastur1940,
         past_1950 = pastur1950,
         past_1960 = pastur1960,
         past_1970 = pastur1970,
         past_1980 = pastur1980,
         past_1990 = pastur1990,
         past_2000 = pastur2000,
         goats = goatct2000,
         buffalo = bfloct2000,
         cattle = ctlect2000,
         sheep = shpct2000,
         poultry = pltyct2000,
         pigs = pigct2000)
drivers_orig$livestock_mam <- rowSums(drivers_orig[, c("goats", "buffalo", "cattle", "sheep", "pigs")])



# Build separate data frame of decade-stratified drivers.
decadal_wide <- drivers_orig[c("gridid", "crop_1970", "crop_1980", "crop_1990", "crop_2000", "past_1970", "past_1980", "past_1990", "past_2000", "pop_1970", "pop_1980", "pop_1990", "pop_2000")]
decadal_wide <- replace_na(decadal_wide, list(crop_1970 = 0, crop_1980 = 0, crop_1990 = 0, crop_2000 = 0, past_1970 = 0, past_1980 = 0, past_1990 = 0, past_2000 = 0, pop_1970 = 0, pop_1980 = 0, pop_1990 = 0, pop_2000 = 0))
# We do this to eliminate NAs (assuming that where cropland = 0, often around where aggregation happened at continent edges, true value is close to zero).

decadal <- melt(decadal_wide, id.vars = "gridid")

# Making a tidy version of this data frame for a future iteration.
decadal_tidy <- decadal %>%
  mutate(year = str_split_fixed(variable, "_", n = 2)[, 2],
         variable = str_split_fixed(variable, "_", n = 2)[, 1])

# The version used in the final analysis.
decadal_years <- c(1970, 1980, 1990, 2000)
decadal <- plyr::ldply(decadal_years, .fun = function(x) {
  d <- decadal[grep(as.character(x), decadal$variable), ]
  d$decade <- x
  e <- dcast(data = d, formula = gridid + decade ~ ...)
  names(e) <- c("gridid", "year", "crop", "past", "pop")
  return(e)
})

# decadal <- replace_na(decadal, list(pop = 0, past = 0, crop = 0))



# Create the "change" data frame.

vars <- c("crop", "past", "pop")

# Create a data frame representing all permutations of years and variables.
years <- c(1975, 1985, 1995)
change_args <- expand.grid(years, vars, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
names(change_args) <- c("year", "var")

# For each row of the data frame, we calculate the years it is the difference between by subtracting and adding five.
change <- mdply(.data = change_args, .fun = function(year, var) {
  year1 <- year - 5
  year2 <- year + 5
  x <- merge(decadal[decadal$year == year1, c("gridid", "year", var)], decadal[decadal$year == year2, c("gridid", "year", var)], by = "gridid", suffixes = c("1", "2"))
  x$year <- year
  x[, var] <- x[, paste(var, "2", sep = "")] - x[, paste(var, "1", sep = "")]
  x <- x[c(1, 6, 7)]
  return(x)
})
change$var <- NULL



change <- melt(change, id.vars = c("gridid", "year"), na.rm = TRUE)
change <- dcast(change, gridid + year ~ ...)

new_names <- paste0(vars, "_change")
names(new_names) <- vars
change <- plyr::rename(change, new_names)

change <- replace_na(change, list(pop_change = 0, past_change = 0, crop_change = 0))



# Save intermediary file for driver superset  
cache_dir <- system.file("cache", package = "hotspots2")
save(drivers_orig, file = file.path(cache_dir, "drivers_orig.RData"))

# Save decadal and change data files
data_dir <- system.file("data", package = "hotspots2")
save(decadal, file = file.path(data_dir, "decadal.RData"))
save(change, file = file.path(data_dir, "change.RData"))