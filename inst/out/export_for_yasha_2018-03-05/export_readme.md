# Hotspots II Model Output & Uncertainty

## Contents

The two CSVs in this directory contain the output of the main bootstrap Hotspots II model, expressed in a few ways.

The file `bsm_output.csv` contains the following variables:

- `lon`, `lat`: Longitude and latitude. The files are 1º resolution.
- `gridid`: The grid cell identifier used in the Hotspots project to track and merge data.
- `response_mean`: The mean response of the 1000 replicate models from the bootstrap model used in the paper. This was used in generating the final map figures for the paper, combined with the weighting variables descried below.
- `response_sd`: The standard deviation of the replicate model values for each grid cell.
- `Q05`, `Q25`, `Q5`, `Q75`, `Q95`: The respective quantiles for the bootstrap model.
- `weight_pubs`, `weight_pop`: These values represent the publication effort and human population layers from the model respectively. They have been transformed such that they sum to 1 across the grid. These, multiplied by the output layer `response_mean`, produce the "expected observed events" and "expected events, adjusting for bias" layers used in the final maps, respectively.

The file `response_tbl.csv` contains a table with each column corresponding to the output of one of the 1000 replicate models, and each row corresponding to a grid cell. The rows are in the same order as the `bsm_output` table.

## Usage

I thought it was better to provide the values to you in this pre-combination form, rather than to do the multiplication with publication and population layers, so that you can decide how you want to use the data.

In R, I would produce, say, the model's predicted events adjusting for bias in the following way:

```r
library(tidyverse)

bsm_output <- read_csv("bsm_output.csv")
bsm_output <- bsm_output %>%
  mutate(bsm_weight_pop = response_mean * weight_pop)
```

To output that as a raster, I would do the following:

```r
library(raster)

# CRS used in the rasters; just the default WGS84 proj4string.
crs_string <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Create an R raster object with the variable to export.
raster_to_report <- bsm_output %>%
  select(x = lon, y = lat, z = bsm_weight_pop) %>%
  rasterFromXYZ(crs = crs_string)

# Give the variable a name, otherwise it will be named 'z'
names(raster_to_export) <- "bsm_weight_pop"

# Write the raster to disk in the desired format.
writeRaster(raster_to_export, filename = "file/path/exported_raster.tif", overwrite = TRUE)
```

Using these files, you should be able to produce whatever uncertainty metrics you find most relevant.