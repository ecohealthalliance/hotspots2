# Hotspots II Models

## Location of Model Outputs

The file `data/predictions.RData` contains the output of the model. All variables are described in more depth in [the paper](https://doi.org/10.1038/s41467-017-00923-8).

- The columns `lon` and `lat` are WGS84 longitude and latitude. `gridid` is a grid cell identifier.
- `bsm_weight_pubs` is the model’s output weighted with publications (“observed”).
- `bsm_weight_pop` is the model’s output weighted with population (“bias factored out”).
- `bsm_response` is the model’s output weighted with neither.
- The other columns are generally predictor variables used in the model.

## Getting Set Up

The data-raw directory contains all the scripts needed to import the data into the repository. The datasets are not in the directory, though. These scripts build all the working datasets to the `data` directory, which *is* under version control, so the code should all run.
