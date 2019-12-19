# Files

This directory contains a few variables in two formats: (1) individual geotiff files, and (2) columns of a lon-lat gridded CSV file. Not all variables are present in each form, but the important ones are.

The variable names are:

- bsm_response — the model's output, corresponding to fig. 4
- bsm_weight_pop - the output weighted by pop (multiplied by the variable weight_pop)
- bsm_weight_pubs - the output weighted by the publication index (multiplied by weight_pubs)
- hotspots2_clipped and hotspots2_clipped_hires - versions of the model with outlier grid cell values clipped, created for plotting purposes, and a "high-res" version, which simply uses bilinear interpolation to produce a smoother output.