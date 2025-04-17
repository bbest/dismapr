# dismapr 0.2.0

* Cleaned up functions and applied `dm_` prefix for DisMAP
* Added `dm_regions` data for lazy loading and referencing `dataset_code` available
* Applied `dataset_code` > `species_scientific` > `years` for `get_dm_raster()`, 
  so user doesn't have to think or worry about the underlying `sliceId`

# dismapr 0.1.0

* Initial version
* Started with Melissa's script `DisMAP_Data_Download_API.R` and asked Claude.ai 
  to generate an R package. Then modified.
