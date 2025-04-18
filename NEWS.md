# dismapr 0.4.0

* Added concepts to functions for Reference section: Indicators, Rasters, Surveys, Data.
* Broke up functions by concept into seperate reference R files.
* Changed `dm_regions` to `dm_datasets` to enforce hierarchical consistency with DisMAP Rasters:
  - **datasets**: region and possibly seasons
  - **layers**: scientific species name or "Species Richness"
  - **years**: years of data available
  
# dismapr 0.3.0

* Made consistent "species" to "layers": `get_dm_dataset_species_years()` -> `get_dm_dataset_layer_years()`.

# dismapr 0.2.0

* Cleaned up functions and applied `dm_` prefix for DisMAP
* Added `dm_regions` data for lazy loading and referencing `dataset_code` available
* Applied `dataset_code` > `species_scientific` > `years` for `get_dm_raster()`, 
  so user doesn't have to think or worry about the underlying `sliceId`

# dismapr 0.1.0

* Initial version
* Started with Melissa's script `DisMAP_Data_Download_API.R` and asked Claude.ai 
  to generate an R package. Then modified.
