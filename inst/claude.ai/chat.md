# PROMPT 2025-04-28

- Extract visualization functions from the vignette @vignettes/articles/dismapr.Rmd. 
Apply reasonable arguments, including for color ramps, and pass on extra arguments
to the default underlying plotting function with an ellipsis (`...`). Add the
ROxygen2 `#' @concept viz` to each of these functions. Update and simplify the
vignette to using them.
- On second thought, move the @R/viz.R functions to the appropriate 
  @R/{indicators|rasters|surveys}.R file and apply the same 
  `#' @concept {indicators|rasters|surveys}`.
- Reorient the naming of all functions so they are prefixed by `dm_*`:
  - Rename `get_dm_*` to `dm_get_*`
  - Rename `plot_*` to `dm_plot_*`
- Add `interactive = T` default to `dm_plot_*` functions, so `interactive = F` 
  results in the static plot for: `dm_plot_raster()`, `dm_plot_survey_locations()`.
- Rename `dm_plot_cog()` to `dm_plot_indicators()`.
- Rearrange sections in @vignettes/articles/dismapr.Rmd to match @README.md and @_pkgdown.yml:
  1. **Surveys**\
     Survey location points
  2. **Rasters**\
     Rasters of interpolated biomass distribution
  3. **Indicators**\
     Indicators of overall distributional shifts by depth and latitude
- Add working examples to all functions (pull code from @vignettes/articles/dismapr.Rmd) and show outputs
  (outputs to from `pkgdown::build_site()`)
     

# PROMPT

Convert this script into a usable R function and library for retrieving IDW rasters of interpolated bottom trawl surveys

- attachment: [DisMAP_Data_Download_API.R](../claude.ai/DisMAP_Data_Download_API.R) 

# RESPONSE

I'll convert the provided script into a more organized and reusable R library for retrieving interpolated depth-weighted biomass rasters from NOAA's Distribution Mapping and Analysis Portal (DisMAP). Let me create an artifact with the R package structure.

- [dismapr.r](./dismapr.r)

Now I'll create an example script showing how to use the functions from the package I've created:

- [example_script.r](./example_script.r)

Let me also create a README file to explain how to use the package:

- [readme.md](./readme.md)

## DisMAP Data Retrieval Package

I've created a complete R package called "dismapr" that converts the provided script into a set of reusable functions for retrieving and analyzing interpolated depth-weighted (IDW) biomass rasters from NOAA's Distribution Mapping and Analysis Portal (DisMAP).

### What I've Created:

1. **R Package with Core Functions**:
   - Functions to download species indicators data (Center of Gravity metrics)
   - Functions to retrieve slice IDs and download interpolated biomass rasters
   - Functions to extract data from rasters and create visualizations
   - Helper functions for working with survey points and region information

2. **Example Usage Script**:
   - Demonstrates how to use the package functions
   - Shows workflows for downloading, processing, and visualizing data
   - Includes examples for creating static and animated distribution maps

3. **README Documentation**:
   - Installation instructions
   - Usage examples
   - Description of available regions and data types
   - References to NOAA documentation

### Key Improvements Over the Original Script:

1. **Modular Design**: Separated functionality into reusable functions with clear inputs and outputs

2. **Better Error Handling**: Added checks and informative messages

3. **Parameter Flexibility**: Made the functions more adaptable to different species, regions, and time periods

4. **Documentation**: Added function descriptions and usage examples

5. **Consistent API**: Created a uniform interface for interacting with different DisMAP data products

### How to Use the Package:

1. Source the dismapr.R file or install it as a package
2. Use the functions to download and process DisMAP data
3. Create visualizations of species distributions
4. Analyze changes in species distributions over time

This package makes it much easier to work with the DisMAP API and to incorporate interpolated bottom trawl survey data into your R workflows.

Would you like me to explain any specific part of the package in more detail?
