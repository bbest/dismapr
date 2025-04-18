# PROMPT

Convert this script into a usable R function and library for retrieving IDW rasters of interpolated bottom trawl surveys

- attachment: [DisMAP_Data_Download_API.R](../claude.ai/DisMAP_Data_Download_API.R) 

# RESPONSE

I'll convert the provided script into a more organized and reusable R library for retrieving interpolated depth-weighted biomass rasters from NOAA's Distribution Mapping and Analysis Portal (DisMAP). Let me create an artifact with the R package structure.

- <./dismapr.r>

Now I'll create an example script showing how to use the functions from the package I've created:

- <./example_script.r>

Let me also create a README file to explain how to use the package:

- <./readme.md>

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
