# DisMapR: NOAA DisMAP Data Retrieval Package

## Overview

DisMapR is an R package that provides functions to easily interact with NOAA's Distribution Mapping and Analysis Portal (DisMAP) APIs. This package allows users to:

1. Download species indicators data (Center of Gravity, depth metrics, etc.)
2. Access and download interpolated biomass raster layers
3. Retrieve survey location points
4. Create visualizations of species distributions

## Installation

```r
# Install from GitHub (once the package is hosted there)
# devtools::install_github("username/dismapr")

# For now, you can source the R file directly
source("path/to/dismapr.R")
```

## Required Packages

DisMapR depends on the following packages:
- httr
- jsonlite
- dplyr
- raster
- sf
- ggplot2
- maps

For creating animated visualizations:
- gganimate
- gifski

For color palettes:
- cmocean (recommended)
- viridis (alternative)

## Basic Usage

### 1. Getting Species Indicators Data

```r
# Get Center of Gravity data for American Lobster in Northeast US Spring
indicators <- get_dismap_indicators(
  common_name = "American lobster", 
  region = "Northeast US Spring"
)

# Plot Center of Gravity changes over time
ggplot(data = indicators) + 
  geom_point(aes(x = lon, y = lat, color = Year)) +
  theme_classic() +  
  labs(title = "American Lobster Center of Gravity")
```

### 2. Downloading Interpolated Biomass Rasters

```r
# Get base URLs for DisMAP services
urls <- get_dismap_urls()

# Download biomass rasters for Summer Flounder
raster_files <- download_dismap_series(
  species_scientific = "Paralichthys dentatus",
  species_common = "SummerFlounder",
  region = "Northeast US Spring",
  region_url = urls$rasters[["Northeast US Spring"]],
  output_dir = "dismap_data"
)

# Extract data from rasters
raster_data <- extract_raster_series(
  raster_files = raster_files$file_path,
  years = raster_files$year
)

# Plot distribution map
plot_dismap_distribution(raster_data)
```

### 3. Creating Animated Distribution Maps

```r
# Create animated map
anim <- plot_dismap_distribution(
  data = raster_data,
  animated = TRUE,
  xlim = c(-78, -65),
  ylim = c(34, 48)
)

# Save animation
gganimate::anim_save("species_distribution.gif", anim)
```

### 4. Getting Survey Locations

```r
# Get survey locations for a region
survey_points <- get_survey_locations("Northeast US Spring")

# Plot survey locations
ggplot() +
  geom_sf(data = survey_points) +
  theme_minimal() +
  labs(title = "Survey Locations")
```

## Available Regions

DisMAP currently includes data for the following regions:
- Aleutian Islands
- Eastern Bering Sea
- Gulf of Alaska
- West Coast Triennial
- West Coast Annual
- Gulf of Mexico
- Northeast US Spring
- Northeast US Fall
- Southeast US Fall
- Southeast US Spring
- Southeast US Summer
- Northern Bering Sea
- Eastern and Northern Bering Sea

## Important Notes

- URLs for the DisMAP services may change as new data releases become available. Check the [DisMAP InPort page](https://www.fisheries.noaa.gov/inport/item/66799) for the most up-to-date URLs.
- No data values in rasters are typically set to 3.4e+38 and should be treated as NA.
- For better visualization, biomass values are often cube-root transformed.

## References

- NOAA DisMAP portal: https://apps-st.fisheries.noaa.gov/dismap/DisMAP.html
- Technical documentation: https://www.fisheries.noaa.gov/inport/item/66799

## License

This package is provided under [license information].

## Citation

If using this package for research, please cite both the package and the NOAA DisMAP data sources.
