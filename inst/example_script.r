# Example script demonstrating how to use the dismapr package
# for retrieving interpolated depth-weighted biomass data from NOAA's DisMAP

# Load required packages
library(dplyr)
library(ggplot2)
library(raster)
library(sf)
library(httr)
library(jsonlite)
# Additional visualization packages (optional)
# library(cmocean)
# library(gganimate)
# library(gifski)
# library(viridis)

# Source the dismapr functions
# In a real package, you would just use library(dismapr)
source("path/to/dismapr.R")

# Example 1: Get indicators data for American Lobster in Northeast US Spring
lobster_indicators <- get_dismap_indicators(
  common_name = "American lobster", 
  region = "Northeast US Spring"
)

# Plot Center of Gravity changes over time
ggplot(data = lobster_indicators) + 
  geom_point(aes(x = lon, y = lat, color = Year)) +
  theme_classic() +  
  labs(y = "Latitude", x = "Longitude", title = "American Lobster Center of Gravity") + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  coord_quickmap(xlim = c(-80, -65), ylim = c(33, 45))

# Example 2: Download interpolated biomass rasters for Summer Flounder in NEUS Spring
# Get information about available DisMAP services
dismap_urls <- get_dismap_urls()

# Set up parameters for downloading
species_scientific <- "Paralichthys dentatus"  # Summer Flounder
species_common <- "SummerFlounder"
region <- "Northeast US Spring"
region_url <- dismap_urls$rasters[[region]]

# Northeast US Spring bounding box (adjust as needed)
bbox <- c(min = 7808000, min = 3950000, max = 8228000, max = 5212000)

# Check available slices (years)
slices <- get_dismap_slices(species_scientific, region_url)
print(slices)

# Download just a few years for demonstration
years_to_download <- c(2015, 2016, 2017, 2018, 2019)
slice_ids <- slices$slices.id[slices$slices.keyValue %in% as.character(years_to_download)]

# Create directory for downloads
output_dir <- "dismap_data"
dir.create(output_dir, showWarnings = FALSE)

# Download each raster
raster_files <- character(length(slice_ids))
for (i in seq_along(slice_ids)) {
  slice_id <- slice_ids[i]
  year <- slices$slices.keyValue[slices$slices.id == slice_id]
  
  file_name <- paste0(species_common, "_", year, "_", slice_id, ".grd")
  file_path <- file.path(output_dir, file_name)
  
  message("Downloading raster for year ", year, " (slice ", slice_id, ")")
  
  download_dismap_raster(
    slice_id = slice_id,
    region_url = region_url,
    bbox = bbox,
    out_file = file_path
  )
  
  raster_files[i] <- file_path
}

# Extract data from downloaded rasters
raster_data <- extract_raster_series(
  raster_files = raster_files,
  years = years_to_download,
  transform = function(x) x^(1/3),  # cube root transformation
  no_data_value = 3.4e+38
)

# Plot a single year
year_data <- raster_data %>% filter(year == 2019)
p <- plot_dismap_distribution(
  data = year_data,
  value_col = "transformed_wtcpue",
  animated = FALSE,
  xlim = c(-78, -65),
  ylim = c(34, 48)
) + 
  labs(title = "Summer Flounder Distribution - 2019")
print(p)

# Create an animated plot (if gganimate and gifski are installed)
if (requireNamespace("gganimate", quietly = TRUE) && 
    requireNamespace("gifski", quietly = TRUE)) {
  
  anim <- plot_dismap_distribution(
    data = raster_data,
    value_col = "transformed_wtcpue",
    animated = TRUE,
    xlim = c(-78, -65),
    ylim = c(34, 48)
  )
  
  # Save animation
  gganimate::anim_save(
    "SummerFlounder_animation.gif", 
    animation = anim,
    path = output_dir
  )
}

# Example 3: Get survey locations
northeast_survey_points <- get_survey_locations(
  region = "Northeast US Spring"
)

# Plot survey locations
ggplot() +
  geom_sf(data = northeast_survey_points) +
  theme_minimal() +
  labs(title = "Northeast US Spring Survey Locations")
