# dismapr: An R Package for NOAA DisMAP Data Retrieval
# Functions for working with the Distribution Mapping and Analysis Portal (DisMAP) API

#' @title Get DisMAP Indicators Table
#' @description Download species indicators data including Center of Gravity metrics
#' @param common_name Character string of species common name
#' @param region Character string of desired region (optional)
#' @param base_url The base URL for the indicators service
#' @return A sf object with indicators data
#' @export
get_dismap_indicators <- function(common_name, 
                                region = NULL, 
                                base_url = "https://services2.arcgis.com/C8EMgrsFcRFL6LrL/ArcGIS/rest/services/Indicators_20220516/FeatureServer/3/query") {
  
  require(httr)
  require(sf)
  
  # Build where clause
  if (!is.null(region)) {
    where_clause <- sprintf("CommonName = '%s' AND OARegion='%s'", common_name, region)
  } else {
    where_clause <- sprintf("CommonName = '%s'", common_name)
  }
  
  # Build URL
  url <- httr::parse_url(base_url)
  url$query <- list(
    where = where_clause,
    outFields = "*",
    f = "geojson"
  )
  
  request <- httr::build_url(url)
  
  # Read data
  indicators_data <- sf::st_read(request, quiet = TRUE)
  
  # Convert to data frame
  indicators_df <- as.data.frame(indicators_data)
  
  # Rename for consistency
  indicators_df <- dplyr::rename(indicators_df,
                           lon = CenterOfGravityLongitude,
                           lat = CenterOfGravityLatitude)
  
  return(indicators_df)
}

#' @title Get DisMAP Slice IDs
#' @description Get slice IDs for a species in a region
#' @param species_name The scientific name of the species
#' @param region_url The URL for the image server for this region
#' @return A data frame with slice information
#' @export
get_dismap_slices <- function(species_name, region_url) {
  require(httr)
  require(jsonlite)
  
  # URL encode the species name for the API
  encoded_name <- URLencode(species_name, reserved = TRUE)
  
  # Build the slices URL
  slices_url <- paste0(
    region_url, 
    "/slices?multidimensionalDefinition=%5B%7B%22variableName%22%3A+%22", 
    encoded_name,
    "%22%7D%5D&f=pjson"
  )
  
  # Get the JSON response
  response <- httr::GET(slices_url)
  text_json <- httr::content(response, type = 'text', encoding = "UTF-8")
  jfile <- jsonlite::fromJSON(text_json)
  
  # Convert to data frame
  df <- as.data.frame(jfile)
  
  return(df)
}

#' @title Download DisMAP Raster
#' @description Download a single interpolated biomass raster by slice ID
#' @param slice_id The slice ID to download
#' @param region_url The URL for the image server for this region
#' @param bbox Bounding box coordinates (xmin, ymin, xmax, ymax)
#' @param out_file Output file path (if NULL, returns a raster object)
#' @return A raster object or path to saved file
#' @export
download_dismap_raster <- function(slice_id, 
                                  region_url,
                                  bbox = NULL,
                                  out_file = NULL) {
  
  require(httr)
  require(raster)
  
  # Build the export image URL
  url <- httr::parse_url(paste0(region_url, "/exportImage"))
  
  # Query arguments
  query_arg <- list(
    imageSR = "4326",
    format = "tiff",
    pixelType = "F32",
    noDataInterpretation = "esriNoDataMatchAny",
    interpolation = "+RSP_BilinearInterpolation",
    sliceID = slice_id,
    f = "json"
  )
  
  # Add bounding box if provided
  if (!is.null(bbox)) {
    bbox_arg <- list(bbox = paste(bbox, collapse = ","))
    query_args <- c(bbox_arg, query_arg)
  } else {
    query_args <- query_arg
  }
  
  # Send request
  res <- httr::GET(url, query = query_args)
  
  # Set up temporary file
  tmpfile <- tempfile(fileext = ".tif")
  content <- httr::content(res, type = "application/json")
  
  # Download the actual raster
  httr::RETRY("GET", content$href, 
             httr::progress(), 
             httr::write_disk(tmpfile),
             times = 10, pause_cap = 10)
  
  # Read the raster
  r <- raster::raster(tmpfile)
  
  # Save to file if out_file is provided
  if (!is.null(out_file)) {
    raster::writeRaster(r, out_file, overwrite = TRUE)
    return(out_file)
  } else {
    return(r)
  }
}

#' @title Download DisMAP Raster Series
#' @description Download multiple interpolated biomass rasters for a species over time
#' @param species_scientific Scientific name of the species
#' @param species_common Common name of the species (for file naming)
#' @param region Region name
#' @param region_url URL for the image server for this region
#' @param bbox Bounding box coordinates (xmin, ymin, xmax, ymax)
#' @param output_dir Directory to save rasters
#' @param years Optional vector of years to download (if NULL, downloads all available years)
#' @return Data frame with paths to downloaded rasters
#' @export
download_dismap_series <- function(species_scientific,
                                  species_common,
                                  region,
                                  region_url,
                                  bbox = NULL,
                                  output_dir = ".",
                                  years = NULL) {
  
  require(dplyr)
  
  # Create output directory if it doesn't exist
  dir_path <- file.path(output_dir, paste0(species_common, "_", gsub(" ", "_", region)))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Get slice IDs for the species
  slice_data <- get_dismap_slices(species_scientific, region_url)
  
  # Filter by years if provided
  if (!is.null(years)) {
    # This assumes slice metadata contains a key for the year
    # May need adjustment based on actual API response
    slice_ids <- slice_data %>%
      dplyr::filter(keyValue %in% as.character(years)) %>%
      dplyr::pull(id)
  } else {
    slice_ids <- slice_data$slices.id
  }
  
  # Download each raster
  results <- data.frame(
    slice_id = integer(),
    year = integer(),
    file_path = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(slice_ids)) {
    slice_id <- slice_ids[i]
    
    # Get year from slice data
    year <- slice_data$slices.keyValue[slice_data$slices.id == slice_id]
    
    # Output file path
    file_name <- paste0(species_common, "_", year, "_", slice_id)
    file_path <- file.path(dir_path, file_name)
    
    # Download raster
    message(sprintf("Downloading raster for %s, year %s (slice %s)", species_common, year, slice_id))
    
    download_dismap_raster(
      slice_id = slice_id,
      region_url = region_url,
      bbox = bbox,
      out_file = file_path
    )
    
    # Add to results
    results <- rbind(results, data.frame(
      slice_id = slice_id,
      year = as.integer(year),
      file_path = file_path,
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

#' @title Extract DisMAP Raster Series Data
#' @description Extract data from a series of rasters into a tidy data frame
#' @param raster_files Vector of raster file paths
#' @param years Vector of years corresponding to each raster
#' @param transform Function to transform values (default is cube root)
#' @param no_data_value Value to treat as NA
#' @return Data frame with lon, lat, year, wtcpue columns
#' @export
extract_raster_series <- function(raster_files, 
                                years,
                                transform = function(x) x^(1/3),
                                no_data_value = 3.4e+38) {
  
  require(raster)
  require(dplyr)
  
  # Preallocate output data frame
  output <- NULL
  
  # Process each raster
  for (i in seq_along(raster_files)) {
    # Read raster
    r <- raster::raster(raster_files[i])
    
    # Extract points
    points <- raster::rasterToPoints(r)
    
    # Convert to data frame
    df <- data.frame(
      lon = points[, 1],
      lat = points[, 2],
      year = years[i],
      wtcpue = points[, 3]
    )
    
    # Append to output
    output <- rbind(output, df)
  }
  
  # Clean and transform data
  output <- output %>%
    dplyr::mutate(
      wtcpue = ifelse(wtcpue > no_data_value, NA, wtcpue),
      transformed_wtcpue = transform(wtcpue)
    )
  
  return(output)
}

#' @title Plot DisMAP Distribution Map
#' @description Create a static or animated plot of species distribution
#' @param data Data frame with lon, lat, year, and transformed biomass columns
#' @param value_col Name of column with values to plot
#' @param animated Whether to create an animated plot
#' @param xlim,ylim Limits for x and y axes
#' @param color_palette Color palette function
#' @return A ggplot object or animated gif
#' @export
plot_dismap_distribution <- function(data,
                                    value_col = "transformed_wtcpue",
                                    animated = FALSE,
                                    xlim = NULL,
                                    ylim = NULL,
                                    color_palette = NULL) {
  
  require(ggplot2)
  require(maps)
  
  # Set defaults for limits if not provided
  if (is.null(xlim)) {
    xlim <- c(min(data$lon, na.rm = TRUE), max(data$lon, na.rm = TRUE))
  }
  
  if (is.null(ylim)) {
    ylim <- c(min(data$lat, na.rm = TRUE), max(data$lat, na.rm = TRUE))
  }
  
  # Set default color palette if not provided
  if (is.null(color_palette)) {
    if (requireNamespace("cmocean", quietly = TRUE)) {
      color_palette <- cmocean::cmocean("matter")(256)
    } else {
      color_palette <- viridis::viridis(256)
    }
  }
  
  # Create base plot
  p <- ggplot(data = data, aes(x = lon, y = lat)) +
    geom_tile(aes_string(fill = value_col)) +
    theme_classic() +
    labs(y = "", x = "") +
    theme(legend.position = "right", legend.title = element_blank()) +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    scale_fill_gradientn(colours = color_palette, 
                          limits = c(0, max(data[[value_col]], na.rm = TRUE))) +
    annotation_map(map_data("world"), colour = "black", fill = "grey50") +
    coord_quickmap(xlim = xlim, ylim = ylim) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  
  # Add animation if requested
  if (animated) {
    if (!requireNamespace("gganimate", quietly = TRUE)) {
      stop("Package 'gganimate' needed for animation. Please install it.")
    }
    
    p <- p +
      gganimate::transition_time(year) +
      gganimate::ease_aes("linear") +
      labs(title = "{frame_time}")
    
    # Render animation
    if (requireNamespace("gifski", quietly = TRUE)) {
      anim <- gganimate::animate(p, 
                               nframes = length(unique(data$year)), 
                               fps = 2, 
                               renderer = gganimate::gifski_renderer())
      return(anim)
    } else {
      warning("Package 'gifski' is recommended for creating GIFs. Using default renderer.")
      anim <- gganimate::animate(p, 
                               nframes = length(unique(data$year)), 
                               fps = 2)
      return(anim)
    }
  } else {
    return(p)
  }
}

#' @title Get DisMAP Survey Locations
#' @description Download survey location points for a region
#' @param region Region name
#' @param base_url Base URL for survey locations service
#' @return A sf object with survey locations
#' @export
get_survey_locations <- function(region,
                              base_url = "https://services2.arcgis.com/C8EMgrsFcRFL6LrL/ArcGIS/rest/services") {
  
  require(httr)
  require(sf)
  
  # Extract region code from name for URL building
  region_code <- gsub(" ", "_", region)
  
  # Build URL
  url <- httr::parse_url(base_url)
  url$path <- paste(url$path, paste0(region_code, "_Survey_Locations_20220516/FeatureServer/1/query"), sep = "/")
  url$query <- list(
    where = sprintf("OARegion='%s'", region),
    outFields = "*",
    f = "geojson"
  )
  
  request <- httr::build_url(url)
  
  # Read data
  survey_locations <- sf::st_read(request, quiet = TRUE)
  
  return(survey_locations)
}

#' @title List Available DisMAP Regions
#' @description Return a list of available regions in DisMAP
#' @return Character vector of region names
#' @export
list_dismap_regions <- function() {
  regions <- c(
    "Aleutian Islands", 
    "Eastern Bering Sea", 
    "Gulf of Alaska", 
    "West Coast Triennial",
    "West Coast Annual", 
    "Gulf of Mexico", 
    "Northeast US Spring", 
    "Northeast US Fall",
    "Southeast US Fall", 
    "Southeast US Spring", 
    "Southeast US Summer", 
    "Northern Bering Sea", 
    "Eastern and Northern Bering Sea"
  )
  return(regions)
}

#' @title Get DisMAP Base URLs
#' @description Get the base URLs for various DisMAP services
#' @return A list with URLs for indicators, surveys, and rasters
#' @export
get_dismap_urls <- function() {
  urls <- list(
    indicators = "https://services2.arcgis.com/C8EMgrsFcRFL6LrL/arcgis/rest/services/Indicators_20220516/FeatureServer",
    surveys = "https://services2.arcgis.com/C8EMgrsFcRFL6LrL/ArcGIS/rest/services",
    rasters = list(
      "Northeast US Spring" = "https://maps.fisheries.noaa.gov/image/rest/services/DisMAP/Northeast_US_Spring_20220516/ImageServer",
      "Northeast US Fall" = "https://maps.fisheries.noaa.gov/image/rest/services/DisMAP/Northeast_US_Fall_20220516/ImageServer",
      # Add other regions as needed
      "Eastern Bering Sea" = "https://maps.fisheries.noaa.gov/image/rest/services/DisMAP/Eastern_Bering_Sea_20220516/ImageServer"
    )
  )
  
  # Add note about checking inPort for latest URLs
  message("NOTE: Check https://www.fisheries.noaa.gov/inport/item/66799 for the most up-to-date URLs")
  
  return(urls)
}
