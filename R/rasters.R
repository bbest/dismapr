#' @title Get DisMAP layers (scientific species name or "Species Richness") for a dataset
#' @description Get a list of available layers for a given region and season
#' @param datset_code The region (and season) code, per available `dataset_code` in `dm_datasets`
#' @return A character vector of layer names, usually species scientific name or "Species Richness"
#' @export
#' @importFrom arcgisutils fetch_layer_metadata
#' @importFrom glue glue
#' @importFrom purrr pluck
#' @concept rasters
#' @examples
#' # show list of available layers for Northeast US Spring dataset
#' dm_get_dataset_layers("NEUS_SPR")
dm_get_dataset_layers <- function(dataset_code){
  # dataset_code = "AI"

  stopifnot(
    dataset_code %in% dm_datasets$dataset_code,
    length(dataset_code) == 1)

  glue::glue(
    "https://maps.fisheries.noaa.gov/image/rest/services/DisMAP/{dataset_code}_IDW_CURRENT/ImageServer/multiDimensionalInfo") |>
    arcgisutils::fetch_layer_metadata() |>
    purrr::pluck("multidimensionalInfo", "variables", "name") |>
    sort()
}

dm_get_dataset_layer_year_slices <- function(dataset_code, layer){
  # internal function so user doesn't have to worry about sliceId, only needs year

  # TODO: setup below as function
  img_url  <- glue::glue("https://maps.fisheries.noaa.gov/image/rest/services/DisMAP/{dataset_code}_IDW_CURRENT/ImageServer")

  lst_slices <- httr2::request(img_url) |>
    httr2::req_url_path_append("slices") |>
    httr2::req_url_query(
      multidimensionalDefinition = glue("[{{variableName: '{layer}'}}]"),
      f = "json") |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("slices")

  tbl_slices <- tibble::tibble(
    sliceId       = purrr::map_int(lst_slices, "sliceId"),
    dimensionName = purrr::map_chr(lst_slices, \(x) purrr::pluck(x, "multidimensionalDefinition", 1, "dimensionName")),
    value         = purrr::map_dbl(lst_slices, \(x) purrr::pluck(x, "multidimensionalDefinition", 1, "values", 1))) |>
    dplyr::arrange(dimensionName, value)

  stopifnot(all(tbl_slices$dimensionName == "StdTime"))

  tbl_slices <- tbl_slices |>
    dplyr::mutate(
      dtime = arcgisutils::from_esri_date(value),
      year  = lubridate::year(dtime))

  # ensure no duplicate years
  # (otherwise dtime varying by something other than year)
  stopifnot(
    tbl_slices |>
      dplyr::group_by(year) |>
      dplyr::summarize(n = dplyr::n()) |>
      dplyr::filter(n > 1) |>
      nrow() == 0)

  tbl_slices |>
    dplyr::select(slice_id = sliceId, year)
}

#' @title Get DisMAP years for a dataset layer
#' @description Get a list of years for a given species in a DisMAP dataset
#' @param dataset_code The DisMAP dataset code (e.g., "AI", "EBS", "GOA"), per `dm_datasets$dataset_code`
#' @param layer The layer in the dataset, typically the scientific name of the species or "Species Richness", per `dm_get_dataset_layers(dataset_code)`
#' @return A vector of years available for the layer in the dataset
#' @export
#' @concept rasters
#' @importFrom dplyr pull
#' @examples
#' # show list of available years for Summer Flounder in Northeast US Spring
#' dm_get_dataset_layer_years("NEUS_SPR", "Paralichthys dentatus")
dm_get_dataset_layer_years <- function(dataset_code, layer){
  dm_get_dataset_layer_year_slices(dataset_code, layer) |>
    dplyr::pull(year)
}

#' @title Get DisMAP raster for dataset layer year
#' @description Download a single interpolated biomass raster by slice ID
#' @param dataset_code The DisMAP dataset code (e.g., "AI", "EBS", "GOA"), per `dm_datasets$dataset_code`
#' @param layer The layer in the dataset, typically the scientific name of the species or "Species Richness", per `dm_get_dataset_layers(dataset_code)`
#' @param year The year of the slice to download, per `dm_get_dataset_layer_years(dataset_code, layer)`
#' @param out_tif optional output GeoTIFF path to write raster
#' @param out_sr optional output spatial reference for output raster; defaults to native spatial reference
#' @param transform Function to transform raster values (default is cube root): function(x) x^(1/3)
#' @param bbox optional bounding box coordinates (xmin, ymin, xmax, ymax); defaults to maximum extent
#' @param bbox_sr optional spatial reference for bounding box; defaults to native spatial reference; you can use geographic (4326)
#' @param overwrite if out_tif exists, overwrite it (default is FALSE)
#' @param verbose if TRUE, print additional information
#' @return A terra::rast object
#' @importFrom httr2 request req_url_query req_perform resp_body_json
#' @importFrom terra plet rast writeRaster
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom dplyr filter select
#' @importFrom purrr pluck
#' @importFrom stringr str_replace_all
#' @importFrom tibble tibble
#' @importFrom sf st_as_sf
#' @importFrom terra rast writeRaster
#' @export
#' @concept rasters
#' @examples
#' # get raster for a specific dataset, layer, and year
#' dataset_code <- "NEUS_SPR"
#' layer <- "Paralichthys dentatus"
#' year <- 2015
#'
#' # download the raster
#' r <- dm_get_raster(dataset_code, layer, year)
#' r
#'
#' # alternatively, save to a file
#' tif_path <- tempfile(fileext = ".tif")
#' r <- dm_get_raster(dataset_code, layer, year, tif_path)
#' file.exists(tif_path)
dm_get_raster <- function(
    dataset_code,
    layer,
    year,
    out_tif   = NULL,
    out_sr    = NULL,
    transform = function(x) x^(1/3),
    bbox      = NULL,
    bbox_sr   = NULL,
    overwrite = F,
    verbose   = F) {

  if (!is.null(out_tif) && file.exists(out_tif) && !overwrite) {
    if (verbose)
      message("File already exists and overwrite is set to FALSE. Skipping download.")
    r <- terra::rast(out_tif)
    return(r)
  }

  # check for valid dataset_code
  if (!dataset_code %in% dm_datasets$dataset_code)
    stop("dataset_code must be one of: ", paste(dm_datasets$dataset_code, collapse = ", "))
  if (length(dataset_code) != 1)
    stop("dataset_code must be a single value")

  # check for valid layer
  lyrs <- dm_get_dataset_layers(dataset_code)
  if (!layer %in% lyrs)
    stop("layer must be one of the dataset's available layers: ", paste(lyrs, collapse = ", "))
  if (length(layer) != 1)
    stop("layer must be a single value")

  # check for valid year
  d_yr_slices <- dm_get_dataset_layer_year_slices(dataset_code, layer)
  yrs <- d_yr_slices$year
  if (!year %in% yrs)
    stop("year must be one of the dataset's available years: ", paste(yrs, collapse = ", "))
  if (!is.numeric(year) || length(year) != 1)
    stop("year must be a single numeric value")
  slice_id <- d_yr_slices |>
    filter(year == !!year) |>
    pull(slice_id)

  if (verbose)
    message("Getting image server information.")
  img_url  <- glue::glue("https://maps.fisheries.noaa.gov/image/rest/services/DisMAP/{dataset_code}_IDW_CURRENT/ImageServer")
  res <- httr2::request(img_url) |>
    httr2::req_url_query(
      f = "json") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  b <- res[["extent"]]
  native_bbox <- c(b$xmin, b$ymin, b$xmax, b$ymax) |>
    paste(collapse = ",")
  native_sr <- b$spatialReference$latestWkid

  if (is.null(bbox))
    bbox <- native_bbox
  if (is.null(bbox_sr))
    bbox_sr <- native_sr
  if (is.null(out_sr))
    out_sr <- native_sr

  # Build the export image URL
  if (verbose)
    message("Fetching image.")
  tmp_tif <- tempfile(fileext = ".tif")
  res <- httr2::request(img_url) |>
    httr2::req_url_path_append("exportImage") |>
    httr2::req_url_query(
      sliceID              = slice_id,
      f                    = "image",
      bbox                 = bbox,
      bboxSR               = bbox_sr,
      format               = "tiff",
      pixelType            = "F32",
      noData               = "3.4e+38",
      noDataInterpretation = "esriNoDataMatchAny",
      interpolation        = "+RSP_BilinearInterpolation",
      imageSR              = out_sr) |>
    httr2::req_perform(
      path = tmp_tif)

  # Read the raster
  if (verbose)
    message("Reading image and applying transformation")
  r <- terra::rast(tmp_tif)
  eval(transform)
  if (!is.null(transform))
    r <- transform(r)
  names(r) <- "wtcpue"
  # r
  # range(values(r, na.rm = T)) # 0.00000 47.82674
  # plet(r)

  if (!is.null(out_tif)) {
    if (verbose)
      message("Writing raster to file.")
    terra::writeRaster(r, out_tif, overwrite = overwrite)
    unlink(tmp_tif)
    r <- terra::rast(out_tif)
  }

  r
}

#' Plot raster data
#'
#' Creates a visualization of the raster data, either interactively or statically.
#'
#' @param r A SpatRaster object
#' @param interactive Whether to create an interactive plot (default: TRUE)
#' @param tiles Background tiles for interactive plot (default: "Esri.OceanBasemap")
#' @param main Map title for interactive plot (default: NULL)
#' @param var Variable name to plot in static plot (default: determined from raster)
#' @param title Plot title for static plot (default: NULL)
#' @param fill_palette Color palette function for static plot (default: scale_fill_viridis_c)
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A mapview object (interactive=TRUE) or ggplot object (interactive=FALSE)
#' @export
#' @concept rasters
#' @importFrom mapview mapView
#' @importFrom ggplot2 ggplot geom_tile aes scale_fill_viridis_c coord_fixed labs
#' @importFrom terra as.data.frame
#'
#' @examples
#' # Get a raster for demonstration
#' dataset_code <- "NEUS_SPR"
#' layer <- "Paralichthys dentatus"
#' year <- 2015
#' r <- dm_get_raster(dataset_code, layer, year)
#'
#' # Interactive plot
#' # Not run: dm_plot_raster(r, interactive = TRUE, main = paste(dataset_code, "Summer Flounder", year))
#'
#' # Static plot
#' dm_plot_raster(r, interactive = FALSE, title = paste(dataset_code, "Summer Flounder", year))
dm_plot_raster <- function(r,
                         interactive = TRUE,
                         tiles = "Esri.OceanBasemap",
                         main = NULL,
                         var = NULL,
                         title = NULL,
                         fill_palette = ggplot2::scale_fill_viridis_c,
                         ...) {
  if (interactive) {
    mapview::mapView(r, map.types = tiles, layer.name = main, ...)
  } else {
    if(is.null(var)) var <- names(r)[1]

    rdf <- terra::as.data.frame(r, xy = TRUE)

    ggplot2::ggplot() +
      ggplot2::geom_tile(
        data = rdf,
        ggplot2::aes(x = x, y = y, fill = .data[[var]]), ...) +
      fill_palette() +
      ggplot2::coord_fixed() +
      ggplot2::labs(title = title)
  }
}

