# dismapr: An R Package for NOAA DisMAP Data Retrieval
# Functions for working with the Distribution Mapping and Analysis Portal (DisMAP) API

#' @title Get DisMAP Indicators Table
#' @description Download species indicators data including Center of Gravity metrics
#' @param ... key = value pairs for filtering; where key could be any of the fields in the indicators table
#' @return A sf object with indicators data
#' @importFrom dplyr arrange filter left_join mutate pull select
#' @importFrom glue glue
#' @importFrom httr2 request req_perform req_url_query  resp_body_json
#' @importFrom janitor clean_names
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_chr pluck
#' @importFrom snakecase to_snake_case to_any_case
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#'
#' @export
get_dm_indicators <- function(
    ...) {

  require(httr2)
  require(jsonlite)
  if (!(require("arcgisutils"))) {
    stop("Please install the required arcgis packages with the command: install.packages('arcgis', repos = 'https://r-arcgis.r-universe.dev')")
  }
  require(stringr)
  require(purrr)
  require(glue)
  require(janitor)

  base_url = "https://services2.arcgis.com/C8EMgrsFcRFL6LrL/ArcGIS/rest/services/Indicators_CURRENT/FeatureServer/1/query"

  # get input arguments as list
  args <- list(...)

  # fields
  info_url <- stringr::str_replace(base_url, "query$", "")
  flds <- info_url |>
    httr2::request() |>
    httr2::req_url_query(
      f = "pjson") |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("fields") |>
    purrr::map_chr("name")

  d_flds <- tibble::tibble(
    fld_esri  = flds,
    fld_snake = snakecase::to_snake_case(flds),
    fld_camel = snakecase::to_any_case(flds, case = "upper_camel", abbreviations = c("SE","OBJECTID")))

  d_flds_bad <- filter(d_flds, fld_esri != fld_camel)
  stopifnot(nrow(d_flds_bad) == 0)

  # build where clause from argument inputs
  args_not_fld <- names(args)[!names(args) %in% d_flds$fld_snake]
  if (length(args_not_fld) > 0)
    stop("The following arguments are not valid field names:", paste(args_not_fld, collapse = ", "),
         "Please use one of: ", paste(d_flds$fld_snake, collapse = ", "))

  # build where clause from arg = 'value' pairs with AND collapsing and check for value being numeric or character for including single quotes
  where_clause <- tibble::tibble(
    fld = names(args),
    val = unlist(args),
    typ = sapply(unlist(args), class)) |>
    left_join(
      d_flds,
      by = c("fld" = "fld_snake")) |>
    dplyr::mutate(
      q = ifelse(
        typ == "character",
        glue::glue("{fld_esri} = '{val}'"),
        glue::glue("{fld_esri} = {val}"))) |>
    dplyr::pull(q) |>
    paste(collapse = " AND ")

  # Build URL
  d <- httr2::request(base_url) |>
    httr2::req_url_query(
      where     = where_clause,
      outFields = "*",
      f         = "json") |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    arcgisutils::parse_esri_json() |>
    janitor::clean_names() |>
    dplyr::tibble()

  return(d)
}

#' @title Get DisMAP layer names, given region season
#' @description Get a list of available layers for a given region and season
#' @param datset_code The region (and season) code, per available `dataset_code` in `dm_regions`
#' @return A character vector of layer names, usually species scientific name or "Species Richness"
#' @export
#' @importFrom arcgisutils fetch_layer_metadata
#' @importFrom glue glue
#' @importFrom purrr pluck
#' @examples
#' get_dm_dataset_layers("AI")
get_dm_dataset_layers <- function(dataset_code){
  # dataset_code = "AI"

  stopifnot(
    dataset_code %in% dm_regions$dataset_code,
    length(dataset_code) == 1)

  glue::glue(
    "https://maps.fisheries.noaa.gov/image/rest/services/DisMAP/{dataset_code}_IDW_CURRENT/ImageServer/multiDimensionalInfo") |>
    arcgisutils::fetch_layer_metadata() |>
    purrr::pluck("multidimensionalInfo", "variables", "name") |>
    sort()
}

get_dm_dataset_species_year_slices <- function(dataset_code, species_scientific){
  # internal function so user doesn't have to worry about sliceId, only needs year

  # TODO: setup below as function
  img_url  <- glue::glue("https://maps.fisheries.noaa.gov/image/rest/services/DisMAP/{dataset_code}_IDW_CURRENT/ImageServer")

  lst_slices <- httr2::request(img_url) |>
    httr2::req_url_path_append("slices") |>
    httr2::req_url_query(
      multidimensionalDefinition = glue("[{{variableName: '{species_scientific}'}}]"),
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

#' @title Get DisMAP years for a species
#' @description Get a list of years for a given species in a DisMAP dataset
#' @param dataset_code The DisMAP dataset code (e.g., "AI", "EBS", "GOA"), per `dm_regions$dataset_code`
#' @param species_scientific The scientific name of the species, per `get_dm_dataset_layers(dataset_code)`
#' @return A vector of years available for the species in the dataset
#' @export
#' @importFrom dplyr pull
#' @examples
#' get_dm_dataset_species_years("AI", "Paralichthys dentatus")
get_dm_dataset_species_years <- function(dataset_code, species_scientific){
  get_dm_dataset_species_year_slices(dataset_code, species_scientific) |>
    dplyr::pull(year)
}

#' @title Download DisMAP Raster
#' @description Download a single interpolated biomass raster by slice ID
#' @param dataset_code The DisMAP dataset code (e.g., "AI", "EBS", "GOA"), per `dm_regions$dataset_code`
#' @param species_scientific The scientific name of the species, per `get_dm_dataset_layers(dataset_code)`
#' @param year The year of the slice to download, per `get_dm_dataset_species_years(dataset_code, species_scientific)`
#' @param out_tif optional output GeoTIFF path to write raster
#' @param out_sr optional output spatial reference for output raster; defaults to native spatial reference
#' @param transform Function to transform raster values (default is cube root): function(x) x^(1/3)
#' @param bbox optional bounding box coordinates (xmin, ymin, xmax, ymax); defaults to maximum extent
#' @param bbox_sr optional spatial reference for bounding box; defaults to native spatial reference; you can use geographic (4326)
#' @param overwrite if out_tif exists, overwrite it (default is FALSE)
#' @param verbose if TRUE, print additional information
#' @return A terra::rast object
#' @importFrom httr2 request req_url_query req_perform resp_body_json
#' @importFrom terra rast writeRaster
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom dplyr filter select
#' @importFrom purrr pluck
#' @importFrom stringr str_replace_all
#' @importFrom tibble tibble
#' @export
get_dm_raster <- function(
    dataset_code,
    species_scientific,
    year,
    out_tif   = NULL,
    out_sr    = NULL,
    transform = function(x) x^(1/3),
    bbox      = NULL,
    bbox_sr   = NULL,
    overwrite = F,
    verbose   = F) {

  require(httr2)
  require(terra)

  if (!is.null(out_tif) && file.exists(out_tif) && !overwrite) {
   if (verbose)
      message("File already exists and overwrite is set to FALSE. Skipping download.")
    r <- terra::rast(out_tif)
    return(r)
  }

  # check for valid dataset_code
  if (!dataset_code %in% dm_regions$dataset_code)
    stop("dataset_code must be one of: ", paste(dm_regions$dataset_code, collapse = ", "))
  if (length(dataset_code) != 1)
    stop("dataset_code must be a single value")

  # check for valid species_scientific
  lyrs <- get_dm_dataset_layers(dataset_code)
  if (!species_scientific %in% lyrs)
    stop("species_scientific must be one of the dataset's available layers: ", paste(lyrs, collapse = ", "))
  if (length(species_scientific) != 1)
    stop("species_scientific must be a single value")

  # check for valid year
  d_yr_slices <- get_dm_dataset_species_year_slices(dataset_code, species_scientific)
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

#' @title Get DisMAP Survey Locations
#' @description Download survey location points for a region
#' @param dataset_code The DisMAP dataset code (e.g., "AI", "EBS", "GOA"), per `dm_regions$dataset_code`
#' @param where Optional SQL WHERE clause to filter results (e.g., "year = 2015")
#' @return A sf object with survey locations
#' @export
get_dm_survey_locations <- function(
    dataset_code,
    where = NULL){

  url <- glue("https://services2.arcgis.com/C8EMgrsFcRFL6LrL/arcgis/rest/services/{dataset_code}_Sample_Locations_CURRENT/FeatureServer")
  fs <- arcgislayers::arc_open(url)
  lyr <- arcgislayers::get_layer(fs, 1)

  arcgislayers::arc_select(
    lyr,
    where = where) |>
    tibble::tibble() |>
    sf::st_as_sf()
}

