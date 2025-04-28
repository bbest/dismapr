#' @title Get DisMAP survey locations, possibly filtered by a where clause
#' @description Download survey location points for a region
#' @param dataset_code The DisMAP dataset code (e.g., "AI", "EBS", "GOA"), per `dm_datasets$dataset_code`
#' @param where Optional SQL WHERE clause to filter results (e.g., "year = 2015")
#' @return A sf object with survey locations
#' @importFrom arcgislayers arc_open get_layer arc_select
#' @importFrom glue glue
#' @importFrom sf st_as_sf
#' @concept surveys
#' @export
#' @examples
#' # Get survey locations for Northeast US Spring 2015
#' dataset_code <- "NEUS_SPR"
#' year <- 2015
#'
#' # Get locations filtered by year
#' pts_yr <- dm_get_survey_locations(dataset_code, where = paste0("Year = ", year))
#' head(pts_yr, 3)
dm_get_survey_locations <- function(
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

#' Plot survey locations
#'
#' Creates a visualization of survey locations, either interactively or statically.
#'
#' @param data A spatial data frame of survey locations
#' @param interactive Whether to create an interactive plot (default: TRUE)
#' @param var Variable name to use for color (default: "wtcpue")
#' @param title Plot title (default: "Survey Locations")
#' @param color_palette Color palette function for static plot (default: scale_color_viridis_c)
#' @param tiles Background tiles for interactive plot (default: c("Esri.OceanBasemap", "OpenStreetMap"))
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A mapview object (interactive=TRUE) or ggplot object (interactive=FALSE)
#' @export
#' @concept surveys
#' @importFrom ggplot2 ggplot geom_sf aes scale_color_viridis_c theme_minimal labs
#' @importFrom mapview mapView
#'
#' @examples
#' # Get survey locations for Northeast US Spring 2015
#' dataset_code <- "NEUS_SPR"
#' year <- 2015
#'
#' pts_yr <- dm_get_survey_locations(dataset_code, where = paste0("Year = ", year))
#' head(pts_yr, 3)
#'
#' # Summarize by biomass
#' pts_yr_sum <- pts_yr |>
#'   dplyr::group_by(Longitude, Latitude) |>
#'   dplyr::summarize(wtcpue = sum(WTCPUE), .groups = "drop")
#'
#' # Interactive plot
#' # Not run: dm_plot_survey_locations(pts_yr_sum, interactive = TRUE,
#'   title = paste("Northeast US Spring Survey Locations", year))
#'
#' # Static plot
#' dm_plot_survey_locations(pts_yr_sum, interactive = FALSE,
#'   title = paste("Northeast US Spring Survey Locations", year))
dm_plot_survey_locations <- function(
    data,
    interactive = TRUE,
    var = "wtcpue",
    title = "Survey Locations",
    color_palette = ggplot2::scale_color_viridis_c,
    tiles = c("Esri.OceanBasemap", "OpenStreetMap"),
    ...) {
  if (interactive) {
    mapview::mapView(
      data, zcol = var,
      layer.name = title,
      map.types = tiles,
      ...)
  } else {
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = data, ggplot2::aes(color = .data[[var]]), ...) +
      color_palette() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = title)
  }
}

