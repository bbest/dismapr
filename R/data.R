#' DisMAP datasets: region and optional season
#'
#' A multiline simple feature boundary of regions and seasons used in DisMAP.
#'
#' @format ## `dm_datasets`
#' @concept data
#' A data frame with 14 features and 4 fields:
#' - `dataset_code`: DisMAP dataset code
#' - `region`: Region name
#' - `season`: Season name
#' - `distribution_project_code`: DisMAP distribution project code
#'
#' @source <https://services2.arcgis.com/C8EMgrsFcRFL6LrL/ArcGIS/rest/services/DisMAP_Regions_CURRENT/FeatureServer/1>
"dm_datasets"

#' Plot spatial datasets
#'
#' Creates a ggplot to visualize the spatial datasets available in DisMAP.
#'
#' @param data A spatial data frame, typically dm_datasets
#' @param color Column name for color aesthetic (default: "dataset_code")
#' @param title Plot title (default: "NOAA DisMAP dataset_code (regions and seasons)")
#' @param ... Additional arguments passed to ggplot2 functions
#'
#' @return A ggplot object
#' @export
#' @concept data
#' @importFrom ggplot2 ggplot geom_sf aes theme_minimal labs
#'
#' @examples
#' # show table of dataset_codes for available regions and seasons
#' dm_datasets |> 
#'   sf::st_drop_geometry() |>
#'   head()
#'
#' # show map of dataset_codes
#' dm_plot_datasets(dm_datasets)
dm_plot_datasets <- function(data, 
                          color = "dataset_code", 
                          title = "NOAA DisMAP dataset_code (regions and seasons)", 
                          ...) {
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = data, 
      ggplot2::aes(color = .data[[color]]), ...) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(
        colour = "black", fill = NA, linewidth = 1))
}
