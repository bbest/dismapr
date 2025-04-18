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

