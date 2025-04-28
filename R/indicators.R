#' @title Get DisMAP indicators table, possibly filtered by field = value pairs
#' @description Download species indicators data including Center of Gravity metrics
#' @param ... key = value pairs for filtering; where key could be any of the fields in the indicators table
#' @return A sf object with indicators data
#' @importFrom arcgisutils parse_esri_json
#' @importFrom dplyr arrange filter left_join mutate pull select
#' @importFrom glue glue
#' @importFrom httr2 request req_perform req_url_query  resp_body_json
#' @importFrom janitor clean_names
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_chr pluck
#' @importFrom snakecase to_snake_case to_any_case
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @concept indicators
#' @export
#' @examples
#' # Get indicators data for American Lobster in Northeast US Spring
#' tbl_lobster_indicators <- dm_get_indicators(
#'   common_name = "American lobster",
#'   region      = "Northeast US",
#'   season      = "Spring")
#' tbl_lobster_indicators
dm_get_indicators <- function(
    ...) {

  if (!(require("arcgisutils"))) {
    stop("Please install the required arcgis packages with the command: install.packages('arcgis', repos = 'https://r-arcgis.r-universe.dev')")
  }

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

#' Plot indicators data
#'
#' Creates a visualization of indicators data, including center of gravity changes over time for a species.
#'
#' @param data A data frame containing indicators data, typically from dm_get_indicators()
#' @param x Column name for longitude (default: "center_of_gravity_longitude")
#' @param y Column name for latitude (default: "center_of_gravity_latitude")
#' @param color Column name for color aesthetic (default: "year")
#' @param title Plot title (default: "Indicators")
#' @param ... Additional arguments passed to ggplot2 functions
#'
#' @return A ggplot object
#' @export
#' @concept indicators
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_point aes labs theme_classic theme element_rect
#'
#' @examples
#' tbl_lobster_indicators <- dm_get_indicators(
#'   common_name = "American lobster",
#'   region      = "Northeast US",
#'   season      = "Spring")
#' tbl_lobster_indicators
#'
#' # plot center of gravity changes over time
#' dm_plot_indicators(
#'   tbl_lobster_indicators,
#'   title = "American Lobster Center of Gravity")
dm_plot_indicators <- function(
    data,
    x = "center_of_gravity_longitude",
    y = "center_of_gravity_latitude",
    color = "year",
    title = "Indicators",
    ...) {
  ggplot2::ggplot(data = data) +
    ggplot2::geom_point(ggplot2::aes(
      x = .data[[x]],
      y = .data[[y]],
      color = .data[[color]]), ...) +
    ggplot2::labs(
      x = "Longitude",
      y = "Latitude",
      title = title) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(
        colour = "black", fill = NA, linewidth = 1))
}
