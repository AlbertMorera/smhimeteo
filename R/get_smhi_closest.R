#' Get SMHI meteorological data from the closest stations to spatial points
#'
#' This function identifies the closest meteorological stations to one or more spatial points (`sf::POINT`)
#' that have available data for the selected parameters and time period. It then downloads the corresponding
#' meteorological data using `get_smhi_data2()`.
#'
#' @param points An `sf` object (of type `POINT`) representing one or more spatial locations.
#' @param parameter_key A character vector with the SMHI parameter IDs (e.g. `"1"` for temperature). 
#' @param period_name A character string indicating the period for which the data is requested (e.g., `"latest-hour"`).
#'    Options include "latest-day", "latest-hour", "latest-months" and "corrected-archive".
#' @param verbose Logical. Whether to display messages during processing. Default is `TRUE`.
#' @param n Integer. Number of closest stations to retrieve for each point. Default is `1`.
#' @param max_radius Numeric. Maximum distance (in kilometers) to search for stations. Default is `100`.
#'
#' @return A tibble with meteorological data from the closest available stations for each point and parameter. 
#' If no stations are found within the given radius, returns `NULL`.
#'
#' @details
#' This function first uses `get_closest_stations()` to find nearby stations for each point that match the given
#' `parameter_key` and `period_name`. Then it downloads and compiles the corresponding data using `get_smhi_data2()`.
#'
#' @seealso [get_closest_stations()], [get_smhi_data2()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' points <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(18.07, 59.33)), crs = 4326))
#' get_smhi_closest(points, parameter_key = "1", period_name = "latest-hour")
#' }

get_smhi_closest <- function(points, parameter_key, period_name, verbose = TRUE,
                             n = 1, max_radius = 100) {
  
  check_valid_period(period_name)
  
  for (par in parameter_key) {
    check_valid_parameter(par)
  }
  
  results <- purrr::map_dfr(1:nrow(points), function(i) {
    point <- points[i, ]
    
    result <- get_closest_stations(point, parameter_key, period_name, max_radius, n)
    
    return(result)
  })
  
  results <-
    results %>%
    get_smhi_data2(period_name = period_name)
  
  if (is.null(results)) return(NULL)
  
  result <-
    result %>%
    dplyr::mutate(quality = dplyr::case_when(quality == "" ~ NA_character_,
                                             TRUE ~ quality)) %>%
    dplyr::select(-geometry)
  
  return(results)
}
