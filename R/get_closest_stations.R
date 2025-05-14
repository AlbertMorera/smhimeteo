#' Get closest SMHI stations with available data
#'
#' This function identifies the closest meteorological stations (from a preloaded `stations_smhi` dataset)
#' to one or more spatial points (`sf::POINT`), filtering by available parameters and time periods.
#'
#' @param points An `sf` object (of type `POINT`) representing one or more spatial locations.
#' @param parameter_key A character vector of SMHI parameter IDs to search for (e.g. `"1"` for temperature).
#' @param period_name A character string indicating the period for which data is requested (e.g., `"latest-month"`).
#'    Options include "latest-day", "latest-hour", "latest-months" and "corrected-archive".
#' @param max_radius Numeric. Maximum distance (in kilometers) to search for stations. Default is `100`.
#' @param n Integer. Number of closest stations to retrieve per point and parameter. Default is `1`.
#'
#' @return A tibble with information about the closest stations for each spatial point and parameter,
#'   including columns for `parameter_id`, `station_id`, and `distance.km`, as well as the original coordinates.
#'
#' @details
#' This function filters the internal `stations_smhi` dataset to find stations matching the given
#' `parameter_key`, and checks that the stations contain data for the selected `period_name` using
#' `get_smhi_period()`. Only stations within the given `max_radius` are considered.
#'
#' @seealso [get_smhi_period()], [get_smhi_data2()], [get_smhi_closest()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' point <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(18.07, 59.33)), crs = 4326))
#' get_closest_stations(point, parameter_key = "1", period_name = "latest-month")
#' }

get_closest_stations <- function(points, parameter_key, period_name, max_radius = 100, n = 1) {
  
  
  all_data <- list()
  
  for(i in 1:nrow(points)) {
    
    pt <- points[i,]
    
    stations_smhi_i <- 
      stations_smhi %>%
      dplyr::filter(parameter_id %in% parameter_key) %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs(4326)) %>%
      dplyr::mutate(distance.km = sf::st_distance(geometry, sf::st_transform(pt, sf::st_crs(4326))) %>% as.numeric()
                    , distance.km = distance.km / 1000
      ) %>%
      dplyr::filter(distance.km <= max_radius) %>%
      dplyr::mutate(periods = purrr::map2(station_id, parameter_id, ~ get_smhi_period(parameter_key = .y, .x))) %>%
      dplyr::filter(map_lgl(periods, ~ period_name %in% .x))
    
    for(par in parameter_key) {
      if (nrow(stations_smhi_i %>% dplyr::filter(parameter_id == par)) == 0) {
        rlang::warn(glue::glue("No stations found with parameter `{par}` and period `{period_name}` within a 
                            radius of {max_radius}km."), 
                    class = "smhi_no_stations")
      }
    }
    
    closest <- stations_smhi_i %>%
      group_by(parameter_id) %>%
      dplyr::slice_min(distance.km, n = n) %>%
      select(parameter_id, station_id, distance.km)
    
    closest <-
      pt %>%
      dplyr::bind_cols(
        closest %>%
          dplyr::as_tibble() %>%
          dplyr::select(-geometry)) %>%
      select(-geometry)
    
    all_data[[i]] <- closest
    
  }
  all_data <- dplyr::bind_rows(all_data)  
  
  return(all_data)
}
