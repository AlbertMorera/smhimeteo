#' Get meteorological data for multiple station/parameter pairs
#'
#' This function retrieves meteorological data from the Swedish Meteorological and Hydrological Institute (SMHI) for multiple station/parameter pairs
#' and returns the data as a tidy tibble.
#'
#' @param pairs_tbl A tibble containing two columns: `station_id` and `parameter_id`. Check `get_smhi_station()` and `get_smhi_parameters()` for
#' stations and parameters information
#'   Each row represents a station/parameter pair for which data will be retrieved.
#' @param period_name A character string indicating the period for which the data is requested. 
#'   Options include "latest-day", "latest-hour", "latest-months" and "corrected-archive".
#' @param verbose A logical value indicating whether to print messages about the progress of the data download. Default is TRUE.
#'
#' @return A tibble containing the meteorological data for the specified station/parameter pairs and period.
#' 
#' @details
#' The function calls the `get_smhi_data()` function internally to retrieve data for each station/parameter pair.
#'
#' @seealso [get_smhi_station()], [get_smhi_parameters()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage to get data for multiple station/parameter pairs
#' pairs_tbl <- tibble::tibble(
#'   station_id = c("22051", "159880"),
#'   parameter_id = c("1", "21")
#' )
#' get_smhi_data2(pairs_tbl, period_name = "latest-hour")
#' }
#' 
get_smhi_data2 <- function(pairs_tbl, period_name, verbose = TRUE) {
  
  check_valid_period(period_name)
  
  if (nrow(pairs_tbl) == 0) {
    rlang::warn("No station/parameter pairs were provided.", class = "smhi_no_stations")
    return(NULL)
  }
  
  purrr::walk2(pairs_tbl$station_id, pairs_tbl$parameter_id, ~ {
    check_valid_station(.x)
    check_valid_parameter(.y)
    check_available_parameter(.x, .y)
    
    available_periods <- get_smhi_period(.x, .y)
    if (!(period_name %in% available_periods)) {
      rlang::abort(
        message = glue::glue("No data for station `{.x}`, parameter `{.y}` and period `{period_name}`."),
        class = "smhi_unavailable_data"
      )
    }
  })
  
  results <- 
    pairs_tbl %>%
    dplyr::mutate(data = purrr::map2(station_id, parameter_id, ~ {
      if (verbose) message(glue::glue("- Downloading data for station {.x}, parameter {.y}..."))
      get_smhi_data(.x, .y, period_name, verbose = FALSE) %>%
        pivot_longer(cols = -c("station_id", "date", "quality")) %>%
        dplyr::select(-station_id, -name)
    })) %>%
    unnest(data) %>%
    dplyr::mutate(parameter_id = paste0("par_", parameter_id)) %>%
    tidyr::pivot_wider(names_from = "parameter_id", values_from = "value")
  
  return(results)
}
