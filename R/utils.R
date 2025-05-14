#' @keywords internal
#' @noRd
.metObsAPI <- "https://opendata-download-metobs.smhi.se/api"

#' Check if station ID is valid
#'
#' Verifies that a given station ID is present in the SMHI stations.
#'
#' @param station A character string representing the station ID to check.
#' @keywords internal
#' @noRd
check_valid_station <- function(station) {
  #data("stations_smhi", package = "smhimeteo")
  
  if (!station %in% unique(stations_smhi$station_id)) {
    rlang::abort(
      message = glue::glue("Station `{station}` is not found in SMHI stations. Check `get_smhi_station()`."),
      class = "smhi_invalid_station"
    )
  }
}

#' Check if a parameter ID is valid
#'
#' Internal helper that verifies if a parameter ID is available in the SMHI station metadata.
#'
#' @param parameter A character string with the parameter ID to check.
#' @noRd
check_valid_parameter <- function(parameter) {
  #data("stations_smhi", package = "smhimeteo")
  
  if (!parameter %in% unique(stations_smhi$parameter_id)) {
    rlang::abort(
      message = glue::glue("Parameter `{parameter}` is not valid. Check get_smhi_parameters()"),
      class = "smhi_invalid_parameter"
    )
  }
}

#' Check if a parameter is available for a specific station
#'
#' Internal helper that checks if a given station provides data for a specified parameter.
#'
#' @param station A character string with the station ID.
#' @param parameter A character string with the parameter ID.
#' @noRd
check_available_parameter <- function(station, parameter) {
  #data("stations_smhi", package = "smhimeteo")
  
  stations_smhi %>% filter(station_id == station & parameter_id == parameter)
  
  if (nrow(stations_smhi %>% filter(station_id == station & parameter_id == parameter)) == 0) {
    rlang::abort(
      message = glue::glue("Parameter `{parameter}` is not available for the station {station}. Check `data('stations_smhi')`"),
      class = "smhi_unavailable_paramter"
    )
  }
}

#' Check if a period is valid
#'
#' Internal helper that verifies if the given period is one of the accepted SMHI values.
#'
#' @param period A character string indicating the period name (e.g. "latest-day"). 
#' Options include "latest-day", "latest-hour", "latest-months" and "corrected-archive".
#' @noRd
check_valid_period <- function(period) {
  valid_periods <- c("latest-hour", "latest-day", "latest-months", "corrected-archive")
  if (!period %in% valid_periods) {
    rlang::abort(
      message = glue::glue("Period `{period}` is invalid. Choose one of: {toString(valid_periods)}."),
      class = "smhi_invalid_period"
    )
  }
}
