#' Get available periods for a specific station and parameter
#'
#' This function retrieves the available periods for a given station and parameter
#' from the SMHI API. The periods represent the time spans for which data is available
#' for the specified station and parameter.
#'
#' @param station_key A character string representing the station ID. Check `get_smhi_station()` for stations names.
#' @param parameter_key A character string representing the parameter ID. Check `get_smhi_parameters()` .
#' @param version A character string indicating the version of the API to use. Default is "latest".
#' Chech `https://opendata-download-metobs.smhi.se/metobs/introduction` for available versions.
#'
#' @return A character vector of period names representing the available periods for the specified station and parameter.
#'
#'
#' @examples
#' # Retrieve available periods for a specific station and parameter
#' periods <- get_smhi_period(station_key = "159880", parameter_key = "1")
#'
#' @export

get_smhi_period <- function(station_key, parameter_key, version = "latest") {
  
  check_valid_station(station_key)
  check_valid_parameter(parameter_key)
    
  url <- paste0(.metObsAPI, "/version/",version,"/parameter/",parameter_key,
                "/station/",station_key,".xml")
  
  doc <- xml2::read_xml(url)
  ns <- xml2::xml_ns(doc)
  
  period_names <- get_smhi_info(doc, ns, 1, "period")
  
  period_names <- stringr::str_extract(period_names, "\\w+-[:letter:]+")
  
  return(period_names)
}
