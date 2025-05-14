#' Get SMHI station information for specified parameters
#'
#' This function retrieves information about SMHI stations based on the provided
#' parameter IDs. It supports the option to return spatial data in the form of
#' an `sf` object with geographic coordinates.
#'
#' @param parameter_key A character vector containing the parameter IDs (e.g., "1" for temperature) to retrieve station data for.
#' @param version A character string indicating the version of the API to use. Default is "latest".
#' Chech `https://opendata-download-metobs.smhi.se/metobs/introduction` for available versions.
#' @param spatial A logical value indicating whether the results should be returned as a spatial object (`sf` object).
#'   If `TRUE`, the result will be a spatial data frame with coordinates for each station.
#'   Default is `FALSE`.
#' 
#' @return A data frame (or `sf` object if `spatial = TRUE`) containing the following columns:
#'   \item{parameter_id}{The parameter ID.}
#'   \item{station_id}{The station ID.}
#'   \item{station_name}{The name of the station.}
#'   \item{longitude}{The longitude of the station.}
#'   \item{latitude}{The latitude of the station.}
#'   \item{height}{The elevation (in metres) of the station.}
#'   \item{owner}{The owner of the station.}
#'   \item{ownerCategory}{The category of the owner.}
#'   \item{active}{Indicates whether the station is active.}
#'   \item{from}{The start date of station data.}
#'   \item{to}{The end date of station data.}
#'   \item{updated}{The date when the station information was last updated.}
#'   
#' @seealso [get_smhi_station()], [get_smhi_parameters()]
#'
#' @examples
#' # Retrieve station information for temperature (parameter_id = "1") from the latest version
#' stations <- get_smhi_station(parameter_key = c("1"))
#' 
#' # Retrieve station information with spatial data (sf object)
#' stations_spatial <- get_smhi_station(parameter_key = c("1"), spatial = TRUE)
#'
#' @export

get_smhi_station <- function(parameter_key, version = "latest", spatial = F) {
  
  for (par in parameter_key) {
    if (!par %in% stations_smhi$parameter_id) {
      rlang::abort(
        message = glue::glue("Parameter '{par}' is not recorded in SMHI stations.\nUse 'get_smhi_parameters()' to check the available parameters."),
        class = "smhi_invalid_parameter",
        parameter = par
      )
    }
  }
  
  all_data <- list()
  
  for(par in parameter_key) {
    
    url <- paste0(.metObsAPI, "/version/",version,"/parameter/",par,".xml")
    doc <- xml2::read_xml(url)
    ns <- xml2::xml_ns(doc)
    
    stations_info <-
      dplyr::tibble(
        parameter_id = par,
        station_id = get_smhi_info(doc, ns, 1, "id"),
        station_name = get_smhi_info(doc, ns, 1, "name"),
        longitude = get_smhi_info(doc, ns, 1, "longitude"),
        latitude = get_smhi_info(doc, ns, 1, "latitude"),
        height = get_smhi_info(doc, ns, 1, "height"),
        owner = get_smhi_info(doc, ns, 1, "owner"),
        ownerCategory = get_smhi_info(doc, ns, 1, "ownerCategory"),
        active = get_smhi_info(doc, ns, 1, "active"),
        from = get_smhi_info(doc, ns, 1, "from"),
        to = get_smhi_info(doc, ns, 1, "to"))
    
    if(length(get_smhi_info(doc, ns, 1, "id")) ==
       length(get_smhi_info(doc, ns, 2, "updated")) ) {
      updates <- get_smhi_info(doc, ns, 2, "updated")
    } else {
      updates <- get_smhi_info(doc, ns, 2, "updated")[-1]
    }
    
    stations_info <-
      stations_info %>% 
      dplyr::bind_cols(tibble(updated = updates))
    
    if(isTRUE(spatial)) {
      stations_info <- sf::st_as_sf(stations_info, 
                                    coords = c("longitude", "latitude"),
                                    crs = sf::st_crs(4326))
    }
    
    all_data[[par]] <- stations_info
  }
  
  all_data <- dplyr::bind_rows(all_data)
  
  return(all_data)
}
