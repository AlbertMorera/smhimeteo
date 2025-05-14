#' Get SMHI data for a given station, parameter, and period
#'
#' This function retrieves meteorological data from the Swedish Meteorological and Hydrological Institute (SMHI) 
#' for a specified station, parameter, and period.
#'
#' @param station_key A character vector of station identifiers. Check `get_smhi_station()` for stations names.
#' @param parameter_key A character vector of parameter identifiers. Check `get_smhi_parameters()` .
#' @param period_name A character string indicating the period for which the data is requested. 
#'   Options include "latest-day", "latest-hour", "latest-months" and "corrected-archive".
#' @param verbose A logical value indicating whether to print messages about the progress of the data download. Default is TRUE.
#'
#' @return A tibble containing the meteorological data for the specified station, parameter, and period.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Get the latest hour temperature data for station 22051
#' get_smhi_data(station_key = "22051", parameter_key = "1", period_name = "latest-hour")
#' }
#' 

get_smhi_data <- function(station_key, parameter_key, period_name, verbose = T) {
  
  check_valid_period(period_name)
  
  for (station in station_key) {
    check_valid_station(station)
    for (par in parameter_key) {
      check_valid_parameter(par)
      
      check_available_parameter(station, par)
      
      available_periods <- get_smhi_period(station, par)
      
      if (!(period_name %in% available_periods)) {
        rlang::abort(
          message = glue::glue("No data for station `{station}`, parameter `{par}` and period `{period_name}`."),
          class = "smhi_unavailable_data"
        )
      }
    }
  }
  
  all_data <- list()
  
  for(station in station_key) {
    
    if (verbose) message(glue::glue("- Downloading `{station}` station info ({match(station, 
                                station_key)}/{length(station_key)}): "), appendLF = F)
    
    for(par in parameter_key) {
      
      url <- paste0(.metObsAPI, "/version/latest/parameter/",par,
                    "/station/",station,"/period/",period_name,"/data.csv")
      
      data <- readLines(url, warn = FALSE, encoding = "UTF-8")
      
      var <- data[5] %>% str_extract("^[^;]*")
      
      data_split <- strsplit(data, ";")
      
      cr <- lapply(data_split, function(x) any(str_detect(x, "Datum") == T)) %>% 
        unlist %>%
        which
      
      names_columns <- data_split[cr] %>% unlist()
      
      position <- which(str_detect(names_columns, "Kvalitet"))
      
      meteo_data <- data_split[c((cr+1):length(data_split))]
      ###############
      #meteo_data <- 
      #  meteo_data[1:max(
      #    which(lapply(meteo_data, function(x) x[1] %>% nchar) %>% 
      #            unlist != 0), na.rm = T)]
      
      non_empty_rows <- which(sapply(meteo_data, function(x) nchar(x[1])) != 0)
      if (length(non_empty_rows) == 0) {
        warning(glue::glue("No data returned for station `{station}`, parameter `{par}`"))
        next
      }
      meteo_data <- meteo_data[1:max(non_empty_rows)]
      ###############
      meteo_data <- as.data.frame(do.call(rbind, lapply(meteo_data, function(x) {x[1:position]})), 
                                  stringsAsFactors = FALSE)
      
      names(meteo_data) <- names_columns[1:position]
      
      if("Representativt dygn" %in% names_columns){
        meteo_data <-
          meteo_data %>% 
          dplyr::mutate(station_id = station) %>%
          dplyr::mutate(date =`Representativt dygn`) %>%
          dplyr::mutate(date = lubridate::parse_date_time(paste(date, "18:00:00"), orders = "ymd HMS")) %>%
          dplyr::mutate(parameter_value = as.numeric(.[[var]])) %>%
          dplyr::mutate(parameter_id = paste0("par_", par)) %>%
          dplyr::select(station_id, date, parameter_value, parameter_id, quality = Kvalitet) %>%
          dplyr::as_tibble()
      }else{
        meteo_data <-
          meteo_data %>%
          dplyr::mutate(station_id = station) %>%
          dplyr::mutate(date = lubridate::parse_date_time(paste(Datum, `Tid (UTC)`), orders = "ymd HMS")) %>%
          dplyr::mutate(parameter_value = as.numeric(.[[var]])) %>%
          dplyr::mutate(parameter_id = paste0("par_", par)) %>%
          dplyr::select(station_id, date, parameter_value, parameter_id, quality = Kvalitet) %>%
          dplyr::as_tibble()
      }
      
      all_data[[paste0(station, par, sep = "_")]] <- meteo_data
      
      par_title <-
        get_smhi_parameters() %>%
        dplyr::filter(parameter_id == par) %>%
        dplyr::pull(title)
      
      if (verbose) message(glue::glue("{par} \u2713 "), appendLF = F)    
    }
    
    if (verbose) message("--> Done!\n")
  }
  
  all_data <- dplyr::bind_rows(all_data)
  
  all_data <-
    all_data %>% 
    tidyr::pivot_wider(names_from = "parameter_id", 
                values_from = "parameter_value")
  
  return(all_data)
}
