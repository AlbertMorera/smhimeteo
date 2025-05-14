library(httr)
library(xml2)
library(rvest)
library(tidyverse)

metObsAPI <- "https://opendata-download-metobs.smhi.se/api"

##############################################################################

get_smhi_info <- function(xml_document, namespace, namespace_num, children){
  
  xml_find_all(
    xml_document, 
    paste0(".//",names(namespace)[namespace_num],":",children), 
    namespace) %>% 
    xml_text()
}

##############################################################################

get_smhi_parameters_id <- function(version = "latest") {
  
  url <- paste0(metObsAPI, "/version/",version,".xml")
  doc <- read_xml(url)
  ns <- xml_ns(doc)
  
  parameter_keys <- get_smhi_info(doc, ns, 1, "key")[-1]
  
  return(parameter_keys)
}

get_parameters_id()

##############################################################################

get_smhi_parameters <- function(version = "latest") {
  
  url <- paste0(metObsAPI, "/version/",version,".xml")
  doc <- read_xml(url)
  ns <- xml_ns(doc)
  
  parameters_info <-
    tibble(
      parameter_id = get_smhi_info(doc, ns, 1, "key")[-1],
      title = get_smhi_info(doc, ns, 1, "title")[-1],
      summary = get_smhi_info(doc, ns, 1, "summary")[-1],
      unit = get_smhi_info(doc, ns, 1, "unit")
    )
  
  
  return(parameters_info)
}

get_parameters() %>% print(n = Inf)

##############################################################################

get_smhi_station <- function(parameter_key, version = "latest") {
  
  url <- paste0(metObsAPI, "/version/",version,"/parameter/",parameter_key,".xml")
  doc <- read_xml(url)
  ns <- xml_ns(doc)

  stations_info <-
    tibble(
      station_id = get_smhi_info(doc, ns, 1, "id"),
      station_name = get_smhi_info(doc, ns, 1, "name"),
      longitude = get_smhi_info(doc, ns, 1, "longitude"),
      latitude = get_smhi_info(doc, ns, 1, "latitude"),
      height = get_smhi_info(doc, ns, 1, "height"),
      owner = get_smhi_info(doc, ns, 1, "owner"),
      ownerCategory = get_smhi_info(doc, ns, 1, "ownerCategory"),
      active = get_smhi_info(doc, ns, 1, "active"),
      from = get_smhi_info(doc, ns, 1, "from"),
      to = get_smhi_info(doc, ns, 1, "to"),
      updated = get_smhi_info(doc, ns, 2, "updated")[-1]
    )
  
  return(stations_info)
}

get_smhi_station("1")

##############################################################################

get_smhi_period <- function(parameter_key, station_key, version = "latest") {
  
  url <- paste0(metObsAPI, "/version/",version,"/parameter/",parameter_key,
                "/station/",station_key,".xml")
  doc <- read_xml(url)
  ns <- xml_ns(doc)
  
  period_names <- get_smhi_info(doc, ns, 1, "period")

  return(period_names)
}

get_smhi_period("1","159880")

##############################################################################

get_smhi_data <- function(parameter_key, station_key, period_name) {
  
  url <- paste0(metObsAPI, "/version/latest/parameter/",parameter_key,
                "/station/",station_key,"/period/",period_name,"/data.csv")
  
  data <- readLines(url, warn = FALSE, encoding = "UTF-8")
  
  var <- data[5] %>% str_extract("^[^;]*")
  
  data_split <- strsplit(data, ";")
  
  cr <- lapply(data_split, function(x) any(str_detect(x, "Datum") == T)) %>% 
    unlist %>%
    which
  
  names_columns <- data_split[cr] %>% unlist()
  
  position <- which(str_detect(names_columns, "Kvalitet"))
  
  meteo_data <- data_split[c((cr+1):length(data_split))]
  
  meteo_data <- meteo_data[1:max(which(lapply(meteo_data, 
                                              function(x) x[1] %>% nchar) %>% 
                                         unlist != 0))]
  
  meteo_data <- as.data.frame(do.call(rbind, meteo_data), 
                              stringsAsFactors = FALSE)[,1:position]
  
  names(meteo_data) <- names_columns[1:position]

  if("Representativt dygn" %in% names_columns){
    meteo_data <-
      meteo_data %>% 
      mutate(date =`Representativt dygn`) %>%
      mutate(date = lubridate::parse_date_time(paste(date, "18:00:00"), orders = "ymd HMS")) %>%
      select(date, all_of(var), Kvalitet) %>%
      mutate(!!var := as.numeric(.[[var]])) %>%
      as_tibble
  }else{
    meteo_data <-
      meteo_data %>% 
      mutate(date = lubridate::parse_date_time(paste(Datum, `Tid (UTC)`), orders = "ymd HMS")) %>%
      select(date, all_of(var), Kvalitet) %>%
      mutate(!!var := as.numeric(.[[var]])) %>%
      as_tibble
  }
  
  return(meteo_data)
}



##############################################################################

aggregate_meteo <- function(data, scale){
  
  var <- names(data)[2]
  
  data %>%
    mutate(day = as.Date(date)) %>%
    group_by(day) %>%
    summarise(
      !!var := mean(.data[[var]])
    )
}

##############################################################################

select_period <- function(data, start, end){
  data %>%
    filter(date >= lubridate::ydm_hms(paste(start, "00:00:00")) &
             date <= lubridate::ydm_hms(paste(end, "00:00:00")))
}

##############################################################################


period_name <- "latest-hour"
period_name <- "latest-day"
period_name <- "latest-months"
period_name <- "corrected-archive"

parameter_key <- "1"
station_key <- "159880"

get_smhi_period(parameter_key, station_key)
data <- get_smhi_data(parameter_key, station_key, period_name)

data %>% print(n = Inf)

data <- get_smhi_data(1, station_key, period_name)

select_period(data, start = "2020-01-01", end = "2020-02-02")

aggregate_meteo(data) %>% print(n = Inf)







