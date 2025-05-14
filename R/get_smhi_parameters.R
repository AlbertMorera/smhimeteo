#' Get SMHI Parameters Information
#'
#' This function retrieves information about the available parameters from the SMHI (Swedish Meteorological and Hydrological Institute) API.
#'
#' @param version A character string specifying the version of the SMHI API to use (default is "latest"). 
#' Chech `https://opendata-download-metobs.smhi.se/metobs/introduction` for available versions.
#' @return A tibble containing the following columns:
#'   \item{parameter_id}{The ID of the parameter.}
#'   \item{title}{The title or name of the parameter.}
#'   \item{summary}{A brief summary or description of the parameter.}
#'   \item{unit}{The unit of measurement for the parameter.}
#' @examples
#' get_smhi_parameters()
#' @export

get_smhi_parameters <- function(version = "latest") {
  
  url <- paste0(.metObsAPI, "/version/",version,".xml")
  doc <- xml2::read_xml(url)
  ns <- xml2::xml_ns(doc)
  
  parameters_info <-
    dplyr::tibble(
      parameter_id = get_smhi_info(doc, ns, 1, "key")[-1],
      title = get_smhi_info(doc, ns, 1, "title")[-1],
      summary = get_smhi_info(doc, ns, 1, "summary")[-1],
      unit = get_smhi_info(doc, ns, 1, "unit")
    )
  
  return(parameters_info)
}
