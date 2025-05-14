
#' Extract information from an SMHI XML documents
#'
#'
#' @param xml_document An XML document object, typically parsed using `xml2::read_xml()`.
#' @param namespace A named vector or list with the XML namespaces.
#' @param namespace_num The index for the desired namespace from the `namespace` vector.
#' @param children A character string with the name of the XML child element to extract.
#' 
#' @return A character vector with the extracted text values for the specified XML element.
#' 
#' @noRd
#' 
get_smhi_info <- function(xml_document, namespace, namespace_num, children){
  
  xml_find_all(
    xml_document, 
    paste0(".//",names(namespace)[namespace_num],":",children), 
    namespace) %>% 
    xml2::xml_text()
}