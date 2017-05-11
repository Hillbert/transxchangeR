#' A basic function to load an xml file and split it into
#' the component lists
#' Requires XML
#' @export

#library(XML)
transxchange_xml_load<-function(file_name){
xml_data <- XML::xmlToList(file_name)
for(j in 1:length(xml_data)){

  assign(names(xml_data)[j],xml_data[[j]],envir = .GlobalEnv)
}

}
