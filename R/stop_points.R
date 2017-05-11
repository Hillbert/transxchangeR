#' A basic function to convert the stop point list to a data frame
#' @export
#'

#library(XML)
stop_point_load<-function(StopPoints){
  stop_points<-data.frame(sapply(StopPoints,function(x) x$StopPointRef),
                          sapply(StopPoints,function(x) x$CommonName),
                          sapply(StopPoints,function(x) x$LocalityName)
                          ,stringsAsFactors=F)
#  names(stop_points)<-names(StopPoints[[1]])
  names(stop_points)<-c("StopPointRef","CommonName","LocalityName")
   return(stop_points)
}
