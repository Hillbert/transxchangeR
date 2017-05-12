#' journey_list
#'
#' Create the journey list with each journey named by the JourneyPatternSection id.
#' @export

journey_list<-function(JourneyPatternSections){
  journeys<-list()
  for(j in JourneyPatternSections){
    d<-data.frame(sapply(j,function(x) x['RunTime'][[1]]),
                  sapply(j,function(x) x['From'][[1]]['StopPointRef'][[1]]),
                  sapply(j,function(x) x['To'][[1]]['StopPointRef'][[1]]),
                  stringsAsFactors = F)
    names(d)<-c("runtime","from","to")

    journeys[[j$.attrs]]<-d
  }
 #print("hello")
  return(journeys)
}
