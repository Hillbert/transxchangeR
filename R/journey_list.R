#' create the journey list
#' @export

journey_list<-function(JourneyPatternSections){
  journeys<-list()
  for(j in JourneyPatternSections){
    d<-data.frame(sapply(j,function(x) x['RunTime'][[1]]),
                  sapply(j,function(x) x['From'][[1]]['StopPointRef'][[1]]),
                  sapply(j,function(x) x['To'][[1]]['StopPointRef'][[1]]),
                  stringsAsFactors = F)
    names(d)<-c("runtime","from","to")
    
    #ignore this section!
    #j$.attrs has the back bit flipped ?
    #so if the final character is a letter then change it
    
    # ids<-unlist(strsplit(j$.attr,'-'))
    # if(length(ids)>1){
    # l<-length(ids)
    # if(grepl("[[:alpha:]]",ids[l])){
    # #if (is.na(as.numeric(ids[l]))){
    #   ids<-ids[c(1:(l-2),l,l-1)]
    # }
    # ids<-paste(ids,collapse='-')
    # }else{ids<-j$.attr}
    # journeys[[ids]]<-d
    
    journeys[[j$.attrs]]<-d
  }
 #print("hello")
  return(journeys)
}
