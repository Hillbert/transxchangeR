#' Services
#'
#' Creates a dataframe to match JourneyPatternRef with JourneyPatternid
#' For some files there are multiple JourneyPatternRefs for each JourneyPatternid so they are iterated over
#'
#' @export


services_lookup<-function(Services){
  s<-Services$Service$StandardService
  #return the lookup between the journey refs

  #there might be more than one JourneyPatternRef
  #so each JourneyPatternRef in the service is iterated through


  d<-c()
  for(j in 3:length(s)){
    id<-s[[j]]$.attrs
    da<-unlist(s[[j]])
    da<-da[grepl("Journey",names(da))]
    d<-rbind(d,data.frame(da,rep(id,length(da))),stringsAsFactors=F)
  }
  names(d)<-c("JourneyPatternRef","JourneyPatternId")
  d<-data.frame(d,stringsAsFactors = F)



  return(d)
}
