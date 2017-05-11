#' services
#'
#' Requires XML
#'
#' @export


services_lookup<-function(Services){
  s<-Services$Service$StandardService
  #return the lookup between the journey refs
  
  #there might be more than one JourneyPatternRef
  
# if(length(s[[3]]))
#   d<-data.frame(sapply(s[-c(1,2)],function(x) x$JourneyPattern),
#   sapply(s[-c(1,2)],function(x) x$.attrs),stringsAsFactors=F)
# names(d)<-c("JourneyPatternRef","JourneyPatternId")

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
