#' Construct full timings
#'change
#' Requires XML
#'
#' @export

runtime_seconds<-function(x){
  x<-gsub('PT','',x)
  x<-gsub('S','',x)
  #if(grepl('M',x)){
  #  x<-gsub('M','.',x)
  #}else{
  #  x<-paste0('.',x)
  #}

  x<-as.numeric(sapply(x,function(x) ifelse(grepl('M',x),gsub('M','.',x),paste0('.',x))))
  x<-floor(x)*60 + 100*(x%%1)
return(x)
  }

timetable<-function(vehicles,journeys,services){
l<-list()

for(j in 1:length(vehicles[,1])){
  l1<-list()
  journey_id<-vehicles$journey_id[j]
  #insert s, might need to do more

  #if the journey_id is not in the identifiers for the journeys then
  #map using the services

  #if(!gsub('JP_','JPS_',journey_id)%in%names(journeys))

  #map
  map<-setNames(services$JourneyPatternRef,services$JourneyPatternId)
#  journey_id<-gsub('JP_','JPS_',journey_id)
  journey_id<-map[journey_id]
   journey<-journeys[[journey_id]]
  journey<-journey[!is.na(journey[,1]),]
  #journey$runtime<-gsub('PT','',journey$runtime)

  #split it by minute

  #journey$runtime<-gsub('S','',journey$runtime)
  #journey$runtime<-as.numeric(gsub('M','',journey$runtime))
  dep_time<-strptime(vehicles$dep_time[j],"%T")
  #stop_times<-dep_time+cumsum(journey$runtime)*60
  stop_times<-dep_time+cumsum(runtime_seconds(journey$runtime))
  times<-strftime(c(dep_time,stop_times),"%T")
  stops<-c(journey$from,tail(journey$to,1))
  d<-data.frame(stops,times)
  l1[[1]]<-journey_id
  l1[[2]]<-vehicles$op_days[j]
  l1[[3]]<-vehicles$nonop_days[j]
  l1[[4]]<-d
  l[[j]]<-l1
}
  return(l)
}
