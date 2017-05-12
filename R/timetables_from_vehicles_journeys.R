#' timetable
#'
#' Construct full timings
#'
#' The timetable constructed is a list of lists with the first three elements containing information
#' and the fourth element containing the actual timetable
#'
#' @export



timetable<-function(vehicles,journeys,services){
l<-list()

for(j in 1:length(vehicles[,1])){
  l1<-list()
  journey_id<-vehicles$journey_id[j]

  #if the journey_id is not in the identifiers for the journeys then
  #map using the services

  #map
  map<-setNames(services$JourneyPatternRef,services$JourneyPatternId)
  journey_id<-map[journey_id]
  journey<-journeys[[journey_id]]
  journey<-journey[!is.na(journey[,1]),]
  dep_time<-strptime(vehicles$dep_time[j],"%T")
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


runtime_seconds<-function(x){
  x<-gsub('PT','',x)
  x<-gsub('S','',x)
  x<-as.numeric(sapply(x,function(x) ifelse(grepl('M',x),gsub('M','.',x),paste0('.',x))))
  x<-floor(x)*60 + 100*(x%%1)
  return(x)
}
