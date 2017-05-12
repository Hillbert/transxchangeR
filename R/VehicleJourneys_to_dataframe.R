#' VehicleJourneys_to_dataframe
#'
#' A basic function to get departure time and operating times for each vehicle journey
#'
#' Requires XML
#'
#' @export


VehicleJourneys_to_dataframe<-function(VehicleJourneys){
vehicles<-c()
for(v in VehicleJourneys){
  dep_time=v$DepartureTime
  journey_id=v$JourneyPatternRef
  private_code<-v$PrivateCode
  journey_ref<-v$VehicleJourneyRef
  regular_operation<-names(v$OperatingProfile$RegularDayType$DaysOfWeek)
  non_operation<-paste(names(v$OperatingProfile$BankHolidayOperation$DaysOfNonOperation),collapse='|')

  if(is.null(dep_time)){dep_time<-NA}
  if(is.null(journey_id)){journey_id<-NA}
  if(is.null(private_code)){private_code<-NA}
  if(is.null(regular_operation)){regular_operation<-NA}
  if(is.null(non_operation)){non_operation<-NA}
  if(is.null(journey_ref)){journey_ref<-NA}


  #journey_ref<-v$JourneyPatternSectionRefs
  vehicles<-rbind(vehicles,c(dep_time,journey_id,journey_ref,private_code,regular_operation,non_operation))
#if(length(vehicles[,1])==2){break()}
}
vehicles<-data.frame(vehicles,stringsAsFactors=F,header=F)[,1:6]
names(vehicles)<-c("dep_time","journey_id","journey_ref","private_code","op_days","nonop_days")


#check that the ids are correct
v1<-vehicles[!is.na(vehicles$journey_id),]
v2<-vehicles[is.na(vehicles$journey_id),]
if(length(v2[,1])>0){
  map<-setNames(v1$journey_id,v1$private_code)
  vehicles$journey_id[is.na(vehicles$journey_id)]<-map[v2$journey_ref]
}

vehicles<-vehicles[,c("dep_time","journey_id","op_days","nonop_days")]


return(vehicles)
}
