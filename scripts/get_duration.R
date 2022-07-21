if (!require(mapsapi)){
  install.packages("mapsapi")
}
library(mapsapi)

key = "AIzaSyAFPzNPjQ0acMkB8M0W6_z5qrpFqJwSj44"

modes = c("transit", "driving", "walking", "bicycling")

dates <- c(as.POSIXct(strptime("2022-07-23 08:00:00", "%Y-%m-%d %H:%M:%S")),
           as.POSIXct(strptime("2022-07-20 08:00:00", "%Y-%m-%d %H:%M:%S")),
           as.POSIXct(strptime("2022-07-23 12:00:00", "%Y-%m-%d %H:%M:%S")),
           as.POSIXct(strptime("2022-07-20 12:00:00", "%Y-%m-%d %H:%M:%S")),
           as.POSIXct(strptime("2022-07-23 16:00:00", "%Y-%m-%d %H:%M:%S")),
           as.POSIXct(strptime("2022-07-20 16:00:00", "%Y-%m-%d %H:%M:%S")),
           as.POSIXct(strptime("2022-07-23 20:00:00", "%Y-%m-%d %H:%M:%S")),
           as.POSIXct(strptime("2022-07-20 20:00:00", "%Y-%m-%d %H:%M:%S")))


get_duration <- function(parcel_long, parcel_lat, origins){
  destination = c(parcel_long, parcel_lat)
  durations <- sapply(modes, get_by_mode, origins=origins, destination=destination, key=key, dates=dates)
  row.names(durations) <- origins
  durations
}

get_by_mode <- function(mode, origins, destination, key, dates){
  apply(simplify2array(lapply(dates, get_duration_s, origins=origins, destination=destination, key=key, mode=mode)),1:2,mean, na.rm=TRUE)
}


get_duration_s <- function(departure_time, origins, destination, key, mode){
  doc = mp_matrix(
    origins = origins,
    destinations = destination,
    key = key,
    quiet = TRUE,
    mode = mode,
    departure_time=departure_time
  )
  #print(paste(departure_time,origins,destination,mode))
  print(doc)
  mp_get_matrix(doc, value = "duration_s")
  #print(test)
}


#destination=c(1.5590642, 41.8513986)
#origins = c("Girona", "Barcelona","Tarragona", "Serinyà")





