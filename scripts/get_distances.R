source("scripts/get_duration.R")

source("scripts/read_data.R")
source("scripts/save_data.R")

source("scripts/get_municipalities.R")

municipalities <- get_municipalities()

get_save_distance_all_parcels <- function(){
  parcels <- get_parcels_not_done()
  for(rown in 1:nrow(parcels)){
    print(parcels[rown,])
    get_save_distance_by_parcel_all_municipalities(parcels[rown,])
  }
}

get_save_distance_by_parcel_all_municipalities <- function(parcel){
  municipalities_split <- split(municipalities, ceiling(seq_along(municipalities)/20))
  data <- do.call(rbind,lapply(municipalities_split, get_distance_by_parcel_25_municipalities, parcel=parcel))
  #data
  colnames(data)[1] <- "municipality"
  save_by_parcel(parcel, data)
}


get_distance_by_parcel_25_municipalities <- function(tfmunicipalities, parcel){
  durations <- get_duration(parcel$coords_longitude, parcel$coords_latitude, tfmunicipalities)
  print("test")
  durations
}

get_distance_by_municipality <- function(municipality, parcel){
  get_duration(parcel$coords_longitude, parcel$coords_latitude, municipality)
}