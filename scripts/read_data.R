
read_distances_data_by_parcel <- function(parcel_id){
  data <- read.csv(paste("data/distances_by_parcel/distances_by_parcel_", parcel_id, ".csv", sep=""))
  data
}

get_parcels_not_done <- function(){
  print("readData")
    data_ifn4 <- readIFN4() 
    parcels_ids_done <- get_parcels_ids_done()
    data_ifn4 <- data_ifn4[!(data_ifn4$plot_id %in% parcels_ids_done),]
    data_ifn4
}

get_parcels_done <- function(){
  ids <- get_parcels_ids_done()
  data_ifn4 <- readIFN4() 
  data_ifn4 <- data_ifn4[data_ifn4$plot_id %in% ids,]
  data_ifn4  
}

get_parcels_ids_done <- function(){
  files <- list.files("data/distances_by_parcel")
  library(stringr)
  parcels_done <- str_replace(files, "distances_by_parcel_", "")
  parcels_done <- str_replace(parcels_done, ".csv", "")
  parcels_done
}


readIFN4 <- function(){
  print("readIFN4")
  ifn4 <- read.csv("./data/20220330_nfi_data.csv")[ 
    ,c('plot_id',
       #'coords_utm_x_ETRS89',
       #'coords_utm_y_ETRS89')]
       'coords_longitude', 
       'coords_latitude')]
  ifn4
}
