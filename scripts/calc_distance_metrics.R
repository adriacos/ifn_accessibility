source("scripts/read_data.R")
source("scripts/save_data.R")
source("scripts/get_municipalities.R")




min_max_norm <- function(x, min_x, max_x) {
  ifelse(!is.na(x), (x - min_x) / (max_x - min_x), NA)
}

calc_distance_metrics <- function(){
  
  parcels <- get_parcels_done()
  print(parcels)
  population <- get_population()[2:947]
  
  accessibilities <- data.frame(matrix(ncol = 4, nrow = 0))
  #accessibilities <- data.frame(matrix(ncol = 5, nrow = 0))
  x <- c("parcel_id","coords_long", "coords_lat", "driving")
  #x <- c("parcel_id", "accessibility_d", "accessibility_t", "accessibility_b", "accessibility_w")
  colnames(accessibilities) <- x
  
  
  max_d <- NA
  #max_t <- NA
  #max_b <- NA
  #max_w <- NA
  
  min_d <- NA
  #min_t <- NA
  #min_b <- NA
  #min_w <- NA
  
  #min_tot <- NA
  #max_tot <- NA
  
  
  for(parcel_id in parcels$plot_id){
    data_p <- read_distances_data_by_parcel(parcel_id)
    max_d_p <- max(data_p$driving, na.rm=T)
    #max_t_p <- max(data_p$transit, na.rm=T)
    #max_b_p <- max(data_p$bicycling, na.rm=T)
    #max_w_p <- max(data_p$walking, na.rm=T)
    
    min_d_p <- min(data_p$driving, na.rm=T)
    #min_t_p <- min(data_p$transit, na.rm=T)
    #min_b_p <- min(data_p$bicycling, na.rm=T)
    #min_w_p <- min(data_p$walking, na.rm=T)
    
    if(is.na(max_d) || max_d_p>max_d){
      max_d <- max_d_p
    }
    #if(is.na(max_tot) || max_t_p>max_tot){
    #  max_t <- max_t_p
    #}
    #if(is.na(max_tot) || max_b_p>max_tot){
    #  max_tot <- max_b_p
    #}
    #if(is.na(max_tot) || max_w_p>max_tot){
    #  max_tot <- max_w_p
    #}
    #
    if(is.na(min_d) || min_d_p<min_d){
      min_d <- min_d_p
    }
    #if(is.na(min_tot) || min_t_p<min_tot){
    #  min_tot <- min_t_p
    #}
    #if(is.na(min_tot) || min_b_p<min_tot){
    #  min_tot <- min_b_p
    #}
    #if(is.na(min_tot) || min_w_p<min_tot){
    #  min_tot <- min_w_p
    #}
  }
  #max_inv_tot <- 1/(log(min_tot+1,10))
  #min_inv_tot <- 1/(log(max_tot+1,10))
  
  for(parcels_row in 1:nrow(parcels)){
    print(parcel)
    data_p <- read_distances_data_by_parcel(parcels[parcels_row,]$plot_id)
    data_p$hab <- population
    
    log_d <- log(data_p$driving+1,10)
    inv_d <- 1/log_d
    min_inv_d <- 1/(log(max_d+1,10))
    max_inv_d <- 1/(log(min_d+1,10))
    norm_d <- min_max_norm(inv_d, min_inv_d, max_inv_d)
    data_p$accessibility_d <- norm_d*data_p$hab
    
    #log_t <- log(data_p$transit+1,10)
    #inv_t <- 1/log_t
    #min_inv_t <- 1/(log(max_t+1,10))
    #max_inv_t <- 1/(log(min_t+1,10))
    #norm_t <- min_max_norm(inv_t, min_inv_tot, max_inv_tot)
    #data_p$accessibility_t <- norm_t*data_p$hab
    
    #log_b <- log(data_p$bicycling+1,10)
    #inv_b <- 1/log_b
    #min_inv_b <- 1/(log(max_b+1,10))
    #max_inv_b <- 1/(log(min_b+1,10))
    #norm_b <- min_max_norm(inv_b, min_inv_tot, max_inv_tot)
    #data_p$accessibility_b <- norm_b*data_p$hab
    
    #log_w <- log(data_p$walking+1,10)
    #inv_w <- 1/log_w
    #min_inv_w <- 1/(log(max_w+1,10))
    #max_inv_w <- 1/(log(min_w+1,10))
    #norm_w <- min_max_norm(inv_w, min_inv_tot, max_inv_tot)
    #data_p$accessibility_w <- norm_w*data_p$hab
      
    accessibilities[nrow(accessibilities)+1,] <- c(parcels[parcels_row,]$plot_id
                                                   , parcels[parcels_row,]$coords_longitude
                                                   , parcels[parcels_row,]$coords_latitude
                             , sum(data_p$accessibility_d, na.rm=T))
    #, sum(data_p$accessibility_t, na.rm=T)
    #, sum(data_p$accessibility_b, na.rm=T)
    #, sum(data_p$accessibility_w, na.rm=T))
  }
  
  accessibilities
  
}



standardise <- function(x){
  x <- (x - mean(x)) / sd(x)
  x
}
