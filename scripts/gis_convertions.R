
rasterise_accessibility_data <- function(accessibility_data){
  
  accessibility_data$coords_long <- as.numeric(accessibility_data$coords_long)
  accessibility_data$coords_lat <- as.numeric(accessibility_data$coords_lat)
  accessibility_data$driving <- as.numeric(accessibility_data$driving)
  
  xy <- accessibility_data[,c("coords_long","coords_lat")]
  
  #using lat long
  coords <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
  spdf <- SpatialPointsDataFrame(coords = xy, data = accessibility_data,
                                 proj4string = coords)
  #using ETRS89 / UTM zone 31N
  coords <- CRS("+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs")
  spdf <- spTransform(spdf, coords)
  
  rast <- raster("test/raster_cat.tif")
  
  rast_p <- rasterize(spdf, rast, spdf$driving)
  
  plot(rast_p)
  
  rast_p
}