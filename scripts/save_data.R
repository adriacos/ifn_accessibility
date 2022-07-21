

save_by_parcel <- function(parcel, durations_data){
  write.csv(durations_data, paste("data/distances_by_parcel/distances_by_parcel_", parcel$plot_id, ".csv", sep = ""))
}