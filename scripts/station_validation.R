# Find how many unique values we have
unique_start_names = unique(tripdata$start_station_id)
unique_end_names = unique(tripdata$end_station_id)

print(paste("unique starting station names:", length(unique_start_names) - 1))  # \ - 1 to account for empties 
print(paste("unique ending station names:", length(unique_end_names) - 1))      # /
rm(unique_start_names, unique_end_names)

print(paste("start station name num:", length(tripdata$start_station_name[!tripdata$start_station_name==""])))
print(paste("start station id num:", length(tripdata$start_station_id[!tripdata$start_station_id==""])))

###########################################

# We have some missing values
missing_data_start = tripdata[xor(tripdata$end_station_name == '', tripdata$end_station_id == ''), ]
print(missing_data_start)
# 0 rows, good

missing_data_start = tripdata[xor(tripdata$start_station_name == '', tripdata$start_station_id == ''), ]
print(missing_data_start)

tripdata[which(tripdata$start_station_id == "WL-008"), ] # Shows us that even just one id can match to multiple stations.
# Mismatched station names and IDs, lets try enforcing the data from the bike stations 


############################################

# Find all stations that map to multiple ids
mismatches = data.table::as.data.table(tripdata[tripdata$start_station_id != tripdata$start_station_name,]) # Ignores empties
mismatches = mismatches[, .N, by = c('start_station_name','start_station_id')]

# Find recurring IDS
n_occur = data.frame(table(mismatches$start_station_id))
n_occur[n_occur$Freq > 1,]
mismatches$Freq = n_occur$Freq

missing_stations = c()
# Station data is uncean.. 
tripdata[tripdata$start_station_id == "2059 Hastings Warehouse Station",]
x = 0
for (row_mismatch in 1:nrow(mismatches)) {
  mismatch_id = mismatches[row_mismatch, "start_station_id"]
  y = x
  
  for (row_station in 1:nrow(Divvy_Bicycle_Stations)) {
    if (Divvy_Bicycle_Stations[row_station, "ID"] == mismatch_id) {
      mismatches[row_mismatch, "start_station_id"] = Divvy_Bicycle_Stations[row_station, "Station.Name"]
      x = x + 1
      break
    }
  }
  
  if (y == x) {
    # Our station wasnt listed
    missing_stations = append(missing_stations, row_mismatch)
  }
}

mismatches[missing_stations, "start_station_name"]

