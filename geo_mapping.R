geodata = st_as_sf(tripdata, coords = c(tripdata$start_lat, tripdata$start_lng))
geodata = st_set_crs(tripdata, crs = 4326)

ggplot(geodata) + geom_sf(aes(color = cluster))
# TODO: Graph stations points