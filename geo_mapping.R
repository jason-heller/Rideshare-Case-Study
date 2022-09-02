#lat_min = min(tripdata$start_lat)
#lat_max = max(tripdata$start_lat)
#lng_min = min(tripdata$start_lng)
#lng_max = max(tripdata$start_lng)
states = map_data("state", region = "illinois")

ggplot(states, aes(long, lat)) +
geom_polygon(aes(group = group)) +
coord_map("albers",  lat0 = 45.75, lat1 = 41.64) +
  geom_point(data = casual_ride_data, aes(x = 43.234, y = -80.42))
#geom_point(data = casual_ride_data, aes(x = start_lng, y = start_lat)) + 
#geom_point(data = member_ride_data, aes(x = start_lng, y = start_lat), color = "blue")