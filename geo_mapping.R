geodata = st_as_sf(tripdata, coords = c(tripdata$start_lat, tripdata$start_lng))
geodata = st_set_crs(tripdata, crs = 4326)

#ggplot(geodata) + geom_sf(aes(color = cluster))
# TODO: Graph stations points and make heatmap

michigan <- map_data("state", region = "michigan")

ggplot(data = michigan, aes(x = tripdata$start_lng, y = tripdata$start_lat, group = group)) +
  geom_polygon(fill="lightgray", color = "white")+
  geom_point(data = df, aes(x = tripdata$start_lng, y = tripdata$start_lat, group = name), color="red")