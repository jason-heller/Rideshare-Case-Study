stopifnot(exists("casual_ride_data"))
stopifnot(exists("member_ride_data"))
stopifnot(exists("Divvy_Bicycle_Stations"))

#install.packages("tibble")
library(tibble)

illinois = map_data('county') %>% 
  filter(region == 'illinois', subregion == 'cook') %>% 
  select(lon = long, lat, group, id = subregion)

# Map bike stations
active_bike_stations = subset(Divvy_Bicycle_Stations, Divvy_Bicycle_Stations$Status == "In Service")

ggplot(illinois, aes(lon, lat)) + 
  geom_polygon(size = .25, show.legend = FALSE, fill = "#999999") +
  coord_quickmap() +
  geom_point(data = active_bike_stations, aes(x = Longitude, y = Latitude), color = "#00FF00", size = .25) +
  xlim(-88.35, -87.35) +
  ylim(41.35, 42.25) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Active Docking Stations")

# Map start/stop points
ggplot(illinois, aes(lon, lat)) + 
  geom_polygon(size = .25, show.legend = FALSE, fill = "#999999") +
  coord_quickmap() +
  geom_point(data = casual_ride_data, aes(x = start_lng, y = start_lat), color = "#FF0000", size = .25) +
  geom_point(data = casual_ride_data, aes(x = end_lng, y = end_lat), color = "#FF0000", size = .25) +
  xlim(-88.35, -87.35) +
  ylim(41.35, 42.25) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Casual Rider Start/End Locations")

ggplot(illinois, aes(lon, lat)) + 
  geom_polygon(size = .25, show.legend = FALSE, fill = "#999999") +
  coord_quickmap() +
  geom_point(data = member_ride_data, aes(x = start_lng, y = start_lat), color = "#0000FF", size = .25) +
  geom_point(data = member_ride_data, aes(x = end_lng, y = end_lat), color = "#0000FF", size = .25) +
  xlim(-88.35, -87.35) +
  ylim(41.35, 42.25) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Membership Rider Start/End Locations")