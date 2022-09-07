stopifnot(exists("casual_ride_data"))
stopifnot(exists("member_ride_data"))

illinois = map_data('county') %>% 
  filter(region == 'illinois', subregion == 'cook') %>% 
  select(lon = long, lat, group, id = subregion)

ggplot(illinois, aes(lon, lat)) + 
  geom_polygon(size = .25, show.legend = FALSE) +
  coord_quickmap() +
  geom_point(data = casual_ride_data, aes(x = start_lng, y = start_lat), color = "#FF0000", size = .75) +
  geom_point(data = casual_ride_data, aes(x = end_lng, y = end_lat), color = "#CC0000", size = .75) +
  xlim(-88.35, -87.35) +
  ylim(41.35, 42.25) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Casual Rider Start/End Locations")

ggplot(illinois, aes(lon, lat)) + 
  geom_polygon(size = .25, show.legend = FALSE) +
  coord_quickmap() +
  geom_point(data = member_ride_data, aes(x = start_lng, y = start_lat), color = "#0000FF", size = .75) +
  geom_point(data = member_ride_data, aes(x = end_lng, y = end_lat), color = "#0000CC", size = .75) +
  xlim(-88.35, -87.35) +
  ylim(41.35, 42.25) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Membership Rider Start/End Locations")