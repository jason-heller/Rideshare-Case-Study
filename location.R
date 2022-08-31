ggplot() +
  geom_point(data = casual_ride_data, aes(x = start_lat, y = start_lng, color = "red")) +
  geom_point(data = member_ride_data, aes(x = start_lat, y = start_lng, color = "blue"))