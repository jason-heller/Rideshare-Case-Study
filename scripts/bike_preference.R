stopifnot(exists("casual_ride_data"))
stopifnot(exists("member_ride_data"))
# Create a bar graph comparing bike types between rider typers

types = c("classic_bike", "electric_bike", "docked_bike")

bike_pref_casual = c(
  nrow(casual_ride_data[which(casual_ride_data$rideable_type == "classic_bike"),]),
  nrow(casual_ride_data[which(casual_ride_data$rideable_type == "electric_bike"),]),
  nrow(casual_ride_data[which(casual_ride_data$rideable_type == "docked_bike"),])
)

pref_sum = sum(bike_pref_casual)
for(x in 0:2) {
  bike_pref_casual[x] = bike_pref_casual[x] / pref_sum
}

bike_pref_member = c(
  nrow(member_ride_data[which(member_ride_data$rideable_type == "classic_bike"),]),
  nrow(member_ride_data[which(member_ride_data$rideable_type == "electric_bike"),]),
  nrow(member_ride_data[which(member_ride_data$rideable_type == "docked_bike"),])
)

pref_sum = sum(bike_pref_member)
for(x in 0:2) {
  bike_pref_member[x] = bike_pref_member[x] / pref_sum
}

rm(pref_sum)

pie_chart = data.frame(
  group = bike_pref_casual,
  value = types
)

# Casual rider preference
ggplot(pie_chart, aes(x = "", y = bike_pref_casual, fill = types)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  ggtitle("Preference of Bike Types Among Casual Riders") +
  labs(fill = "Bike Type")

pie_chart = data.frame(
  group = bike_pref_member,
  value = types
)

# Member rider preference
ggplot(pie_chart, aes(x = "", y = bike_pref_member, fill = types)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  ggtitle("Preference of Bike Types Among Membership Riders") +
  labs(fill = "Bike Type")