stopifnot(exists("casual_ride_data"))
stopifnot(exists("member_ride_data"))

# Create another graph of total riders per day
# TODO: Readability
riders_per_weekday = c(
  nrow(casual_ride_data[which(casual_ride_data$day_of_trip == "Sunday"   ),]),
  nrow(member_ride_data[which(member_ride_data$day_of_trip == "Sunday"   ),]),
  
  nrow(casual_ride_data[which(casual_ride_data$day_of_trip == "Monday"   ),]),
  nrow(member_ride_data[which(member_ride_data$day_of_trip == "Monday"   ),]),
  
  nrow(casual_ride_data[which(casual_ride_data$day_of_trip == "Tuesday"  ),]),
  nrow(member_ride_data[which(member_ride_data$day_of_trip == "Tuesday"  ),]),
  
  nrow(casual_ride_data[which(casual_ride_data$day_of_trip == "Wednesday"),]),
  nrow(member_ride_data[which(member_ride_data$day_of_trip == "Wednesday"),]),
  
  nrow(casual_ride_data[which(casual_ride_data$day_of_trip == "Thursday" ),]),
  nrow(member_ride_data[which(member_ride_data$day_of_trip == "Thursday" ),]),
  
  nrow(casual_ride_data[which(casual_ride_data$day_of_trip == "Friday"   ),]),
  nrow(member_ride_data[which(member_ride_data$day_of_trip == "Friday"   ),]),
  
  nrow(casual_ride_data[which(casual_ride_data$day_of_trip == "Saturday" ),]),
  nrow(member_ride_data[which(member_ride_data$day_of_trip == "Saturday" ),])
)

weekday = c(rep("Sunday", 2), rep("Monday", 2), rep("Tueday", 2), rep("Wednesday", 2), rep("Thursday", 2), rep("Friday", 2), rep("Saturday", 2))
membership = rep(c("Casual", "Member"), 7)
plot_df = data.frame(weekday, membership, riders_per_weekday)

ggplot(plot_df, aes(x = weekday, y = riders_per_weekday, fill = membership)) + 
  geom_col(position = "dodge") +
  scale_x_discrete(limits = unique(weekday)) +
  #ggtitle("Total Riders per Weekday") +
  labs(x = "Weekday", y = "Total Riders", fill = "Membership") +
  scale_fill_manual("Legend", values = c("Casual" = "#eb3b3b", "Member" = "#0d9ee0"))

# Get the total riders per month

total_per_month = c()
for (x in 1:12) {
  monthly_avg = nrow(casual_ride_data[which(as.integer(format(casual_ride_data$started_at, format = "%m")) == x),])
  total_per_month = append(total_per_month, monthly_avg)
  
  monthly_avg = nrow(member_ride_data[which(as.integer(format(member_ride_data$started_at, format = "%m")) == x),])
  total_per_month = append(total_per_month, monthly_avg)
}

months = rep(1:12, each=2)
membership = rep(c("Casual", "Member"), 12)
plot_df = data.frame(months, membership, total_per_month)

ggplot(plot_df, aes(x = months, y = total_per_month, fill = membership)) + 
  geom_line(aes(color = membership), position = "dodge", size = 2) +
  scale_x_discrete(limits = unique(months)) +
  #ggtitle("Total Riders per Month") +
  labs(x = "Month", y = "Number of Riders", fill = "Membership") +
  scale_fill_manual("Legend", values = c("Casual" = "#eb3b3b", "Member" = "#0d9ee0"))