install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

#####################################

# Validate ride_id
tripdata = subset(tripdata, ride_id != 'ride_id') # Remove some rows I left in from excel

# Validate bike types
test_ride_types = unique(tripdata$rideable_type)
allowable_types = c("electric_bike", "classic_bike", "docked_bike")
stopifnot(test_ride_types %in% allowable_types)
print("Validated rideable types")
rm(test_ride_types)
rm(allowable_types)

# Validate types of subscriptions
test_subscriptions = unique(tripdata$member_casual)
allowable_types = c("member", "casual")
stopifnot(test_subscriptions %in% allowable_types)
print("Validated membership types")
rm(test_subscriptions)
rm(allowable_types)

# Make dates all the same format
tripdata$started_at = parse_date_time(tripdata$started_at, c("m/d/Y HM", "Y-m-d HMS "))
tripdata$ended_at   = parse_date_time(tripdata$ended_at, c("m/d/Y HM", "Y-m-d HMS "))

# Ensure lat/long are doubles
tripdata$start_lat  = as.double(tripdata$start_lat)
tripdata$start_lng  = as.double(tripdata$start_lng)
tripdata$end_lat    = as.double(tripdata$end_lat)
tripdata$end_lng    = as.double(tripdata$end_lng)

# Validate datetimes
stopifnot(!is.na(as_datetime(tripdata$started_at)))
stopifnot(!is.na(as_datetime(tripdata$ended_at)))
print("Validated ride times")

# Validate lat/long
stopifnot(!is.na(as.numeric(tripdata$start_lat)))
stopifnot(!is.na(as.numeric(tripdata$start_lng)))
#stopifnot(!is.na(as.numeric(tripdata$end_lat)))
#stopifnot(!is.na(as.numeric(tripdata$end_lng)))
print("Validated coordinates")

# Determine ride length
tripdata$ride_length = difftime(tripdata$ended_at, tripdata$started_at)
tripdata$ride_length = as.numeric(tripdata$ride_length, units="secs")

# Then clean data, by removing any rides with ride lengths are positive
tripdata = subset(tripdata, ride_length > 0.0)
# Remove rides longer than three days
tripdata = subset(tripdata, ride_length < ((60.0 * 60.0) * (24.0 * 3.0)))

# Determine weekdays of trips
tripdata$day_of_trip = weekdays(tripdata$started_at)

# no_end_loc = subset(tripdata, is.na(tripdata$end_lng))

casual_ride_data = tripdata[which(tripdata$member_casual=="casual"),]#$ride_length
member_ride_data = tripdata[which(tripdata$member_casual=="member"),]#$ride_length

# TODO: Readability
avg_per_weekday = c(
  mean(casual_ride_data[which(casual_ride_data$day_of_trip == "Sunday"   ),]$ride_length) / 60.0,
  mean(member_ride_data[which(member_ride_data$day_of_trip == "Sunday"   ),]$ride_length) / 60.0,
  
  mean(casual_ride_data[which(casual_ride_data$day_of_trip == "Monday"   ),]$ride_length) / 60.0,
  mean(member_ride_data[which(member_ride_data$day_of_trip == "Monday"   ),]$ride_length) / 60.0,
  
  mean(casual_ride_data[which(casual_ride_data$day_of_trip == "Tuesday"  ),]$ride_length) / 60.0,
  mean(member_ride_data[which(member_ride_data$day_of_trip == "Tuesday"  ),]$ride_length) / 60.0,
  
  mean(casual_ride_data[which(casual_ride_data$day_of_trip == "Wednesday"),]$ride_length) / 60.0,
  mean(member_ride_data[which(member_ride_data$day_of_trip == "Wednesday"),]$ride_length) / 60.0,
  
  mean(casual_ride_data[which(casual_ride_data$day_of_trip == "Thursday" ),]$ride_length) / 60.0,
  mean(member_ride_data[which(member_ride_data$day_of_trip == "Thursday" ),]$ride_length) / 60.0,
  
  mean(casual_ride_data[which(casual_ride_data$day_of_trip == "Friday"   ),]$ride_length) / 60.0,
  mean(member_ride_data[which(member_ride_data$day_of_trip == "Friday"   ),]$ride_length) / 60.0,
  
  mean(casual_ride_data[which(casual_ride_data$day_of_trip == "Saturday" ),]$ride_length) / 60.0,
  mean(member_ride_data[which(member_ride_data$day_of_trip == "Saturday" ),]$ride_length) / 60.0
)

weekday = c(rep("Sun", 2), rep("Mon", 2), rep("Tue", 2), rep("Wed", 2), rep("Thu", 2), rep("Fri", 2), rep("Sat", 2))
membership = rep(c("Casual", "Member"), 7)
plot_df = data.frame(weekday, membership, avg_per_weekday)

ggplot(plot_df, aes(x = weekday, y = avg_per_weekday, fill = membership)) + 
  geom_col(position="dodge") +
  scale_x_discrete(limits = unique(weekday)) +
  ggtitle("Average Ride Length per Weekday") +
  labs(x = "Weekday", y = "Avg. Time in Minutes", fill = "Membership")

rm(casual_ride_data)
rm(member_ride_data)

