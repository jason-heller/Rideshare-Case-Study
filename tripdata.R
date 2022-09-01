# Init

install.packages("tidyverse")
install.packages("sf")
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(sf)

#####################################

#options(scipen=2)

# Remove duplicate ride_ids
tripdata = subset(tripdata, tripdata$ride_id != "ride_id")

ind = duplicated(tripdata[,1])
tripdata = tripdata[!ind,]

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
#stopifnot(!is.na(as.numeric(tripdata$end_lat)))    # \
#stopifnot(!is.na(as.numeric(tripdata$end_lng)))    # / Not all have end positions
print("Validated coordinates")

# Ensure stations id/names are unique
# TODO

# Determine ride length
tripdata$ride_length = difftime(tripdata$ended_at, tripdata$started_at)
tripdata$ride_length = as.numeric(tripdata$ride_length, units="secs")

# Then clean data, by removing any rides with ride lengths are positive
tripdata = subset(tripdata, ride_length > 0.0)

# Remove rides longer than a day, assuming trips spanning a time longer than this are erroneous
tripdata = subset(tripdata, ride_length < (60 * 60) * 24)

# Determine weekdays of trips
tripdata$day_of_trip = weekdays(tripdata$started_at)

# For use comparing rider data
casual_ride_data = tripdata[which(tripdata$member_casual == "casual"),]
member_ride_data = tripdata[which(tripdata$member_casual == "member"),]

# get a better sense of the data layout
mean(tripdata$ride_length)
max(tripdata$ride_length) / 60.0