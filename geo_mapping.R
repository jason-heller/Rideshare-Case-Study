my_sf <- st_as_sf(my_df, coords = c('LON', 'LAT')

my_sf <- st_set_crs(my_sf, crs = 4326)

#Plot it:

ggplot(my_sf) + 
  geom_sf(aes(color = cluster))
