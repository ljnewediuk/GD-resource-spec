
library(tidyverse)
library(sf)

# TO DO:
#   1) Add a column for year-month to the data (compare foraging locations between
#      individuals only within the same month and year)
#   2) Sort series of state 2s into successive "foraging bouts"
#       Number by sequence
#   3) Find UTM zone of data and convert to projected coords
#       UTM Zone is "floor((longitude + 180) / 6) + 1"
#   4) Get centroid of each foraging bout?



new_crs <- CRS(paste0("+proj=utm +zone=", (floor((dd.long + 180) / 6) + 1), "ellps=WGS84"))

reindeer_sf <- st_as_sf(reindeer_states, 
                        coords = c('location.long', 'location.lat'),
                        crs = 4326) %>%
  st_transform(new_crs) %>%
  arrange(individual.id, timestamp)

