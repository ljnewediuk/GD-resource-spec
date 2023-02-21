
library(tidyverse)
library(sf)

# TO DO:
#   1) Add a column for year-month to the data (compare foraging locations between
#      individuals only within the same month and year?)
#   2) Check time stamps between locations
#   2) Sort series of state 2s into successive "foraging bouts"
#       Number by sequence
#   3) Find UTM zone of data and convert to projected coords
#       UTM Zone is "floor((longitude + 180) / 6) + 1"
#   4) Get centroid of each foraging bout? Is this required for the DFD method?
#      (Look at Ramellini et al. 2022 methods)
#   5) Next script: Make bbox around locations up to the size Chloe used and
#      get land cover data. Then extract locations and quantify heterogeneity
#      and energy availability for the landscape.
#      
#      NOTE: Some studies include multiple populations (e.g., 942509734
#            wildebeest study); check the study info for more details and 
#            download the reference data if appropriate

#1 - Load dataset ====
dat <- readRDS('hmm_files/alces_alces_178994931.rds') %>%
  mutate(taxon = str_replace(tolower(individual.taxon.canonical.name), ' ', '_')) %>%
  select(study.id, individual.id, taxon, timestamp:location.lat, state, sl_mean:ta_conc) %>%
  rename('study' = study.id, 'id' = individual.id, 'lat' = location.lat, 'long' = location.long)

#2 - Filter timesteps too far from mean timestep ====
check_times <- dat  %>%
  # Group by ID to get fix times by individual
  group_by(id) %>%
  mutate(diff = as.numeric(c(difftime(tail(timestamp, -1), head(timestamp, -1)), 0))) %>%
  # Remove the last row in the group ( = 0 hrs)
  slice(1: n() -1) %>%
  #  Get fix mean and the tolerance, i.e. the % difference from the mean fix rate
  mutate(fix_mean = mean(diff),
         fix_tolerance = abs(mean(diff) - diff)/mean(diff)) %>%
  # Remove the fixes that are more than 30% different from the mean fix rate
  filter(! fix_tolerance > 0.3)

#3 - Find UTM zone of locs and project ====
proj_dat <- check_times %>%
  st_as_sf(coords = c('long', 'lat'), crs = sp::CRS("+proj=longlat +ellips=WGS84"), remove = F) %>%
  mutate(utm_crs = paste0("+proj=utm +zone=", (floor((long + 180) / 6) + 1), " ellps=WGS84")) %>%
  group_by(utm_crs) %>%
  group_split()

for(i in 1:length(proj_dat)) {
  
  new_crs <- unique(proj_dat[[i]]$utm_crs)
  dat_sf <- proj_dat[[i]] %>%
    st_transform(crs = new_crs) %>%
    select(! utm_crs)
  
  # Group by successive states
  # Save
  # saveRDS(dat_sf, paste0('sf_files/', unique(dat_sf$taxon), '_', unique(dat_sf$study), 
  #              '_utm_', str_sub(new_crs, 17, 18), '.rds'), dat_sf)
}




#   CRS(paste0("+proj=utm +zone=", (floor((dd.long + 180) / 6) + 1), "ellps=WGS84"))
# 
# 
# new_crs <- paste0("+proj=utm +zone=", (floor((check_times[1,]$long + 180) / 6) + 1), " ellps=WGS84")
# 
# reindeer_sf <- st_as_sf(reindeer_states, 
#                         coords = c('location.long', 'location.lat'),
#                         crs = 4326) %>%
#   st_transform(new_crs) %>%
#   arrange(individual.id, timestamp)

