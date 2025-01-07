
library(stars)
library(tidyverse)
library(ggspatial)
library(rnaturalearth)

# This script is to make the data used for MEMs, taking point estimates
# of species richness at locations of home range size estimates

# Load the home range size data
home_range <- read.csv('input/HomeRangeData_2023_09_06.csv')

# Annual home ranges for individuals, populations, groups ====
# 
# Individual = single individual; group = home range of all individuals in 
# group-living species; population = mean of multiple individual home ranges
# (Working with annual for now... not enough daily home ranges, which would match 
# the movement stuff, but maybe ok because this is at a larger scale)

# Get annual home ranges
hr_summ_spp <- home_range %>%
  filter(grepl('Annual', HR_Span))

# Summarize home ranges for individuals (need to group by species and study
# to get an average size for each population)
hr_indivs <- hr_summ_spp %>%
  filter(HR_Level == 'Individual') %>%
  group_by(Species, Study_ID) %>%
  summarize(mean_size = mean(Home_Range_km2),
            # Keep lat and long for spatial
            lat = mean(Latitude),
            long = mean(Longitude))

# Summarize population home ranges
hr_pops <- hr_summ_spp %>%
  filter(grepl('Population', HR_Level)) %>%
  group_by(Species, Study_ID) %>%
  summarize(mean_size = mean(Home_Range_km2),
            lat = mean(Latitude),
            long = mean(Longitude)) %>%
  # Filter out species from the same study populations so no overlap
  filter(! Study_ID %in% unique(hr_indivs$Study_ID))

# Summarize group home ranges
hr_grps <- hr_summ_spp %>%
  filter(grepl('Group', HR_Level)) %>%
  group_by(Species, Study_ID) %>%
  summarize(mean_size = mean(Home_Range_km2),
            lat = mean(Latitude),
            long = mean(Longitude)) %>%
  filter(! Study_ID %in% c(unique(hr_indivs$Study_ID), unique(hr_pops$Study_ID)))

# Bind all home ranges together
hrs_total <- bind_rows(hr_indivs, hr_pops, hr_grps) %>%
  group_by(Species) %>%
  # Remove any NAs and species with < 2 populations
  mutate(N_pops = n()) %>%
  # filter(! N_pops < 2) %>%
  na.omit() %>%
  # Remove domestic species
  filter(! Species %in% c('Capra hircus', 'Felis catus'))

# Extract species richness from rasters ====
#
# This uses the mammal species richness data from the IUCN 
# (https://www.iucnredlist.org/resources/other-spatial-downloads#SR_2022)
# We are making distance buffers around the population locations (100 km for now)
# then extracting mean species richness from those buffers plus at point locs

# IUCN species richness raster
spp_richness <- read_stars('input/Mammals_SR_2022.tif')

# Make home range data frame into sf object
hrs_sf <- st_as_sf(hrs_total, coords = c('long', 'lat'), crs = 4326)

# Reproject home ranges to match species richness raster
hrs_reproj <- st_transform(hrs_sf, st_crs(spp_richness))

# Buffer (distance for reprojected raster is in metres)
hrs_buff <- st_buffer(hrs_reproj, dist = 50000)

# Make species richness raster into sf
spp_richness_sf <- st_as_sf(spp_richness)

# Join species richness vector and buffered home range locations to get
# within-buffer estimates of species richness
hrs_buff_sr <- st_join(spp_richness_sf, hrs_buff) %>%
  na.omit() %>%
  rename('richness' = `Mammals_SR_2022.tif`) %>%
  group_by(Study_ID, Species) %>%
  # Get mean richness within the buffer
  summarize(mean_size = mean(mean_size),
            mean_richness = mean(richness)) %>%
  ungroup() %>%
  # Scale richness for plotting
  mutate(mean_richness_sc = scale(mean_richness)[,1]) %>%
  relocate(mean_richness_sc, .after = mean_richness)

# Extract species richness from reprojected home ranges to get point ests
# of species richness
hrs_pt_sr <- hrs_reproj %>%
  bind_cols(richness = st_extract(spp_richness, 
                                  hrs_reproj)$`Mammals_SR_2022.tif`) %>%
  mutate(richness_sc = scale(richness)[,1]) %>%
  relocate(geometry, .after = richness_sc) %>%
  na.omit()

# Plot map of home range locations on species richness raster
ggplot() + geom_stars(data = spp_richness) + geom_sf(data = hrs_reproj)

# Plot relationship between richness and home range size
ggplot(hrs_pt_sr, aes(x = richness, y = log(mean_size))) + geom_point()

# Get continent locations for all species points ====
# 
# We're using the rnaturalearth package to intersect point home range locations
# with continents so we can make separate MEMs

# Make function to get a df of points by continent (default is using point
# layer, but can also use the buffered home range locations)
pts_by_cont <- function(df = hrs_pt_sr, rast = spp_richness, cont) {
  
  # Get sf object of continent
  # (need to get country if australia or new zealand)
  if(cont %in% c('australia', 'new zealand')) {
    sf_cont <- ne_countries(country = cont, returnclass = 'sf')
  } else {
    sf_cont <- ne_countries(continent = cont, returnclass = 'sf')
  }
  
  # Reproject to same projection as home range/spp richness data
  sf_cont_trans <- sf_cont %>%
    st_transform(st_crs(rast))
  
  # Get hr points intersecting with the continent, then add a new variable to
  # the df with the continent name and remove extra cols
  hrs_pt_cont <- st_intersection(df, sf_cont_trans) %>%
    mutate(continent = cont) %>%
    relocate(continent, .after = Study_ID) %>%
    select(Species:richness_sc)
  
  # Return the df
  return(hrs_pt_cont)
}

# Loop through to compile a new df with home range data by continent

# Continent names
cont_names <- c('north america', 'south america', 'africa', 
                'asia', 'europe', 'australia', 'new zealand')
# Loop through to make df
hrs_by_cont <- data.frame()
for(i in cont_names) {
  hrs_i <- pts_by_cont(cont = i)
  hrs_by_cont <- hrs_by_cont %>% rbind(hrs_i)
}

# Save data for MEMs ====

saveRDS(hrs_by_cont, 'output/cleaned_spp_richness.rds')

