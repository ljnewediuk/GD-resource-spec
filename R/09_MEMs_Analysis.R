
library(tidyverse)
library(sf)
library(vegan)
library(adespatial)

# Source get MEMs function
source('functions/CalcMEMs.R')

# Load species richness/home range size data and reproject into WGS84
dat <- readRDS('output/cleaned_spp_richness.rds') %>%
  st_transform(4326)

# Run MEM function
glob_MEM <- get_MEMs(df = dat, 
                     vars = c('mean_size', 'richness'), 
                     broad.thresh = c(0.1, 0.1), 
                     shared_mems = 'yes')

# Get partitioned variance and pivot for plotting
shared_var <- glob_MEM$variance_part %>%
  pivot_longer(cols = c(ATS: ANSS), names_to = 'variance_component')

# Get fitted data for mapping
map_dat <- glob_MEM$fitted_vals %>%
  pivot_longer(cols = c(fitted_vals_richness, fitted_vals_mean_size), 
               values_to = 'fitted',
               names_to = 'var') %>%
  # Rename variables for plots
  mutate(var = ifelse(var == 'fitted_vals_richness', 
                      'Species richness', 'Home range size'))

# Save data for mapping/plotting
saveRDS(map_dat, 'output/global_morans_vals.rds')
saveRDS(shared_var_foo, 'output/global_morans_shared_var.rds')

