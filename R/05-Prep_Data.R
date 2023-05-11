

library(tidyverse)

# 05 Prep data movement and landscape ====
# Combine movement data with landscape data for plots and models

# Load and prep data for movement ~ landscape models
# Landscape
lc_summary <- readRDS('output/lc_summary.rds') %>%
  mutate(across(simpsons_D:pet, list(sc = function(x) as.vector(scale(x, center = T)))))
# Movement
movement_summary <- readRDS('output/movement_summary.rds')
# Home range data
hr_summary <- readRDS('output/hr_summary.rds')
# Pantheria for body mass
panther <- read.csv('input/imputation_phylo_1.csv') %>%
  mutate(taxon = tolower(gsub(' ', '_', phylacine_binomial))) %>%
  select(taxon, adult_mass_g)

# Combine data
all_dat <- movement_summary %>%
  left_join(lc_summary) %>%
  # Add intensity of home range use
  mutate(intensity_use = cumulat_dist/(hr_area^2),
         # Add speed (m/hour)
         speed = ifelse(time_units == 'secs', speed*3600, speed),
         speed = ifelse(time_units == 'mins', speed*60, speed),
         speed = ifelse(time_units == 'days', speed/24, speed)) %>%
  select(! hr_area) %>%
  left_join(hr_summary) %>%
  # Add value to hr_area to prevent negative logs
  mutate(hr_area = hr_area + 0.1) %>%
  # Log values
  mutate(across(c(speed, hr_area, intensity_use), list(log = function(x) log(x)))) %>%
  # Add body size data
  left_join(panther) %>%
  # Remove NAs
  na.omit()


# Save data for plotting and modeling
saveRDS(all_dat, 'output/prepped_data.rds')

