

library(tidyverse)

# 05 Prep data movement and landscape ====
# Combine movement data with landscape data for plots and models

# Load and prep data for movement ~ landscape models

# Landscape data
lc_summary <- readRDS('output/lc_summary.rds') %>%
  mutate(across(simpsons_D:pet, list(sc = function(x) as.vector(scale(x, center = T)))))

# Movement data
movement_summary <- readRDS('output/movement_summary.rds')

# Home range data
hr_summary <- readRDS('output/hr_summary.rds')

# Pantheria for body mass data
panther <- read.csv('input/imputation_phylo_1.csv') %>%
  mutate(taxon = tolower(gsub(' ', '_', phylacine_binomial))) %>%
  select(taxon, adult_mass_g)

# Combine data
all_dat <- movement_summary %>%
  left_join(lc_summary) %>%
  # Add speed (m/hour)
  mutate(speed = case_when(time_units == 'secs' ~ speed*3600,
                           time_units == 'mins' ~ speed*60,
                           time_units == 'hours' ~ speed,
                           time_units == 'days' ~ speed/24)) %>%
  # Convert all time intervals to hours
  mutate(reg_time = ifelse(time_units == 'mins', reg_time/60, reg_time)) %>%
  mutate(reg_time = ifelse(time_units == 'secs', reg_time/3600, reg_time)) %>%
  # Add calculated hr data
  dplyr::select(! hr_area) %>%
  left_join(hr_summary) %>%
  # Add small value to hr_area to prevent negative logs
  mutate(hr_area = hr_area + 0.1) %>%
  # Log speed and home range area
  mutate(across(c(speed, hr_area), list(log = function(x) log(x)))) %>%
  # Factor study ID and individual ID
  mutate(study = factor(study), 
         id = factor(id)) %>%
  # Add body size data
  left_join(panther, relationship = 'many-to-many') %>%
  # Scale data collection interval and adult body mass
  ungroup() %>%
  mutate(reg_time_sc = scale(reg_time)[,1],
         adult_mass_g_sc = scale(adult_mass_g)[,1]) %>%
  relocate(reg_time_sc, .after = reg_time) %>%
  # Remove NAs
  na.omit() %>%
  # Select movement/environment variables of interest
  dplyr::select(study:reg_time_sc, simpsons_D_sc, hab_contrast_sc, speed_log,
                hr_area_log, adult_mass_g_sc) %>%
  # Distinct individuals
  distinct()

# List of species (to classify taxa by order)
taxon_order <- data.frame(taxon = unique(all_dat$taxon)) %>%
  mutate(order = taxize::tax_name(taxon, get = 'order', pref = 'both')[[3]])

# Join order data and classify movement mode
model_dat <- all_dat %>%
  left_join(taxon_order) %>%
  mutate(move_mode = ifelse(order == 'Chiroptera', 'flying', 'ground')) %>%
  relocate(c(order, move_mode), .after = taxon)

# Save data for plotting and modeling
saveRDS(model_dat, 'output/prepped_data.rds')

