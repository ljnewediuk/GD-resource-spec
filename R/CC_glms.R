
# Load libraries
library(tidyverse)
library(brms)

# List of bat species to classify movement mode
bats <- c('tadarida_brasiliensis', 'carollia_castanea', 'vespertilio_murinus',
          'carollia_perspicillata', 'sturnira_lilium', 'rhinolophus_ferrumequinum', 
          'myotis_vivesi', 'tadarida_teniotis', 'trachops_cirrhosus', 
          'macroderma_gigas', 'eidolon_helvum', 'pteropus_lylei', 
          'hypsignathus_monstrosus', 'pteropus_melanotus', 'pteropus_alecto', 
          'nyctalus_noctula') 

# Load movement ~ environment data
move_dat <- readRDS('prepped_data.rds') %>%
  ungroup() %>%
  # Add study as a factor
  mutate(study = factor(study),
         # Scale data collection interval and adult body mass
         reg_time_sc = scale(reg_time)[,1],
         adult_mass_g_sc = scale(adult_mass_g)[,1],
         # Add factor for movement mode
         move_mode = ifelse(taxon %in% bats, 'flying', 'ground'))

# Function to fit models
# Specify random slope and intercept varying among taxon and studies within taxon
fit_mod <- function(dat, fixed_eff, ran_eff, resp_var) {
  
  mod <- brm(reformulate(c(fixed_eff, ran_eff), response = resp_var),
             data = dat, family = gaussian, 
             iter = 10000, warmup = 5000, chains = 4, cores = 4, 
             prior = prior(normal(0,1), class = b),
             control = list(adapt_delta = 0.99, max_treedepth = 18),
             backend = 'cmdstanr')
  
  return(mod)
}

# SUPPLEMENTARY MODELS
# Fit models to test effects of movement mode*environment interaction on movement
speed_simps_mm <- fit_mod(move_dat, 'simpsons_D_sc*move_mode', '(simpsons_D_sc | taxon/study)', 'speed_log')
hr_het_mm <- fit_mod(move_dat, 'hab_contrast_sc*move_mode', '(hab_contrast_sc | taxon/study)', 'hr_area_log')
hr_simps_mm <- fit_mod(move_dat, 'simpsons_D_sc*move_mode', '(simpsons_D_sc | taxon/study)', 'hr_area_log')
hr_pet_mm <- fit_mod(move_dat, 'pet_sc*move_mode', '(pet_sc | taxon/study)', 'hr_area_log')
# Fit models to test effects of body mass*environment interaction on movement 
speed_simps_bm <- fit_mod(move_dat, 'simpsons_D_sc*adult_mass_g_sc', '(simpsons_D_sc | taxon/study)', 'speed_log')
hr_het_bm <- fit_mod(move_dat, 'hab_contrast_sc*adult_mass_g_sc', '(hab_contrast_sc | taxon/study)', 'hr_area_log')
hr_simps_bm <- fit_mod(move_dat, 'simpsons_D_sc*adult_mass_g_sc', '(simpsons_D_sc | taxon/study)', 'hr_area_log')
hr_pet_bm <- fit_mod(move_dat, 'pet_sc*adult_mass_g_sc', '(pet_sc | taxon/study)', 'hr_area_log')
speed_het_bm <- fit_mod(move_dat, 'hab_contrast_sc*adult_mass_g_sc', '(hab_contrast_sc | taxon/study)', 'speed_log')
speed_pet_bm <- fit_mod(move_dat, 'pet_sc*adult_mass_g_sc', '(pet_sc | taxon/study)', 'speed_log')

# Save models
saveRDS(speed_simps_mm, 'speed_simps_mm.rds')
saveRDS(hr_het_mm, 'hr_het_mm.rds')
saveRDS(hr_simps_mm, 'hr_simps_mm.rds')
saveRDS(hr_pet_mm, 'hr_pet_mm.rds')
saveRDS(speed_simps_bm, 'speed_simps_bm.rds')
saveRDS(hr_het_bm, 'hr_het_bm.rds')
saveRDS(hr_simps_bm, 'hr_simps_bm.rds')
saveRDS(hr_pet_bm, 'hr_pet_bm.rds')
saveRDS(speed_het_bm, 'speed_het_bm.rds')
saveRDS(speed_pet_bm, 'speed_pet_bm.rds')




