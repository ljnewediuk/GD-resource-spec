
library(tidyverse)
library(tidybayes)
library(brms)
library(emmeans)
library(modelr)
library(bayestestR)
library(performance)

# 06 Models ====
# Model movement (speed and HR area) on heterogeneity and pet

# List of bat species
bats <- c('tadarida_brasiliensis', 'carollia_castanea', 'vespertilio_murinus',
          'carollia_perspicillata', 'sturnira_lilium', 'rhinolophus_ferrumequinum', 
          'myotis_vivesi', 'tadarida_teniotis', 'trachops_cirrhosus', 
          'macroderma_gigas', 'eidolon_helvum', 'pteropus_lylei', 
          'hypsignathus_monstrosus', 'pteropus_melanotus', 'pteropus_alecto', 
          'nyctalus_noctula') 

# Load movement ~ environment data
move_dat <- readRDS('output/prepped_data.rds') %>%
  ungroup() %>%
  # Add study as a factor
  mutate(study = factor(study),
         # Scale data collection interval and adult body mass
         reg_time_sc = scale(reg_time)[,1],
         adult_mass_g_sc = scale(adult_mass_g)[,1],
         # Add factor for movement mode
         move_mode = ifelse(taxon %in% bats, 'flying', 'ground'))

# Save model data for later use
saveRDS(move_dat, 'output/model_data.rds')

# Sub data set excluding the longest 25 percentile of time intervals
move_sub <- move_dat %>% 
  # Convert all time intervals to hours
  mutate(reg_time = ifelse(time_units == 'mins', reg_time/60, reg_time)) %>%
  mutate(reg_time = ifelse(time_units == 'secs', reg_time/3600, reg_time)) %>%
  # Exclude top 25 percentile
  filter(reg_time < quantile(reg_time, probs = 0.75))

# Load fst data
fst_dat <- readRDS('output/prepped_fst_data.rds')

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

# MAIN TEXT MODELS

# Fit models for habitat heterogeneity, habitat diversity, and evapotranspiration
speed_het <- fit_mod(move_dat, 'hab_contrast_sc', '(hab_contrast_sc | taxon/study)', 'speed_log')
speed_simps <- fit_mod(move_dat, 'simpsons_D_sc', '(simpsons_D_sc | taxon/study)', 'speed_log')
hr_het <- fit_mod(move_dat, 'hab_contrast_sc', '(hab_contrast_sc | taxon/study)', 'hr_area_log')
hr_simps <- fit_mod(move_dat, 'simpsons_D_sc', '(simpsons_D_sc | taxon/study)', 'hr_area_log')
hr_pet <- fit_mod(move_dat, 'pet_sc', '(pet_sc | taxon/study)', 'hr_area_log')
speed_pet <- fit_mod(move_dat, 'pet_sc', '(pet_sc | taxon/study)', 'speed_log')

# Fit models for Fst ~ dispersal distance and HR size
fst_disp <- fit_mod(fst_dat, 'dispersal_km_sc', NULL, 'global_fst_sc')
fst_hr <- fit_mod(fst_dat, 'log_hr_km2', NULL, 'global_fst_sc')

# SUPPLEMENTARY MODELS

# Fit models to test how time interval of data collection affects speed/hr size estimates
hr_check <- fit_mod(move_dat, 'reg_time_sc', '(reg_time_sc | taxon/study)', 'hr_area_log')
speed_check <- fit_mod(move_dat, 'reg_time_sc', '(reg_time_sc | taxon/study)', 'speed_log')

# Fit models for sub analysis to test whether excluding the top 25 percentile of data
# collection intervals changes model results
speed_het_sub <- fit_mod(move_sub, 'hab_contrast_sc', '(hab_contrast_sc | taxon/study)', 'speed_log')
speed_simps_sub <- fit_mod(move_sub, 'simpsons_D_sc', '(simpsons_D_sc | taxon/study)', 'speed_log')
hr_het_sub <- fit_mod(move_sub, 'hab_contrast_sc', '(hab_contrast_sc | taxon/study)', 'hr_area_log')
hr_simps_sub <- fit_mod(move_sub, 'simpsons_D_sc', '(simpsons_D_sc | taxon/study)', 'hr_area_log')
hr_pet_sub <- fit_mod(move_sub, 'pet_sc', '(pet_sc | taxon/study)', 'hr_area_log')
speed_pet_sub <- fit_mod(move_sub, 'pet_sc', '(pet_sc | taxon/study)', 'speed_log')
hr_het_mm <- fit_mod(move_dat, 'hab_contrast_sc*move_mode', '(hab_contrast_sc | taxon/study)', 'hr_area_log')
hr_simps_mm <- fit_mod(move_dat, 'simpsons_D_sc*move_mode', '(simpsons_D_sc | taxon/study)', 'hr_area_log')

# Fit models to test effects of movement mode
# Movement mode on ground reduces speed but no effect of interaction with PET on speed
speed_het_mm <- fit_mod(move_dat, 'hab_contrast_sc*move_mode', '(hab_contrast_sc | taxon/study)', 'speed_log')
speed_pet_mm <- fit_mod(move_dat, 'pet_sc*move_mode', '(pet_sc | taxon/study)', 'speed_log')
speed_simps_mm <- fit_mod(move_dat, 'simpsons_D_sc*move_mode', '(simpsons_D_sc | taxon/study)', 'speed_log')
hr_pet_mm <- fit_mod(move_dat, 'pet_sc*move_mode', '(pet_sc | taxon/study)', 'hr_area_log')
# Fit models to test effects of adult body mass
speed_simps_bm <- fit_mod(move_dat, 'simpsons_D_sc*adult_mass_g_sc', 
                          '(simpsons_D_sc | taxon/study)', 'speed_log')
speed_het_bm <- fit_mod(move_dat, 'hab_contrast_sc*adult_mass_g_sc', 
                        '(hab_contrast_sc | taxon/study)', 'speed_log')
speed_pet_bm <- fit_mod(move_dat, 'pet_sc*adult_mass_g_sc', 
                        '(pet_sc | taxon/study)', 'speed_log')
hr_het_bm <- fit_mod(move_dat, 'hab_contrast_sc*adult_mass_g_sc', 
                     '(hab_contrast_sc | taxon/study)', 'hr_area_log')
hr_simps_bm <- fit_mod(move_dat, 'simpsons_D_sc*adult_mass_g_sc', 
                       '(simpsons_D_sc | taxon/study)', 'hr_area_log')
hr_pet_bm <- fit_mod(move_dat, 'pet_sc*adult_mass_g_sc', 
                     '(pet_sc | taxon/study)', 'hr_area_log')

# Body size and movement mode alone
move_speed <- fit_mod(move_dat, 'move_mode', NULL, 'speed_log')
body_size_speed <- fit_mod(move_dat, 'adult_mass_g_sc', NULL, 'speed_log')
move_hr <- fit_mod(move_dat, 'move_mode', NULL, 'hr_area_log')
body_size_hr <- fit_mod(move_dat, 'adult_mass_g_sc', NULL, 'hr_area_log')

# SAVE main text models
saveRDS(speed_het, 'model_files/main/speed_het.rds')
saveRDS(speed_simps, 'model_files/main/speed_simps.rds')
saveRDS(speed_pet, 'model_files/main/speed_pet.rds')
saveRDS(hr_het, 'model_files/main/hr_het.rds')
saveRDS(hr_simps, 'model_files/main/hr_simps.rds')
saveRDS(hr_pet, 'model_files/main/hr_pet.rds')
saveRDS(fst_disp, 'model_files/main/fst_disp.rds')
saveRDS(fst_hr, 'model_files/main/fst_hr.rds')

# SAVE supplementary model checks and subanalyses
# Checks for speed and home range size
saveRDS(hr_check, 'model_files/supplement/supplement_hr_check.rds')
saveRDS(speed_check, 'model_files/supplement/supplement_speed_check.rds')
# Sub models
saveRDS(speed_het_sub, 'model_files/supplement/speed_het_sub.rds')
saveRDS(speed_simps_sub, 'model_files/supplement/speed_simps_sub.rds')
saveRDS(speed_pet_sub, 'model_files/supplement/speed_pet_sub.rds')
saveRDS(hr_het_sub, 'model_files/supplement/hr_het_sub.rds')
saveRDS(hr_pet_sub, 'model_files/supplement/hr_pet_sub.rds')
saveRDS(hr_simps_sub, 'model_files/supplement/hr_simps_sub.rds')
# Movement mode effects
saveRDS(speed_pet_mm, 'model_files/supplement/speed_pet_mm.rds')
saveRDS(speed_het_mm, 'model_files/supplement/speed_het_mm.rds')
saveRDS(speed_simps_mm, 'model_files/supplement/speed_simps_mm.rds')
saveRDS(hr_het_mm, 'model_files/supplement/hr_het_mm.rds')
saveRDS(hr_simps_mm, 'model_files/supplement/hr_simps_mm.rds')
saveRDS(hr_pet_mm, 'model_files/supplement/hr_pet_mm.rds')
# Body mass effects
saveRDS(speed_simps_bm, 'model_files/supplement/speed_simps_bm.rds')
saveRDS(hr_het_bm, 'model_files/supplement/hr_het_bm.rds')
saveRDS(hr_simps_bm, 'model_files/supplement/hr_simps_bm.rds')
saveRDS(hr_pet_bm, 'model_files/supplement/hr_pet_bm.rds')
saveRDS(speed_het_bm, 'model_files/supplement/speed_het_bm.rds')
saveRDS(speed_pet_bm, 'model_files/supplement/speed_pet_bm.rds')

