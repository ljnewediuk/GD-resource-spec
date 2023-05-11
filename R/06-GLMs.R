
library(tidyverse)
library(tidybayes)
library(brms)
library(emmeans)
library(modelr)

# 06 Models ====
# Model movement (speed and HR area) on heterogeneity and pet

# Load movement ~ environment data
move_dat <- readRDS('output/prepped_data.rds') %>%
  mutate(study = factor(study))

# Load fst data
fst_dat <- readRDS('output/prepped_fst_data.rds')

# List of bat species
bats <- c('tadarida_brasiliensis', 'carollia_castanea', 'vespertilio_murinus',
          'carollia_perspicillata', 'sturnira_lilium', 'rhinolophus_ferrumequinum', 
          'myotis_vivesi', 'tadarida_teniotis', 'trachops_cirrhosus', 
          'macroderma_gigas', 'eidolon_helvum', 'pteropus_lylei', 
          'hypsignathus_monstrosus', 'pteropus_melanotus', 'pteropus_alecto', 
          'nyctalus_noctula') 

# Pantheria for body mass
panther <- read.csv('input/imputation_phylo_1.csv') %>%
  mutate(taxon = tolower(gsub(' ', '_', phylacine_binomial))) %>%
  select(taxon, adult_mass_g)

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

# Fit models for habitat heterogeneity, habitat diversity, and evapotranspiration
speed_het <- fit_mod(move_dat, 'hab_contrast_sc', '(hab_contrast_sc | taxon/study)', 'speed_log')
speed_simps <- fit_mod(move_dat, 'simpsons_D_sc', '(simpsons_D_sc | taxon/study)', 'speed_log')
hr_het <- fit_mod(move_dat, 'hab_contrast_sc', '(hab_contrast_sc | taxon/study)', 'hr_area_log')
hr_simps <- fit_mod(move_dat, 'simpsons_D_sc', '(simpsons_D_sc | taxon/study)', 'hr_area_log')

# Fit models for Fst ~ dispersal distance and HR size
fst_disp <- fit_mod(fst_dat, 'dispersal_km_sc', NULL, 'global_fst_sc')
fst_hr <- fit_mod(fst_dat, 'log_hr_km2', NULL, 'global_fst_sc')

# Posterior draws from fst models
draws_fst_disp <- fst_dat |>
  data_grid(dispersal_km_sc = seq_range(dispersal_km_sc, n = 1000))  |>
  add_predicted_draws(object = fst_disp, ndraws = 10000)

draws_fst_hr <- fst_dat |>
  data_grid(log_hr_km2 = seq_range(log_hr_km2, n = 1000))  |>
  add_predicted_draws(object = fst_hr, ndraws = 10000) 

# Population effects from heterogeneity models

speed_het_fixed <- speed_het %>%
  spread_draws(b_hab_contrast_sc) %>%
  median_qi(b_hab_contrast_sc) %>%
  rename('slope' = b_hab_contrast_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat contrast',
         response = 'speed')

speed_simps_fixed <- speed_simps %>%
  spread_draws(b_simpsons_D_sc) %>%
  median_qi(b_simpsons_D_sc) %>%
  rename('slope' = b_simpsons_D_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat diversity',
         response = 'speed')

hr_het_fixed <- hr_het %>%
  spread_draws(b_hab_contrast_sc) %>%
  median_qi(b_hab_contrast_sc) %>%
  rename('slope' = b_hab_contrast_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat contrast',
         response = 'hr')

hr_simps_fixed <- hr_simps %>%
  spread_draws(b_simpsons_D_sc) %>%
  median_qi(b_simpsons_D_sc) %>%
  rename('slope' = b_simpsons_D_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat diversity',
         response = 'hr')

# Bind together
all_fixed_vars <- speed_het_fixed %>%
  rbind(speed_simps_fixed) %>%
  rbind(hr_het_fixed) %>%
  rbind(hr_simps_fixed)

# Same for individual slopes... maybe rename the variable in the function?
# Individual slopes
speed_het_vars <- speed_het %>%
  spread_draws(b_hab_contrast_sc, r_taxon[study, term]) %>%
  mutate(species_m = b_hab_contrast_sc + r_taxon) %>%
  filter(term == 'hab_contrast_sc') %>%
  arrange(study) %>%
  group_by(study) %>% 
  summarize(mean_slope = mean(species_m), se_slope = sd(species_m)/sqrt(length(species_m))*1.96) %>%
  mutate(var = 'Habitat contrast',
         response = 'speed') %>%
  rename('taxon' = study) %>%
  mutate(lifestyle = ifelse(taxon %in% bats, 'bat', 'terrestrial'))

speed_simps_vars <- speed_simps %>%
  spread_draws(b_simpsons_D_sc, r_taxon[study, term]) %>%
  mutate(species_m = b_simpsons_D_sc + r_taxon) %>%
  filter(term == 'simpsons_D_sc') %>%
  arrange(study) %>%
  group_by(study) %>% 
  summarize(mean_slope = mean(species_m), se_slope = sd(species_m)/sqrt(length(species_m))*1.96) %>%
  mutate(var = 'Habitat diversity',
         response = 'speed') %>%
  rename('taxon' = study) %>%
  mutate(lifestyle = ifelse(taxon %in% bats, 'bat', 'terrestrial'))

hr_het_vars <- hr_het %>%
  spread_draws(b_hab_contrast_sc, r_taxon[study, term]) %>%
  mutate(species_m = b_hab_contrast_sc + r_taxon) %>%
  filter(term == 'hab_contrast_sc') %>%
  arrange(study) %>%
  group_by(study) %>% 
  summarize(mean_slope = mean(species_m), se_slope = sd(species_m)/sqrt(length(species_m))*1.96) %>%
  mutate(var = 'Habitat contrast',
         response = 'hr') %>%
  rename('taxon' = study) %>%
  mutate(lifestyle = ifelse(taxon %in% bats, 'bat', 'terrestrial'))

hr_simps_vars <- hr_simps %>%
  spread_draws(b_simpsons_D_sc, r_taxon[study, term]) %>%
  mutate(species_m = b_simpsons_D_sc + r_taxon) %>%
  filter(term == 'simpsons_D_sc') %>%
  arrange(study) %>%
  group_by(study) %>% 
  summarize(mean_slope = mean(species_m), se_slope = sd(species_m)/sqrt(length(species_m))*1.96) %>%
  mutate(var = 'Habitat diversity',
         response = 'hr') %>%
  rename('taxon' = study) %>%
  mutate(lifestyle = ifelse(taxon %in% bats, 'bat', 'terrestrial'))

# Bind together
all_rand_vars <- speed_het_vars %>%
  rbind(speed_simps_vars) %>%
  rbind(hr_het_vars) %>%
  rbind(hr_simps_vars) %>%
  left_join(panther)

# Save models
saveRDS(speed_het, 'model_files/speed_het.rds')
saveRDS(speed_simps, 'model_files/speed_simps.rds')
saveRDS(speed_pet, 'model_files/speed_pet.rds')
saveRDS(hr_het, 'model_files/hr_het.rds')
saveRDS(hr_simps, 'model_files/hr_simps.rds')
saveRDS(hr_pet, 'model_files/hr_pet.rds')
saveRDS(fst_disp, 'model_files/fst_disp.rds')
saveRDS(fst_hr, 'model_files/fst_hr.rds')

# Save posterior draws
saveRDS(all_fixed_vars, 'output/movement_fixed_draws.rds')
saveRDS(all_rand_vars, 'output/movement_rand_draws.rds')
saveRDS(draws_fst_disp, 'output/fst_disp_draws.rds')
saveRDS(draws_fst_hr, 'output/fst_hr_draws.rds')

