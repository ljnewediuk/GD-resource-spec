
library(tidyverse)
library(tidybayes)
library(brms)
library(emmeans)
library(modelr)
library(bayestestR)
library(performance)

# List models
mod_file_names_m <- list.files('model_files/main')

# Load all main text models then print bayes R2 and probability of direction
for(n in 1:length(mod_file_names_m)) {
  mod <- readRDS(paste0('model_files/main/', mod_file_names_m[n]))
  # Print model name and R2
  print(str_extract(mod_file_names_m[n], "[^.]+"))
  print(r2_bayes(mod))
  cat('\n')
  print(p_direction(mod))
  cat('\n', '\n')
  # Assign object to name
  assign(str_extract(mod_file_names_m[n], "[^.]+"), mod)
}

# DRAWS FROM POSTERIOR FOR PLOTTING

# Posterior draws from fst models
# Dispersal
draws_fst_disp <- fst_dat |>
  data_grid(dispersal_km_sc = seq_range(dispersal_km_sc, n = 1000))  |>
  add_epred_draws(object = fst_disp, ndraws = 1000)
# Home range
draws_fst_hr <- fst_dat |>
  data_grid(log_hr_km2 = seq_range(log_hr_km2, n = 1000))  |>
  add_epred_draws(object = fst_hr, ndraws = 1000) 

# Population effects from heterogeneity models
# Speed ~ habitat heterogeneity
speed_het_fixed <- speed_het %>%
  spread_draws(b_hab_contrast_sc) %>%
  median_qi(b_hab_contrast_sc) %>%
  rename('slope' = b_hab_contrast_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat contrast',
         response = 'speed')
# Speed ~ habitat diversity
speed_simps_fixed <- speed_simps %>%
  spread_draws(b_simpsons_D_sc) %>%
  median_qi(b_simpsons_D_sc) %>%
  rename('slope' = b_simpsons_D_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat diversity',
         response = 'speed')
# Speed ~  potential evapotranspiration
speed_pet_fixed <- speed_pet %>%
  spread_draws(b_pet_sc) %>%
  median_qi(b_pet_sc) %>%
  rename('slope' = b_pet_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Potential evapotranspiration',
         response = 'speed')
# Home range size ~ habitat heterogeneity
hr_het_fixed <- hr_het %>%
  spread_draws(b_hab_contrast_sc) %>%
  median_qi(b_hab_contrast_sc) %>%
  rename('slope' = b_hab_contrast_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat contrast',
         response = 'hr')
# Home range size ~ habitat diversity
hr_simps_fixed <- hr_simps %>%
  spread_draws(b_simpsons_D_sc) %>%
  median_qi(b_simpsons_D_sc) %>%
  rename('slope' = b_simpsons_D_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat diversity',
         response = 'hr')
# Home range size ~ potential evapotranspiration
hr_pet_fixed <- hr_pet %>%
  spread_draws(b_pet_sc) %>%
  median_qi(b_pet_sc) %>%
  rename('slope' = b_pet_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Potential evapotranspiration',
         response = 'hr')
# Bind together
all_fixed_vars <- speed_het_fixed %>%
  rbind(speed_simps_fixed) %>%
  rbind(speed_pet_fixed) %>%
  rbind(hr_het_fixed) %>%
  rbind(hr_simps_fixed) %>%
  rbind(hr_pet_fixed)

# Individual slopes for plotting
# Speed ~ habitat heterogeneity
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
# Speed ~ habitat diversity
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
# Speed ~ potential evapotranspiration
speed_pet_vars <- speed_pet %>%
  spread_draws(b_pet_sc, r_taxon[study, term]) %>%
  mutate(species_m = b_pet_sc + r_taxon) %>%
  filter(term == 'pet_sc') %>%
  arrange(study) %>%
  group_by(study) %>% 
  summarize(mean_slope = mean(species_m), se_slope = sd(species_m)/sqrt(length(species_m))*1.96) %>%
  mutate(var = 'Potential evapotranspiration',
         response = 'speed') %>%
  rename('taxon' = study) %>%
  mutate(lifestyle = ifelse(taxon %in% bats, 'bat', 'terrestrial'))
# Home range size ~ habitat heterogeneity
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
# Home range size ~ habitat diversity
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
# Home range size ~ potential evapotranspiration
hr_pet_vars <- hr_pet %>%
  spread_draws(b_pet_sc, r_taxon[study, term]) %>%
  mutate(species_m = b_pet_sc + r_taxon) %>%
  filter(term == 'pet_sc') %>%
  arrange(study) %>%
  group_by(study) %>% 
  summarize(mean_slope = mean(species_m), se_slope = sd(species_m)/sqrt(length(species_m))*1.96) %>%
  mutate(var = 'Potential evapotranspiration',
         response = 'hr') %>%
  rename('taxon' = study) %>%
  mutate(lifestyle = ifelse(taxon %in% bats, 'bat', 'terrestrial'))
# Bind together
all_rand_vars <- speed_het_vars %>%
  rbind(speed_simps_vars) %>%
  rbind(speed_pet_vars) %>%
  rbind(hr_het_vars) %>%
  rbind(hr_simps_vars) %>%
  rbind(hr_pet_vars) %>%
  left_join(panther)

# SAVE posterior draws
saveRDS(all_fixed_vars, 'output/movement_fixed_draws.rds')
saveRDS(all_rand_vars, 'output/movement_rand_draws.rds')
saveRDS(draws_fst_disp, 'output/fst_disp_draws.rds')
saveRDS(draws_fst_hr, 'output/fst_hr_draws.rds')
