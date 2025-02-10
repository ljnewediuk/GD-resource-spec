
# 06 Posterior draws ====
# Get posterior draws from main models to summarize/plot 

libs <- list('tidyverse', 'tidybayes', 'brms', 'emmeans', 
             'modelr', 'bayestestR', 'performance')

lapply(libs, require, character.only = T)

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

# Load species mass data and model data for movement type (flying/not flying)
trait_dat <- readRDS('output/model_data.rds') %>%
  select(taxon, adult_mass_g, adult_mass_g_sc, move_mode) %>%
  distinct()

# DRAWS FROM POSTERIOR FOR PLOTTING

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
# Bind together
all_fixed_vars <- speed_het_fixed %>%
  rbind(speed_simps_fixed) %>%
  rbind(hr_het_fixed) %>%
  rbind(hr_simps_fixed) 

# Individual slopes for plotting
# Speed ~ habitat heterogeneity
speed_het_vars <- speed_het %>%
  spread_draws(b_hab_contrast_sc, r_taxon[taxon, term]) %>%
  mutate(species_m = b_hab_contrast_sc + r_taxon) %>%
  filter(term == 'hab_contrast_sc') %>%
  arrange(taxon) %>%
  group_by(taxon) %>% 
  summarize(mean_slope = mean(species_m), se_slope = sd(species_m)/sqrt(length(species_m))*1.96) %>%
  mutate(var = 'Habitat contrast',
         response = 'speed')
# Speed ~ habitat diversity
speed_simps_vars <- speed_simps %>%
  spread_draws(b_simpsons_D_sc, r_taxon[taxon, term]) %>%
  mutate(species_m = b_simpsons_D_sc + r_taxon) %>%
  filter(term == 'simpsons_D_sc') %>%
  arrange(taxon) %>%
  group_by(taxon) %>% 
  summarize(mean_slope = mean(species_m), se_slope = sd(species_m)/sqrt(length(species_m))*1.96) %>%
  mutate(var = 'Habitat diversity',
         response = 'speed')
# Home range size ~ habitat heterogeneity
hr_het_vars <- hr_het %>%
  spread_draws(b_hab_contrast_sc, r_taxon[taxon, term]) %>%
  mutate(species_m = b_hab_contrast_sc + r_taxon) %>%
  filter(term == 'hab_contrast_sc') %>%
  arrange(taxon) %>%
  group_by(taxon) %>% 
  summarize(mean_slope = mean(species_m), se_slope = sd(species_m)/sqrt(length(species_m))*1.96) %>%
  mutate(var = 'Habitat contrast',
         response = 'hr')
# Home range size ~ habitat diversity
hr_simps_vars <- hr_simps %>%
  spread_draws(b_simpsons_D_sc, r_taxon[taxon, term]) %>%
  mutate(species_m = b_simpsons_D_sc + r_taxon) %>%
  filter(term == 'simpsons_D_sc') %>%
  arrange(taxon) %>%
  group_by(taxon) %>% 
  summarize(mean_slope = mean(species_m), se_slope = sd(species_m)/sqrt(length(species_m))*1.96) %>%
  mutate(var = 'Habitat diversity',
         response = 'hr')

# Bind together
all_rand_vars <- speed_het_vars %>%
  rbind(speed_simps_vars) %>%
  rbind(hr_het_vars) %>%
  rbind(hr_simps_vars) %>%
  left_join(trait_dat)

# SAVE posterior draws
saveRDS(all_fixed_vars, 'output/movement_fixed_draws.rds')
saveRDS(all_rand_vars, 'output/movement_rand_draws.rds')
