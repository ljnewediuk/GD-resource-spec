
library(tidyverse)
library(tidybayes)
library(brms)
library(emmeans)
library(modelr)

# 06 Models ====
# Model movement (speed and HR area) on heterogeneity and pet

# Load data
dat <- readRDS('output/prepped_data.rds') %>%
  mutate(study = factor(study))

# List of bat species
bats <- c('tadarida_brasiliensis', 'carollia_castanea', 'vespertilio_murinus',
          'carollia_perspicillata', 'sturnira_lilium', 'rhinolophus_ferrumequinum', 
          'myotis_vivesi', 'tadarida_teniotis', 'trachops_cirrhosus', 
          'macroderma_gigas', 'eidolon_helvum', 'pteropus_lylei', 
          'hypsignathus_monstrosus', 'pteropus_melanotus', 'pteropus_alecto', 
          'nyctalus_noctula') 

# Data frame of log adult mass
mass_dat <- dat %>%
  ungroup() %>%
  select(taxon, adult_mass_g) %>%
  group_by(taxon) %>%
  summarize(mass = mean(log(adult_mass_g)))

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
speed_het <- fit_mod(dat, 'hab_contrast_sc', '(hab_contrast_sc | taxon/study)', 'speed_log')
speed_simps <- fit_mod(dat, 'simpsons_D_sc', '(simpsons_D_sc | taxon/study)', 'speed_log')
speed_pet <- fit_mod(dat, 'pet_sc', '(pet_sc | taxon/study)', 'speed_log')

# Population effects from models
# WILL NOT WORK IN FUNCTION because spread_draws tries to evaluate a string
# passed as an argument... Maybe the solution is to rename the variable name 
# in the model within the function?

speed_het_fixed <- speed_het %>%
  spread_draws(b_hab_contrast_sc) %>%
  median_qi(b_hab_contrast_sc) %>%
  rename('slope' = b_hab_contrast_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'heterogeneity')

speed_pet_fixed <- speed_pet %>%
  spread_draws(b_pet_sc) %>%
  median_qi(b_pet_sc) %>%
  rename('slope' = b_pet_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'evapotranspiration')

speed_simps_fixed <- speed_simps %>%
  spread_draws(b_simpsons_D_sc) %>%
  median_qi(b_simpsons_D_sc) %>%
  rename('slope' = b_simpsons_D_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'habitat diversity')

# Bind together
all_fixed_vars <- speed_het_fixed %>%
  rbind(speed_pet_fixed) %>%
  rbind(speed_simps_fixed)

# Same for individual slopes... maybe rename the variable in the function?
# Individual slopes
speed_het_vars <- speed_het %>%
  spread_draws(b_hab_contrast_sc, r_taxon[study, term]) %>%
  mutate(species_m = b_hab_contrast_sc + r_taxon) %>%
  filter(term == 'hab_contrast_sc') %>%
  arrange(study) %>%
  group_by(study) %>% 
  summarize(mean_slope = mean(species_m), se_slope = sd(species_m)/sqrt(length(species_m))*1.96) %>%
  mutate(var = 'heterogeneity') %>%
  rename('taxon' = study) %>%
  mutate(lifestyle = ifelse(taxon %in% bats, 'bat', 'terrestrial')) %>%
  left_join(mass_dat)

speed_pet_vars <- speed_pet %>%
  spread_draws(b_pet_sc, r_taxon[study, term]) %>%
  mutate(species_m = b_pet_sc + r_taxon) %>%
  filter(term == 'pet_sc') %>%
  arrange(study) %>%
  group_by(study) %>% 
  summarize(mean_slope = mean(species_m), se_slope = sd(species_m)/sqrt(length(species_m))*1.96) %>%
  mutate(var = 'evapotranspiration') %>%
  rename('taxon' = study) %>%
  mutate(lifestyle = ifelse(taxon %in% bats, 'bat', 'terrestrial')) %>%
  left_join(mass_dat)

speed_simps_vars <- speed_simps %>%
  spread_draws(b_simpsons_D_sc, r_taxon[study, term]) %>%
  mutate(species_m = b_simpsons_D_sc + r_taxon) %>%
  filter(term == 'simpsons_D_sc') %>%
  arrange(study) %>%
  group_by(study) %>% 
  summarize(mean_slope = mean(species_m), se_slope = sd(species_m)/sqrt(length(species_m))*1.96) %>%
  mutate(var = 'habitat diversity') %>%
  rename('taxon' = study) %>%
  mutate(lifestyle = ifelse(taxon %in% bats, 'bat', 'terrestrial')) %>%
  left_join(mass_dat)

all_rand_vars <- speed_het_vars %>%
  rbind(speed_pet_vars) %>%
  rbind(speed_simps_vars)

# Plot
ggplot() + 
  geom_hline(yintercept = 0) + 
  geom_jitter(data = all_rand_vars, aes(x = var, y = mean_slope, colour = lifestyle, size = mass), 
              alpha = 0.3) + 
  scale_color_manual(values = c('#1e1d22', '#00ab66')) +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank()) +
  geom_linerange(data = all_fixed_vars, 
                 aes(x = var, ymin = .lower, ymax = .upper),
                 lwd = 2.5) +
  geom_pointrange(data = all_fixed_vars,
                  aes(x = var, y = slope, ymin = .lower, ymax = .upper),
                  lwd = 1, shape = 21, fill = 'white', stroke = 3) +
  coord_flip() + ylab('slope')


# Save
saveRDS(speed_het, 'model_files/speed_het.rds')
saveRDS(speed_simps, 'model_files/speed_simps.rds')
saveRDS(speed_pet, 'model_files/speed_pet.rds')
