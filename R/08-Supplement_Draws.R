
library(tidyverse)
library(tidybayes)
library(brms)
library(emmeans)
library(modelr)
library(bayestestR)
library(performance)

# List models
mod_file_names_s <- list.files('model_files/supplement')

# Load all main text models then print bayes R2 and probability of direction
for(n in 1:length(mod_file_names_s)) {
  mod <- readRDS(paste0('model_files/supplement/', mod_file_names_s[n]))
  # Print model name and R2
  print(str_extract(mod_file_names_s[n], "[^.]+"))
  print(r2_bayes(mod))
  cat('\n')
  print(p_direction(mod))
  cat('\n', '\n')
  # Assign object to name
  assign(str_extract(mod_file_names_s[n], "[^.]+"), mod)
}

# Load model data
mod_dat <- readRDS('output/model_data.rds')

# DRAWS FROM POSTERIOR FOR PLOTTING

# POPULATION EFFECTS FROM MOVEMENT MODE MODELS

# Start data frame for collecting effect sizes
mm_draws <- data.frame()

# Get posterior predictive difference for speed ~ habitat heterogeneity*movement mode

# Speed ~ habitat heterogeneity*movement mode
# Movement mode flying
speed_het_mm_fly <- speed_het_mm %>%
  spread_draws(b_hab_contrast_sc) %>%
  median_qi(b_hab_contrast_sc) %>%
  rename('slope' = b_hab_contrast_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat contrast',
         response = 'speed',
         mm = 'flying')
# Movement mode ground
speed_het_mm_gr <- speed_het_mm %>%
  spread_draws(b_hab_contrast_sc, `b_hab_contrast_sc:move_modeground`) %>%
  mutate(b_gr = b_hab_contrast_sc + `b_hab_contrast_sc:move_modeground`) %>%
  median_qi(b_gr) %>%
  rename('slope' = b_gr) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat contrast',
         response = 'speed',
         mm = 'ground')

# Bind with effect sizes
mm_draws <- mm_draws %>% 
  rbind(speed_het_mm_fly) %>%
  rbind(speed_het_mm_gr)

# Speed ~ habitat diversity*movement mode
# Movement mode flying
speed_simps_mm_fly <- speed_simps_mm %>%
  spread_draws(b_simpsons_D_sc) %>%
  median_qi(b_simpsons_D_sc) %>%
  rename('slope' = b_simpsons_D_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat diversity',
         response = 'speed',
         mm = 'flying')
# Movement mode ground
speed_simps_mm_gr <- speed_simps_mm %>%
  spread_draws(b_simpsons_D_sc, `b_simpsons_D_sc:move_modeground`) %>%
  mutate(b_gr = b_simpsons_D_sc + `b_simpsons_D_sc:move_modeground`) %>%
  median_qi(b_gr) %>%
  rename('slope' = b_gr) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat diversity',
         response = 'speed',
         mm = 'ground')

# Bind with effect sizes
mm_draws <- mm_draws %>% 
  rbind(speed_simps_mm_fly) %>%
  rbind(speed_simps_mm_gr)

# Speed ~ Potential evapotranspiration*movement mode
# Movement mode flying
speed_pet_mm_fly <- speed_pet_mm %>%
  spread_draws(b_pet_sc) %>%
  median_qi(b_pet_sc) %>%
  rename('slope' = b_pet_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Potential evapotranspiration',
         response = 'speed',
         mm = 'flying')
# Movement mode ground
speed_pet_mm_gr <- speed_pet_mm %>%
  spread_draws(b_pet_sc, `b_pet_sc:move_modeground`) %>%
  mutate(b_gr = b_pet_sc + `b_pet_sc:move_modeground`) %>%
  median_qi(b_gr) %>%
  rename('slope' = b_gr) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Potential evapotranspiration',
         response = 'speed',
         mm = 'ground')

# Bind with effect sizes
mm_draws <- mm_draws %>% 
  rbind(speed_pet_mm_fly) %>%
  rbind(speed_pet_mm_gr)

# Home range size ~ habitat heterogeneity*movement mode
# Movement mode flying
hr_het_mm_fly <- hr_het_mm %>%
  spread_draws(b_hab_contrast_sc) %>%
  median_qi(b_hab_contrast_sc) %>%
  rename('slope' = b_hab_contrast_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat contrast',
         response = 'hr',
         mm = 'flying')
# Movement mode ground
hr_het_mm_gr <- hr_het_mm %>%
  spread_draws(b_hab_contrast_sc, `b_hab_contrast_sc:move_modeground`) %>%
  mutate(b_gr = b_hab_contrast_sc + `b_hab_contrast_sc:move_modeground`) %>%
  median_qi(b_gr) %>%
  rename('slope' = b_gr) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat contrast',
         response = 'hr',
         mm = 'ground')

# Bind with effect sizes
mm_draws <- mm_draws %>% 
  rbind(hr_het_mm_fly) %>%
  rbind(hr_het_mm_gr)

# Speed ~ habitat diversity*movement mode
# Movement mode flying
hr_simps_mm_fly <- hr_simps_mm %>%
  spread_draws(b_simpsons_D_sc) %>%
  median_qi(b_simpsons_D_sc) %>%
  rename('slope' = b_simpsons_D_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat diversity',
         response = 'hr',
         mm = 'flying')
# Movement mode ground
hr_simps_mm_gr <- hr_simps_mm %>%
  spread_draws(b_simpsons_D_sc, `b_simpsons_D_sc:move_modeground`) %>%
  mutate(b_gr = b_simpsons_D_sc + `b_simpsons_D_sc:move_modeground`) %>%
  median_qi(b_gr) %>%
  rename('slope' = b_gr) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat diversity',
         response = 'hr',
         mm = 'ground')

# Bind with effect sizes
mm_draws <- mm_draws %>% 
  rbind(hr_simps_mm_fly) %>%
  rbind(hr_simps_mm_gr)

# Speed ~ Potential evapotranspiration*movement mode
# Movement mode flying
hr_pet_mm_fly <- hr_pet_mm %>%
  spread_draws(b_pet_sc) %>%
  median_qi(b_pet_sc) %>%
  rename('slope' = b_pet_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Potential evapotranspiration',
         response = 'hr',
         mm = 'flying')
# Movement mode ground
hr_pet_mm_gr <- hr_pet_mm %>%
  spread_draws(b_pet_sc, `b_pet_sc:move_modeground`) %>%
  mutate(b_gr = b_pet_sc + `b_pet_sc:move_modeground`) %>%
  median_qi(b_gr) %>%
  rename('slope' = b_gr) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Potential evapotranspiration',
         response = 'hr',
         mm = 'ground')

# Bind with effect sizes
mm_draws <- mm_draws %>% 
  rbind(hr_pet_mm_fly) %>%
  rbind(hr_pet_mm_gr)


# POPULATION EFFECTS FROM BODY SIZE MODELS

# Start data frame for collecting effect sizes
bm_draws <- data.frame()

# Speed ~ habitat heterogeneity*body mass
# Without interaction
speed_het_bm_noint <- speed_het_bm %>%
  spread_draws(b_hab_contrast_sc) %>%
  median_qi(b_hab_contrast_sc) %>%
  rename('slope' = b_hab_contrast_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat contrast',
         response = 'speed',
         bm_effect = 'no')
# With interaction
speed_het_bm_int <- speed_het_bm %>%
  spread_draws(b_hab_contrast_sc, `b_hab_contrast_sc:adult_mass_g_sc`) %>%
  mutate(b_gr = b_hab_contrast_sc + `b_hab_contrast_sc:adult_mass_g_sc`) %>%
  median_qi(b_gr) %>%
  rename('slope' = b_gr) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat contrast',
         response = 'speed',
         bm_effect = 'yes')

# Bind with effect sizes
bm_draws <- bm_draws %>% 
  rbind(speed_het_bm_noint) %>%
  rbind(speed_het_bm_int)

# Speed ~ habitat diversity*body mass
# Without interaction
speed_simps_bm_noint <- speed_simps_bm %>%
  spread_draws(b_simpsons_D_sc) %>%
  median_qi(b_simpsons_D_sc) %>%
  rename('slope' = b_simpsons_D_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat diversity',
         response = 'speed',
         bm_effect = 'no')

# With interaction
speed_simps_bm_int <- speed_simps_bm %>%
  spread_draws(b_simpsons_D_sc, `b_simpsons_D_sc:adult_mass_g_sc`) %>%
  mutate(b_gr = b_simpsons_D_sc + `b_simpsons_D_sc:adult_mass_g_sc`) %>%
  median_qi(b_gr) %>%
  rename('slope' = b_gr) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat diversity',
         response = 'speed',
         bm_effect = 'yes')

# Bind with effect sizes
bm_draws <- bm_draws %>% 
  rbind(speed_simps_bm_noint) %>%
  rbind(speed_simps_bm_int)

# Speed ~ Potential evapotranspiration*body mass
# Without interaction
speed_pet_bm_noint <- speed_pet_bm %>%
  spread_draws(b_pet_sc) %>%
  median_qi(b_pet_sc) %>%
  rename('slope' = b_pet_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Potential evapotranspiration',
         response = 'speed',
         bm_effect = 'no')

# With interaction
speed_pet_bm_int <- speed_pet_bm %>%
  spread_draws(b_pet_sc, `b_pet_sc:adult_mass_g_sc`) %>%
  mutate(b_gr = b_pet_sc + `b_pet_sc:adult_mass_g_sc`) %>%
  median_qi(b_gr) %>%
  rename('slope' = b_gr) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Potential evapotranspiration',
         response = 'speed',
         bm_effect = 'yes')

# Bind with effect sizes
bm_draws <- bm_draws %>% 
  rbind(speed_pet_bm_noint) %>%
  rbind(speed_pet_bm_int)

# Home range size ~ habitat heterogeneity*body mass
# Without interaction
hr_het_bm_noint <- hr_het_bm %>%
  spread_draws(b_hab_contrast_sc) %>%
  median_qi(b_hab_contrast_sc) %>%
  rename('slope' = b_hab_contrast_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat contrast',
         response = 'hr',
         bm_effect = 'no')
# With interaction
hr_het_bm_int <- hr_het_bm %>%
  spread_draws(b_hab_contrast_sc, `b_hab_contrast_sc:adult_mass_g_sc`) %>%
  mutate(b_gr = b_hab_contrast_sc + `b_hab_contrast_sc:adult_mass_g_sc`) %>%
  median_qi(b_gr) %>%
  rename('slope' = b_gr) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat contrast',
         response = 'hr',
         bm_effect = 'yes')

# Bind with effect sizes
bm_draws <- bm_draws %>% 
  rbind(hr_het_bm_noint) %>%
  rbind(hr_het_bm_int)

# Speed ~ habitat diversity*body mass
# Without interaction
hr_simps_bm_noint <- hr_simps_bm %>%
  spread_draws(b_simpsons_D_sc) %>%
  median_qi(b_simpsons_D_sc) %>%
  rename('slope' = b_simpsons_D_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat diversity',
         response = 'hr',
         bm_effect = 'no')

# With interaction
hr_simps_bm_int <- hr_simps_bm %>%
  spread_draws(b_simpsons_D_sc, `b_simpsons_D_sc:adult_mass_g_sc`) %>%
  mutate(b_gr = b_simpsons_D_sc + `b_simpsons_D_sc:adult_mass_g_sc`) %>%
  median_qi(b_gr) %>%
  rename('slope' = b_gr) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Habitat diversity',
         response = 'hr',
         bm_effect = 'yes')

# Bind with effect sizes
bm_draws <- bm_draws %>% 
  rbind(hr_simps_bm_noint) %>%
  rbind(hr_simps_bm_int)

# Speed ~ Potential evapotranspiration*body mass
# Without interaction
hr_pet_bm_noint <- hr_pet_bm %>%
  spread_draws(b_pet_sc) %>%
  median_qi(b_pet_sc) %>%
  rename('slope' = b_pet_sc) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Potential evapotranspiration',
         response = 'hr',
         bm_effect = 'no')

# With interaction
hr_pet_bm_int <- hr_pet_bm %>%
  spread_draws(b_pet_sc, `b_pet_sc:adult_mass_g_sc`) %>%
  mutate(b_gr = b_pet_sc + `b_pet_sc:adult_mass_g_sc`) %>%
  median_qi(b_gr) %>%
  rename('slope' = b_gr) %>%
  select(slope, .lower, .upper) %>%
  mutate(var = 'Potential evapotranspiration',
         response = 'hr',
         bm_effect = 'yes')

# Bind with effect sizes
bm_draws <- bm_draws %>% 
  rbind(hr_pet_bm_noint) %>%
  rbind(hr_pet_bm_int)

# HYPOTHESIS TESTING

# Hypothesis testing for interaction effect in movement mode model
speed_het_mm_hyp <- hypothesis(speed_het_mm, '`hab_contrast_sc:move_modeground` > 0')[['hypothesis']] %>%
  mutate(var = 'Habitat contrast', response = 'speed')
speed_simps_mm_hyp <- hypothesis(speed_simps_mm, '`simpsons_D_sc:move_modeground` > 0')[['hypothesis']] %>%
  mutate(var = 'Habitat diversity', response = 'speed')
speed_pet_mm_hyp <- hypothesis(speed_pet_mm, '`pet_sc:move_modeground` > 0')[['hypothesis']] %>%
  mutate(var = 'Potential evapotranspiration', response = 'speed')
hr_het_mm_hyp <- hypothesis(hr_het_mm, '`hab_contrast_sc:move_modeground` > 0')[['hypothesis']] %>%
  mutate(var = 'Habitat contrast', response = 'hr')
hr_simps_mm_hyp <- hypothesis(hr_simps_mm, '`simpsons_D_sc:move_modeground` > 0')[['hypothesis']] %>%
  mutate(var = 'Habitat diversity', response = 'hr')
hr_pet_mm_hyp <- hypothesis(hr_pet_mm, '`pet_sc:move_modeground` > 0')[['hypothesis']] %>%
  mutate(var = 'Potential evapotranspiration', response = 'hr')
# Bind together
mm_hyps <- speed_het_mm_hyp %>%
  rbind(speed_simps_mm_hyp) %>%
  rbind(speed_pet_mm_hyp) %>%
  rbind(hr_het_mm_hyp) %>%
  rbind(hr_simps_mm_hyp) %>%
  rbind(hr_pet_mm_hyp)

# Hypothesis testing for interaction effect in body mass model
speed_het_bm_hyp <- hypothesis(speed_het_bm, '`hab_contrast_sc:adult_mass_g_sc` > 0')[['hypothesis']] %>%
  mutate(var = 'Habitat contrast', response = 'speed')
speed_simps_bm_hyp <- hypothesis(speed_simps_bm, '`simpsons_D_sc:adult_mass_g_sc` > 0')[['hypothesis']] %>%
  mutate(var = 'Habitat diversity', response = 'speed')
speed_pet_bm_hyp <- hypothesis(speed_pet_bm, '`pet_sc:adult_mass_g_sc` > 0')[['hypothesis']] %>%
  mutate(var = 'Potential evapotranspiration', response = 'speed')
hr_het_bm_hyp <- hypothesis(hr_het_bm, '`hab_contrast_sc:adult_mass_g_sc` > 0')[['hypothesis']] %>%
  mutate(var = 'Habitat contrast', response = 'hr')
hr_simps_bm_hyp <- hypothesis(hr_simps_bm, '`simpsons_D_sc:adult_mass_g_sc` > 0')[['hypothesis']] %>%
  mutate(var = 'Habitat diversity', response = 'hr')
hr_pet_bm_hyp <- hypothesis(hr_pet_bm, '`pet_sc:adult_mass_g_sc` > 0')[['hypothesis']] %>%
  mutate(var = 'Potential evapotranspiration', response = 'hr')
# Bind together
bm_hyps <- speed_het_bm_hyp %>%
  rbind(speed_simps_bm_hyp) %>%
  rbind(speed_pet_bm_hyp) %>%
  rbind(hr_het_bm_hyp) %>%
  rbind(hr_simps_bm_hyp) %>%
  rbind(hr_pet_bm_hyp)

# SAVE DRAWS AND TESTS
saveRDS(bm_draws, 'output/body_mass_model_draws.rds')
saveRDS(mm_draws, 'output/movement_mode_model_draws.rds')
saveRDS(bm_hyps, 'output/body_mass_model_htests.rds')
saveRDS(mm_hyps, 'output/movement_mode_model_htests.rds')

