
# 06 Plots ====
# Plot movement on environment, GD on environment, GD on movement

# Load data
# Landscape
lc_summary <- readRDS('output/lc_summary.rds') %>%
  mutate(across(simpsons_D:pet, list(sc = function(x) as.vector(scale(x, center = T)))))
# Movement
movement_summary <- readRDS('output/movement_summary.rds')
# Pantheria body mass
panther <- read.csv('input/imputation_phylo_1.csv') %>%
  mutate(taxon = tolower(gsub(' ', '_', phylacine_binomial))) %>%
  select(taxon, adult_mass_g)

# Combine data
all_dat <- movement_summary %>%
  left_join(lc_summary) %>%
  mutate(speed = ifelse(time_units == 'secs', speed*3600, speed),
         speed = ifelse(time_units == 'mins', speed*60, speed),
         speed = ifelse(time_units == 'days', speed/24, speed),
         hr_area = ifelse(time_units == 'secs', hr_area*3600, hr_area),
         hr_area = ifelse(time_units == 'mins', hr_area*60, hr_area),
         hr_area = ifelse(time_units == 'days', hr_area/24, hr_area)) %>%
  mutate(log_speed = log(abs(speed+.001))) %>%
  mutate(log_hr_area = log(hr_area+.001)) %>%
  mutate(intensity_use = speed/(hr_area^2)) %>%
  filter(! taxon %in% 'nyctalus_noctula') %>%
  pivot_longer(cols = c(hab_contrast_sc, simpsons_D_sc, pet_sc), names_to = 'env_char') %>%
  mutate(env_char = factor(env_char),
         study = factor(study)) %>%
  left_join(panther)
  # NOTE: below lines depend on having checked which species are in common
  # between movement and genetic diversity data in 03-GD-Movement-Intersect
  # mutate(env_char = factor(env_char),
  #        taxon = ifelse(! taxon %in% tolower(mv_gd_mtch), 'other', taxon)) %>%
  # filter(! taxon  == 'other')

# Model lm
mod <- glmmTMB::glmmTMB(log_speed ~ value +  (value | taxon), data = all_dat[all_dat$env_char == 'simpsons_D_sc',])

# Effects on speed
ggplot(all_dat, aes(x = value, y = log_speed, colour = taxon)) + 
  geom_point() + 
  facet_wrap(~env_char, scales = 'free_x') + 
  geom_smooth(method = 'lm')
ggplot(all_dat, aes(x = log(adult_mass_g), y = log_speed)) + 
  geom_point() + 
  facet_wrap(~env_char, scales = 'free_x') + 
  geom_smooth(method = 'lm')

# Effects on MCP area
ggplot(all_dat, aes(x = value, y = log_hr_area)) + 
  geom_point() + 
  facet_wrap(~env_char, scales = 'free_x') + 
  geom_smooth(method = 'lm')

# Effects on use intensity
ggplot(all_dat, aes(x = value, y = log(intensity_use))) + 
  geom_point() +
  facet_wrap(~env_char, scales = 'free')


