
library(tidyverse)

# 06 Plots ====
# Plot movement on environment, GD on environment, GD on movement

# Load data
dat <- readRDS('output/prepped_data.rds')

# Pivot environmental variables for plotting
plot_dat <- dat %>%
  pivot_longer(cols = c(hab_contrast_sc, simpsons_D_sc, pet_sc), names_to = 'env_char') %>%
  mutate(env_char = factor(env_char),
         study = factor(study))

# Effects on speed
ggplot(plot_dat, aes(x = value, y = log_speed, colour = taxon)) + 
  geom_point() + 
  # geom_smooth(method = 'lm', se = F) +
  scale_colour_viridis_d() +
  facet_wrap(~env_char, scales = 'free_x') +
  ylab('Log speed (m/h)') + xlab('Scaled value') +
  theme(legend.position = 'none')

# Speed
ggplot(plot_dat, aes(x = log(adult_mass_g), y = log_speed)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(plot_dat, aes(x = log(adult_mass_g), y = log_hr_area_d)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

# Effects on MCP area
ggplot(plot_dat, aes(x = value, y = log_hr_area_d, colour = taxon)) + 
  geom_point() + 
  # geom_smooth(method = 'lm', se = F) +
  scale_colour_viridis_d() +
  facet_wrap(~env_char, scales = 'free_x') +
  theme(legend.position = 'none') +
  ylab('Log home range area (m2)') + xlab('Scaled value')

