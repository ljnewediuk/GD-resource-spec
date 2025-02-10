
# 10 Main Plots ====
# Orchard plots of movement ~ environment
# Scatterplot of Fst ~ environment

library(tidyverse)
library(cowplot)
library(png)
library(rnaturalearth)

# Load data
rand_vars <- readRDS('output/movement_rand_draws.rds') %>%
  mutate(response = factor(response, 
                           levels = c('speed', 'hr'), 
                           labels = c('Speed', 'Home range size'))) 

fixed_vars <- readRDS('output/movement_fixed_draws.rds') %>%
  mutate(response = factor(response, 
                           levels = c('speed', 'hr'), 
                           labels = c('Speed', 'Home range size')))  

# Moran's I vals and global map from rnaturalearth
map_dat <- readRDS('output/global_morans_vals.rds')
morans_df <- readRDS('output/global_morans_shared_var.rds')
global_map <- ne_countries(returnclass = 'sf')

# Orchard plot for main text
ggplot() + 
  geom_hline(yintercept = 0) + 
  geom_jitter(data = rand_vars, 
              aes(x = var, y = mean_slope, colour = move_mode, size = log(adult_mass_g)),
              alpha = 0.4) + 
  scale_color_manual(values = c('#009ce9', '#B29784')) +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank()) +
  geom_linerange(data = fixed_vars, 
                 aes(x = var, ymin = .lower, ymax = .upper),
                 lwd = 2.5) +
  geom_pointrange(data = fixed_vars,
                  aes(x = var, y = slope, ymin = .lower, ymax = .upper),
                  lwd = 1, shape = 21, fill = 'white', stroke = 3) +
  coord_flip() + ylab('slope') +
  theme(panel.background = element_rect(colour = 'black', fill = 'white', linewidth = 1),
        axis.text.y = element_text(size = 18, colour = 'black'), 
        axis.text.x = element_text(size = 18, colour = 'black'),
        axis.title.x = element_text(size = 18, vjust = -3),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.spacing = unit(1, 'cm'),
        strip.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 18, colour = 'black')) +
  ylab('Slope estimate') +
  facet_wrap(~ response, scales = 'free_x')

# Save
ggsave('orchard_plot.tiff', last_plot(), path = 'figures/', device = 'tiff', dpi = 300, width = 10, height = 5, units = 'in')

# Moran's eigenvector maps for species richness and home range size

# Home ranges
hr_map <- ggplot() + 
  geom_sf(data = global_map, fill = '#C0C0C0', colour = '#C0C0C0') + 
  geom_sf(data = map_dat[map_dat$var == 'Home range size',], aes(colour = fitted), size = 3) +
  scale_colour_viridis_c(breaks = c(-0.2, .6), 
                         labels = c('Small', 'Large'), option = 'inferno') +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid = element_blank(),
        legend.position = c(.15, .3),
        legend.title = element_blank(),
        legend.direction = 'horizontal',
        legend.text = element_text(colour = 'black', size = 16),
        legend.key.width = unit(0.7, 'cm'),
        legend.background = element_rect(fill = 'white'))

# Species richness
spr_map <- ggplot() + 
  geom_sf(data = global_map, fill = '#C0C0C0', colour = '#C0C0C0') + 
  geom_sf(data = map_dat[map_dat$var == 'Species richness',], aes(colour = fitted), size = 3) +
  scale_colour_viridis_c(breaks = c(-1.1, 1.3), 
                         labels = c('Low', 'High'), option = 'inferno') +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid = element_blank(),
        legend.position = c(.15, .3),
        legend.title = element_blank(),
        legend.direction = 'horizontal',
        legend.text = element_text(colour = 'black', size = 16),
        legend.key.width = unit(0.7, 'cm'),
        legend.background = element_rect(fill = 'white'))

# Plot panels
map_grid <- plot_grid(spr_map, hr_map, labels = c('A', 'B'), 
                      label_size = 18, align = 'hv', ncol = 1)

# Global Moran's I stacked bar chart
morans_df_f <- morans_df %>%
  # Get only variance components for plotting
  filter(variance_component %in% c('ANS', 'ASS', 'ANSS')) %>%
  mutate(var = ifelse(var == 'mean_size', 'Home range size', 'Species richness'),
         variance_component = factor(variance_component, 
                                     levels = c('ASS', 'ANSS', 'ANS'),
                                     labels = c('Shared spatial', 'Non-shared spatial',
                                                'Non-spatial')))
# Plot
bar_var <- ggplot(morans_df_f, aes(x = var, y = value, fill = variance_component)) +
  geom_bar(stat = 'identity') +
  ylab('Proportion of variation') +
  scale_fill_manual(values = c('#EEA63D', '#C15249', '#C0C0C0')) +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 1), 'cm'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour = 'black', size = 17, vjust = 5),
        axis.text = element_text(colour = 'black', size = 17),
        axis.ticks = element_line(colour = 'black'),
        axis.line = element_line(colour = 'black'),
        legend.justification = c(0,0),
        legend.position = 'top',
        legend.direction = 'vertical',
        legend.title = element_blank(),
        legend.text = element_text(colour = 'black', size = 16),
        legend.key.width = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm'),
        legend.background = element_rect(fill = 'white'))

plot_grid(map_grid, bar_var, labels = c('', 'C'), 
          label_size = 18, rel_widths = c(1, 0.85), ncol = 2)

# Save
ggsave('mem_map.tiff', last_plot(), path = 'figures/', device = 'tiff', dpi = 300, width = 12, height = 6, units = 'in', bg = 'white')
