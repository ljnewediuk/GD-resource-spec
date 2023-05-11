
library(tidyverse)
library(cowplot)

# 06 Plots ====
# Orchard plots of movement ~ environment
# Scatterplot of Fst ~ environment

# Load data
rand_vars <- readRDS('output/movement_rand_draws.rds') %>%
  mutate(response = factor(response, 
                           levels = c('speed', 'hr'), 
                           labels = c('Speed', 'Home range size')))
fixed_vars <- readRDS('output/movement_fixed_draws.rds') %>%
  mutate(response = factor(response, 
                           levels = c('speed', 'hr'), 
                           labels = c('Speed', 'Home range size')))
fst_dat <- readRDS('output/prepped_fst_data.rds')

# Get mean of dispersal, hr, and fst for unscaling
mean_disp <- mean(fst_dat$dispersal_km, na.rm = T)
sd_disp <- sd(fst_dat$dispersal_km, na.rm = T)
mean_hr <- mean(fst_dat$log_hr_km2, na.rm = T)
sd_hr <- sd(fst_dat$log_hr_km2, na.rm = T)
mean_fst <- mean(fst_dat$global_fst, na.rm = T)
sd_fst <- sd(fst_dat$global_fst, na.rm = T)

fst_hr_draws <- readRDS('output/fst_hr_draws.rds') %>%
  mutate(fst = .prediction * sd_fst + mean_fst)

fst_disp_draws <- readRDS('output/fst_disp_draws.rds') %>%
  mutate(dispersal_km = dispersal_km_sc * sd_disp + mean_disp,
         fst = .prediction * sd_fst + mean_fst)

# Orchard plot
ggplot() + 
  geom_hline(yintercept = 0) + 
  geom_jitter(data = rand_vars, 
              aes(x = var, y = mean_slope, colour = lifestyle, size = log(adult_mass_g)),
              alpha = 0.3) + 
  scale_color_manual(values = c('#ff3442', '#009ce9')) +
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
ggsave('orchard_plot.tiff', last_plot(), path = 'figures/', device = 'tiff', dpi = 300, width = 10, height = 6, units = 'in')

# Panel scatterplots for Fst models
disp_plot <- ggplot() +
  ggdist::stat_lineribbon(data = fst_disp_draws, aes(x = dispersal_km, y = fst), 
                          .width = c(0.95, 0.8, 0.5), alpha = 0.6) +
  geom_point(data = fst_dat, aes(x = dispersal_km, y = global_fst), alpha = 0.5) +
  scale_fill_brewer(palette = 'Blues') +
  theme(panel.background = element_rect(colour = 'white', fill = 'white', linewidth = 1),
        axis.line.x = element_line(colour = 'black'),
        axis.line.y = element_line(colour = 'black'),
        axis.text.y = element_text(size = 18, colour = 'black'), 
        axis.text.x = element_text(size = 18, colour = 'black'),
        axis.title.x = element_text(size = 18, vjust = -3),
        axis.title.y = element_text(size = 18, vjust = 5),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm')) +
  xlab('Maximum dispersal distance (km)') + ylab(bquote('Global '~italic(F)[ST]))

hr_plot <- ggplot() +
  ggdist::stat_lineribbon(data = fst_hr_draws, aes(x = log_hr_km2, y = fst), 
                          .width = c(0.95, 0.8, 0.5), alpha = 0.6) +
  geom_point(data = fst_dat, aes(x = log_hr_km2, y = global_fst), alpha = 0.5) +
  scale_fill_brewer(palette = 'Reds') +
  theme(panel.background = element_rect(colour = 'white', fill = 'white', linewidth = 1),
        axis.line.x = element_line(colour = 'black'),
        axis.line.y = element_line(colour = 'black'),
        axis.text.y = element_text(size = 18, colour = 'black'), 
        axis.text.x = element_text(size = 18, colour = 'black'),
        axis.title.x = element_text(size = 18, vjust = -2.5),
        axis.title.y = element_blank(),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm')) +
  xlab(bquote('log Home range size'~(km^2)))

panel_brms <- plot_grid(disp_plot, hr_plot, labels = c('A', 'B'), label_size = 18, align = 'hv')

# Save
ggsave('brms_scatter.tiff', last_plot(), path = 'figures/', device = 'tiff', dpi = 300, width = 10, height = 6, units = 'in')
