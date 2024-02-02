
library(tidyverse)
library(cowplot)

# 10 Supplementary Plots ====
# Supplementary orchard plots

# Load data

# Movement mode models
mm_draws <- readRDS('output/movement_mode_model_draws.rds') %>%
  # Add variable for specific models and factor to arrange in order
  mutate(model = paste(response, tolower(var), mm)) %>%
  mutate(model = factor(model, 
                        levels = c('speed potential evapotranspiration flying',
                                   'speed potential evapotranspiration ground',
                                   'speed habitat diversity flying',
                                   'speed habitat diversity ground',
                                   'speed habitat contrast flying',
                                   'speed habitat contrast ground',
                                   'hr potential evapotranspiration flying',
                                   'hr potential evapotranspiration ground',
                                   'hr habitat diversity flying',
                                   'hr habitat diversity ground',
                                   'hr habitat contrast flying',
                                   'hr habitat contrast ground')), rank = 1:12)

# Movement mode difference hypothesis tests
mm_hyps <- readRDS('output/movement_mode_model_htests.rds') %>%
  # Factor to arrange in order
  mutate(model = paste(response, tolower(var), sep = ' ~ ')) %>%
  mutate(model = factor(model,
                        levels = c('hr ~ habitat contrast', 'hr ~ habitat diversity',
                                   'hr ~ potential evapotranspiration',
                                   'speed ~ habitat contrast', 'speed ~ habitat diversity',
                                   'speed ~ potential evapotranspiration')), rank = 1:6)

# Body mass models
bm_draws <- readRDS('output/body_mass_model_draws.rds') %>%
  # Factor to arrange in order
  mutate(model = paste(response, tolower(var), bm_effect)) %>%
  mutate(model = factor(model, 
                        levels = c('speed potential evapotranspiration yes',
                                   'speed potential evapotranspiration no',
                                   'speed habitat diversity yes',
                                   'speed habitat diversity no',
                                   'speed habitat contrast yes',
                                   'speed habitat contrast no',
                                   'hr potential evapotranspiration yes',
                                   'hr potential evapotranspiration no',
                                   'hr habitat diversity yes',
                                   'hr habitat diversity no',
                                   'hr habitat contrast yes',
                                   'hr habitat contrast no')), rank = 1:12)

# Body mass hypothesis tests
bm_hyps <- readRDS('output/body_mass_model_htests.rds') %>%
  # Factor to arrange in order
  mutate(model = paste(response, tolower(var), sep = ' ~ ')) %>%
  mutate(model = factor(model,
                        levels = c('hr ~ habitat contrast', 'hr ~ habitat diversity',
                                   'hr ~ potential evapotranspiration',
                                   'speed ~ habitat contrast', 'speed ~ habitat diversity',
                                   'speed ~ potential evapotranspiration')), rank = 1:6)

# Plots

# Movement mode panel orchard plot

# Effect sizes
orchard_effects_mm <- ggplot() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 2.5, linetype = 'dashed') +
  geom_vline(xintercept = 4.5, linetype = 'dashed') +
  geom_vline(xintercept = 6.5, linetype = 'dashed') +
  geom_vline(xintercept = 8.5, linetype = 'dashed') +
  geom_vline(xintercept = 10.5, linetype = 'dashed') +
  geom_linerange(data = mm_draws, 
                 aes(x = rank, ymin = .lower, ymax = .upper, colour = mm),
                 lwd = 2.5) +
  geom_pointrange(data = mm_draws,
                  aes(x = rank, y = slope, ymin = .lower, ymax = .upper, colour = mm),
                  lwd = 1, shape = 21, fill = 'white', stroke = 3) +
  scale_color_manual(values = c('#009ce9', '#696348')) +
  coord_flip() + ylab('Estimate') +
  theme(panel.background = element_rect(colour = 'black', fill = 'white', linewidth = 1),
        axis.text.x = element_text(size = 18, colour = 'black'), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18, colour = 'black', vjust = -3),
        axis.ticks = element_blank(),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 0), 'cm'),
        panel.spacing = unit(1, 'cm'),
        strip.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 18, colour = 'black')) +
  ylab('Slope estimate') +
  ylim(-2.1, 2.1)

# Hypothesis tests
orchard_hyps_mm <- ggplot() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 1.6, linetype = 'dashed') +
  geom_vline(xintercept = 2.5, linetype = 'dashed') +
  geom_vline(xintercept = 3.5, linetype = 'dashed') +
  geom_vline(xintercept = 4.4, linetype = 'dashed') +
  geom_vline(xintercept = 5.3, linetype = 'dashed') +
  geom_linerange(data = mm_hyps, 
                 aes(x = rank, ymin = CI.Lower, ymax = CI.Upper),
                 lwd = 2.5) +
  geom_pointrange(data = mm_hyps,
                  aes(x = rank, y = Estimate, ymin = CI.Lower, ymax = CI.Upper),
                  lwd = 1, shape = 21, fill = 'white', stroke = 3) +
  scale_x_continuous(breaks = 1:6, 
                     labels = c('hr ~ habitat contrast', 'hr ~ habitat diversity',
                                'hr ~ potential evapotranspiration',
                                'speed ~ habitat contrast', 'speed ~ habitat diversity',
                                'speed ~ potential evapotranspiration')) +
  annotate(geom = 'text', y = c(-1, -1), x = c(.9, 3.9), label = c('*', '*'), size = 15) +
  coord_flip() + ylab('Estimate') +
  theme(panel.background = element_rect(colour = 'black', fill = 'white', linewidth = 1),
        axis.text.y = element_text(size = 18, colour = 'black'), 
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18, colour = 'black'),
        axis.title.x = element_text(size = 18, vjust = -3),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.spacing = unit(1, 'cm'),
        strip.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 18, colour = 'black')) +
  ylab('Slope estimate') +
  ylim(-2.1,2.1)

# Plot panels
plot_grid(NULL, orchard_hyps_mm, NULL, orchard_effects_mm, 
          rel_widths = c(0, 3, 0.2, 1), ncol = 4, align = 'h')

# Save
ggsave('movement_mode_orchard.tiff', last_plot(), path = 'figures/', device = 'tiff', dpi = 300, width = 10, height = 6, units = 'in')

# Body mass panel orchard plot

# Effect sizes
orchard_effects_bm <- ggplot() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 2.5, linetype = 'dashed') +
  geom_vline(xintercept = 4.5, linetype = 'dashed') +
  geom_vline(xintercept = 6.5, linetype = 'dashed') +
  geom_vline(xintercept = 8.5, linetype = 'dashed') +
  geom_vline(xintercept = 10.5, linetype = 'dashed') +
  geom_linerange(data = bm_draws, 
                 aes(x = rank, ymin = .lower, ymax = .upper, colour = bm_effect),
                 colour = 'black', lwd = 2.5) +
  geom_pointrange(data = bm_draws,
                  aes(x = rank, y = slope, ymin = .lower, ymax = .upper, size = bm_effect),
                  colour = 'black', lwd = 1, shape = 21, fill = 'white', stroke = 3) +
  scale_size_manual(values = c(0.5, 1.5)) +
  scale_linewidth_manual(values = c(1, 2.5)) +
  coord_flip() + ylab('Estimate') +
  theme(panel.background = element_rect(colour = 'black', fill = 'white', linewidth = 1),
        axis.text.x = element_text(size = 18, colour = 'black'), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18, colour = 'black', vjust = -3),
        axis.ticks = element_blank(),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 0), 'cm'),
        panel.spacing = unit(1, 'cm'),
        strip.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 18, colour = 'black')) +
  ylab('Slope estimate') +
  ylim(-1.5, 1.5)

# Hypothesis tests
orchard_hyps_bm <- ggplot() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 1.6, linetype = 'dashed') +
  geom_vline(xintercept = 2.5, linetype = 'dashed') +
  geom_vline(xintercept = 3.5, linetype = 'dashed') +
  geom_vline(xintercept = 4.4, linetype = 'dashed') +
  geom_vline(xintercept = 5.3, linetype = 'dashed') +
  geom_linerange(data = bm_hyps, 
                 aes(x = rank, ymin = CI.Lower, ymax = CI.Upper),
                 lwd = 2.5) +
  geom_pointrange(data = bm_hyps,
                  aes(x = rank, y = Estimate, ymin = CI.Lower, ymax = CI.Upper),
                  lwd = 1, shape = 21, fill = 'white', stroke = 3) +
  scale_x_continuous(breaks = 1:6, 
                     labels = c('hr ~ habitat contrast', 'hr ~ habitat diversity',
                                'hr ~ potential evapotranspiration',
                                'speed ~ habitat contrast', 'speed ~ habitat diversity',
                                'speed ~ potential evapotranspiration')) +
  coord_flip() + ylab('Estimate') +
  theme(panel.background = element_rect(colour = 'black', fill = 'white', linewidth = 1),
        axis.text.y = element_text(size = 18, colour = 'black'), 
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18, colour = 'black'),
        axis.title.x = element_text(size = 18, vjust = -3),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.spacing = unit(1, 'cm'),
        strip.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 18, colour = 'black')) +
  ylab('Slope estimate') +
  ylim(-1.5, 1.5)

# Plot panels
plot_grid(NULL, orchard_hyps_bm, NULL, orchard_effects_bm, 
          rel_widths = c(0, 3, 0.2, 1), ncol = 4, align = 'h')

# Save
ggsave('body_size_orchard.tiff', last_plot(), path = 'figures/', device = 'tiff', dpi = 300, width = 10, height = 6, units = 'in')

