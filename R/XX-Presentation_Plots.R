
library(tidyverse)
library(ggnewscale)
library(cowplot)
library(png)

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
  mutate(fst = .epred * sd_fst + mean_fst)

fst_disp_draws <- readRDS('output/fst_disp_draws.rds') %>%
  mutate(dispersal_km = dispersal_km_sc * sd_disp + mean_disp,
         fst = .epred * sd_fst + mean_fst)

# Orchard plot
ggplot() + 
  geom_jitter(data = rand_vars,
              aes(x = var, y = mean_slope, fill = var, colour = var),
              alpha = 0.6, shape = 21, size = 4) +
  scale_fill_manual(values = c('#d2ea57', '#d2ea57', '#d2ea57')) +
  scale_colour_manual(values = c('black', 'black', 'black')) +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank()) +
  geom_linerange(data = fixed_vars,
                 aes(x = var, ymin = .lower, ymax = .upper),
                 lwd = 4, colour = '#e5e5e5') +
  geom_pointrange(data = fixed_vars,
                  aes(x = var, y = slope, ymin = .lower, ymax = .upper),
                  lwd = 1, shape = 21, fill = '#36454f', colour = '#e5e5e5', stroke = 5) +
  geom_hline(yintercept = 0, colour = '#e5e5e5') + 
  coord_flip() + ylab('slope') +
  theme(plot.background = element_rect(fill = '#36454f', colour = '#36454f'), 
        panel.background = element_rect(colour = '#e5e5e5', fill = '#36454f', linewidth = 1),
        axis.ticks = element_line(colour = '#e5e5e5'),
        axis.text.y = element_text(size = 22, colour = '#e5e5e5'), 
        axis.text.x = element_text(size = 22, colour = '#e5e5e5'),
        axis.title.x = element_text(size = 22, vjust = -3, colour = '#e5e5e5'),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.spacing = unit(1, 'cm'),
        strip.background = element_rect(fill = '#36454f'),
        strip.text = element_text(size = 22, colour = '#e5e5e5')) +
  ylab('Slope estimate') +
  facet_wrap(~ response, scales = 'free_x')

# Save
ggsave('orchard_plot_ppt.tiff', last_plot(), path = 'figures/', device = 'tiff', dpi = 300, width = 13, height = 6, units = 'in')

# Panel scatterplots for Fst models
ggplot() +
  geom_point(data = fst_dat, aes(x = dispersal_km, y = global_fst), 
             shape = 21, fill = '#d2ea57', colour = 'black', alpha = 0.4, size = 3) +
  ggdist::stat_lineribbon(data = fst_disp_draws, aes(x = dispersal_km, y = fst), 
                          .width = 0.95, colour = 'white', fill = '#e5e5e550', linewidth = 0.5) +
  theme(plot.background = element_rect(fill = '#36454f', colour = '#36454f'),
        panel.background = element_rect(colour = '#e5e5e5', fill = '#36454f', linewidth = 1),
        axis.line.x = element_line(colour = '#e5e5e5'),
        axis.line.y = element_line(colour = '#e5e5e5'),
        axis.ticks = element_line(colour = '#e5e5e5'),
        axis.text.y = element_text(size = 22, colour = '#e5e5e5'), 
        axis.text.x = element_text(size = 22, colour = '#e5e5e5'),
        axis.title.x = element_text(size = 22, vjust = -3,  colour = '#e5e5e5'),
        axis.title.y = element_text(size = 22, vjust = 5,  colour = '#e5e5e5'),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm')) +
  xlab('Maximum dispersal distance (km)') + ylab(bquote('Population-specific '~italic(F)[ST]))

# Save
ggsave('scatter_fst_disp_ppt.tiff', last_plot(), path = 'figures/', device = 'tiff', dpi = 300, width = 8, height = 6, units = 'in')

ggplot() +
  geom_point(data = fst_dat, aes(x = log_hr_km2, y = global_fst), 
             shape = 21, colour = 'black', fill = '#d2ea57', alpha = 0.4, size = 3) +
  ggdist::stat_lineribbon(data = fst_hr_draws, aes(x = log_hr_km2, y = fst), 
                          .width = 0.95, colour = '#e5e5e5', fill = '#e5e5e550', linewidth = 0.5) +
  scale_fill_brewer(palette = 'Greys') +
  theme(plot.background = element_rect(fill = '#36454f', colour = '#36454f'),
        panel.background = element_rect(colour = '#e5e5e5', fill = '#36454f', linewidth = 1),
        axis.line.x = element_line(colour = '#e5e5e5'),
        axis.line.y = element_line(colour = '#e5e5e5'),
        axis.ticks = element_line(colour = '#e5e5e5'),
        axis.text.y = element_text(size = 22, colour = '#e5e5e5'), 
        axis.text.x = element_text(size = 22, colour = '#e5e5e5'),
        axis.title.x = element_text(size = 22, vjust = -3,  colour = '#e5e5e5'),
        axis.title.y = element_text(size = 22, vjust = 5,  colour = '#e5e5e5'),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm')) +
  xlab(bquote('log Home range size'~(km^2))) + ylab(bquote('Population-specific '~italic(F)[ST]))

# Save
ggsave('scatter_fst_hr_ppt.tiff', last_plot(), path = 'figures/', device = 'tiff', dpi = 300, width = 8, height = 6, units = 'in')

# Plot WOS hits including genetic diversity in past ~ 30 years

# Hits per year with "genetic diversity" as a keyword from top biodiversity
# conservation journals (Global Change Biology, Conservation Letters,
# Biological Conservation, Conservation Biology, Journal of Applied Ecology)
gd_hits <- data.frame(Year = c(1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 
                               1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 
                               2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 
                               2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 
                               2019, 2020, 2021, 2022),
                      Hits = c(159, 190, 219, 230, 246, 289, 322, 384, 446, 489, 
                               463, 418, 452, 519, 562, 497, 559, 637, 691, 716, 
                               703, 741, 761, 746, 760, 651, 803, 785, 849, 788, 
                               907, 1007, 988, 1044, 1021, 763),
                      With_GD = c(0, 0, 0, 2, 1, 5, 2, 10, 6, 11, 12, 8, 16, 13, 
                                  17, 15, 19, 18, 21, 17, 22, 21, 21, 27, 26, 28, 
                                  20, 20, 28, 36, 32, 29, 21, 29, 17, 28)) %>%
  mutate(Ppn = With_GD/Hits)

ggplot(gd_hits, aes(x = Year, y = Ppn)) + 
  geom_smooth(se = F, colour = '#f15152', linewidth = 1.5) +
  theme(plot.background = element_rect(fill = '#36454f', colour = '#36454f'),
        panel.background = element_rect(colour = '#e5e5e5', fill = '#36454f', linewidth = 3),
        axis.line.x = element_line(colour = '#e5e5e5'),
        axis.line.y = element_line(colour = '#e5e5e5'),
        axis.ticks = element_line(colour = '#e5e5e5'),
        axis.text.y = element_text(size = 22, colour = '#e5e5e5', face = 'bold'), 
        axis.text.x = element_text(size = 22, colour = '#e5e5e5', face = 'bold'),
        axis.title.x = element_text(size = 22, vjust = -3,  colour = '#e5e5e5', face = 'bold'),
        axis.title.y = element_text(size = 22, vjust = 5,  colour = '#e5e5e5', face = 'bold'),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm')) +
  ylab('Proportion articles') +
  xlab('Year')

# Save
ggsave('article_trends_ppt.tiff', last_plot(), path = 'figures/', device = 'tiff', dpi = 300, width = 6, height = 5, units = 'in')

