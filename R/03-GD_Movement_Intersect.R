
# 03 GD Movement Intersect ====
# Make list of mammal species in common between genetic diversity and movement
# datasets

library(tidyverse)

# Load data
gd_dat <- read.csv('input/synthesized_geneticdiversity_dryad.csv')

mv_dat <- readRDS('output/movement_summary.rds') %>%
  mutate(taxon = ifelse(taxon == 'lynx', 'lynx_rufus', taxon)) %>%
  ungroup %>% select(taxon) %>% distinct() %>%
  mutate(species = str_to_sentence(taxon))

# Find matches
mv_gd_mtch <- intersect(mv_dat$species, gd_dat$species)

# Filter genetic data
gd_sub <- gd_dat %>%
  filter(species %in% mv_gd_mtch)



