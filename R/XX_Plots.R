
library(tidyverse)

phylo_dat <- read.csv('input/imputation_phylo_1.csv') %>%
  mutate(taxon = tolower(iucn2020_binomial))

phylo_dat$taxon <- gsub(' ', '_', phylo_dat$taxon)

dat <- readRDS('output/heterogeneity.rds') %>%
  left_join(readRDS('output/specialization.rds')) %>%
  left_join(phylo_dat)

# Hypothesis: TNW increases with heterogeneity
ggplot(dat, aes(x = calculated_D, y = TNW)) +
  geom_point()

# Hypothesis: specialization increases with heterogeneity
ggplot(dat, aes(x = calculated_D, y = WIC/TNW)) +
  geom_point()

#  Hypothesis: specialization increases with heterogeneity, but depends on
#              interspecific competition and predation


# Do they use the same foraging patches over time? How do those patches differ
# from the population?




