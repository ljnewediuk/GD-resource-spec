
library(tidyverse)

# 05 Prep data Fst and movement data ====
# Combine Fst data with movement data and remove historical data, island 
# populations, invasives, and keep only the most recent year of multi-year studies

# Load and prep data for Fst ~ movement models
# Dispersal distance data
disp_dat <- read.csv('input/dispersal_distance_data_whitmee_2013.csv') %>%
  filter(Measure == 'Maximum') %>%
  mutate(species = gsub(' ', '_', Species)) %>%
  mutate(Value = ifelse(Units == 'Metres - Linear', Value/1000, Value)) %>%
  mutate(Value = ifelse(Units == 'Miles - Linear', Value*1.60934, Value)) %>%
  select(species, Value) %>%
  group_by(species) %>%
  summarize(dispersal_km = mean(Value))

# Home range size data
hr_dat <- read.csv('input/hr_size_data_broekman_et_al_2022/HomeRangeData_2022_11_11.csv') %>%
  mutate(species = gsub(' ', '_', Species)) %>%
  group_by(species, HR_Level, HR_Span) %>%
  summarize(mean_hr_km2 = mean(Home_Range_km2)) %>%
  filter(HR_Span %in% grep('Seasonal', unique(HR_Span), value = TRUE),
         HR_Level == 'Individual') %>%
  summarize(mean_hr_km2 = mean(mean_hr_km2)) 

# Filtering genetic data:
gdata_fil <- read.csv('input/fst_data_levi.csv') %>% 
  filter(hybrid == 0,  # remove hybrid species
         is.na(invasive) | invasive == 0, # remove sites not in native range (keep NA)
         is.na(historical) | historical == 0, # remove historical samples (keep NA)
         species != 'Mus_musculus')

## Filter temporal replicates -----

# Find & group temporal replicates
temp_reps <- gdata_fil %>% filter(temporal_replicate != 'none')

# Get complete list of pops with replicates:
pops_w_reps <- unique(unlist(strsplit(temp_reps$temporal_replicate, ' AND ')))
temp_reps2 <- gdata_fil %>% 
  filter(pop %in% pops_w_reps) # note not all replicates listed here MUST be in the data; could have gotten filtered out (e.g. if they were historical)
# This means they won't have another pop to average with so we can leave them alone

## Group temporal replicates in a new grouping variable
# Identify groups: Put all replicates (pop column + replicate column) together:
colsel <- which(names(temp_reps2) %in% c('pop', 'temporal_replicate'))

pops_and_their_replicates <- apply(temp_reps2, 1, function(x) x[colsel], simplify = FALSE)

pop_AND <- lapply(pops_and_their_replicates, function(x) paste(x[1], x[2], sep = ' AND '))

pop_SPL <- lapply(pop_AND, function(x) strsplit(x, split = ' AND '))

# Now put them all in the same order so we can unique() it:
pop_SPL_ALPH <- lapply(pop_SPL, function(x){
  y <- unlist(x)
  y_ord <- order(y)
  y[y_ord]
})

## This is a list of pops that should be grouped together to average:
temporal_rep_groups <- unique(pop_SPL_ALPH)

# Create a grouping ID (added text to make it a factor)
t_groupings <- paste0('TGROUP_', c(1:length(temporal_rep_groups)))

groupings_dfl <- Map(function(x, y){ # list of dataframes with pop + group ID
  df <- data.frame(pop = unlist(x),
                   temporal_group = y)
},
x = temporal_rep_groups, y = t_groupings
)

groupings_df <- do.call('rbind', groupings_dfl) # dataframe with pop + group ID

## Take the most recent replicate & add metadata to the temp reps:
group_metadat <- merge(gdata_fil, groupings_df, by = 'pop', all.x = FALSE, all.y = TRUE)

latest_pops <- group_metadat %>% 
  group_by(temporal_group) %>% 
  slice_max(temporal_sequence) %>% 
  ungroup() %>% 
  select(-temporal_group)

## Remove replicate pops from the main dataset & replace with only latest pops
gdata_fil2 <- gdata_fil %>% 
  filter(!pop %in% groupings_df$pop)

Fst_dat <- rbind(gdata_fil2, latest_pops) %>% ## Add to main dataset
  select(species, global_fst) %>%
  # Add dispersal and hr data
  left_join(disp_dat) %>%
  left_join(hr_dat) %>%
  mutate(log_hr_km2 = log(mean_hr_km2)) %>%
  mutate(across(c(dispersal_km, log_hr_km2, global_fst), list(sc = function(x) as.vector(scale(x, center = T)))))

# Save data for plotting and modeling
saveRDS(Fst_dat, 'output/prepped_fst_data.rds')
saveRDS(disp_dat, 'output/cleaned_dispersal_data.rds')
saveRDS(hr_dat, 'output/cleaned_hr_data.rds')
