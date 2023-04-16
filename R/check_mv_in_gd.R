
library(tidyverse)
library(move)

# Get Movebank data
MV_dat <- getMovebank('study')

# Get TetraDensity data
dens_dat <- read.csv('input/TetraDENSITY.csv') %>%
  mutate(species = paste(Genus, Species, sep = ' '))

# Dispersal distance data
disp_dat <- read.csv('input/dispersal_distance_data_whitmee_2013.csv') %>%
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

# Load names of species with GD data
# GD_dat <- read.delim('input/chloe_mammals_list.txt')
GD_dat <- read.csv('input/synthesized_geneticdiversity_dryad.csv') %>%
  select(species, global_fst) %>%
  left_join(disp_dat) %>%
  left_join(hr_dat) %>%
  mutate(log_hr_km2 = log(mean_hr_km2))
  

ggplot(GD_dat, aes(x = log_hr_km2, y = global_fst)) + geom_point() + geom_smooth(method = 'lm')
summary(lm(global_fst ~ log_hr_km2, data = GD_dat))





  dplyr::select(species) %>%
  distinct() %>%
  # Remove underscores
  mutate(species = str_replace(species, '_', ' '),
         # Get order and filter to only ungulates
         order = taxize::tax_name(species, get = 'order')[[3]]) %>%
  filter(order == 'Artiodactyla')
# Replace cervus elaphus
GD_dat[GD_dat$species == 'Cervus elaphus_nannodes', 'species'] <- 'Cervus elaphus'



# Subset ungulates in Movebank data
MV_ungs <- MV_dat %>% 
  # About 50% studies with download access... Maybe we ask for permission?
  filter(i_have_download_access == 'true') %>%
  dplyr::select(id, taxon_ids, number_of_individuals,
                main_location_lat:main_location_long) %>%
  rename('species' = taxon_ids) %>%
  # Get only ungulates in GD data
  filter(species %in% c(GD_dat$species),
         # Filter out any populations wtih < 5 individuals
         number_of_individuals >= 5)
  
# Subset ungulates in GD data
GD_ungs <- read.csv('input/synthesized_geneticdiversity_dryad.csv') %>%
  mutate(species = str_replace(species, '_', ' ')) %>%
  filter(species %in% GD_dat$species)






species_class <- MV_unq %>%
  dplyr::select(species) %>%
  distinct() %>%
  mutate(class = taxize::tax_name(species, get = 'class')[[3]])

MV_in_GD <- MV_unq %>% 
  filter(! is.na(number_of_individuals),
         species %in% GD_dat$species)

MV_in_dens <- MV_ungs %>%
  filter(! is.na(number_of_individuals),
         species %in% dens_dat$species) %>%
  filter(number_of_individuals > 4) %>%
  # Get order and filter to only ungulates
  mutate(class = taxize::tax_name(species, get = 'class')[[3]]) 

MV_in_GD_spp <- MV_in_GD %>%
  group_by(species) %>%
  summarize(n_studies = n(), avg_indiv = mean(number_of_individuals))
  

# Next steps: Find common names for all species in Chloe's list... Also look
# for species names in taxon_ids. 
# 
# There are some taxon_ids with multiple species in a study so they won't match 
# up with Chloe's list. Species names may be in study name but not in taxon_ids
# 
# There may be common names for species that are not in taxon_ids but are in
# study name.

# Get common names for scientific names
foo <- unlist(taxize::sci2comm(GD_dat[2,], db = 'itis'))

# Checks if the common name is in row 1600 col 'name' of the Movebank df
agrep(foo[1], MV_dat[1600,]$name, max.distance = 0.2)

# Checks if the scientific name is in row 1600 col 'name' of the Movebank df
agrep(GD_dat[2,], MV_dat[1600,]$name, max.distance = 0.2)


###*####**********#########

# Make column in GD_dat with list of common names
GD_dat$comm_name <- NA
for(i in 1:nrow(GD_dat)) {
  # Names need to be unlisted before they can be matched
  GD_dat[i,]$comm_name <- list(taxize::sci2comm(GD_dat[i,]$species, db = 'itis'))
  
}

# Loop to match species names to common names if possible
# Need to make a new df with one column as sci names and second column 
# as string of common names
# Match each row in MV_dat individually
extra_spp_names <- c()

for(j in 1:nrow(MV_dat)) {
  
  # comm_nms <- unlist(taxize::sci2comm(GD_dat[j], db = 'itis'))
  for(i in 1:nrow(GD_dat)) {
    
    cnames <- unlist(GD_dat[i,]$comm_name)
    
    tax_matches <- c()
    for(l in 1:length(cnames)){
      
      tax_match <- agrep(cnames[l], MV_dat[j,]$name, max.distance = 0.2)
      tax_matches <- c(tax_matches, tax_match)
      
    }
  }
  
  extra_spp_names[j] <- ifelse(length(tax_matches) > 0, GD_dat[j,]$species, NA) 
  
}



