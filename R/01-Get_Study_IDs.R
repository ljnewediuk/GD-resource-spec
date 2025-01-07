
# 01 Get Study IDs ====
# Make list of Movebank studies with mammal data

library(tidyverse)
library(move)

# Get list of Movebank data with download access
# Note: mv_creds = Movebank credentials for used
MV_dat <- getMovebank('study', login = mv_creds) %>%
  filter(i_have_download_access == 'true')

# Get only taxa and study ID
MV_taxa <- MV_dat %>%
  dplyr::select(id, taxon_ids, number_of_individuals, 
                main_location_lat, main_location_long) %>%
  rename('n_indivs' = number_of_individuals) %>%
  # Separate each taxon separated by column into a new row
  separate_rows(taxon_ids, sep = ',') 

# Either classify taxa to class, or load list if already classified
if(file.exists('output/movebank_species_classes.rds')) {
  
  taxon_list <- readRDS('output/movebank_species_classes.rds')
  
} else {
  
  # Make list of taxa
  taxon_list <- MV_taxa %>%
    dplyr::select(taxon_ids) %>%
    # Filter out blanks and get only unique species
    filter(! taxon_ids == '') %>%
    distinct()  %>%
    # Label each taxon with class
    mutate(class = taxize::tax_name(taxon_ids, get = 'class')[[3]])
  
  # Save list
  saveRDS(taxon_list, 'output/movebank_species_classes.rds')
  
}

# Filter terrestrial mammals from taxa and remove any with number of animals < 5
# Also remove domestic species, humans, and marine mammals
MV_mamms <- MV_taxa %>%
  left_join(taxon_list) %>%
  filter(class == 'Mammalia',
         ! n_indivs < 5,
         ! taxon_ids %in% c('Pusa hispida', 'Homo sapiens', 'Bos taurus',
                            'Balaenoptera borealis', 'Balaenoptera physalus',
                            'Balaenoptera musculus', 'Physeter macrocephalus',
                            'Balaena mysticetus', 'Pseudorca crassidens', 
                            'Odobenus rosmarus', 'Zalophus californianus',
                            'Mirounga angustirostris', 'Felis catus', 'Ovis aries'),
         # Also remove this study, which seems to have some weird overlap between 
         # raccoons and snapping turtles (1605024900), sensor issues (509393372),
         # or no location data (40906102)
         ! id %in% c(1605024900, 509393372, 40906102),
         # Remove these studies for now because files too large
         ! id %in% c(7023252, 53460105))


# Get locations of studies
MV_locs <- MV_mamms %>% 
  dplyr::select(id, main_location_lat, main_location_long)

# Save list of studies from which to download data
saveRDS(MV_mamms, 'output/mammal_studies.rds')
saveRDS(MV_locs, 'output/mammal_study_locs.rds')



