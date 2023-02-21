
library(tidyverse)
library(move)

# Movebank credentials
mv_creds <- movebankLogin('levi_newediuk', 'COWelk2019')

# Load study IDs
MV_mamms <- readRDS('output/mammal_studies.rds')

# Source HMM function
source('functions/GetMovementStates.R') 

# Download data
for(i in unique(MV_mamms$id)) {
  
  # Indicate study
  cat('Processing study ', i, '\n')
  
  # If already exists in files, move to next
  if({any(sapply(list.files('hmm_files/', full.names=TRUE), 
                         function(x) str_detect(x, as.character(i))))}) {
    cat('Study already processed', '\n')
    next
    }
  
  # Download data and classify movement
  hmm_states <- movement_states(study_id = i, mv_creds = mv_creds)
  
  # If no tracks could be assessed, move on without saving
  if(nrow(hmm_states) == 0) next
  
  # Name file for saving
  hmm_file_name <- paste0('hmm_files/', 
                          str_replace(tolower(
                            unique(hmm_states$individual.taxon.canonical.name)), 
                            ' ', '_'), '_', i, '.rds')
  
  # If multiple species in study, split them and save as separate .csvs
  if(length(hmm_file_name) > 1) {
    
    # Split data frame into list by taxon
    multi_taxa <- split(hmm_states, hmm_states$individual.taxon.canonical.name)
    # Save each individually
    for(taxa in 1:length(multi_taxa)) {
      # Make new filename
      multi_taxa_file_name <- paste0('hmm_files/', str_replace(tolower(
        unique(multi_taxa[[taxa]]$individual.taxon.canonical.name)), 
        ' ', '_'), '_', i, '.rds')
      # Save
      saveRDS(multi_taxa[[taxa]], multi_taxa_file_name)
    }
    
  } else {
    
    # Otherwise save as one .csv
    saveRDS(hmm_states, hmm_file_name)
    
  }
  
  # Report whether study saved
  cat('Study ', i, ' saved successfully', '\n')
  
}





