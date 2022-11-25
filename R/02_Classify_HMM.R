
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
  if({patterns <- sapply(list.files('hmm_files/', full.names=TRUE), 
                         function(x) str_detect(x, as.character(i)))}) {
    cat('Study already processed', '\n')
    next
    }
  
  # Download data
  hmm_states <- movement_states(study_id = i, mv_creds = mv_creds)
  
  # Name file for saving
  hmm_file_name <- paste0('hmm_files/', 
                          str_replace(tolower(
                            unique(bat_states$individual.taxon.canonical.name)), 
                            ' ', '_'), '_', i, '.csv')
  
  # Save
  saveRDS(hmm_states, hmm_file_name)
  
  # Report whether study saved
  cat('Study ', i, ' saved successfully', '\n')
  
}



# Some tests of the function:
bat_states <- movement_states(study_id = 625284084, mv_creds = mv_creds)
sheep_states <- movement_states(study_id = 942509734, mv_creds = mv_creds)
elk_states <- movement_states(study_id = 897981076, mv_creds = mv_creds) # 175 animals; will take time
reindeer_states <- movement_states(study_id = 216040785, mv_creds = mv_creds)

# Save the data
saveRDS(reindeer_states, 'hmm_files/rangifer_tarandus_216040785.csv')





