
library(tidyverse)
library(sf)
library(RInSp)

# Subset all individuals in population by time overlap (i.e., isolate points from
# the same day if units are minutes/hourly, or by week/month if units are daily,
# etc.)
# Also make sure individuals overlap in space (i.e., from same population according
# to Movebank study and actually have some maximum distance between)

# Get list of all sf files 
sf_list <- list.files('sf_files/')

spec_dat <- data.frame()
# Loop through proportion data
for(i in 2:length(sf_list)) {
  
  # Get the appropriate dataset
  sf_dat <- readRDS(paste0('sf_files/', sf_list[i]))
  
  # Count number of individuals in dataset
  n_indiv <- length(unique(sf_dat$id))
  
  # Get df of habitat use by individuals in the population
  pij_dat <- sf_dat %>%
    st_drop_geometry() %>%
    # Summarize number of habitat used by each individual in the population
    mutate(hab = paste0('hab_', X2021_Map)) %>%
    group_by(id, hab) %>%
    summarize(count = n()) %>%
    group_by(id) %>%
    # Pivot so habitat types are across columns
    pivot_wider(id_cols = id, names_from = hab, values_from = count) %>%
    rownames_to_column('id_no')
  
  # Replace NAs in matrix with zeroes
  pij_dat[is.na(pij_dat)] <- 0
  
  # Convert to RInSp object
  hab_use <- import.RInSp(pij_dat, row.names = 1, info.cols = 2)
  
  # Catch errors
  possibleError <- tryCatch(
    WTdMC(hab_use, replicates = 99, pop.diet = 'sum'),
    error=function(e) e
  )
  if(inherits(possibleError, "error")) next
  
  # Calculate specialzation
  spec <- as.numeric(WTdMC(hab_use, replicates = 99, pop.diet = 'sum')$montecarlo[1,])
  
  spec_row <- data.frame(study = unique(sf_dat$study), 
                         taxon = unique(sf_dat$taxon), 
                         WIC = spec[2],
                         BIC = spec[3],
                         TNW = spec[4],
                         n = n_indiv)
  
  spec_dat <- rbind(spec_dat, spec_row)
  
}

# Save
saveRDS(spec_dat, 'output/specialization.rds')



