
#  Possible land cover data:
#  
#  ESA WorldCover 10m v100 (only available 2020-21 but v high spatial res)

# Extract the land cover data for each individual from each dataset and 
# safe it in the sf_files/ folder

# Packages
library(sf)
library(rgee)
library(tidyverse)

# Set up Earth Engine credentials
ee_Initialize(user = 'j.newedi@gmail.com', drive = TRUE)

# Get list of all hmm dataset paths
hmm_dsets <- list.files('hmm_files', full.names = T)
# Remove for now because causing issues - maybe landcover doesn't extend here? Kept 1241071371 with fewer N
hmm_dsets <- hmm_dsets[! hmm_dsets == 'hmm_files/macroderma_gigas_2019410651.rds' | 
                         hmm_dsets == 'hmm_files/vulpes_lagopus_1241071371.rds']

for(i in 1:length(hmm_dsets)) {
  
  cat(paste0('Processing file ', hmm_dsets[i], '\n'))
  
  # Check if the sf_file already exists in the output folder
  if(file.exists(paste0('sf_files/', substr(hmm_dsets[i], 11, nchar(hmm_dsets[i]))))) {
    cat('File already processed', '\n')
    next
  }
  
  #1 - Load dataset ====
  dat <- readRDS(hmm_dsets[i]) %>%
    mutate(taxon = str_replace(tolower(individual.taxon.canonical.name), ' ', '_')) %>%
    select(study.id, individual.id, taxon, timestamp:location.lat, state, sl_mean:ta_conc) %>%
    rename('study' = study.id, 'id' = individual.id, 'lat' = location.lat, 'long' = location.long)
  
  #2 - Filter timesteps too far outside mean timestep interval ====
  # check_times <- dat  %>%
  #   # Group by ID to get fix times by individual
  #   group_by(id) %>%
  #   mutate(diff = as.numeric(c(difftime(tail(timestamp, -1), head(timestamp, -1)), 0))) %>%
  #   # Remove the last row in the group ( = 0 hrs)
  #   slice(1: n() -1) %>%
  #   #  Get fix mean and the tolerance, i.e. the % difference from the mean fix rate
  #   mutate(fix_mean = mean(diff),
  #          fix_tolerance = abs(mean(diff) - diff)/mean(diff)) %>%
  #   # Remove the fixes that are more than 30% different from the mean fix rate
  #   filter(! fix_tolerance > 0.3)
  
  #3 - Convert to sf object ====
  sf_dat <- dat %>%
    # Filter out any missing coordinates
    filter(! is.na(long)) %>%
    st_as_sf(coords = c('long', 'lat'), crs = sp::CRS("+proj=longlat +ellips=WGS84"), remove = F)
  
  #4 - Convert to EE feature collection and extract land cover at all points ====

  # Initiate data frame for results
  lc_dat <- data.frame()
  for(animal_id in unique(sf_dat$id)) {
    
    # Convert to ee Feature Collection
    pt_dat <- sf_dat %>%
      filter(id == animal_id) %>%
      mutate(timestamp = as.character(timestamp))
    
    # Split into multiple dfs if > 1,000 features
    if(nrow(pt_dat) > 1000) {
      # Create a grouping index for rows of data by 1,000s
      n <- nrow(pt_dat)
      row_grps  <- rep(1: ceiling(n/1000), each = 1000)[1: n]
      # Split chunks into list of data frames
      pt_dat_list <- split(pt_dat, row_grps)

    } else {
      # Else make pt dat into list of 1
      pt_dat_list <- list(pt_dat)
    }
    
    # For each element in the pt_dat_list, make an ee_Feature collection and 
    # extract points
    lc_samples <- list()
    for(j in 1:length(pt_dat_list)) {
      # Run for each item in pt_dat list
      pt_dat_ee <- pt_dat_list[[j]] %>%
        sf_as_ee()
      # Import land cover Image Collection
      lc <- ee$
        ImageCollection('ESA/WorldCover/v200')$
        filterBounds(pt_dat_ee)$
        select('Map')
      
      # Extract land cover at each location point
      lc_samples[[j]] <- suppressWarnings(ee_extract(lc, pt_dat_ee, scale = 10, sf = T))
      
      # If the points did not contain any lc values, fill sf with NAs to match
      # number of columns in lc_dat
      if(length(lc_samples) == 0) {
        lc_samples[[j]] <- pt_dat %>%
          mutate(X2021_Map = NA)
      }
      if(is.null(lc_samples[[j]]$X2021_Map)) {
        lc_samples[[j]] <- pt_dat %>%
          mutate(X2021_Map = NA)
      }
      
      # Bind with remaining samples
      lc_dat <- rbind(lc_dat, lc_samples[[j]])
    }
    
  }
  
  #5 - Save the df ====
  saveRDS(lc_dat, paste0('sf_files/', unique(lc_dat$taxon), '_', unique(lc_dat$study), '.rds'))
  
}
