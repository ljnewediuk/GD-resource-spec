
# Packages
library(sf)
library(rgee)
library(tidyverse)

# Set up Earth Engine credentials
ee_Initialize(user = 'j.newedi@gmail.com', drive = TRUE)

# List of sf files already processed
sf_dsets <- list.files('sf_files', full.names = T)
# Remove for now because too big to process
sf_dsets <- sf_dsets[- c(43, 79)]

# Calculate Simpson's D (heterogeneity) for all studies/species

# 1 - Check if the heterogeneity file exists, and load
if(! file.exists('output/heterogeneity.rds')) {
  het_dat <- data.frame()
} else {
  het_dat <- readRDS('output/heterogeneity.rds')
}

for(i in 2:length(sf_dsets)) {
  
  #1 - Check if study/species has already been processed
  # Get study (s) and taxon (t)
  s <- as.numeric(str_extract(sf_dsets[i], "[[:digit:]]+"))
  t <- strsplit(sf_dsets[i], '_')[[1]]
  # Separate into genus and species
  genus <- strsplit(t[2], '/')[[1]][2]
  sp <- t[3]
  # Get latin name and print
  t_t <- ifelse(length(t) < 4, paste0(genus, '_'), paste0(genus, '_', sp))
  cat(t_t, ' ', s, '\n')
  # Check if latin name/study ID  combination are already in df
  if(nrow(het_dat[which(het_dat$taxon == t_t & het_dat$study == s),]) > 0) next
  
  #2 - Load dataset
  dat <- readRDS(sf_dsets[i]) 
  
  # Make multipoint feature and convex hull
  sf_dat <- dat %>%
    st_union() %>%
    st_convex_hull()
  
  #2 - Convert to ee object and make ee polygon convex hull ====
  mcp_ee <- sf_dat %>%
    sf_as_ee()
  
  #3 - Extract frequency of land cover classes from polygon
  
  # Get land cover map for each land cover class in band
  lc_vals <- c(seq(10, 90, by = 10), 95, 100)
  # Set a scale (10 m/pixel)
  sc <- 10
  
  # Make list of pixel counts for each band value
  ee_list <- list()
  for(k in lc_vals) {
  # Select Map band = k
  imgk <- lc <- ee$
    ImageCollection('ESA/WorldCover/v200')$first()$select('Map')$eq(k)
  # Use histogram reducer to count classes at 100 m scale
  ee_histo <- imgk$reduceRegion(
    # reducer = ee$Reducer$frequencyHistogram(),
    reducer = ee$Reducer$sum(),
    geometry = mcp_ee,
    scale = sc,
    maxPixels = 1E12
  )
  
  # Get frequency of each class as list
  ee_classes <- ee_histo$getInfo()
  ee_list[[as.character(k)]] <- ee_classes$Map[[1]]
  }
  
  #4 - Calculate Simpson's D (heterogeneity); 
  #    0 = no heterogeneity, 1 = high heterogeneity
  #    
  #    D = 1 - (∑n(n-1)/N(N-1))
  #    
  #    Where n = total frequency of a particular land cover classes
  #          N = total number of land covers
  
  # Sum frequency of all land covers for N parameter
  N <- sum(unlist(ee_list))
  
  # Calculate ∑n(n-1)
  ni_list <- list()
  for(j in 1:length(ee_list)) {
    ni <- ee_list[[j]] * (ee_list[[j]]- 1)
    # Add to list for summing
    ni_list[[j]] <- ni
  }
  
  # Calculate Simpson's D (diversity of habitats within range)
  D <- 1 - (sum(unlist(ni_list))/(N * (N- 1)))
  
  # Get habitat contrast (contrast between adjoining pixels as measure of
  # range heterogeneity) and Simpson's D (for comparison purposes)
  # Get info at: https://gee-community-catalog.org/projects/ghh/
  img_cont <- ee$Image('projects/sat-io/open-datasets/global_habitat_heterogeneity/contrast_1km')
  img_D <- ee$Image('projects/sat-io/open-datasets/global_habitat_heterogeneity/simpson_1km')
  
  # Get annual potential evapo-transpiration for range (measure of range
  # energy availability averaged 1970-2000)
  # Get info at: https://gee-community-catalog.org/projects/et0/
  img_evap <- ee$Image('projects/sat-io/open-datasets/global_et0/global_et0_yearly')
  
  # Reduce regions
  ee_cont <- img_cont$reduceRegion(reducer = ee$Reducer$mean(), geometry = mcp_ee)
  ee_D <- img_D$reduceRegion(reducer = ee$Reducer$mean(), geometry = mcp_ee)
  ee_evap <- img_evap$reduceRegion(reducer = ee$Reducer$mean(), geometry = mcp_ee)
  
  # Compile row of data
  het_row <- data.frame(study = unique(dat$study), 
                        taxon = unique(dat$taxon),
                        calculated_D = D,
                        downloaded_D = ee_D$getInfo()$b1,
                        hab_contrast = ee_cont$getInfo()$b1,
                        pet = ee_evap$getInfo()$b1,
                        scale = sc)
  # Bind together
  het_dat <- rbind(het_dat, het_row)
  
}

#5 - Save
saveRDS(het_dat, 'output/heterogeneity.rds')


