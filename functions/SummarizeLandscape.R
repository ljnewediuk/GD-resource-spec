
# Packages
library(sf)
library(rgee)
library(tidyverse)

summarize_lc <- function(study) {
  
  move_dat <- readRDS(paste0('move_files/', study))
  
  lc_summary <- data.frame()
  # Run loop
  for(i in unique(move_dat$individual.id)) {
    
    # Get study ID and name of species
    study_id <- unique(move_dat$study.id)
    cat('STUDY ID: ', study_id, '\n')
    cat('individual: ', i, '\n')
    taxon_name <- unique(gsub(' ', '_', tolower(move_dat$individual.taxon.canonical.name)))
    
    # Check if individuals, study id, taxon already done, and if so move to next
    # if(nrow(lc_summary) > 0) {
    #   studies_complete <- paste(lc_summary$study, lc_summary$id, lc_summary$taxon, sep = '_')
    #   new_study <- paste(study_id, i, taxon_name, sep = '_')
    #   if(any(is.na(match(studies_complete, new_study)) == F)) next
    # }
    
    # Skip if individual not in data
    # if(! i %in% move_dat$individual.id) next
    
    # Skip if individual has no
    if(nrow(filter(move_dat, individual.id == i)) == 0) next
    
    # Subset movement data to individual and convert to sf
    id_move <- move_dat %>% 
      filter(individual.id == i) %>%
      # mutate(abs_ta_mean = abs(ta_mean)) %>%
      filter(! is.na(location.long) & ! is.na(location.lat)) %>%
      st_as_sf(coords = c('location.long', 'location.lat'), 
               crs = sp::CRS("+proj=longlat +ellips=WGS84"), remove = F) %>%
      mutate(utm_crs = paste0("+proj=utm +zone=", 
                              (floor((location.long + 180) / 6) + 1), " ellps=WGS84"))
    
    # Make multipoint feature and convex hull
    id_hull <- id_move %>%
      st_union() %>%
      st_convex_hull()
    
    #2 - Convert to ee object and make ee polygon convex hull ====
    hull_ee <- id_hull %>%
      sf_as_ee()
    
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
    ee_cont <- img_cont$reduceRegion(reducer = ee$Reducer$mean(), geometry = hull_ee)$getInfo()$b1
    ee_D <- img_D$reduceRegion(reducer = ee$Reducer$mean(), geometry = hull_ee)$getInfo()$b1
    ee_evap <- img_evap$reduceRegion(reducer = ee$Reducer$mean(), geometry = hull_ee)$getInfo()$b1
    
    # Move to next if no spatial data available
    if(is.null(ee_cont) | is.null(ee_D) | is.null(ee_evap)) next
    
    # Otherwise combine data into df
    id_row <- data.frame(study = study_id, taxon = taxon_name, id = i, 
                         simpsons_D = ee_D,
                         hab_contrast = ee_cont,
                         pet = ee_evap)
    
    lc_summary <- rbind(lc_summary, id_row)
  }
  
  return(lc_summary)
  
}






