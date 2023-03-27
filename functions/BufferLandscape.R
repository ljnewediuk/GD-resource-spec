
# Packages
library(sf)
library(rgee)
library(tidyverse)
library(landscapemetrics)

lc_buffer <- function(study, buff_size) {
  
  #2 - Load dataset
  move_dat <- readRDS(paste0('move_files/', study)) %>%
    # Remove missing locations
    filter(! is.na(location.long) & ! is.na(location.lat)) %>%
    # Project coordinates
    st_as_sf(coords = c('location.long', 'location.lat'), 
             crs = sp::CRS("+proj=longlat +ellips=WGS84"), remove = F) %>%
    mutate(utm_crs = paste0("+proj=utm +zone=", 
                            (floor((location.long + 180) / 6) + 1), " ellps=WGS84"))
  
  # Get study ID and species name
  study <- unique(move_dat$study.id)
  taxon <- unique(gsub(' ', '_', tolower(move_dat$individual.taxon.canonical.name)))
  
  # Make multipoint feature and get centroid
  sf_dat <- move_dat %>%
    st_union() %>%
    st_centroid()
  
  # Buffer centroid at 5,000 km2 (~  80 km diameter)
  buffer_dat <- sf_dat %>%
    st_buffer(dist = buff_size)
  
  #2 - Convert to ee object and get ee centroid ====
  buffer_ee <- buffer_dat %>%
    sf_as_ee()
  
  # Get land cover layer (2009 LC from ESA global landcover)
  img <- ee$Image("ESA/GLOBCOVER_L4_200901_200912_V2_3")
  
  # Get raster within buffer region 
  eeR <- ee_as_raster(img, buffer_ee)
  
  # Pull out land cover band
  eeLC <- eeR$landcover[[1]]
  
  # Calculate mesh size for landscape
  mesh_size <- landscapemetrics::lsm_l_mesh(eeLC) %>%
    pull(value)
  
  # Build df
  lc_summary <- data.frame(study, taxon, mesh_size)
  
  # Remove container from G Drive
  ee_clean_container(name = "rgee_backup", type = "drive", quiet = T)
  
  return(lc_summary)
  
}

