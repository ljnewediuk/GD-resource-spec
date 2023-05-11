
library(tidyverse)
library(sf)

summarize_hrs <- function(type, study) {
  
  move_dat <- readRDS(paste0(type, '_files/', study))
  
  # Start data frame for summarizing movement completed
  if(file.exists('output/hr_summary.rds')) {
    hr_summary <- readRDS('output/hr_summary.rds')
  } else {
    hr_summary <- data.frame()
  }
  # Data frame for summarizing new movement
  hr_new <- data.frame()
  
  # Run loop
  for(i in unique(move_dat$individual.id)) {
    
    print(i)
    
    # Get study ID and name of species
    study_id <- unique(move_dat$study.id)
    print(study_id)
    taxon_name <- unique(gsub(' ', '_', tolower(move_dat$individual.taxon.canonical.name)))
    
    # Check if individuals, study id, taxon already done, and if so move to next
    if(nrow(hr_summary) > 0) {
      studies_complete <- paste(hr_summary$study, hr_summary$id, hr_summary$taxon, sep = '_')
      new_study <- paste(study_id, i, taxon_name, sep = '_')
      if(any(is.na(match(studies_complete, new_study)) == F)) next
    }
    
    # Skip if individual not in data
    if(! i %in% move_dat$individual.id) next
    
    # Subset movement data to individual and convert to sf
    id_move <- move_dat %>% 
      filter(individual.id == i) %>%
      filter(! is.na(location.long) & ! is.na(location.lat)) %>%
      # Convert to sf and get utm zone
      st_as_sf(coords = c('location.long', 'location.lat'), 
               crs = sp::CRS("+proj=longlat +ellips=WGS84"), remove = F) %>%
      mutate(utm_crs = paste0("+proj=utm +zone=", 
                              (floor((location.long + 180) / 6) + 1), " ellps=WGS84")) %>%
      # Group by day
      group_by(day = floor_date(timestamp, "day"))
    
    id_summary <- data.frame()
    # Get home ranges and landscape by day
    for(d in unique(id_move$day)) {
      
      # Filter out data for day
      id_day <- id_move %>%
        filter(day == d)
      
      # Move to next if < 5 locations that day (needed for MCP)
      if(nrow(id_day) < 5) next
      
      # Move to next if time between first and last location is less than 18 hours
      time_elapsed <- as.numeric(difftime(tail(id_day$timestamp, 1), 
                                          head(id_day$timestamp, 1)), units = 'hours')
      if((time_elapsed) < 18) next
      
      # Get the CRS in projected coordinates (UTM zone)
      new_crs <- id_day %>%
        pull(utm_crs) %>%
        unique()
      
      # Project coordinates to UTM
      proj_id_day <- id_day %>%
        st_transform(crs = new_crs[1])
      # Add coordinates as X and Y
      proj_id_day <- proj_id_day %>%
        cbind(st_coordinates(proj_id_day))
      
      # Make track object
      id_track <- proj_id_day %>%
        amt::make_track(.x = X, .y = Y, 
                        .t = timestamp, crs = st_crs(proj_id_day)) %>% na.omit()
      
      # Home range area (MCP) in m2
      mcp_area <- amt::hr_mcp(id_track, levels = 1) %>%
        amt::hr_area()
      
      id_row <- data.frame(study = study_id, taxon = taxon_name, id = i,
                           hr_area = mcp_area[[3]], time_elapsed, units = 'm^2/18-24 h')
      id_summary <- rbind(id_summary, id_row)
    }
    
    # Skip if no data for individual
    if(nrow(id_summary) < 1) next
    
    hr_row <- id_summary %>% 
      group_by(study, taxon, id, units) %>%
      summarize(across(c(hr_area:time_elapsed), ~ mean(.x, na.rm = TRUE)))
    
    # Compile data
    hr_new <- rbind(hr_new, hr_row)
    
  }
  
  return(hr_new)
  
}
