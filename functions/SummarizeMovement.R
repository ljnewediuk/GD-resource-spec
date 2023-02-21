
library(tidyverse)
library(sf)

summarize_mvmt <- function(type, study) {
  
  move_dat <- readRDS(paste0(type, '_files/', study))
  
  # Start data frame for summarizing movement
  if(file.exists('output/movement_summary.rds')) {
    move_summary <- readRDS('output/movement_summary.rds')
  } else {
    move_summary <- data.frame()
  }
  
  # Run loop
  for(i in unique(move_dat$individual.id)) {
    
    print(i)
    
    # Get study ID and name of species
    study_id <- unique(move_dat$study.id)
    print(study_id)
    taxon_name <- unique(gsub(' ', '_', tolower(move_dat$individual.taxon.canonical.name)))
    
    # Check if individuals, study id, taxon already done, and if so move to next
    if(nrow(move_summary) > 0) {
      studies_complete <- paste(move_summary$study, move_summary$id, move_summary$taxon, sep = '_')
      new_study <- paste(study_id, i, taxon_name, sep = '_')
      if(any(is.na(match(studies_complete, new_study)) == F)) next
    }
    
    # Skip if individual not in data
    if(! i %in% move_dat$individual.id) next
    
    # Subset movement data to individual and convert to sf
    id_move <- move_dat %>% 
      filter(individual.id == i) %>%
      # mutate(abs_ta_mean = abs(ta_mean)) %>%
      filter(! is.na(location.long) & ! is.na(location.lat)) %>%
      st_as_sf(coords = c('location.long', 'location.lat'), 
               crs = sp::CRS("+proj=longlat +ellips=WGS84"), remove = F) %>%
      mutate(utm_crs = paste0("+proj=utm +zone=", 
                              (floor((location.long + 180) / 6) + 1), " ellps=WGS84"))
    
    # Summarize individual movement
    id_summary <- data.frame()
    for(j in unique(id_move$segment)) {
      
      # Get individual segment
      id_segment <- id_move %>%
        filter(segment == j)
 
      # Get n steps and total time for movement bout
      n_steps <- nrow(id_segment)
      total_time <- sum(id_segment$diff)
      
      # Get the CRS in projected coordinates (UTM zone)
      new_crs <- id_segment %>%
        distinct(utm_crs) %>%
        pull(utm_crs)
      
      # Project
      proj_id_segment <- id_segment %>%
        st_transform(crs = new_crs[1])
      
      proj_id_segment <- proj_id_segment %>%
        cbind(st_coordinates(proj_id_segment))
      
      # Make track object
      id_track <- proj_id_segment %>%
        amt::make_track(.x = X, .y = Y, 
                        .t = timestamp, crs = st_crs(proj_id_segment)) %>% na.omit()
      
      # Move to next if id_track has fewer than 5 points
      if(nrow(id_track) < 5) next
      
      # Calculate some movement metrics using 'amt' package
      # Sinuosity
      sinuos <- id_track %>% amt::sinuosity()
      # Home range area (MCP)
      mcp_area <- amt::hr_mcp(id_track, levels = 1) %>%
        amt::hr_area()
      # Mean squared displacement
      ms_disp <- id_track %>% amt::msd()
      # Total distance
      total_dist <- id_track %>% amt::tot_dist()
      # Cumulative distance
      cumulat_dist <- id_track %>% amt::cum_dist()
      # Mean turn angle correlation
      ta_corr <- id_track %>% amt::tac()
      
      # HR area = area used over time
      # Speed = distance traveled over time
      id_row <- data.frame(study = study_id, taxon = taxon_name, id = i, 
                           time_units = unique(id_segment$time_units),
                           n_steps, segment = unique(id_segment$segment),
                           reg_time = unique(id_segment$reg_time), total_time,
                           sinuos, ta_corr, hr_area = mcp_area[[3]]/total_time,
                           total_dist, cumulat_dist, speed = cumulat_dist/total_time)
      
      id_summary <- rbind(id_summary, id_row)
    }
    
    # Skip if no data for individual
    if(nrow(id_summary) < 1) next
    
    move_row <- id_summary %>% dplyr::select(! segment) %>% 
      group_by(study, taxon, id, time_units) %>%
      summarize(across(c(reg_time, sinuos:hr_area, speed), ~ mean(.x, na.rm = TRUE)),
                across(c(n_steps, total_time, total_dist, cumulat_dist), sum))
    
    # Compile data
    move_summary <- rbind(move_summary, move_row)
    
  }
  
  return(move_summary)
  
}
