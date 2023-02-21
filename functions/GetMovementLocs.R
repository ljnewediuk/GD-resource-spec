
library(tidyverse)
library(moveHMM)
library(move)

movement_locs <- function(study_id, mv_creds) {
  
  # Get location data from Movebank
  df <- getMovebankLocationData(study = study_id, login = mv_creds)
  
  # Initiate df
  timeblocks <- data.frame()
  
  for(j in unique(df$individual.id)) {
    
    # Subset out individual from dataset and print name for warnings/errors
    print(j)
    dat_j <- df %>%
      filter(individual.id == j)
    
    # If individual has < 5 points total, move to next
    if(nrow(dat_j) < 5) next
    
    # Filter timesteps too far outside regular timestep interval ====
    check_times_j <- dat_j  %>%
      mutate(diff_time = c(difftime(tail(timestamp, -1), head(timestamp, -1)), 0)) %>%
      mutate(diff = as.numeric(diff_time),
             time_units = units(diff_time)) %>%
      dplyr::select(! diff_time) %>%
      # Remove the last row in the group ( = 0 hrs)
      slice(1: n() -1) %>%
      group_by(diff) %>%
      mutate(n = n()) %>%
      ungroup()
    
    # Get the most frequent time interval
    reg_time <- check_times_j %>%
      filter(n == max(check_times_j$n)) %>%
      distinct(diff) 
    
    reg_time <- max(reg_time)
    
    # For all rows in check_times, if fix is outside of tolerance (i.e., there is 
    # a gap in locations), assign value to start a new track segment
    timeblocks_j <- check_times_j %>%
      mutate(reg_time) %>%
      mutate(segment = cumsum(diff > reg_time * 1.3 | diff < reg_time * 0.7)) %>%
      # Count number of points in each segment
      group_by(segment) %>%
      mutate(n_segment_pts = length(segment)) %>%
      # Remove segments with < 5 points
      filter(! n_segment_pts < 5)
    
    # Bind j's data with the rest of the data
    timeblocks <-  rbind(timeblocks, timeblocks_j)
    
  }
  
  return(timeblocks)
  
}


