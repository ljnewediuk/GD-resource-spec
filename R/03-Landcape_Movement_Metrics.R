
# 03 Landscape Movement Metrics ====
# Summarize movement metrics and environmental characteristics for each individual
# in Movebank data

# Call functions
source('functions/SummarizeLandscape.R')
source('functions/SummarizeMovement.R')
source('functions/SummarizeHomeRanges.R')
source('functions/BufferLandscape.R')

# List of studies with movement data
studies <- list.files('move_files/')

# SUMMARIZE LANDSCAPE within 100% MCP encompassing all points from individual

# Start data frame for summarizing landscape, or load  existing data frame if
# already started
if(file.exists('output/lc_summary.rds')) {
  # Use existing df if it exists
  all_lc_data <- readRDS('output/lc_summary.rds')
  # Get list of completed studies
  filter_studies <- all_lc_data %>%
    select(study, taxon) %>%
    distinct()
  done_studies <- paste0(filter_studies$taxon, '_', filter_studies$study, '.rds')
  # Filter out completed
  studies <- studies[! studies %in% done_studies]
} else {
  all_lc_data <- data.frame()
}

# Loop through studies
for(j in studies) {
  study_summary <- summarize_lc(study = j)
  all_lc_data <- rbind(all_lc_data, study_summary)
}

# SUMMARIZE MOVEMENT within 100% MCP encompassing all points from individual

# Start data frame
all_move_data <- data.frame()

# Loop through studies
for(j in studies) {
  
  study_summary <- summarize_mvmt(type = 'move', study = j)
  all_move_data <- rbind(all_move_data, study_summary)
  if(nrow(all_move_data) == 0) all_move_data <- readRDS('output/movement_summary.rds')
  
}

# SUMMARIZE DAILY HOME RANGE SIZE using average HR size surrounding individual points

# Start data frame
if(file.exists('output/hr_summary.rds')) {
  all_hr_data <- readRDS('output/hr_summary.rds')
} else {
  all_hr_data <- data.frame()
}

# Loop through studies
for(j in studies) {
  
  study_summary <- summarize_hrs(type = 'move', study = j)
  all_hr_data <- rbind(all_hr_data, study_summary)
  
}

# BUFFER LANDSCAPE within buffer (in m) from centroid of all points in population
# and CALCULATE MESH SIZE as measure of heterogeneity

# Start data frame
mesh_data <- data.frame()

# Loop through studies
for(k in studies) {
  
  study_summary <- lc_buffer(study = k, buff_size = 80000)
  mesh_data <- rbind(mesh_data, study_summary)
  
}


# SAVE
saveRDS(all_move_data, 'output/movement_summary.rds')
saveRDS(all_hr_data, 'output/hr_summary.rds')
saveRDS(all_lc_data, 'output/lc_summary.rds')
saveRDS(mesh_data, 'output/lc_pop_buffers.rds')

