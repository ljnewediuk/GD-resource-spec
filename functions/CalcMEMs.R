
library(tidyverse)
library(sf)
library(vegan)
library(adespatial)


# MEM function
get_MEMs <- function(df, vars, broad.thresh, shared_mems = F){
  
  # Function to get overall model p value (for 1st step of forward selection)
  lmp <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
  }
  
  # Get coords
  coords <- data.frame(lon = unlist(map(df$geometry, 1)),
                   lat = unlist(map(df$geometry, 2)))
  # Detrend data
  # m.dbme <- dbmem(dist, silent=FALSE) #distance matrix from marmaps
  m.dbme <- dbmem(coords, silent = F)
  mlistw <- attributes(m.dbme)$listw
  # convert to dataframe
  m.dbmem <- as.data.frame(m.dbme)
  
  df_fitted <- df
  # Repeat for each var
  for(i in 1:length(vars)){
    
    # Get var
    var <- df %>% st_drop_geometry %>% select(vars[i]) %>% unlist()
    
    # test for linear trends
    md_i <- anova(lm(var ~ ., data = coords))
    if((md_i$`Pr(>F)`[c(1,2)] < 0.05)[1] + (md_i$`Pr(>F)`[c(1,2)] < 0.05)[2] == 0){ # if neither lat or lon are significant, don't detrend
      detdat_i <- var
    } else if(length(which(md_i$`Pr(>F)`[c(1,2)] < 0.05)) > 1){ # if both lat & lon are significant, detrend on both
      detdat_i <- resid(lm(var ~ ., data=coords))
    } else if(which(md_i$`Pr(>F)`[c(1,2)] < 0.05) == 1){ # if only the 1st is significant, detrend on it
      detdat_i <- resid(lm(var ~ coords[,1], data=coords))
    } else detdat_i <- resid(lm(var ~ coords[,2], data=coords)) # if the 2nd variable is significant, detrend on 2nd
    
    # Global significance test
    memlms_i <- lm(detdat_i ~., m.dbmem)
    if(lmp(memlms_i) > 0.05){
      print('Fwd selection step 1: global model not significant')
      next
    } 
    ## MEM selection
    sr2da_i <- RsquareAdj(memlms_i)$adj.r.squared
    # forward selection
    smemfwd_i <- forward.sel(detdat_i, as.matrix(m.dbmem), 
                           adjR2thresh = sr2da_i)
    if(nrow(smemfwd_i) < 1) {
      print('Fwd selection step 2: No significant MEMs')
      next
    } 
    # sort & extract selected MEMs
    smems_i <- sort(smemfwd_i[,2])
    smem.red_i <- m.dbmem[,smems_i]
  
  ## Moran's I for each MEM to assign cut-off for broad scale Moran's I 
  moranis_i <- moran.randtest(as.matrix(smem.red_i), mlistw)
  morani_sp_i <- moranis_i$obs[moranis_i$obs > broad.thresh[i]]
  # if(length(morani_sp_i) < 1) {
  #   print('No MEMs with Moran I > broad.thresh')
  #   next
  # }
  # If no MEMs at broad scale, keep reducing scale to get broadest possible MEM
  if(length(morani_sp_i) < 1) new.thresh <- broad.thresh[i] - 0.00005
  
  if(length(morani_sp_i) < 1) repeat {
    morani_sp_i <- moranis_i$obs[moranis_i$obs > new.thresh]
    new.thresh <- new.thresh - 0.00005
    if(length(morani_sp_i) >= 1) {
      print(paste0('No MEMs with Moran I > broad threshold; new threshold = ', new.thresh))
      break
    }
  }
  
  # Get broad-scale MEMs
  smem.broad_i <- m.dbmem[,smems_i[c(1:length(morani_sp_i))]]
  
  ## Fitted values for maps
  if(length(morani_sp_i) == 1){
    fitsp_i <- lm(scale(var)~smem.broad_i)} else{
      fitsp_i <- lm(scale(var)~., data = smem.broad_i)
    }
  
  # Fitted values
  fitted_vals <- predict(fitsp_i)
  
  # Combine with all variables
  df_fitted <- df_fitted %>%
    cbind(fitted_vals)
  
  # Name columns in df
  names(df_fitted)[ncol(df_fitted)-1] <- paste0('fitted_vals_', vars[i])
  
  # Get mem names and df of mems if variance partitioning
  assign(paste0('smems.', vars[i]), smems_i)
  assign(paste0('smem.red.', vars[i]), smem.red_i)
  
  }

  ## Variance partitioning
  if(shared_mems == 'yes'){
    # Shared MEMs
    shared <- get(paste0('smems.', 
                        vars[1]))[get(paste0('smems.', 
                                             vars[1])) %in% get(paste0('smems.',
                                                                       vars[2]))]
    # Data frame of shared mems
    shared_df <- m.dbmem[, shared]
    
    # Partition the variance (for each variable, check its total spatial
    # variation (ATS), non-spatial variation(ANS), shared spatial variation (ASS),
    # proportion shared spatial variation (perc_ASS), and non-shared spatial
    # variation (ANSS))
    s_var <- data.frame()
    for(i in 1:length(vars)) {
      var_i <- df %>% st_drop_geometry %>% select(vars[i]) %>% unlist()
      # Variation explained by MEMs (total spatial variation)
      var_i_mem_tot <- lm(scale(var_i) ~ ., data = get(paste0('smem.red.', vars[i])))
      # Variation explained by shared MEMs (shared spatial variation)
      var_i_mem_share <- lm(scale(var_i) ~ ., data = shared_df)
      # Get all variance components in row
      s_var_row <- data.frame(
        var = vars[i],
        ATS = summary(var_i_mem_tot)$adj.r.squared,
        ANS = 1 - summary(var_i_mem_tot)$adj.r.squared,
        ASS = summary(var_i_mem_share)$adj.r.squared, 
        perc_ASS = summary(var_i_mem_share)$adj.r.squared/
          summary(var_i_mem_tot)$adj.r.squared, 
        ANSS = summary(var_i_mem_tot)$adj.r.squared - 
          summary(var_i_mem_share)$adj.r.squared
      )
      # Bind in df
      s_var <- rbind(s_var, s_var_row)
      
    }
    # Save output (fitted values for MEM plotting plus partitioned variance)
    outp <- list(fitted_vals = df_fitted, variance_part = s_var)
    
    ## Return output (either both MEMs and partitioned variance or just MEMs)
    return(outp)
  } else {
    return(df_fitted)
  }
  
  
}

# RUN EXAMPLE
# Load species richness/home range size data
dat <- readRDS('output/cleaned_spp_richness.rds') %>%
  st_transform(4326)

# Pick continents
conts <- c('europe')
conts <- c('north america')
conts <- c('south america')
conts <- c('asia')
conts <- c('africa')
conts <- c('new zealand', 'australia')

# Get desired continent
dat_cont <- dat %>%
  filter(continent %in% conts)

# Run MEM function
foo <- get_MEMs(df = dat_cont, 
                vars = c('mean_size', 'richness'), 
                broad.thresh = c(0.1, 0.1), 
                shared_mems = 'yes')

# Print shared var
shared_var <- foo$variance_part

# Map
map_dat <- foo$fitted_vals %>%
  pivot_longer(cols = c(fitted_vals_richness, fitted_vals_mean_size), 
               values_to = 'fitted',
               names_to = 'var') %>%
  mutate(var = ifelse(var == 'fitted_vals_richness', 
                      'Species richness', 'Home range size'))
  

global_map <- rnaturalearth::ne_countries(continent = unique(dat_cont$continent), 
                                          returnclass = 'sf') 

global_map <- rnaturalearth::ne_countries(returnclass = 'sf') 

ggplot() + 
  geom_sf(data = global_map, fill = '#E5E5E5', colour = '#E5E5E5') + 
  geom_sf(data = map_dat, aes(colour = fitted), size = 3) +
  scale_colour_viridis_c(breaks = c(-.5, .4), 
                         labels = c('Low', 'High'), option = 'inferno') +
  theme(plot.background = element_rect(fill = '#33414B', colour = '#33414B'),
        panel.background = element_rect(fill = '#33414B', colour = '#33414B'),
        panel.grid = element_blank(),
        legend.position = c(.1, .1),
        legend.title = element_blank(),
        legend.direction = 'horizontal',
        legend.text = element_text(colour = '#E5E5E5', size = 16),
        legend.key.width = unit(1, 'cm'),
        legend.background = element_rect(fill = '#33414B')) +
  facet_wrap(~ var)




