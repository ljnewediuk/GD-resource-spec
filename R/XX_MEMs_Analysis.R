
library(tidyverse)
library(sf)
library(vegan)
library(adespatial)

# Load species richness/home range size data
dat <- readRDS('output/cleaned_spp_richness.rds') %>%
  st_transform(4326)

# Pick continents
conts <- 'asia'

# Get desired continent
dat_cont <- dat %>%
  filter(continent %in% conts)

xy <- data.frame(lon = unlist(map(dat_cont$geometry, 1)),
                 lat = unlist(map(dat_cont$geometry, 2)))

# Check for linear gradient
anova(lm(dat_cont$richness ~., data = xy))
anova(lm(dat_cont$mean_size ~., data = xy))

# Detrend data
rich_det <- resid(lm(dat_cont$richness ~., data = xy))
hr_det <- resid(lm(dat_cont$mean_size ~., data = xy))

# dbMEM
na_mem <- dbmem(xy, silent = F)

# Pull spatial weights out of dbMEM object
sp_weights <- attributes(na_mem)$listw

# Convert MEM to data frame
na_mem_df <- as.data.frame(na_mem)
length(attributes(na_mem_df)$values) # of MEMs

# Global significance test
memlm_rich <- lm(rich_det ~ ., na_mem_df)
memlm_hr <- lm(hr_det  ~ ., na_mem_df)

# MEM selection: richness
rich_select <- RsquareAdj(memlm_rich)$adj.r.squared
# Forward selection
rich_fwd <- forward.sel(rich_det, 
                        as.matrix(na_mem_df),
                        adjR2thresh = rich_select)
# Sort & extract selected MEMs
rich_mems <-sort(rich_fwd[, 2])
rich_red <- na_mem_df[, rich_mems]

# MEM selection: home range
hr_select <- RsquareAdj(memlm_hr)$adj.r.squared
# Forward selection
hr_fwd <- forward.sel(hr_det, 
                      as.matrix(na_mem_df),
                      adjR2thresh = hr_select)
# Sort & extract selected MEMs
hr_mems <-sort(hr_fwd[, 2])
hr_red <- na_mem_df[, hr_mems]

# Shared MEMs
shared <- rich_mems[rich_mems %in% hr_mems]
shared_df <- na_mem_df[, shared]

# Moran's I for each MEM to assign cut-off for broad scale Moran's I > 0.25
# But no vals > 0.25...
morani_rich <- moranNP.randtest(as.matrix(rich_red), sp_weights)
morani_rich_vals <- morani_rich$obs[morani_rich$obs > 0.25]
if(is_empty(morani_rich_vals)) morani_rich_vals <- morani_rich$obs

morani_hr <- moranNP.randtest(as.matrix(hr_red), sp_weights)
morani_hr_vals <- morani_hr$obs[morani_hr$obs > 0.25]
if(is_empty(morani_hr_vals)) morani_hr_vals <- morani_hr$obs

rich_mem_broad <- na_mem_df[, rich_mems[c(1:length(morani_rich_vals))]]
hr_mem_broad <- na_mem_df[, hr_mems[c(1:length(morani_hr_vals))]]


# Predicted values for maps
fit_rich <- lm(scale(dat_cont$richness) ~., data = as.data.frame(rich_mem_broad)) 
fitted_rich <- predict(fit_rich)

fit_hr <- lm(scale(dat_cont$mean_size) ~., data = as.data.frame(hr_mem_broad))
fitted_hr <- predict(fit_hr)

# Plot map
sf_cont <- ne_countries(continent = conts, returnclass = 'sf')

map_dat <- dat_cont %>%
  mutate(richness_mems = fitted_rich, 
         hr_mems = fitted_hr)

ggplot() + 
  geom_sf(data = sf_cont) + 
  geom_sf(data = map_dat, aes(colour = hr_mems))
ggplot() + 
  geom_sf(data = sf_cont) + 
  geom_sf(data = map_dat, aes(colour = richness_mems))

# Variation partitioning

# Variation explained by MEMs (total spatial variation)
rich_mem_tot <- lm(scale(dat_cont$richness) ~ ., data = rich_red)
hr_mem_tot <- lm(log(dat_cont$mean_size) ~ ., data = hr_red)

# Variation explained by shared MEMs (shared spatial variation)
rich_mem_share <- lm(scale(dat_cont$richness) ~ ., data = as.data.frame(shared_df))
hr_mem_share <- lm(log(dat_cont$mean_size) ~ ., data = as.data.frame(shared_df))

# Species richness
rich_ATS <- summary(rich_mem_tot)$adj.r.squared # Total spatial variation
rich_ANS <- 1 - rich_ATS # Nonspatial
rich_ASS <- summary(rich_mem_share)$adj.r.squared # Shared spatial variation
rich_ASS/rich_ATS # % shared spatial of total variation
rich_ANSS <- rich_ATS - rich_ASS # Non-shared spatial variation

# Home range size
hr_ATS <- summary(hr_mem_tot)$adj.r.squared # Total spatial variation
hr_ANS <- 1 - hr_ATS # Nonspatial
hr_ASS <- summary(hr_mem_share)$adj.r.squared # Shared spatial variation
hr_ASS/hr_ATS # % shared spatial of total variation
hr_ANSS <- hr_ATS - hr_ASS # Non-shared spatial variation




