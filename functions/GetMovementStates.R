
library(tidyverse)
library(moveHMM)
library(move)

movement_states <- function(study_id, mv_creds) {
  
  # Get location data from Movebank
  df <- getMovebankLocationData(study = study_id, login = mv_creds)
  
  # Initiate df
  state_data <- data.frame()
  
  for(j in unique(df$individual.id)) {
    
    # Subset out individual from dataset and print name for warnings/errors
    print(j)
    data_j <- df %>%
      filter(individual.id == j)
    
    # Screen out individuals with fewer than 5 locations
    if(nrow(data_j) < 5) next
    
    # Prep data for HMM
    prepped_j <- data_j %>%
      dplyr::select(location.long, location.lat, individual.id) %>%
      rename('ID' = individual.id) %>%
      prepData(type = 'LL', coordNames = c('location.long', 'location.lat'))
    
    # Define parameters
    
    # Check step lengths = 0 and get indices
    whichzero <- which(prepped_j$step == 0)
    # Get proportion of steps lengths = 0 in data set (for zero-inflation param)
    zeroParam <- length(whichzero)/nrow(prepped_j)
    
    # Try different starting values for all other params, 
    mod_list <- list()
    rep_no <- 0
    max_rep <- 0
    repeat {
      
      rep_no <- rep_no + 1
      max_rep <- max_rep + 1
      
      if(max_rep > 1000) {
        cat("Can't find appropriate starting values", "\n")
        break
      } 
      
      stepMean0 <- runif(2, 
                         min = c(quantile(prepped_j$step, probs = 0.1, na.rm = T),
                                 quantile(prepped_j$step, probs = 0.5, na.rm = T)),
                         max = c(quantile(prepped_j$step, probs = 0.5, na.rm = T),
                                 quantile(prepped_j$step, probs = 0.9, na.rm = T)))
      stepSD0 <- runif(2, 
                       min = c(quantile(prepped_j$step, probs = 0.1, na.rm = T),
                               quantile(prepped_j$step, probs = 0.5, na.rm = T)),
                       max = c(quantile(prepped_j$step, probs = 0.5, na.rm = T),
                               quantile(prepped_j$step, probs = 0.9, na.rm = T)))
      angleMean0 <- c(0, 0)
      angleCon0 <- runif(2,
                        min = c(quantile(prepped_j$angle, probs = 0.5, na.rm = T),
                                quantile(prepped_j$angle, probs = 0.7, na.rm = T)),
                        max = c(quantile(prepped_j$angle, probs = 0.7, na.rm = T),
                                quantile(prepped_j$angle, probs = 0.9, na.rm = T)))
      
      if(length(whichzero) == 0) {
        stepPar0 <- c(stepMean0, stepSD0)
      } else {
        stepPar0 <- c(stepMean0, stepSD0, rep(zeroParam, 2))
      }
      
      anglePar0 <- c(angleMean0, angleCon0)
      
      if(any(c(angleCon0) < 0)) {
        rep_no <- rep_no - 1
        next
      }
      
      tryCatch({
        hmm_m <- fitHMM(prepped_j, nbStates = 2, stepPar0 = stepPar0, anglePar0 = anglePar0)
      }, error=function(e){
        rep_no <- rep_no - 1
        cat("ERROR :",conditionMessage(e), "\n")
      })
      
      mod_list[[rep_no]] <- hmm_m
      
      if(rep_no >= 25) break
      
      }
      
    
    
    # Move to next animal if can't find a model
    if(length(mod_list) == 0) next
    
    # Get log likelihoods for models based on starting params
    # *** LOOK OUT FOR ERRORS
    allnllk <- unlist(lapply(mod_list, function(m) m$mod$minimum))
    
    # Get model with min negative log likelihood
    best_mod <- mod_list[[which(allnllk == min(allnllk))[1]]]
    
    # Add most likely state sequence to data (index of min log likelihood),
    # plus movement parameters
    state_j <- data_j %>%
      mutate(state = factor(viterbi(best_mod))) %>%
      mutate(sl_mean = ifelse(state == 1, 
                              best_mod$mle$stepPar[[1]], best_mod$mle$stepPar[[3]]),
             sl_sd = ifelse(state == 1, 
                            best_mod$mle$stepPar[[2]], best_mod$mle$stepPar[[4]]),
             ta_mean = ifelse(state == 1, 
                              best_mod$mle$anglePar[[1]], best_mod$mle$anglePar[[3]]),
             ta_conc = ifelse(state == 1, 
                            best_mod$mle$anglePar[[2]], best_mod$mle$anglePar[[4]]))
    
    # *** Add mean turn angles and step lengths for each state (should be high TA/
    #     low step lengths for foraging behaviour)
    
    state_data <- rbind(state_data, state_j)
    
  }
  
  return(state_data)
  
}



