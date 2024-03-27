# load pkgs
pacman::p_load(tidyverse, digest, crayon)
source("code/helper_functions/fun_compute_cumulative_stats.R") # call functions for computing cumulative stats

# test set
problems <- read_rds("data/choice_problems.rds")
n_agents <- 200 # number of synthetic agents

# simulation parameters
param <- expand.grid(psi = seq(.1, 1, .1) , # switching probability
                     theta = seq(15, 75, 15)) # thresholds

# simulation: for each parameter combination (rows of param), all choice problems (rows of choice_problems) are solved by all agents

set.seed(19543)
param_list <- vector("list", nrow(param)) 
for (set in seq_len(nrow(param))) { # loop over parameter combinations
  
  psi <- param[[set,"psi"]]
  theta <- param[[set, "theta"]]
  
  problem_list <- vector("list", nrow(choice_problems))
  
  for (problem in seq_len(nrow(choice_problems))) { # loop over choice problems
    
    # retrieve problem features here
    
    agents_list <- vector("list", n_agents)
    for (agent in seq_along(1:n_agents)){ # loop over agents

      ## parameters to initiate trials with
      
      attended <- NULL
      samples_r <- NULL
      samples_s <- NULL
      DV_risky <- NULL
      DV_safe <- NULL
      DV_trace <- NULL
      
      boundary_reached <- FALSE
      
      init <- sample(c("r", "s"), size=1) # option attended first; no attention bias
      attend <- init

      ## sampling and accumulation process
      
      while(boundary_reached == FALSE) {
        
        attended <- c(attended, attend)
        
        ### draw sample and update evidence for respective option

        if(attend == "r") {
          
          sampled_outcome <- sample(x = c(r_low, r_high), size = 1, prob = c(p_r_low, p_r_high))
          samples_r <- c(samples_r, sampled_outcome)
          samples_s <- c(samples_s, NA)
          DV_risky <- DV_risky + sampled_outcome
          
          p_attend_r <- 1-psi
          
          } else {
            
            samples_s <- c(samples_s, safe)
            samples_r <- c(samples_r, NA)
            DV_safe <- DV_safe + safe
            
            p_attend_r <- psi
            
          }
        
        ### check if accumulated evidence reached threshold
        
        DV <- DV_risky - DV_safe # returns NA if not at least one sample from each option has been drawn
        DV_trace <- c(DV_trace, DV)
        choice <- ifelse(DV >= theta, "r", ifelse(DV <= -1*theta, "s", NA))

        ### if threshold isn't reached, draw new sample according to psi

        if(is.na(choice)) {
          attend <- sample(c("r", "s"), size=1, prob=c(p_attend_r, 1-p_attend_r))
        } else {
          boundary_reached <- TRUE
        }
        
      } # close loop choice trial
      agents_list[[agent]] <- expand.grid(agent, fd)
    } # close loop agents
    all_agents <- agents_list %>% bind_rows()
    problem_list[[problem]] <- expand.grid(problem, all_agents)
    print(paste("\u2713 Parameter Set No. ", set, ", Problem No. ", problem, " finished!"))
  } # close loop choice problems
  all_problems <- problem_list %>% bind_rows()
  param_list[[set]] <- expand.grid(param[set, ], all_problems)
} # close loop parameters
simulation_summary <- param_list %>% bind_rows()

# save data  
simulation_summary <- read_rds("data/merged/simulation_summary_merged.rds.bz2")

## full data set
checksum_simulation_summary <- digest(simulation_summary, "sha256")
write_rds(simulation_summary, "data/simulation_summary.rds.bz2", compress = "bz2")

## relative thresholds (default)
#simulation_summary_rt <- simulation_summary %>% filter(threshold == "relative")
#checksum_simulation_summary_rt <- digest(simulation_summary_rt, "sha256")
#write_rds(simulation_summary_rt, "data/relative_thresholds/simulation_summary_rt.rds.bz2", compress = "bz2")

## absolute thresholds
#simulation_summary_at <- simulation_summary %>% filter(threshold == "absolute")
#checksum_simulation_summary_at <- digest(simulation_summary_at, "sha256")
#write_rds(simulation_summary_at, "data/absolute_thresholds/simulation_summary_absolute.rds.bz2", compress = "bz2")
