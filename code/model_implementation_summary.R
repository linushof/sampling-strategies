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
      
      fd <- data.frame() # storage for sampled outcomes (fd = frequency distribution)
      attented <- c()
      boundary_reached <- FALSE
      
      init <- sample(c("r", "s"), size=1) # option attended first; no attention bias
      attend <- init

      ## sampling and accumulation process
      
      while(boundary_reached == FALSE) {
        
        attended <- c(attended, attend)
        
        ### draw single sample from either risky (r) or safe (s) option

        if(attend == "r") {
          
          single_smpl <- choice_problems[problem, ] %>%
            mutate(attended = attend ,
                   r = sample(x = c(r_low, r_high), size = 1, prob = c(p_r_low, p_r_high)) ,
                   s = NA)
          p_attend_r <- 1-psi
          
          } else {
            
            single_smpl <- choice_problems[problem, ] %>%
              mutate(attended = attend ,
                     r = NA ,
                   s = safe)
            p_attend_r <- psi
            
          }
        
        ### add sample to other sampled outcomes and update evidence
        
        fd <- bind_rows(fd, single_smpl) %>%
          mutate(r_sum = cumsum2(r, na.rm = TRUE) ,
                 s_sum = cumsum2(s, na.rm = TRUE))

        ### after each sample, check if accumulated evidence reached threshold

        fd <- fd %>%
          mutate(diff = round(r_sum - s_sum, 2) ,
                 choice = case_when(diff >= theta ~ "r",
                                    diff <= -1*theta ~ "s"))

        ### if threshold isn't reached, draw new sample according to psi

        if(is.na(fd[[nrow(fd), "choice"]]) == FALSE) {
          boundary_reached <- TRUE
        } else {
          attend <- sample(c("r", "s"), size=1, prob = c(p_attend_r, 1-p_attend_r))
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
