# load pkgs
pacman::p_load(tidyverse, digest, crayon, readxl)
source("code/helper_functions/fun_compute_cumulative_stats.R") # call functions for computing cumulative stats

# test set
choice_problems <- read_xlsx("rerun/problems_rerun.xlsx")
n_agents <- 1000 # number of synthetic agents

# simulation parameters
param <- expand_grid(psi = seq(-.5, .4, .1) , # probability increment added to unbiased sampling probability of p = .5 (switching probability)
                     threshold = "relative" , # threshold type
                     theta = seq(15, 75, 15)) # thresholds

# simulation: for each parameter combination (rows of param), all choice problems (rows of choice_problems) are solved by all agents

set.seed(16254)
param_list <- vector("list", nrow(param)) 
for (set in seq_len(nrow(param))) { # loop over parameter combinations
  problem_list <- vector("list", nrow(choice_problems))
  for (problem in seq_len(nrow(choice_problems))) { # loop over choice problems
    agents_list <- vector("list", n_agents)
    for (agent in seq_along(1:n_agents)){ # loop over agents

      ## parameters to initiate trials with
      
      fd <- tibble() # storage for sampled outcomes (fd = frequency distribution)
      p <- .5  # no attention bias
      psi <- 0
      init <- sample(c("r", "s"), size = 1, prob = c(p + psi, p - psi)) # option attended first
      attend <- init
      boundary_reached <- FALSE

      ## sampling and accumulation process
      
      while(boundary_reached == FALSE) {
        
        ### draw single sample from either risky (r) or safe (s) option

        if(attend == "r") {
          single_smpl <- choice_problems[problem, ] %>%
            mutate(attended = attend ,
                   r = sample(x = c(r_1, r_2), size = 1, prob = c(p_r_1, p_r_2)) ,
                   s = NA)
          psi <- param[[set, "psi"]] # to update the probability of sampling from r again
          } else {
            single_smpl <- choice_problems[problem, ] %>%
              mutate(attended = attend ,
                     r = NA ,
                   s = safe)
            psi <- -1*param[[set, "psi"]] # to update the probability of sampling from s again
          }
        
        ### add sample to other sampled outcomes and update evidence
        
        fd <- bind_rows(fd, single_smpl) %>%
          mutate(r_sum = cumsum2(r, na.rm = TRUE) ,
                 s_sum = cumsum2(s, na.rm = TRUE))

        ### after each sample, check if accumulated evidence reached threshold

        if(param[[set, "threshold"]] == "absolute") {
          fd <- fd %>%
            mutate(choice = case_when(r_sum >= param[[set, "theta"]] ~ "r" ,
                                      s_sum >= param[[set, "theta"]] ~ "s"))
          } else {
            fd <- fd %>%
              mutate(diff = round(r_sum - s_sum, 2) ,
                     choice = case_when(diff >= param[[set, "theta"]] ~ "r",
                                      diff <= -1*param[[set, "theta"]] ~ "s"))
        }

        ### if threshold isn't reached, draw new sample according to psi

        if(is.na(fd[[nrow(fd), "choice"]]) == FALSE) {
          boundary_reached <- TRUE
        } else {
          attend <- sample(c("r", "s"), size = 1, prob = c(p + psi, p - psi))
        }
      } # close loop choice trial
      agents_list[[agent]] <- expand_grid(agent, fd)
    } # close loop agents
    all_agents <- agents_list %>% bind_rows()
    problem_list[[problem]] <- expand_grid(problem, all_agents)
    print(paste("\u2713 Parameter Set No. ", set, ", Problem No. ", problem, " finished!"))
  } # close loop choice problems
  all_problems <- problem_list %>% bind_rows()
  param_list[[set]] <- expand_grid(param[set, ], all_problems)
} # close loop parameters
simulation_summary <- param_list %>% bind_rows()

# save data  
#simulation_summary <- read_rds("rerun/simulation_summary_rerun.rds.bz2")

## full data set
#checksum_simulation_summary <- digest(simulation_summary, "sha256")
write_rds(simulation_summary, "rerun/simulation_summary_rerun.rds.bz2", compress = "bz2")

## relative thresholds (default)
#simulation_summary_rt <- simulation_summary %>% filter(threshold == "relative")
#checksum_simulation_summary_rt <- digest(simulation_summary_rt, "sha256")
#write_rds(simulation_summary_rt, "data/relative_thresholds/simulation_summary_rt.rds.bz2", compress = "bz2")

## absolute thresholds
#simulation_summary_at <- simulation_summary %>% filter(threshold == "absolute")
#checksum_simulation_summary_at <- digest(simulation_summary_at, "sha256")
#write_rds(simulation_summary_at, "data/absolute_thresholds/simulation_summary_absolute.rds.bz2", compress = "bz2")
