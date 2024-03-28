# load pkgs
pacman::p_load(tidyverse, digest, crayon)

# test set
problems <- as.data.frame(readRDS("data/choice_problems_balanced.rds"))
n_agents <- 1000 # number of synthetic agents

# simulation parameters
param <- expand.grid(psi = seq(.1, 1, .1) , # switching probability
                     theta = seq(100, 300, 50)) # thresholds

# simulation: for each parameter combination (rows of param), all choice problems (rows of choice_problems) are solved by all agents

set.seed(8172651)
param_list <- vector("list", nrow(param)) 
for (set in seq_len(nrow(param))) { # loop over parameter combinations
  
  psi <- param[[set,"psi"]]
  theta <- param[[set, "theta"]]
  
  problem_list <- vector("list", nrow(problems))
  
  for (problem in seq_len(nrow(problems))) { # loop over choice problems
    
    # retrieve problem features here
    p_r_1 <- problems[[problem, "p_r_1"]]
    p_r_2 <- problems[[problem, "p_r_2"]]
    r_1 <- problems[[problem, "r_1"]]
    r_2 <- problems[[problem, "r_2"]]
    safe <- problems[[problem, "safe"]]
    
    agents_list <- vector("list", n_agents)
    
    for (agent in seq_along(1:n_agents)){ # loop over agents

      ## parameters to initiate trials with
      
      attended <- NULL
      sample_n <- 0
      samples_n_r <- 0
      samples_n_s <- 0
      samples_r <- NULL
      samples_s <- NULL
      DV_risky <- 0
      DV_safe <- 0
      
      sample_trace <- NULL
      DV_risky_trace <- NULL
      DV_safe_trace <- NULL
      DV_trace <- NULL
      choice_trace <- NULL
      
      boundary_reached <- FALSE
      
      init <- sample(c("r", "s"), size=1) # option attended first; no attention bias
      attend <- init

      ## sampling and accumulation process
      
      while(boundary_reached == FALSE) {
        
        attended <- c(attended, attend)
        sample_n <- sample_n + 1
        sample_trace <- c(sample_trace, sample_n)
        
        ### draw sample and update evidence for respective option

        if(attend == "r") {
          
          samples_n_r <- samples_n_r + 1
          
          sampled_outcome <- sample(x = c(r_1, r_2), size = 1, prob = c(p_r_1, p_r_2))
          samples_r <- c(samples_r, sampled_outcome)
          samples_s <- c(samples_s, NA)
          DV_risky <- DV_risky + sampled_outcome
          DV_risky_trace <- c(DV_risky_trace, DV_risky)
          DV_safe_trace <- c(DV_safe_trace, DV_safe)
          
          p_attend_r <- 1-psi
          
          } else {
            
            samples_n_s <- samples_n_s + 1
            
            samples_s <- c(samples_s, safe)
            samples_r <- c(samples_r, NA)
            DV_safe <- DV_safe + safe
            DV_safe_trace <- c(DV_safe_trace, DV_safe)
            DV_risky_trace <- c(DV_risky_trace, DV_risky)
            
            p_attend_r <- psi
            
          }
        
        ### check if accumulated evidence reached threshold
        
        if(samples_n_s > 0 & samples_n_r > 0){
        DV <- DV_risky - DV_safe # returns NA if not at least one sample from each option has been drawn
        DV_trace <- c(DV_trace, DV)
        } else {
          DV <- 0
          DV_trace <- c(DV_trace, DV)
        }
        
        choice <- ifelse(DV >= theta, "risky", ifelse(DV <= -1*theta, "safe", NA))
        choice_trace <- c(choice_trace, choice)

        ### if threshold isn't reached, draw new sample according to psi

        if(is.na(choice)) {
          attend <- sample(c("r", "s"), size=1, prob=c(p_attend_r, 1-p_attend_r))
          } else {
            boundary_reached <- TRUE
        }
        
      } # close loop choice trial
      agents_list[[agent]] <- data.frame(agent, sample_trace, attended, samples_r, samples_s, DV_risky_trace, DV_safe_trace,DV_trace, choice_trace)
    } # close loop agents
    all_agents <- bind_rows(agents_list)
    problem_list[[problem]] <- as.data.frame(expand_grid(problems[problem, ], all_agents))
  } # close loop choice problems
  all_problems <- bind_rows(problem_list) 
  param_list[[set]] <- as.data.frame(expand_grid(param[set, ], all_problems))
  print(paste("\u2713 Parameter Set No. ", set, " finished!"))
} # close loop parameters
simulation_summary <- bind_rows(param_list)

# save data
checksum_simulation_summary <- digest(simulation_summary, "sha256")
write_rds(simulation_summary, "data/simulation_summary_balanced.rds.bz2", compress = "bz2")