# load pkgs
pacman::p_load(tidyverse, digest, crayon)

# test set
problems <- as.data.frame(readRDS("data/choice_problems_balanced.rds"))
n_agents <- 1000 # number of synthetic agents

# simulation parameters
param <- expand.grid(psi = seq(.1, 1, .1) , # switching probability
                     theta = 1:5) # thresholds

# simulation: for each parameter combination (rows of param), all choice problems (rows of choice_problems) are solved by all agents

set.seed(7126527)
param_list <- vector("list", nrow(param))
for (set in seq_len(nrow(param))) {  # loop over parameter combinations
  
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
      
      at <- NULL
      
      N_samples <- 0
      N_samples_trace <- NULL
      
      samples_r <- NULL
      samples_s <- NULL
      #N_samples_r <- 0
      #N_samples_s <- 0
      
      round_N_samples_r <- 0
      round_N_samples_r_trace <- NULL
      
      round_sum_r <- 0
      round_sum_r_trace <- NULL
      
      round_winner_trace <- NULL
      
      all_rounds <- NULL
      
      DV <- 0
      DV_trace <- NULL
      
      choice_trace <- NULL
      

      
      boundary_reached <- FALSE
      
      
      round <- 1
      init <- sample(c("r", "s"), size=1) # option attended first; no attention bias
      attend <- init
  
      
      ## sampling and accumulation process

      while(boundary_reached == FALSE) {

        #initial option
        
        while(attend == init) { 
          
          at <- c(at, attend)
          N_samples <- N_samples + 1
          N_samples_trace <- c(N_samples_trace, N_samples)
          
          #round_sample_n <- round_sample_n + 1
          #round_sample_n_trace <- c(round_sample_n_trace, round_sample_n)
          
          ### draw sample and update evidence for respective option
          
          if(attend == "r") {
            
            #N_samples_r <- N_samples_r + 1
            
            sampled_outcome <- sample(x = c(r_1, r_2), size = 1, prob = c(p_r_1, p_r_2))
            samples_r <- c(samples_r, sampled_outcome)
            samples_s <- c(samples_s, NA)
            
            round_N_samples_r <- round_N_samples_r + 1
            round_N_samples_r_trace <- c(round_N_samples_r_trace, round_N_samples_r)
            
            round_sum_r <- round_sum_r + sampled_outcome
            round_sum_r_trace <- c(round_sum_r_trace, round_sum_r)
            
            p_attend_r <- 1-psi
            
            
            } else {
              
              #N_samples_s <- N_samples_s + 1
              
              samples_s <- c(samples_s, safe)
              samples_r <- c(samples_r, NA)
              
              round_N_samples_r_trace <- c(round_N_samples_r_trace, round_N_samples_r)
              
              round_sum_r_trace <- c(round_sum_r_trace, round_sum_r)
              
              p_attend_r <- psi
              
            }
          
          attend <- sample(c("r", "s"), size = 1, prob = c(p_attend_r, 1-p_attend_r)) # switching according to psi
          round_winner_trace <- c(round_winner_trace, NA)
          DV_trace <- c(DV_trace, DV)
          choice_trace <- c(choice_trace, NA)
        } 
        
        
        # second option
        
        while(attend != init) {
          
          at <- c(at, attend)
          N_samples <- N_samples + 1
          N_samples_trace <- c(N_samples_trace, N_samples)
          
          #round_sample_n <- round_sample_n + 1
          #round_sample_n_trace <- c(round_sample_n_trace, round_sample_n)
          
          if(attend == "r") {
            
            #N_samples_r <- N_samples_r + 1
            
            sampled_outcome <- sample(x = c(r_1, r_2), size = 1, prob = c(p_r_1, p_r_2))
            samples_r <- c(samples_r, sampled_outcome)
            samples_s <- c(samples_s, NA)
            
            round_N_samples_r <- round_N_samples_r + 1
            round_N_samples_r_trace <- c(round_N_samples_r_trace, round_N_samples_r)
            
            round_sum_r <- round_sum_r + sampled_outcome
            round_sum_r_trace <- c(round_sum_r_trace, round_sum_r)
            
            p_attend_r <- 1-psi
            
            } else {
              
              #N_samples_s <- N_samples_s + 1
              
              samples_s <- c(samples_s, safe)
              samples_r <- c(samples_r, NA)
              
              round_N_samples_r_trace <- c(round_N_samples_r_trace, round_N_samples_r)
              
              round_sum_r_trace <- c(round_sum_r_trace, round_sum_r)
              
              p_attend_r <- psi
              
            }
          
          attend <- sample(c("r", "s"), size = 1, prob = c(p_attend_r, 1-p_attend_r)) # switching according to psi
          if(attend!=init){
            round_winner_trace <- c(round_winner_trace, NA)
            DV_trace <- c(DV_trace, DV)
            choice_trace <- c(choice_trace, NA)
            }
        } 
        
        
        ### after each sampling round, update evidence and check if threshold is reached
        
        round_mean_r <- round_sum_r/round_N_samples_r
        round_winner <- ifelse(round_mean_r > safe, 1, ifelse(round_mean_r < safe, -1, 0))
        round_winner_trace <- c(round_winner_trace, round_winner)
        DV <- DV + round_winner
        DV_trace <- c(DV_trace, DV)
        
        round_store <- data.frame(round, round_N_samples_r_trace, round_sum_r_trace, round_winner_trace)
        all_rounds <- rbind(all_rounds, round_store)
        
        choice <- ifelse(DV >= theta, "r", ifelse(DV <= -1*theta, "s", NA))
        choice_trace <- c(choice_trace, choice)
        
        
        ### if threshold isn't reached, start new comparison round
        if(is.na(choice)) {
          
          round <- round + 1
          
          round_N_samples_r <- 0
          round_N_samples_r_trace <- NULL
          
          round_sum_r <- 0
          round_sum_r_trace <- NULL
          
          round_winner_trace <- NULL
          
          
        } else {
          boundary_reached <- TRUE
        }
        
        } # close loop choice trial
      
      trial_data <- cbind(N_samples_trace, at, samples_s, samples_r, all_rounds, DV_trace, choice_trace)
      agents_list[[agent]] <- expand_grid(agent, trial_data)
      
    } # close loop agents
    all_agents <- bind_rows(agents_list)
    problem_list[[problem]] <- expand_grid(id=problems[problem,"id"], all_agents)
  } # close loop problems
  all_problems <- bind_rows(problem_list)
  param_list[[set]] <- expand_grid(param[set, ], all_problems)
  print(paste("\u2713 Parameter Set No. ", set, " finished!"))
} # close loop parameters
simulation_roundwise <- bind_rows(param_list)

# save data
checksum_simulation_roundwise <- digest(simulation_roundwise, "sha256")
write_rds(simulation_roundwise, "data/simulation_roundwise_balanced.rds.bz2", compress = "bz2")
