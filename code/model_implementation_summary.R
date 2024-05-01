# Preparation -------------------------------------------------------------

# load packages
pacman::p_load(tidyverse, digest, crayon, readxl)

# load choice problems
problems <- as.data.frame(read_xlsx("data/choice_problems_balanced_refined.xlsx"))

# Simulation --------------------------------------------------------------
# for each strategy (combination of the search rule and stopping rule; rows of param), all choice problems (rows of problems) are solved by N agents

# specify parameters for search rule (psi; switching probability) and stopping rule (theta; threshold)
param <- expand.grid(psi = seq(.1, 1, .1) , 
                     theta = seq(100, 300, 50))

n_agents <- 1000 # specify number of agents (iterations per strategy and problem)

set.seed(1961578) # seed random number generator to make simulations reproducible

# loop over strategies
param_list <- vector("list", nrow(param)) 
for (set in seq_len(nrow(param))) {
  
  # retrieve settings for the search rule and the stopping rule
  psi <- param[[set,"psi"]]
  theta <- param[[set, "theta"]]
  
  # loop over choice problems
  problem_list <- vector("list", nrow(problems))
  for (problem in seq_len(nrow(problems))) {
    
    # retrieve problem features
    p_r_1 <- problems[[problem, "p_r_1"]] # probability risky outcome 1
    p_r_2 <- problems[[problem, "p_r_2"]] # probability risky outcome 2
    r_1 <- problems[[problem, "r_1"]] # risky outcome 1
    r_2 <- problems[[problem, "r_2"]] # risky outcome 2
    safe <- problems[[problem, "safe"]] # safe outcome
    
    # loop over agents
    agents_list <- vector("list", n_agents)
    for (agent in seq_along(1:n_agents)){

      # specify settings to initiate each choice trial
      
      at <- NULL # attended option
      smp_total <- 0 # total number of sampled outcomes
      smp_total_r <- 0 # total number of sampled outcomes (risky option)
      smp_total_s <- 0 # total number of sampled outcomes (safe option)
      out_r <- NULL # sampled outcome (risky option)
      out_s <- NULL # sampled outcome (safe option)
      D_r <- 0 # accumulated evidence (risky option) 
      D_s <- 0 # accumulated evidence (safe option)
      boundary_reached <- FALSE # choice trial is stopped when TRUE
      
      # specify vectors to store sampling & evidence history
      
      smp <- NULL # total number of sampled outcomes
      D <- NULL # trajectory of decision variable
      choice <- NULL # final choice (risky or safe or no choice)

      # START SAMPLING AND ACCUMULATION PROCESS
      
      init <- sample(c("r", "s"), size=1) # randomly choose option to initially sample from
      attend <- init
      while(boundary_reached == FALSE) {
        
        # update number of samples
        
        at <- c(at, attend)
        smp_total <- smp_total + 1
        smp <- c(smp, smp_total)
        
        # sample outcome and update evidence for respective option
        
        if(attend == "r") {# risky option
          
          smp_total_r <- smp_total_r + 1
          sampled_outcome <- sample(x = c(r_1, r_2), size = 1, prob = c(p_r_1, p_r_2))
          out_r <- c(out_r, sampled_outcome)
          out_s <- c(out_s, NA)
          D_r <- D_r + sampled_outcome
          p_attend_r <- 1-psi # probability of remaining on the risky option (for the next sample)
        
          } else {# safe option
            
            smp_total_s <- smp_total_s + 1
            out_s <- c(out_s, safe)
            out_r <- c(out_r, NA)
            D_s <- D_s + safe
            p_attend_r <- psi # probability of switching to the safe option (for the next sample)
            
          }
        
        # update decision variable
        
        if(smp_total_s > 0 & smp_total_r > 0){ # choice is only possible after both options were sampled
        diff <- D_r - D_s
        D <- c(D, diff)
        } else { 
          diff <- 0
          D <- c(D, diff)
        }
        
        # check if the decision threshold is reached: if true, make a choice and stop trial
        
        boundary <- ifelse(diff >= theta, "r", ifelse(diff <= -1*theta, "s", NA))
        choice <- c(choice, boundary)
        
        if(is.na(boundary)) { # if threshold isn't reached (no choice), determine from which option to sample next and continue
          
          attend <- sample(c("r", "s"), size=1, prob=c(p_attend_r, 1-p_attend_r))
          
        } else {
          
          boundary_reached <- TRUE
          
          # STOP SAMPLING AND ACCUMULATION PROCESS
          
        }
        
      } # close loop choice trial
      agents_list[[agent]] <- data.frame(agent, smp, at, out_r, out_s, D, choice)
    } # close loop agents
    all_agents <- bind_rows(agents_list)
    problem_list[[problem]] <- as.data.frame(expand_grid(id=problems[problem,"id"], all_agents))
  } # close loop choice problems
  all_problems <- bind_rows(problem_list) 
  param_list[[set]] <- as.data.frame(expand_grid(param[set, ], all_problems))
  print(paste("\u2713 Parameter Set No. ", set, " finished!"))
} # close loop strategies
simulation_summary <- bind_rows(param_list)

# Storage --------------------------------------------------------------

checksum_simulation_summary <- digest(simulation_summary, "sha256")
write_rds(simulation_summary, "data/simulation_summary_balanced_refined.rds.bz2", compress = "bz2")