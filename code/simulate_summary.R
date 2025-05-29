# NOTE --------------------------------------------------------------------

'In the paper, a scaling of outcomes by d=.01 is assumed in the specification of the summary comparison rule.
In the implementation below, scaling is omitted without any effect on the results of the simulation and their interpretation.
'

# Preparation -------------------------------------------------------------

# load packages
pacman::p_load(tidyverse, digest, crayon, readxl)

# load choice problems
problems <- as.data.frame(read_rds("data/choice_problems.rds"))

# Simulation --------------------------------------------------------------
# for each strategy (combination of the search rule and stopping rule; rows of param), all choice problems (rows of problems) are solved by N agents

# specify parameters for search rule (psi; switch rate) and stopping rule (theta; threshold)
param <- expand.grid(psi = seq(.1, 1, .1) , 
                     theta = seq(100, 300, 50))

n_agents <- 2 # specify number of agents (iterations per strategy and problem)

set.seed(36151) # seed random number generator to make simulations reproducible

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
    
    # option 1 (for SR gambles: risky option)
    o1_1 <- problems[[problem, "o1_1"]] # (smaller) outcome 1 
    o1_2 <- problems[[problem, "o1_2"]] # (larger) outcome 2 
    o1_p1 <- problems[[problem, "o1_p1"]] # probability outcome 1  
    o1_p2 <- problems[[problem, "o1_p2"]] # probability outcome 2 
    
    # option 2 (for SR gambles: safe option)
    o2_1 <- problems[[problem, "o2_1"]] # (smaller/safe) outcome 1
    o2_2 <- problems[[problem, "o2_2"]] # (larger/missing) outcome 2
    o2_p1 <- problems[[problem, "o2_p1"]] # probability outcome 1 (for SR gambles: 1) 
    o2_p2 <- problems[[problem, "o2_p2"]] # probability outcome 2 (for SR gambles: 0)
    
    # loop over agents
    agents_list <- vector("list", n_agents)
    for (agent in seq_along(1:n_agents)){
      
      # specify settings to initiate each choice trial
      
      at <- NULL # attended option
      smp_total <- 0 # total number of sampled outcomes
      smp_total_1 <- 0 # total number of sampled outcomes (option 1)
      smp_total_2 <- 0 # total number of sampled outcomes (option 2)
      out_1 <- NULL # sampled outcome (option 1)
      out_2 <- NULL # sampled outcome (option 2)
      D_1 <- 0 # accumulated evidence (option 1) 
      D_2 <- 0 # accumulated evidence (option 2)
      boundary_reached <- FALSE # choice trial is stopped when TRUE
      
      # specify vectors to store sampling & evidence history
      
      smp <- NULL # total number of sampled outcomes
      D <- NULL # trajectory of decision variable
      choice <- NULL # final choice (risky or safe or no choice)
      
      # START SAMPLING AND ACCUMULATION PROCESS
      
      init <- sample(c("o1", "o2"), size=1) # randomly choose option to initially sample from
      attend <- init
      while(boundary_reached == FALSE) {
        
        # update number of samples
        
        at <- c(at, attend)
        smp_total <- smp_total + 1
        smp <- c(smp, smp_total)
        
        # sample outcome and update evidence for respective option
        
        if(attend == "o1") {# option 1
          
          smp_total_1 <- smp_total_1 + 1
          sampled_outcome <- sample(x = c(o1_1, o1_2), size = 1, prob = c(o1_p1, o1_p2))
          out_1 <- c(out_1, sampled_outcome)
          out_2 <- c(out_2, NA)
          D_1 <- D_1 + sampled_outcome
          p_attend_1 <- 1-psi # probability of remaining on option 1 (for the next sample)
          
        } else {# safe option
          
          smp_total_2 <- smp_total_2 + 1
          sampled_outcome <- sample(x = c(o2_1, o2_2), size = 1, prob = c(o2_p1, o2_p2)) 
          out_2 <- c(out_2, sampled_outcome)
          out_1 <- c(out_1, NA)
          D_2 <- D_2 + sampled_outcome
          p_attend_1 <- psi # probability of switching to option 1 (for the next sample)
          
        }
        
        # update decision variable
        
        if(smp_total_1 > 0 & smp_total_2 > 0){ # choice is only possible after both options were sampled
          diff <- D_1 - D_2
          D <- c(D, diff)
        } else { 
          diff <- 0
          D <- c(D, diff)
        }
        
        # check if the decision threshold is reached: if true, make a choice and stop trial
        
        boundary <- ifelse(diff >= theta, "o1", ifelse(diff <= -1*theta, "o2", NA))
        choice <- c(choice, boundary)
        
        if(is.na(boundary)) { # if threshold isn't reached (no choice), determine from which option to sample next and continue
          
          attend <- sample(c("o1", "o2"), size=1, prob=c(p_attend_1, 1-p_attend_1))
          
        } else {
          
          boundary_reached <- TRUE
          
          # STOP SAMPLING AND ACCUMULATION PROCESS
          
        }
        
      } # close loop choice trial
      agents_list[[agent]] <- data.frame(agent, smp, at, out_1, out_2, D, choice)
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
write_rds(simulation_summary, "data/simulation_summary.rds.bz2", compress = "bz2")