# Preparation -------------------------------------------------------------

# load packages
pacman::p_load(tidyverse, digest, crayon, readxl)

# load choice problems
problems <- as.data.frame(read_xlsx("data/choice_problems.xlsx"))

# Simulation --------------------------------------------------------------
# for each strategy (combination of the search rule and stopping rule; rows of param), all choice problems (rows of problems) are solved by N agents

# specify parameters for search rule (psi; switching probability) and stopping rule (theta; threshold)
param <- expand.grid(psi = seq(.1, 1, .1) ,
                     theta = 1:5)

n_agents <- 1000 # specify number of agents (iterations per strategy and problem)

set.seed(77816535) # seed random number generator to make simulations reproducible

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
      out_r <- NULL # sampled outcome (risky option)
      out_s <- NULL # sampled outcome (safe option)
      round <- 1 # number of comparison rounds
      round_smp_r_total <- 0 # number of risky samples (within a round) 
      round_sum_r_total <- 0 # sum of risky samples (within a round)
      diff <- 0 # accumulated evidence
      boundary_reached <- FALSE # choice trial is stopped when TRUE
      
      # specify vectors to store sampling & evidence history
      
      smp <- NULL # total number of sampled outcomes 
      D <- NULL # trajectory of decision variable
      choice <- NULL # final choice (risky or safe or no choice)
      win <- NULL # round win indicator
      all_rounds <- NULL
      #round_smp_r <- NULL # number of risky samples (within a round)
      #round_sum_r <- NULL # sum of risky samples (within a round)
  
      # START SAMPLING AND ACCUMULATION PROCESS
      
      init <- sample(c("r", "s"), size=1) # randomly choose option to initially sample from
      attend <- init
      while(boundary_reached == FALSE) {

        # start new comparison round on the initially sampled option
        
        while(attend == init) { 
          
          # update total number of samples
          
          at <- c(at, attend)
          smp_total <- smp_total + 1
          smp <- c(smp, smp_total)
          
          # sample outcome for the initially sampled option and update number and sum of sampled outcomes (round level)
          
          if(attend == "r") {# risky option
            
            sampled_outcome <- sample(x = c(r_1, r_2), size = 1, prob = c(p_r_1, p_r_2))
            out_r <- c(out_r, sampled_outcome)
            out_s <- c(out_s, NA)
            round_smp_r_total <- round_smp_r_total + 1
            #round_smp_r <- c(round_smp_r, round_smp_r_total)
            round_sum_r_total <- round_sum_r_total + sampled_outcome
            #round_sum_r <- c(round_sum_r, round_sum_r_total)
            p_attend_r <- 1-psi # probability of remaining on the risky option (for the next sample)
            
            } else {# safe option
              
              out_s <- c(out_s, safe)
              out_r <- c(out_r, NA)
              #round_smp_r <- c(round_smp_r, round_smp_r_total)
              #round_sum_r <- c(round_sum_r, round_sum_r_total)
              p_attend_r <- psi # probability of switching to the safe option (for the next sample)
              
            }
          
          # determine from which option to sample next
          
          attend <- sample(c("r", "s"), size = 1, prob = c(p_attend_r, 1-p_attend_r)) # switching according to psi
          
          # add an entry to the following vectors, indicating that round is not completed yet
          win <- c(win, NA) # no round winner is determined 
          D <- c(D, diff) # decision variable does not change
          choice <- c(choice, NA) # no choice determined
          
        } 
        
        # switch to the other option
        
        while(attend != init) {
          
          # update total number of samples
          
          at <- c(at, attend)
          smp_total <- smp_total + 1
          smp <- c(smp, smp_total)
          
          # sample outcome for the other option and update number and sum of sampled outcomes (round level)
          
          if(attend == "r") {# risky option
            
            sampled_outcome <- sample(x = c(r_1, r_2), size = 1, prob = c(p_r_1, p_r_2))
            out_r <- c(out_r, sampled_outcome)
            out_s <- c(out_s, NA)
            round_smp_r_total <- round_smp_r_total + 1
            #round_smp_r <- c(round_smp_r, round_smp_r_total)
            round_sum_r_total <- round_sum_r_total + sampled_outcome
            #round_sum_r <- c(round_sum_r, round_sum_r_total)
            p_attend_r <- 1-psi # probability of remaining on the risky option (for the next sample)
            
            } else {# safe option
              
              out_s <- c(out_s, safe)
              out_r <- c(out_r, NA)
              #round_smp_r <- c(round_smp_r, round_smp_r_total)
              #round_sum_r <- c(round_sum_r, round_sum_r_total)
              p_attend_r <- psi # probability of switching to the safe option (for the next sample)
              
            }
          
          # determine from which option to sample next
          
          attend <- sample(c("r", "s"), size = 1, prob = c(p_attend_r, 1-p_attend_r)) # switching according to psi
          
          # check for switch back to the initial option
          
          if(attend!=init){ # no switch to initial option (round incomplete)
            
            # add an entry to the following vectors, indicating that round is not completed yet
            win <- c(win, NA)
            D <- c(D, diff)
            choice <- c(choice, NA)
            
            }
        } 
        
        # switch to initial option (round complete): determine round winner and update decision variable
        
        round_mean_r <- round_sum_r_total/round_smp_r_total # average sampled outcome (risky option, round level)
        round_winner <- ifelse(round_mean_r > safe, 1, ifelse(round_mean_r < safe, -1, 0))
        win <- c(win, round_winner)
        diff <- diff + round_winner
        D <- c(D, diff)
        
        #round_store <- data.frame(round, round_smp_r, round_sum_r, win)
        round_store <- data.frame(round, win)
        all_rounds <- rbind(all_rounds, round_store)
        
        # check if decision threshold is reached: if true, make a choice and stop trial
        
        boundary <- ifelse(diff >= theta, "r", ifelse(diff <= -1*theta, "s", NA))
        choice <- c(choice, boundary)
        
        if(is.na(boundary)) { # if threshold isn't reached (no choice), start new comparison round
          
          round <- round + 1
          round_smp_r_total <- 0
          #round_smp_r <- NULL
          round_sum_r_total <- 0
          #round_sum_r <- NULL
          win <- NULL
          
        } else {
          
          boundary_reached <- TRUE
          
          # STOP SAMPLING AND ACCUMULATION PROCESS
          
        }
        
        } # close loop choice trial
      agents_list[[agent]] <- data.frame(agent, smp, at, out_r, out_s, all_rounds, D, choice)
    } # close loop agents
    all_agents <- bind_rows(agents_list)
    problem_list[[problem]] <- as.data.frame(expand_grid(id=problems[problem,"id"], all_agents))
  } # close loop choice problems
  all_problems <- bind_rows(problem_list)
  param_list[[set]] <- as.data.frame(expand_grid(param[set, ], all_problems))
  print(paste("\u2713 Parameter Set No. ", set, " finished!"))
} # close loop strategies
simulation_roundwise <- bind_rows(param_list)

# Storage -----------------------------------------------------------------

checksum_simulation_roundwise <- digest(simulation_roundwise, "sha256")
write_rds(simulation_roundwise, "data/simulation_roundwise.rds.bz2", compress = "bz2")
