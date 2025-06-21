
simulate_roundwise_decreasing <- function(problems, param, n_agents){
  
  # load required packages
  packages <- c("tidyverse", "digest", "crayon")
  sapply(packages, require, character.only = TRUE)
  
  # for each strategy (rows of param), all choice problems (rows of problems) are solved by N agents
  
  # loop over strategies
  param_list <- vector("list", nrow(param))
  for (set in seq_len(nrow(param))) {
    
    # retrieve settings for the search rule and the stopping rule
    base <- param[[set,"base"]]
    rate <- param[[set,"rate"]]
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
        out_1 <- NULL # sampled outcome (option 1)
        out_2 <- NULL # sampled outcome (option 2)
        round <- 1 # number of comparison rounds
        round_smp_1_total <- 0 # number of option 1 samples (within a round) 
        round_sum_1_total <- 0 # sum of option 1 samples (within a round)
        round_smp_2_total <- 0 # number of option 2 samples (within a round) 
        round_sum_2_total <- 0 # sum of option 2 samples (within a round)
        diff <- 0 # accumulated evidence
        boundary_reached <- FALSE # choice trial is stopped when TRUE
        psi <- 1
        
        # specify vectors to store sampling & evidence history
        smp <- NULL # total number of sampled outcomes 
        D <- NULL # trajectory of decision variable
        choice <- NULL # final choice (risky or safe or no choice)
        win <- NULL # round win indicator
        all_rounds <- NULL
        
        # START SAMPLING AND ACCUMULATION PROCESS
        
        init <- sample(c("o1", "o2"), size=1) # randomly choose option to initially sample from
        attend <- init
        while(boundary_reached == FALSE) {
          
          # start new comparison round on the initially sampled option
          
          while(attend == init) { 
            
            # update total number of samples
            
            at <- c(at, attend)
            smp_total <- smp_total + 1
            smp <- c(smp, smp_total)
            
            # sample outcome for the initially sampled option and update number and sum of sampled outcomes (round level)
            
            if(attend == "o1") {# risky option
              
              sampled_outcome <- sample(x = c(o1_1, o1_2), size = 1, prob = c(o1_p1, o1_p2))
              out_1 <- c(out_1, sampled_outcome)
              out_2 <- c(out_2, NA)
              round_smp_1_total <- round_smp_1_total + 1
              round_sum_1_total <- round_sum_1_total + sampled_outcome
              p_attend_1 <- 1-psi # probability of remaining on the risky option (for the next sample)
              psi <- base + (1 - base) * exp(-rate * smp_total)
              
              } else {# safe option
                
                sampled_outcome <- sample(x = c(o2_1, o2_2), size = 1, prob = c(o2_p1, o2_p2))
                out_2 <- c(out_2, sampled_outcome)
                out_1 <- c(out_1, NA)
                round_smp_2_total <- round_smp_2_total + 1
                round_sum_2_total <- round_sum_2_total + sampled_outcome
                p_attend_1 <- psi # probability of switching to the safe option (for the next sample)
                psi <- base + (1 - base) * exp(-rate * smp_total)
                
              }
            
            # determine from which option to sample next
            attend <- sample(c("o1", "o2"), size = 1, prob = c(p_attend_1, 1-p_attend_1)) # switching according to psi
            
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
            
            if(attend == "o1") {# option 1
              
              sampled_outcome <- sample(x = c(o1_1, o1_2), size = 1, prob = c(o1_p1, o1_p2))
              out_1 <- c(out_1, sampled_outcome)
              out_2 <- c(out_2, NA)
              round_smp_1_total <- round_smp_1_total + 1
              round_sum_1_total <- round_sum_1_total + sampled_outcome
              p_attend_1 <- 1-psi # probability of remaining on the risky option (for the next sample)
              psi <- base + (1 - base) * exp(-rate * smp_total)
              
              } else {# option 2
                
                sampled_outcome <- sample(x = c(o2_1, o2_2), size = 1, prob = c(o2_p1, o2_p2))
                out_2 <- c(out_2, sampled_outcome)
                out_1 <- c(out_1, NA)
                round_smp_2_total <- round_smp_2_total + 1
                round_sum_2_total <- round_sum_2_total + sampled_outcome
                p_attend_1 <- psi # probability of switching to the safe option (for the next sample)
                psi <- base + (1 - base) * exp(-rate * smp_total)
                
              }
            
            # determine from which option to sample next
            attend <- sample(c("o1", "o2"), size = 1, prob = c(p_attend_1, 1-p_attend_1)) # switching according to psi
            
            # check for switch back to the initial option
            if(attend!=init){ # no switch to initial option (round incomplete)
              
              # add an entry to the following vectors, indicating that round is not completed yet
              win <- c(win, NA)
              D <- c(D, diff)
              choice <- c(choice, NA)
              
            }
          }
          
          # switch to initial option (round complete): determine round winner and update decision variable
          round_mean_1 <- round_sum_1_total/round_smp_1_total # average sampled outcome (option 1, round level)
          round_mean_2 <- round_sum_2_total/round_smp_2_total # average sampled outcome (option 2, round level)
          round_winner <- ifelse(round_mean_1 > round_mean_2, 1, ifelse(round_mean_1 < round_mean_2, -1, 0))
          
          win <- c(win, round_winner)
          diff <- diff + round_winner
          D <- c(D, diff)
          
          #round_store <- data.frame(round, round_smp_r, round_sum_r, win)
          round_store <- data.frame(round, win)
          all_rounds <- rbind(all_rounds, round_store)
          
          # check if decision threshold is reached: if true, make a choice and stop trial
          boundary <- ifelse(diff >= theta, "o1", ifelse(diff <= -1*theta, "o2", NA))
          choice <- c(choice, boundary)
          
          if(is.na(boundary)) { # if threshold isn't reached (no choice), start new comparison round
            
            round <- round + 1
            round_smp_1_total <- 0
            round_sum_1_total <- 0
            round_smp_2_total <- 0
            round_sum_2_total <- 0
            win <- NULL
            
            } else {
              
              boundary_reached <- TRUE # STOP SAMPLING AND ACCUMULATION PROCESS
              
            }
          } # close loop choice trial
        agents_list[[agent]] <- data.frame(agent, smp, at, out_1, out_2, all_rounds, D, choice)
        } # close loop agents
      all_agents <- bind_rows(agents_list)
      problem_list[[problem]] <- as.data.frame(expand_grid(id=problems[problem,"id"], all_agents))
      } # close loop choice problems
    all_problems <- bind_rows(problem_list)
    param_list[[set]] <- as.data.frame(expand_grid(param[set, ], all_problems))
    print(paste("\u2713 Parameter Set No. ", set, " finished!"))
    } # close loop strategies
  simdata <- bind_rows(param_list)
  return(simdata)
}
