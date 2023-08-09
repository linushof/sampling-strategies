# load pkgs
pacman::p_load(tidyverse, digest, crayon)
source("code/helper_functions/fun_compute_cumulative_stats.R") # call functions for computing cumulative stats

# test set
choice_problems <- read_rds("data/choice_problems.rds")
n_agents <- 200 # number of synthetic agents

# simulation parameters
param <- expand_grid(psi = seq(-.5, .4, .1) , # probability increment added to unbiased sampling probability of p = .5 (switching probability)
                     threshold = c("absolute", "relative") , # threshold type
                     theta = seq(1, 5, 1)) # thresholds

# simulation: for each parameter combination (rows of param), all choice problems (rows of choice_problems) are solved by all agents

set.seed(56221)
param_list <- vector("list", nrow(param))
for (set in seq_len(nrow(param))) {  # loop over parameter combinations
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
      round <- 1
      boundary_reached <- FALSE

      ## sampling and accumulation process

      while(boundary_reached == FALSE) {

        smpl_round <- tibble()
        while(attend == init) { # sampling sequence from option attended first
          
          ### draw sample from either risky (r) or (s) safe option
          
          if(attend == "r") {
            single_smpl <- choice_problems[problem, ] %>%
              mutate(round = round ,
                     attended = attend ,
                     r = sample(x = c(r_low, r_high), size = 1, prob = c(p_r_low, p_r_high)) ,
                     s = NA)
            psi <- param[[set, "psi"]] # to update the probability of sampling from r again
            } else {
              single_smpl <- choice_problems[problem, ] %>%
                mutate(round = round ,
                       attended = attend ,
                       r = NA ,
                       s = safe)
              psi <- -1*param[[set, "psi"]] # to update the probability of of sampling from s again
            }
          
          ### add sample to sampling round
          
          smpl_round <- bind_rows(smpl_round, single_smpl)
          attend <- sample(c("r", "s"), size = 1, prob = c(p + psi, p - psi)) # switching according to psi
        } # close sampling sequence on first option
        
        while(attend != init) { # sampling sequence from option attended second
          
          ### draw sample from either risky (r) or (s) safe option
          
          if(attend == "r") {
            single_smpl <- choice_problems[problem, ] %>%
              mutate(round = round ,
                     attended = attend ,
                     r = sample(x = c(r_low, r_high), size = 1, prob = c(p_r_low, p_r_high)) ,
                     s = NA)
            psi <- param[[set, "psi"]] # to update the probability of sampling from r again
            } else {
              single_smpl <- choice_problems[problem, ] %>%
                mutate(round = round ,
                       attended = attend ,
                       r = NA ,
                       s = safe)
              psi <- -1*param[[set, "psi"]] # to update the probability of sampling from s again
            }
          
          ### add sample to sampling round
          
          smpl_round <- bind_rows(smpl_round, single_smpl)
          attend <- sample(c("r", "s"), size = 1, prob = c(p + psi, p - psi)) # switching according to psi
        } # close sampling sequence on second option (sampling round)
        
        ### compare means over sampling sequences and assign round wins
        
        smpl_round <- smpl_round %>%
          mutate(r_rmean = cummean2(r, na.rm = TRUE) ,
                 s_rmean = cummean2(s, na.rm = TRUE) ,
                 rdiff = r_rmean - s_rmean)
        smpl_round[[nrow(smpl_round), "r_win"]] <- case_when(smpl_round[[nrow(smpl_round), "rdiff"]] > 0 ~ 1 ,
                                                             smpl_round[[nrow(smpl_round), "rdiff"]] <= 0 ~ 0)
        smpl_round[[nrow(smpl_round), "s_win"]] <- case_when(smpl_round[[nrow(smpl_round), "rdiff"]] >= 0 ~ 0 ,
                                                             smpl_round[[nrow(smpl_round), "rdiff"]] < 0 ~ 1)
        
        ### add sampling round to other samples and update evidence

        fd <- bind_rows(fd, smpl_round)
        fd[[nrow(fd), "r_sum"]] <- sum(fd[["r_win"]], na.rm = TRUE)
        fd[[nrow(fd), "s_sum"]] <- sum(fd[["s_win"]], na.rm = TRUE)
        
        ### after each sampling round, check if accumulated evidence reached threshold
        
        if(param[[set, "threshold"]] == "absolute") {
          fd <- fd %>%
            mutate(choice = case_when(r_sum >= param[[set, "theta"]] ~ "r" ,
                                      s_sum >= param[[set, "theta"]] ~ "s"))
          } else {
            fd[[nrow(fd), "diff"]] <- fd[[nrow(fd), "r_sum"]] - fd[[nrow(fd), "s_sum"]]
            fd <- fd %>%
              mutate(choice = case_when(diff >= param[[set, "theta"]] ~ "r" ,
                                        diff <= -1*param[[set, "theta"]] ~ "s"))
          }
        
        ### if threshold isn't reached, start new comparison round
        
        if(is.na(fd[[nrow(fd), "choice"]]) == FALSE) {
          boundary_reached <- TRUE
          } else {
            round <- round + 1
          }
        } # close loop choice trial
      agents_list[[agent]] <- expand_grid(agent, fd)
    } # close loop agents
    all_agents <- agents_list %>% bind_rows()
    problem_list[[problem]] <- expand_grid(problem, all_agents)
    print(paste("\u2713 Parameter Set No. ", set, ", Problem No. ", problem, " finished!"))
  } # close loop problems
  all_problems <- problem_list %>% bind_rows()
  param_list[[set]] <- expand_grid(param[set, ], all_problems)
} # close loop parameters
simulation_roundwise <- param_list %>% bind_rows()

# save data
checksum_simulation_roundwise <- digest(simulation_roundwise, "sha256")
write_rds(simulation_roundwise, "data/merged/simulation_roundwise.rds.bz2", compress = "bz2")

round_rt <- simulation_roundwise %>% filter(threshold == "relative")
checksum_simulation_roundwise <- digest(round_rt, "sha256")
write_rds(round_rt, "data/simulation_roundwise.rds.bz2", compress = "bz2")

round_at <- simulation_roundwise %>% filter(threshold == "absolute")
checksum_simulation_roundwise <- digest(round_at, "sha256")
write_rds(round_at, "data/absolute_thresholds/simulation_roundwise_absolute.rds.bz2", compress = "bz2")
