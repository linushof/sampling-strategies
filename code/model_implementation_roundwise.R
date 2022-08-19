pacman::p_load(tidyverse)
source("R/helper_functions/fun_compute_cumulative_stats.R") # call functions for computing cumulative stats

# test set
choice_problems <- read_rds("data/choice_problems.rds")
n_agents <- 100 # number of synthetic agents

# simulation parameters
param <- expand_grid(psi = seq(-.5, .4, .1), # probability increment added to unbiased sampling probability of p = .5
                     threshold = c("absolute", "relative"), # threshold type
                     theta = seq(1, 5, 1)) # thresholds

# simulation
## for each parameter combination (rows of param), all choice problems are played by all agents
## 100 (parameter combinations) x 60 (problems) x 100 (agents) = 600.000 trials

set.seed(56221)
param_list <- vector("list", length(nrow(param)))
for (set in seq_len(nrow(param))) {  # loop over parameter combinations
  problem_list <- vector("list", length(nrow(choice_problems)))
  for (problem in seq_len(nrow(choice_problems))) { # loop over gambles
    agents_list <- vector("list", n_agents)
    for (agent in seq_along(1:n_agents)){ # loop over agents

      # initiate trials in a state of ignorance

      fd <- tibble() # frequency distribution of sampled outcomes
      p <- .5  # no attention bias
      psi <- 0  # no switching at process initiation
      init <- sample(c("r", "s"), size = 1, prob = c(p + psi, p - psi)) # prospect attended first
      attend <- init
      round <- 1
      boundary_reached <- FALSE

      # sampling of outcomes from risky (r) and safe (s) prospect continues until boundary is reached

      while(boundary_reached == FALSE) {

        # prospects are compared round-wise
        ## a round consists of an uninterrupted sequence of sampled outcomes each from r and s
        ## prospects with a higher mean of sampled outcomes within a round earn a round-win

        smpl_round <- tibble()
        while(attend == init) {# sequence of single samples from prospect attended first

          # draw single sample from either r or s

          if(attend == "r") {
            single_smpl <- choice_problems[problem, ] %>%
              mutate(round = round,
                     attended = attend,
                     r = sample(x = c(x_low, x_high), size = 1, prob = c(p_x_low, p_x_high)),
                     s = NA)
            psi <- param[[set, "psi"]] # to update the probability of sampling from r again
          } else {
            single_smpl <- choice_problems[problem, ] %>%
              mutate(round = round,
                     attended = attend,
                     r = NA,
                     s = safe)
            psi <- -1*theta[[set, "psi"]] # to update the probability of of sampling from s again
          }
          smpl_round <- bind_rows(smpl_round, single_smpl)
          attend <- sample(c("r", "s"), size = 1, prob = c(p + psi, p - psi))
        }

        while(attend != init) { # sequence of single samples from prospect attended second

          # draw single sample from either r or s

          if(attend == "r") {
            single_smpl <- choice_problems[problem, ] %>%
              mutate(round = round,
                     attended = attend,
                     r = sample(x = c(x_low, x_high), size = 1, prob = c(p_x_low, p_x_high)),
                     s = NA)
            psi <- theta[[set, "psi"]]
          } else {
            single_smpl <- choice_problems[problem, ] %>%
              mutate(round = round,
                     attended = attend,
                     r = NA,
                     s = safe)
            psi <- -1*theta[[set, "psi"]]
          }
          smpl_round <- bind_rows(smpl_round, single_smpl)
          attend <- sample(c("r", "s"), size = 1, prob = c(p + psi, p - psi))
        }

        # compare means over sampled outcomes from r and s
        # assign round-win

        smpl_round <- smpl_round %>%
          mutate(r_rmean = cummean2(r, na.rm = TRUE),
                 s_rmean = cummean2(s, na.rm = TRUE),
                 rdiff = r_rmean - s_rmean)
        smpl_round[[nrow(smpl_round), "r_win"]] <- case_when(smpl_round[[nrow(smpl_round), "rdiff"]] > 0 ~ 1,
                                                             smpl_round[[nrow(smpl_round), "rdiff"]] <= 0 ~ 0)
        smpl_round[[nrow(smpl_round), "s_win"]] <-  case_when(smpl_round[[nrow(smpl_round), "rdiff"]] >= 0 ~ 0,
                                                              smpl_round[[nrow(smpl_round), "rdiff"]] < 0 ~ 1)

        # add sampling round to frequency distribution of sampled outcomes

        fd <- bind_rows(fd, smpl_round)
        fd[[nrow(fd), "r_sum"]] <- sum(fd[["r_win"]], na.rm = TRUE)
        fd[[nrow(fd), "s_sum"]] <- sum(fd[["s_win"]], na.rm = TRUE)

        # evaluate accumulated evidence (as round-wins) over fd
        # evidence is either compared against the absolute or relative boundary

        if(theta[[set, "boundary"]] == "absolute") {
          fd <- fd %>%
            mutate(choice = case_when(r_sum >= param[[set, "theta"]] ~ "r",
                                      s_sum >= param[[set, "theta"]] ~ "s"))
        } else {
          fd[[nrow(fd), "diff"]] <- fd[[nrow(fd), "r_sum"]] - fd[[nrow(fd), "s_sum"]]
          fd <- fd %>%
            mutate(choice = case_when(diff >= param[[set, "theta"]] ~ "r",
                                      diff <= -1*param[[set, "theta"]] ~ "s"))
        }

        # if boundary is not reached, start new sampling round

        if(is.na(fd[[nrow(fd), "choice"]]) == FALSE) {
          boundary_reached <- TRUE
        } else {
          round <- round + 1
        }
      }
      agents_list[[agent]] <- expand_grid(agent, fd)
    }
    all_agents <- agents_list %>% map_dfr(as.list)
    problem_list[[problem]] <- expand_grid(problem, all_agents)
  }
  all_problems <- problem_list %>% map_dfr(as.list)
  param_list[[set]] <- expand_grid(param[set, ], all_problems)
}
simulation_roundwise <- param_list %>% map_dfr(as.list)

# Data validation: 
## the hash function (sha-256 algorithm) must return the checksum displayed below. If not, the newly simulated data is not the same as the original data.
checksum_simulation_roundwise <- digest(simulation_roundwise, "sha256")
if(checksum_simulation_roundwise != "d4cdd2eb149d1802a0173dddb615cb4d494d3081b00b4d50607f3c73e4a4f854"){
  warning("Mismatch between current and original data. Current checksum is: '", checksum_simulation_roundwise, "'")
} else{cat(green("Data validated. Current data matches the original data."))
}

# safe as compressed data file
write_rds(simulation_roundwise, "data/simulation_roundwise.rds.bz2", compress = "bz2")