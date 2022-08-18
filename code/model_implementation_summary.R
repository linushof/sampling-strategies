pacman::p_load(tidyverse)
source("R/fun_cumulative_stats.R") # call functions for computing cumulative stats

# test set
choice_problems <- read_rds("data/choice_problems.rds")
n_agents <- 100 # number of synthetic agents

# simulation parameters
param <- expand_grid(psi = seq(-.5, .4, .1), # probability increment added to unbiased sampling probability of p = .5
                     threshold = c("absolute", "relative"), # threshold type
                     theta = seq(15, 75, 15)) # thresholds

# simulation
## for each parameter combination (rows of param), all choice problems are played by all agents
## 100 (parameter combinations) x 60 (problems) x 100 (agents) = 600.000 trials

set.seed(19543)
param_list <- vector("list", length(nrow(param)))
for (set in seq_len(nrow(param))) { # loop over parameter combinations
  problem_list <- vector("list", length(nrow(choice_problems)))
  for (problem in seq_len(nrow(choice_problems))) { # loop over choice problems
    agents_list <- vector("list", n_agents)
    for (agent in seq_along(1:n_agents)){ # loop over agents

      # initiate trials in a state of ignorance

      fd <- tibble() # frequency distribution of sampled outcomes
      p <- .5  # no attention bias
      psi <- 0  # no switching at process initiation
      init <- sample(c("r", "s"), size = 1, prob = c(p + psi, p - psi)) # prospect attended first
      attend <- init
      boundary_reached <- FALSE

      # sampling of outcomes from risky (r) and safe (s) prospect continues until boundary is reached

      while(boundary_reached == FALSE) {
        
        # draw single sample from either r or s

        if(attend == "r") {
          single_smpl <- choice_problems[problem, ] %>%
            mutate(attended = attend,
                   r = sample(x = c(x_low, x_high), size = 1, prob = c(p_x_low, p_x_high)),
                   s = NA)
          psi <- param[[set, "psi"]] # to update the probability of sampling from r again
        } else {
          single_smpl <- choice_problems[problem, ] %>%
            mutate(attended = attend,
                   r = NA,
                   s = safe)
          psi <- -1*param[[set, "psi"]] # to update the probability of sampling from s again
        }

        # add single sample to frequency distribution of sampled outcomes

        fd <- bind_rows(fd, single_smpl) %>%
          mutate(r_sum = cumsum2(r, na.rm = TRUE),
                 s_sum = cumsum2(s, na.rm = TRUE))

        # evaluate accumulated evidence over fd
        # evidence is either compared against the absolute or relative boundary

        if(param[[set, "threshold"]] == "absolute") {
          fd <- fd %>%
            mutate(choice = case_when(r_sum >= param[[set, "theta"]] ~ "r",
                                      s_sum >= param[[set, "theta"]] ~ "s"))
        } else {
          fd <- fd %>%
            mutate(diff = round(r_sum - s_sum, 2),
                   choice = case_when(diff >= param[[set, "theta"]] ~ "r",
                                      diff <= -1*param[[set, "theta"]] ~ "s"))
        }

        # if boundary is not reached, draw new sample from r (s) according to psi

        if(is.na(fd[[nrow(fd), "choice"]]) == FALSE) {
          boundary_reached <- TRUE
        } else {
          attend <- sample(c("r", "s"), size = 1, prob = c(p + psi, p - psi))
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
simulation_summary <- param_list %>% map_dfr(as.list)

# safe as compressed data file
write_rds(simulation_summary, "data/simulation_summary.rds.bz2", compress = "bz2")