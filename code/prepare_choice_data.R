pacman::p_load(tidyverse)

# summary model
simulation_summary <- read_rds("data/simulation_summary.rds.bz2")

## transform data to obtain trial summaries
choice_data_summary <- simulation_summary %>%
  group_by(psi, threshold, theta, problem, agent) %>%
  mutate(n_sample = n(), # total number of single samples
         n_s = sum(is.na(r)), # number of single samples drawn from safe option
         n_r = n_sample - n_s, # number of single samples drawn from risky option
         ep_r_high = round(sum(if_else(r == r_high, 1, 0), na.rm = TRUE)/n_r, 2), # experienced probability of higher risky outcome
         ep_r_low = round(1 - ep_r_high, 2), # experienced probability of lower risky outcome
         mean_r = round(mean(r, na.rm = TRUE), 2)) %>% # sampled mean risky prospect
  ungroup() %>%
  filter(!is.na(choice)) # discard single samples

## tidy data 
choice_data_summary <- choice_data_summary %>% 
  mutate(psi = 1-(psi+.5), 
         model = "summary") %>% # to interpret psi as switching probability
  select(model, psi:problem, rare, p_r_low:ev_ratio, agent, n_sample, n_s, n_r, ep_r_low, ep_r_high, mean_r, r_sum, s_sum, diff, choice)

# store data
write_rds(choice_data_summary, "data/choice_data_summary.rds.bz2", compress = "bz2")

# roundwise model
simulation_roundwise <- read_rds("data/simulation_roundwise.rds.bz2")

## transform data to obtain trial summaries
choice_data_roundwise <- simulation_roundwise %>% 
  group_by(psi, threshold, theta, problem, agent) %>% 
    mutate(n_sample = n(),
           n_s = sum(is.na(r)),
           n_r = n_sample - n_s, 
           ep_r_high = round(sum(if_else(r == r_high, 1, 0), na.rm = TRUE)/n_r, 2),
           ep_r_low = round(1 - ep_r_high, 2),
           mean_r = round(mean(r, na.rm = TRUE), 2)) %>%
  ungroup() %>%
  filter(!is.na(choice))
    
## tidy data
choice_data_roundwise <- choice_data_roundwise %>% 
  mutate(psi = 1-(psi+.5),
         model = "roundwise") %>%
  select(model, psi:problem, rare, p_r_low:ev_ratio, agent, n_sample, n_s, n_r, ep_r_low, ep_r_high, mean_r, r_sum, s_sum, diff, choice)

# store data
write_rds(choice_data_roundwise, "data/choice_data_roundwise.rds.bz2", compress = "bz2")

# join data sets and save as compressed data file
choice_data <- bind_rows(choice_data_summary, choice_data_roundwise)
write_rds(choice_data, "data/choice_data.rds.bz2", compress = "bz2")