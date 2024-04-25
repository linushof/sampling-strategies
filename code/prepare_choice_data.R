pacman::p_load(tidyverse, digest)

# summary model
simulation_summary <- read_rds("data/simulation_summary.rds.bz2")

## transform data to obtain trial summaries
choice_data_summary <- simulation_summary %>%
  group_by(psi, theta, id, agent) %>%
  mutate(n_sample = n(), # total number of single samples
         n_s = sum(is.na(samples_r)), # number of single samples drawn from safe option
         n_r = n_sample - n_s, # number of single samples drawn from risky option
         ep_r_1 = round(sum(if_else(samples_r == r_1, 1, 0), na.rm = TRUE)/n_r, 2), # experienced probability of higher risky outcome
         ep_r_2 = round(1 - ep_r_1, 2), # experienced probability of lower risky outcome
         mean_r = round(mean(samples_r, na.rm = TRUE), 2)) %>% # sampled mean risky prospect
  ungroup() %>%
  filter(!is.na(choice_trace)) # discard single samples

## tidy data 
choice_data_summary <- choice_data_summary %>% 
  mutate(model = "summary") %>% # to interpret psi as switching probability
  rename(N_samples_trace = "sample_trace") %>% 
  select(model, psi:samples_r, n_sample, n_s, n_r, ep_r_1, ep_r_2, mean_r, choice_trace)
# rm(simulation_summary)

# roundwise model
#simulation_roundwise <- read_rds("data/simulation_roundwise.rds.bz2")
simulation_roundwise <- read_rds("data/simulation_roundwise_balanced_test.rds.bz2")

## transform data to obtain trial summaries
choice_data_roundwise <- simulation_roundwise %>% 
  group_by(psi, theta, id, agent) %>% 
    mutate(n_sample = n(),
           n_s = sum(is.na(samples_r)),
           n_r = n_sample - n_s, 
           ep_r_1 = round(sum(if_else(samples_r == r_1, 1, 0), na.rm = TRUE)/n_r, 2),
           ep_r_2 = round(1 - ep_r_1, 2),
           mean_r = round(mean(samples_r, na.rm = TRUE), 2)) %>%
  ungroup() %>%
  filter(!is.na(choice_trace))
    
## tidy data
choice_data_roundwise <- choice_data_roundwise %>% 
  mutate(model = "roundwise") %>%
  select(-samples_s) %>% 
  select(model, psi:samples_r, n_sample, n_s, n_r, ep_r_1, ep_r_2, mean_r, choice_trace)
#rm(simulation_roundwise)

# join data sets and save as compressed data file

## full data set
choice_data <- bind_rows(choice_data_summary, choice_data_roundwise)
checksum_choice_data <- digest(choice_data, "sha256")
write_rds(choice_data, "data/choice_data_balanced.rds.bz2", compress = "bz2")
