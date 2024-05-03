# load packages
pacman::p_load(tidyverse, digest, readxl)

# load data
problems <- as.data.frame(read_xlsx("data/choice_problems.xlsx"))
simulation_summary <- read_rds("data/simulation_summary.rds.bz2")
simulation_roundwise <- read_rds("data/simulation_roundwise.rds.bz2")

# summary comparison ------------------------------------------------------

simulation_summary <- left_join(simulation_summary, problems, by=join_by(id)) # add problem features

# compute summaries of choice trials
choice_data_summary <- simulation_summary %>%
  group_by(psi, theta, id, agent) %>%
  mutate(n_smp = n(), # number of samples
         smp_s = sum(is.na(out_r)), # number of samples safe option
         smp_r = n_smp - smp_s, # number of samples risky option
         sp_r_1 = round(sum(if_else(out_r == r_1, 1, 0), na.rm = TRUE)/smp_r, 2), # sampled probability risky outcome 1
         sp_r_2 = round(1 - sp_r_1, 2), # sampled probability risky outcome 2
         avg_r = round(mean(out_r, na.rm = TRUE), 2)) %>% # sampled average risky option
  ungroup() %>%
  filter(!is.na(choice)) # discard samples without choice

# tidy data 
choice_data_summary <- choice_data_summary %>% 
  mutate(model = "summary") %>%
  select(model, psi, theta, id, agent, n_smp, smp_s, smp_r, sp_r_1, sp_r_2, avg_r, choice) %>% 
  rename(smp = "n_smp")


# roundwise comparison ----------------------------------------------------

simulation_roundwise <- left_join(simulation_roundwise, problems, by=join_by(id)) # add problem features

# compute summaries of choice trials
choice_data_roundwise <- simulation_roundwise %>% 
  group_by(psi, theta, id, agent) %>% 
    mutate(n_smp = n(), # number of samples
           smp_s = sum(is.na(out_r)), # number of samples safe option
           smp_r = n_smp - smp_s, # number of samples risky option
           sp_r_1 = round(sum(if_else(out_r == r_1, 1, 0), na.rm = TRUE)/smp_r, 2), # sampled probability risky outcome 1
           sp_r_2 = round(1 - sp_r_1, 2), # sampled probability risky outcome 2
           avg_r = round(mean(out_r, na.rm = TRUE), 2)) %>% # sampled average risky option
  ungroup() %>%
  filter(!is.na(choice)) # discard samples without choice
    
# tidy data
choice_data_roundwise <- choice_data_roundwise %>% 
  mutate(model = "roundwise") %>%
  select(model, psi, theta, id, agent, n_smp, smp_s, smp_r, sp_r_1, sp_r_2, avg_r, choice) %>% 
  rename(smp = "n_smp")

# storage -----------------------------------------------------------------

choices <- bind_rows(choice_data_summary, choice_data_roundwise) # merge data sets
checksum_choices <- digest(choices, "sha256")
write_rds(choices, "data/choice_data.rds.bz2", compress = "bz2")