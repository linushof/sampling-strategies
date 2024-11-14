# README --------------------------------------------------------------------

'The dataset of Wulff et al. (2018) can be retrieved from: https://www.dirkwulff.org/#data)'

# Preparation -------------------------------------------------------------

# load packages 
pacman::p_load(tidyverse, brms, posterior, scico, ggExtra, gridExtra, knitr, ggpubr, cowplot)

# load data
data <- read.table("data/exp.txt") %>% as_tibble()

# helper functions
source("code/helper_functions/fun_compute_cumulative_stats.R")

# Preprocessing -----------------------------------------------------------

# compute search data

dat <- 
  data %>% 
  select(paper, id, subject, problem, # unique combinations = single trial
         dom, cert, type, outA1:probB5, und, # features of choice task
         choice, # choice data
         trial, option, outcome ) %>% # search data
  rename(sample = trial, # sample number
         attended = option) %>% # option from which sample was drawn (0/A, 1/B)
  group_by(paper, id, subject, problem) %>% # to compute search data separately for each trial
  mutate(
    n_sample = max(sample) , # total sample size
    n_sample_1 = sum(attended) , # sample size option 1/B
    n_sample_0 = n_sample - n_sample_1 , # sample size option 0/A
    switch = ifelse(attended != lag(attended), 1, 0) , # identify switches
    n_switch = sum(switch, na.rm = TRUE) , # switch count
    r_switch = round(n_switch/(n_sample - 1), 2)  # switching frequency
  ) %>% 
  ungroup()


# compute predictions of summary and roundwise comparison rule

predictions <- 
  
  dat %>%
  mutate(attendedc = attended) %>% # copy to track attended option after pivot_wider()
  pivot_wider(names_from = attended, values_from = outcome, names_prefix = "out_") %>% # show samples from option 0/A and 1/B in separate columns
  rename(attended = attendedc) %>% 
  group_by(paper, id, subject, problem) %>% 
  
  ## predictions for summary strategy
  
  mutate(
    
    # integrate sampled outcomes by summing (as assumed by sampling strategies)
    
    sum_0 = round(sum(out_0, na.rm = TRUE), 2) , # evidence for option 0/A
    sum_1 = round(sum(out_1, na.rm = TRUE), 2) , # evidence for option 1/B
    summary_winner = case_when(sum_0 > sum_1 ~ 0 , # predict choice
                               sum_0 < sum_1 ~ 1, 
                               sum_0 == sum_1 ~ NA) , # equal evidence
    choose_summary_winner = case_when(summary_winner == choice ~ 1 , # compare predicted and observed choice
                                      summary_winner != choice ~ 0 ,
                                      is.na(summary_winner) ~ .5) , # guessing given equal evidence 
    
    # integrate sampled outcomes by averaging (as assumed by Hills & Hertwig)
    
    mean_0 = round(mean(out_0, na.rm = TRUE), 2) ,
    mean_1 = round(mean(out_1, na.rm = TRUE), 2) , 
    summary_winner_mean = case_when(mean_0 > mean_1 ~ 0 , 
                                    mean_0 < mean_1 ~ 1) , 
    choose_summary_winner_mean = case_when(summary_winner_mean == choice ~ 1 ,
                                           summary_winner_mean != choice ~ 0 ,
                                           is.na(summary_winner_mean) ~ .5) 
    
  ) %>% 
  
  
  ## predictions for roundwise strategy 
  
  ### identify sampling rounds
  
  mutate(
    start = ifelse(sample == 1 & attended == 0, 0, ifelse(sample == 1 & attended == 1, 1, NA)) , # identify start option
    start_o = ifelse(is.na(start), first(start), start) , 
    stop = ifelse(sample == max(sample), TRUE, NA) ,
    new_round = case_when(switch == 0 | is.na(switch) ~ 0 , # no switch = no new round
                          switch == 1 & start_o == 1 & is.na(out_1) ~ 0 , # switch from starting option: no new round  
                          switch == 1 & start_o == 0 & is.na(out_0) ~ 0 , 
                          switch == 1 & start_o == 0 & is.na(out_1) ~ 1 , # switch to starting option: new round
                          switch == 1 & start_o == 1 & is.na(out_0) ~ 1) ,
    round = 1 + cumsum2(new_round) , # assign round number
    complete = case_when(stop == TRUE & start_o == 1 & is.na(out_0) ~ 0 , # incomplete round: first and last sample from same option
                         stop == TRUE & start_o == 0 & is.na(out_1) ~ 0 ,
                         stop == TRUE & start_o == 0 & is.na(out_0) ~ 1 , # complete round: first and last sample from different options
                         stop == TRUE & start_o == 1 & is.na(out_1) ~ 1) , 
    complete = if_else(is.na(complete), last(complete), complete)
  )

### compute roundwise comparison and predict choices
### if the final round is incomplete, the sampled option wins the round as long as the sampled evidence is non-negative

predictions_win <- 
  predictions %>%
  mutate(n_round = max(round)) %>% 
  group_by(paper, id, subject, problem, round) %>%
  mutate(
    r_mean_0 = round(mean(out_0, na.rm = TRUE), 2) , # compute roundwise mean of option A/0
    r_mean_1 = round(mean(out_1, na.rm = TRUE), 2) , 
    r_winner = case_when(r_mean_0 > r_mean_1 ~ -1 , # option A/0 approaches negative threshold
                         r_mean_0 < r_mean_1 ~ 1 , # option B/1 approaches positive threshold
                         r_mean_0 == r_mean_1 ~ 0 ,
                         is.na(r_mean_0) & r_mean_1 >= 0 ~ 1 , # assign round wins for incomplete rounds
                         is.na(r_mean_0) & r_mean_1 < 0 ~ -1 ,
                         is.na(r_mean_1) & r_mean_0 >= 0 ~ -1 ,
                         is.na(r_mean_1) & r_mean_0 < 0 ~ 1 
    )
  ) %>% 
  ungroup() %>% 
  distinct(paper, id, subject, problem, round, .keep_all = TRUE) %>% # keep only one row for each round that summarizes the comparison
  group_by(paper, id, subject, problem) %>% 
  mutate(
    round_tally = sum(r_winner, na.rm = TRUE) , # compute accumulated evidence over rounds
    roundwise_winner = case_when(round_tally > 0 ~ 1 , # predict choice
                                 round_tally < 0 ~ 0 ,
                                 round_tally == 0 ~ NA) , # equal evidence
    choose_roundwise_winner = case_when(roundwise_winner == choice ~ 1 , # compare predicted and observed choice
                                        roundwise_winner != choice ~ 0 ,
                                        is.na(roundwise_winner) ~ .5) # guessing given equal evidence
  ) %>%
  ungroup() %>%
  filter(round == n_round) %>% # keep only one row for each trial that summarizes all comparison rounds
  select(!c(sample, switch, attended, out_0, out_1, # drop uninformative sampling and round data
            complete, start, start_o, stop, new_round, round, r_mean_0, r_mean_1, r_winner))



### incomplete rounds are not considered 

predictions_drop <- 
  predictions %>% 
  mutate(drop = if_else(complete == 0 & round == max(round), 1, 0)) %>% # indicate samples in incomplete last round
  filter(drop == 0) %>% # drop incomplete last round
  mutate(n_round = max(round)) %>%  # determine number of remaining rounds
  group_by(paper, id, subject, problem, round) %>%
  mutate(
    r_mean_0 = round(mean(out_0, na.rm = TRUE), 2) , # compute roundwise mean of option A/0
    r_mean_1 = round(mean(out_1, na.rm = TRUE), 2) , 
    r_winner = case_when(r_mean_0 > r_mean_1 ~ -1 , # option A/0 approaches negative threshold
                         r_mean_0 < r_mean_1 ~ 1 , # option B/1 approaches positive threshold
                         r_mean_0 == r_mean_1 ~ 0)
  ) %>% 
  ungroup() %>% 
  distinct(paper, id, subject, problem, round, .keep_all = TRUE) %>% # keep only one row for each round that summarizes the comparison
  group_by(paper, id, subject, problem) %>% 
  mutate(
    round_tally = sum(r_winner, na.rm = TRUE) , # compute accumulated evidence over rounds
    roundwise_winner = case_when(round_tally > 0 ~ 1 , # predict choice
                                 round_tally < 0 ~ 0 ,
                                 round_tally == 0 ~ NA) , 
    choose_roundwise_winner = case_when(roundwise_winner == choice ~ 1 , # compare predicted and observed choice
                                        roundwise_winner != choice ~ 0 ,
                                        is.na(roundwise_winner) ~ .5) 
  ) %>%
  ungroup() %>%
  filter(round == n_round) %>% # keep only one row for each trial that summarizes all comparison rounds
  select(!c(sample, switch, attended, out_0, out_1, # drop uninformative sampling and round data
            complete, start, start_o, stop, new_round, round, r_mean_0, r_mean_1, r_winner))

