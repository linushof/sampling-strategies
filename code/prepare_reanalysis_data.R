# load packages 
pacman::p_load(tidyverse)

# load data from Wulff et al. (2018, retrieved from: https://www.dirkwulff.org/#data)
data <- read.table("data/exp.txt") %>% as_tibble()

# helper functions
source("code/helper_functions/fun_compute_cumulative_stats.R")

# preprocessing -----------------------------------------------------------

dat <- 
  data %>% 
  filter(
    dom == "Gain", cert = TRUE , # filter gain problems with safe option
    probA3 == 0 & probA4 == 0 & probA5 == 0 , # filter problems with maximum two outcomes
    probB3 == 0 & probB4 == 0 & probB5 == 0
    ) %>%
  select(paper:choice) %>% 
  rename(sample = trial, attended = option) %>% 
  group_by(paper, id, subject, problem) %>% # to compute variables on the trial level
  mutate(
    n_sample = max(sample) , # sample size
    n_sample_1 = sum(attended) , # sample size option 1/B
    n_sample_0 = n_sample - n_sample_1 , # sample size option 0/A
    switch = ifelse(attended != lag(attended), 1, 0) , # did switch occur
    n_switch = sum(switch, na.rm = TRUE) , # number of switches
    r_switch = round(n_switch/((n_sample - 1)), 2)  # observed switch rate
    ) %>% 
  ungroup()
  

# obtain predictions of comparison strategies

predictions <- 

dat %>% 
  mutate(attendedc = attended) %>% 
  pivot_wider(names_from = attended, values_from = outcome, names_prefix = "out_") %>% 
  rename(attended = attendedc) %>% 
  group_by(paper, id, subject, problem) %>%
  
  ## summary strategy
  
  mutate(
    sum_0 = round(sum(out_0, na.rm = TRUE), 2) ,
    sum_1 = round(sum(out_1, na.rm = TRUE), 2) , 
    summary_winner = if_else(sum_0 > sum_1, 0, if_else(sum_0 < sum_1, 1, NA)), 
    choose_summary_winner = choice == summary_winner
         ) %>% 
  
  
  
  ## roundwise strategy 
  
  mutate(
    start = ifelse(sample == 1 & attended == 0, 0, ifelse(sample == 1 & attended == 1, 1, NA)) , # identify start option
    start_o = ifelse(is.na(start), first(start), start) ,
    stop = ifelse(sample == max(sample), TRUE, NA) ,
    new_round = case_when(switch == 0 | is.na(switch) ~ 0 , 
                          switch == 1 & start_o == 1 & is.na(out_1) ~ 0 ,
                          switch == 1 & start_o == 0 & is.na(out_0) ~ 0 ,
                          switch == 1 & start_o == 0 & is.na(out_1) ~ 1 ,
                          switch == 1 & start_o == 1 & is.na(out_0) ~ 1
                               ) ,
    round = 1 + cumsum2(new_round) , 
    complete = case_when(stop == TRUE & start_o == 1 & is.na(out_0) ~ 0 ,
                         stop == TRUE & start_o == 0 & is.na(out_1) ~ 0 ,
                         stop == TRUE & start_o == 0 & is.na(out_0) ~ 1 ,
                         stop == TRUE & start_o == 1 & is.na(out_1) ~ 1
                         ) , 
    complete = if_else(is.na(complete), last(complete), complete)
    ) 

    # sampled option in incomplete rounds wins
    
    predictions_win <- 
      predictions %>%
      mutate(n_round = max(round)) %>% 
      group_by(paper, id, subject, problem, round) %>%
      mutate(
        r_mean_0 = round(mean(out_0, na.rm = TRUE), 2) ,
        r_mean_1 = round(mean(out_1, na.rm = TRUE), 2) , 
        r_winner = case_when(is.na(r_mean_0) ~ 1 , 
                             is.na(r_mean_1) ~ -1 , 
                             r_mean_0 > r_mean_1 ~ -1 , 
                             r_mean_0 < r_mean_1 ~ 1,
                             r_mean_0 == r_mean_1 ~ 0)
        ) %>% 
      ungroup() %>% 
      distinct(paper, id, subject, problem, subject, choice, n_sample, n_sample_0, n_sample_1, r_switch, sum_0, sum_1, summary_winner, choose_summary_winner, n_round, round, r_mean_0, r_mean_1, r_winner) %>% 
      group_by(paper, id, subject, problem) %>% 
      mutate(
        round_tally = sum(r_winner, na.rm = TRUE) ,
        roundwise_winner = if_else(round_tally > 0, 1, if_else(round_tally < 0, 0, NA)) , 
        choose_roundwise_winner = choice == roundwise_winner
        ) %>% 
      ungroup() %>% 
      filter(round == n_round) %>% 
      filter(n_sample_0 != 0 & n_sample_1 != 0) %>% 
      select(paper, id, subject, problem, choice, n_sample, r_switch, sum_0, sum_1, summary_winner, choose_summary_winner, n_round, round_tally, roundwise_winner, choose_roundwise_winner)
    
    write_rds(predictions_win, "data/Wulff_et_al_2018_predictions", compress = "bz2")
    
    # incomplete rounds are dropped
    
    predictions_drop <- 
      predictions %>% 
      mutate(drop = if_else(complete == 0 & round == max(round), 1, 0)) %>% 
      filter(drop == 0) %>% 
      mutate(n_round = max(round)) %>% 
      group_by(paper, id, subject, problem, round) %>%
      mutate(
        r_mean_0 = round(mean(out_0, na.rm = TRUE), 2) ,
        r_mean_1 = round(mean(out_1, na.rm = TRUE), 2) , 
        r_winner = case_when(r_mean_0 > r_mean_1 ~ -1 , 
                             r_mean_0 < r_mean_1 ~ 1 ,
                             r_mean_0 == r_mean_1 ~ 0) # better option 0 - negative increment, better option 1 - positive increment
      ) %>% 
      ungroup() %>% 
      distinct(paper, id, subject, problem, subject, choice, n_sample, n_sample_0, n_sample_1, r_switch, sum_0, sum_1, summary_winner, choose_summary_winner, n_round, round, r_mean_0, r_mean_1, r_winner) %>% 
      group_by(paper, id, subject, problem) %>% 
      mutate(
        round_tally = sum(r_winner, na.rm = TRUE) ,
        roundwise_winner = if_else(round_tally > 0, 1, if_else(round_tally < 0, 0, NA)) , 
        choose_roundwise_winner = choice == roundwise_winner
      ) %>%
      ungroup() %>% 
      filter(round == n_round) %>% 
      filter(n_sample_0 != 0 & n_sample_1 != 0) %>% 
      select(paper, id, subject, problem, choice, n_sample, r_switch, sum_0, sum_1, summary_winner, choose_summary_winner, n_round, round_tally, roundwise_winner, choose_roundwise_winner)
  
    

    