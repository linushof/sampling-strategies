# README --------------------------------------------------------------------

'The dataset of Wulff et al. (2018) can be retrieved from: https://www.dirkwulff.org/#data)'

# Preparation -------------------------------------------------------------

# load packages 
pacman::p_load(tidyverse, brms, posterior, scico, ggExtra, gridExtra, ggpubr, cowplot)

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


# Preparation -------------------------------------------------------------

source("code/helper_functions/fun_logistic.R")

# Main Analysis ----------------------------------------------------------------

d1 <- 
  predictions_win %>%
  filter(
    dom == "Gain" , # gains only 
    cert == TRUE , # includes safe option
    type == "free" ,  # free sampling conditions with gains only and a safe option
    probA3 == 0 & probA4 == 0 & probA5 == 0 , # problems with maximum two outcomes per option
    probB3 == 0 & probB4 == 0 & probB5 == 0
  ) %>% 
  filter(n_sample_0 != 0 & n_sample_1 != 0) # delete trials where only one option was sampled

## Description (Table 2) -----------------------------------------------------

Ntrial1 <- d1 %>% nrow()
Ntrial1 # 5627 trials

Npart1 <- d1 %>% distinct(paper, id, subject) %>% nrow()
Npart1 # 1877 participants

# compute proportion of trials where predictions were correct, false, and indifferent
consistency1 <- d1 %>% 
  group_by(choose_summary_winner, choose_roundwise_winner) %>% 
  summarise(N = n() ,
            mSwitch = mean(r_switch)) %>% 
  mutate(perc = round(N/Ntrial1, 4)) %>% 
  ungroup() %>% 
  select(choose_summary_winner, choose_roundwise_winner, N, perc, mSwitch)

# proportion correct
consistency1 %>% 
  filter(choose_summary_winner == 1 | choose_roundwise_winner == 1) %>%
  summarise(N = sum(N), prop = sum(perc)) # 82.5%

# proportion false
consistency1 %>% 
  filter(choose_summary_winner == FALSE & choose_roundwise_winner == FALSE) %>% 
  summarise(N = sum(N), prop = sum(perc)) # 12.2%

# proportion indifferent
consistency1 %>% 
  filter( (choose_summary_winner == .5 & choose_roundwise_winner == 0)  | (choose_summary_winner == 0 & choose_roundwise_winner == .5)  | (choose_summary_winner == .5 & choose_roundwise_winner == .5)   ) %>% 
  summarise(N = sum(N),
            prop = sum(perc)) # 5.4%


## Regression (Figure 12) -------------------------------------------

# only analyse trials where one comparison rule makes a correct prediction and the other a false prediction

reg_d1 <- d1 %>% 
  filter( (choose_summary_winner == 1 & choose_roundwise_winner == 0) | (choose_summary_winner == 0 & choose_roundwise_winner == 1) )


Ntrial_reg_d1 <- nrow(reg_d1)
Ntrial_reg_d1 # 1010 trials

Npart_reg_d1 <- reg_d1 %>% distinct(paper, id, subject) %>% nrow()
Npart_reg_d1 # 631


## regression of roundwise (vs. summary) choice on switch rate

fit_d1 <- brm(choose_roundwise_winner ~ 1 + r_switch + (1 | paper) , 
              data = reg_d1 , 
              family = bernoulli() , 
              chains = 6 , 
              cores = 6,
              iter = 4000,
              seed = 5161)

summary(fit_d1)

## plot results  

fit_d1_post <- as_draws_array(fit_d1)
m_summary_d1 <- summarise_draws(fit_d1_post, default_summary_measures()) %>% 
  mutate(paper = if_else(grepl("r_paper", variable) ,
                         sub("r_paper\\[(.*),.*\\]", "\\1", variable) ,
                         NA_character_) ,
         variable = if_else(grepl("r_paper", variable) ,
                            sub("r_paper\\[.*,([^]]*)\\]", "\\1", variable) ,
                            variable)) %>%
  select(variable, paper, everything()) %>%
  filter(! variable %in% c("sd_paper__Intercept", "sd_paper__r_switch", "cor_paper__Intercept__r_switch", "sigma", "lprior", "lp__"))

# obtain regression lines

regline_mean <- m_summary_d1[1:2, "mean"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = mean) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))

regline_5 <- m_summary_d1[1:2, "q5"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = q5) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))


regline_95 <- m_summary_d1[1:2, "q95"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = q95) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))


prop_d1 <- d1 %>% 
  filter( (choose_summary_winner == 1 & choose_roundwise_winner == 0) | (choose_summary_winner == 0 & choose_roundwise_winner == 1) ) %>%  # keep trials with different predictions of comparison rules
  group_by(paper, id, subject) %>%
  mutate(nChoices = n() , 
         mSwitch = mean(r_switch) ,
         propRoundwise = sum(choose_roundwise_winner)/nChoices) %>% 
  ungroup() %>% 
  distinct(paper, id, subject, nChoices, mSwitch, propRoundwise)


p_d1 <- prop_d1 %>%
  ggplot() +
  geom_line(data = regline_mean, aes(x=rate, y=probRoundwise), linewidth = 1, color = "#9c179e") +
  geom_line(data = regline_5, aes(x=rate, y=probRoundwise), linewidth = .3, color = "#9c179e") +
  geom_line(data = regline_95, aes(x=rate, y=probRoundwise), linewidth = .3, color = "#9c179e") +
  geom_ribbon(data = regline_5, 
              aes(x = rate, ymin = probRoundwise, ymax = regline_95$probRoundwise), 
              fill = "#9c179e", alpha = 0.2) +
  geom_point(aes(x=mSwitch, y=propRoundwise, size=nChoices), alpha = .1) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1, .5)) + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1, .5)) + 
  labs(x = "Average Switching Frequency",
       y = "Proportion of Roundwise Choices",
       size = "Choices",
       color = "Paper",
       fill = "Paper") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom", 
        legend.box = "vertical") + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))


p_d1_marginal <- ggMarginal(p_d1, type="density", fill = "gray")
p_d1_marginal
ggsave(p_d1_marginal, file = "manuscript/figures/reanalysis.png", width = 6, height = 6)


# Appendix (Figure F1) ----------------------------------------------------------------

## F1A) Drop incomplete rounds ----------------------------------------------------------------

# sampled option in incomplete rounds wins 

## logistic regression of roundwise (vs. summary) choice on switch rate 

d2 <- 
  predictions_drop %>%
  filter(
    dom == "Gain" , # gains only 
    cert == TRUE , # includes safe option
    type == "free" ,  # free sampling conditions with gains only and a safe option
    probA3 == 0 & probA4 == 0 & probA5 == 0 , # problems with maximum two outcomes per option
    probB3 == 0 & probB4 == 0 & probB5 == 0
  ) %>% 
  filter(n_sample_0 != 0 & n_sample_1 != 0) # delete trials where only one option was sampled

consistency2 <- d2 %>% 
  group_by(choose_summary_winner_mean, choose_roundwise_winner) %>% 
  summarise(N = n() ,
            mSwitch = mean(r_switch)) %>% 
  mutate(perc = round(N/Ntrial1, 3)) %>% 
  ungroup() %>% 
  select(choose_summary_winner_mean, choose_roundwise_winner, N, perc, mSwitch)

consistency2 %>% 
  select(!c(N, mSwitch)) %>% 
  rename(Summary = choose_summary_winner_mean, 
         Roundwise = choose_roundwise_winner,
         Proporion = perc) %>% 
  mutate(Summary = case_when(Summary == 0 ~ "F",
                             Summary == 1 ~ "C",
                             Summary == .5 ~ "In") ,
         Roundwise = case_when(Roundwise == 0 ~ "F",
                               Roundwise == 1 ~ "C",
                               Roundwise == .5 ~ "In")) %>% 
  mutate(Prediction = case_when(Summary == "C" | Roundwise == "C" ~ "Correct" , 
                                Summary == "F" & Roundwise == "F" ~ "False" , 
                                (Summary == "In" & Roundwise == "F")  | (Summary == "F" & Roundwise == "In")  | (Summary == "In" & Roundwise == "In") ~ "Indifferent")
  ) %>% 
  select(Prediction, everything()) %>% 
  arrange(Prediction, Summary)



reg_d2 <- d2 %>% 
  filter( (choose_summary_winner == 1 & choose_roundwise_winner == 0) | (choose_summary_winner == 0 & choose_roundwise_winner == 1) )

Ntrial_reg_d2 <- nrow(reg_d2)
Ntrial_reg_d2 # 1170 trials

Npart_reg_d2 <- reg_d2 %>% distinct(paper, id, subject) %>% nrow()
Npart_reg_d2 # 710

fit_d2 <- brm(choose_roundwise_winner ~ 1 + r_switch + (1 | paper)  , 
              data = reg_d2 , 
              family = bernoulli() , 
              chains = 6 , 
              cores = 6, 
              iter = 4000,
              seed = 61718)
summary(fit_d2)


# plot results  

fit_d2_post <- as_draws_array(fit_d2)
m_summary_d2 <- summarise_draws(fit_d2_post, default_summary_measures()) %>% 
  mutate(paper = if_else(grepl("r_paper", variable) ,
                         sub("r_paper\\[(.*),.*\\]", "\\1", variable) ,
                         NA_character_) ,
         variable = if_else(grepl("r_paper", variable) ,
                            sub("r_paper\\[.*,([^]]*)\\]", "\\1", variable) ,
                            variable)) %>%
  select(variable, paper, everything()) %>% 
  filter(! variable %in% c("sd_paper__Intercept", "sd_paper__r_switch", "cor_paper__Intercept__r_switch", "sigma", "lprior", "lp__"))


# obtain regression lines

regline_mean <- m_summary_d2[1:2, "mean"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = mean) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3)) 

regline_5 <- m_summary_d2[1:2, "q5"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = q5) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))

regline_95 <- m_summary_d2[1:2, "q95"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = q95) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))



prop_d2 <- d2 %>% 
  filter( (choose_summary_winner == 1 & choose_roundwise_winner == 0) | (choose_summary_winner == 0 & choose_roundwise_winner == 1) ) %>%  # keep trials with different predictions of comparison rules
  group_by(paper, id, subject) %>%
  mutate(nChoices = n() , 
         mSwitch = mean(r_switch) ,
         propRoundwise = sum(choose_roundwise_winner)/nChoices) %>% 
  ungroup() %>% 
  distinct(paper, id, subject, nChoices, mSwitch, propRoundwise)


p_d2 <- prop_d2 %>%
  ggplot() +
  geom_line(data = regline_mean, aes(x=rate, y=probRoundwise), linewidth = 1, color = "#9c179e") +
  geom_line(data = regline_5, aes(x=rate, y=probRoundwise), linewidth = .3, color = "#9c179e") +
  geom_line(data = regline_95, aes(x=rate, y=probRoundwise), linewidth = .3, color = "#9c179e") +
  geom_ribbon(data = regline_5, 
              aes(x = rate, ymin = probRoundwise, ymax = regline_95$probRoundwise), 
              fill = "#9c179e", alpha = 0.2) +
  geom_point(aes(x=mSwitch, y=propRoundwise, size=nChoices), alpha = .1) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1, .5)) + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1, .5)) + 
  labs(x = "Average Switching Frequency",
       y = "Proportion of Roundwise Choices",
       size = "Choices",
       color = "Paper",
       fill = "Paper") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom", 
        legend.box = "vertical") + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

p_d2_marginal <- ggMarginal(p_d2, type="density", fill = "gray")
p_d2_marginal

#ggsave(p_d2_marginal, file = "manuscript/figures/appendix/reanalysis_dropped.png", width = 6, height = 6)


## F1B) All data ------------------------------------------------------------

d3 <- 
  predictions_win %>%
  filter(type == "free") %>%   # free sampling conditions with gains only and a safe option
  filter(n_sample_0 != 0 & n_sample_1 != 0) # delete trials where only one option was sampled

reg_d3 <- d3 %>% 
  filter( (choose_summary_winner == 1 & choose_roundwise_winner == 0) | (choose_summary_winner == 0 & choose_roundwise_winner == 1) )


Ntrial_reg_d3 <- nrow(reg_d3)
Ntrial_reg_d3 # 6532 trials

Npart_reg_d3 <- reg_d3 %>% distinct(paper, id, subject) %>% nrow()
Npart_reg_d3 # 1569


## regression of roundwise (vs. summary) choice on switch rate

fit_d3 <- brm(choose_roundwise_winner ~ 1 + r_switch + (1 | paper) , 
              data = reg_d3 , 
              family = bernoulli() , 
              chains = 6 , 
              cores = 6,
              iter = 4000,
              seed = 61801)

summary(fit_d3)

## plot results  

fit_d3_post <- as_draws_array(fit_d3)
m_summary_d3 <- summarise_draws(fit_d3_post, default_summary_measures()) %>% 
  mutate(paper = if_else(grepl("r_paper", variable) ,
                         sub("r_paper\\[(.*),.*\\]", "\\1", variable) ,
                         NA_character_) ,
         variable = if_else(grepl("r_paper", variable) ,
                            sub("r_paper\\[.*,([^]]*)\\]", "\\1", variable) ,
                            variable)) %>%
  select(variable, paper, everything()) %>%
  filter(! variable %in% c("sd_paper__Intercept", "sd_paper__r_switch", "cor_paper__Intercept__r_switch", "sigma", "lprior", "lp__"))

# obtain regression lines

regline_mean <- m_summary_d3[1:2, "mean"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = mean) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))

regline_5 <- m_summary_d3[1:2, "q5"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = q5) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))


regline_95 <- m_summary_d3[1:2, "q95"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = q95) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))


prop_d3 <- d3 %>% 
  filter( (choose_summary_winner == 1 & choose_roundwise_winner == 0) | (choose_summary_winner == 0 & choose_roundwise_winner == 1) ) %>%  # keep trials with different predictions of comparison rules
  group_by(paper, id, subject) %>%
  mutate(nChoices = n() , 
         mSwitch = mean(r_switch) ,
         propRoundwise = sum(choose_roundwise_winner)/nChoices) %>% 
  ungroup() %>% 
  distinct(paper, id, subject, nChoices, mSwitch, propRoundwise)

p_d3 <- prop_d3 %>%
  mutate(trials = if_else(nChoices >= 10, "N >= 10", "N < 10")) %>% 
  ggplot() +
  geom_point(aes(x=mSwitch, y=propRoundwise, size=nChoices, color = trials), alpha =.5) +
  geom_line(data = regline_mean, aes(x=rate, y=probRoundwise), linewidth = 1, color = "darkgray") +
  geom_line(data = regline_5, aes(x=rate, y=probRoundwise), linewidth = .3, color = "darkgray") +
  geom_line(data = regline_95, aes(x=rate, y=probRoundwise), linewidth = .3, color = "darkgray") +
  geom_ribbon(data = regline_5, 
              aes(x = rate, ymin = probRoundwise, ymax = regline_95$probRoundwise), 
              fill = "gray", alpha = 0.2) +
  scale_color_scico_d(palette = "vanimo", labels = c("N < 10", "N \u2265 10"), begin = .1, end = .9) + 
  labs(x = "Average Switching Frequency",
       y = "Proportion of Roundwise Choices",
       size = "Choices",
       color = "Trials",
       fill = "Paper") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom", 
        legend.box = "vertical") +
  guides(shape = guide_legend(order = 2, nrow = 1) , 
         color = guide_legend(order = 1, nrow = 1, override.aes = list(size = 5))
  )


p_d3_marginal <- ggMarginal(p_d3, type="density", groupFill = TRUE)
p_d3_marginal
ggsave(p_d3_marginal, file = "manuscript/figures/appendix/reanalysis_large.png", width = 6, height = 6)


## Hills and Hertwig (2010) -------------------------------------------------------


### F1C) Data ----------------------------------------------------------------------

d5 <- d3 %>% filter(paper %in% c("Hertwig04", "Hau08", "Ungemach09", "Hertwig10"))


reg_d5 <- d5 %>% 
  filter( (choose_summary_winner_mean == 1 & choose_roundwise_winner == 0) | (choose_summary_winner_mean == 0 & choose_roundwise_winner == 1) )

fit_d5 <- brm(choose_roundwise_winner ~ 1 + r_switch , 
              data = reg_d5 , 
              family = bernoulli() , 
              chains = 6 , 
              cores = 6,
              iter = 4000,
              seed = 1891)

summary(fit_d5)


## plot results  

fit_d5_post <- as_draws_array(fit_d5)
m_summary_d5 <- summarise_draws(fit_d5_post, default_summary_measures()) %>% 
  mutate(paper = if_else(grepl("r_paper", variable) ,
                         sub("r_paper\\[(.*),.*\\]", "\\1", variable) ,
                         NA_character_) ,
         variable = if_else(grepl("r_paper", variable) ,
                            sub("r_paper\\[.*,([^]]*)\\]", "\\1", variable) ,
                            variable)) %>%
  select(variable, paper, everything()) %>%
  filter(! variable %in% c("sd_paper__Intercept", "sd_paper__r_switch", "cor_paper__Intercept__r_switch", "sigma", "lprior", "lp__"))

# obtain regression lines

regline_mean <- m_summary_d5[1:2, "mean"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = mean) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))

regline_5 <- m_summary_d5[1:2, "q5"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = q5) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))


regline_95 <- m_summary_d5[1:2, "q95"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = q95) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))


prop_d5 <- d5 %>% 
  filter( (choose_summary_winner == 1 & choose_roundwise_winner == 0) | (choose_summary_winner == 0 & choose_roundwise_winner == 1) ) %>%  # keep trials with different predictions of comparison rules
  group_by(paper, id, subject) %>%
  mutate(nChoices = n() , 
         mSwitch = mean(r_switch) ,
         propRoundwise = sum(choose_roundwise_winner)/nChoices) %>% 
  ungroup() %>% 
  distinct(paper, id, subject, nChoices, mSwitch, propRoundwise)


p_d5 <- prop_d5 %>%
  ggplot() +
  geom_line(data = regline_mean, aes(x=rate, y=probRoundwise), linewidth = 1, color = "#9c179e") +
  geom_line(data = regline_5, aes(x=rate, y=probRoundwise), linewidth = .3, color = "#9c179e") +
  geom_line(data = regline_95, aes(x=rate, y=probRoundwise), linewidth = .3, color = "#9c179e") +
  geom_ribbon(data = regline_5, 
              aes(x = rate, ymin = probRoundwise, ymax = regline_95$probRoundwise), 
              fill = "#9c179e", alpha = 0.2) +
  geom_point(aes(x=mSwitch, y=propRoundwise, size=nChoices), alpha = .1) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1, .5)) + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1, .5)) + 
  labs(x = "Average Switching Frequency",
       y = "Proportion of Roundwise Choices",
       size = "Choices",
       color = "Paper",
       fill = "Paper") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom", 
        legend.box = "vertical") + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

p_d5_marginal <- ggMarginal(p_d5, type="density", fill = "gray")
p_d5_marginal


### F1D) Mean --------------------------------------------------------------------

d4 <- d1

reg_d4 <- d4 %>% 
  filter( (choose_summary_winner_mean == 1 & choose_roundwise_winner == 0) | (choose_summary_winner_mean == 0 & choose_roundwise_winner == 1) )


Ntrial_reg_d4 <- nrow(reg_d4)
Ntrial_reg_d4 # 591 trials

Npart_reg_d4 <- reg_d4 %>% distinct(paper, id, subject) %>% nrow()
Npart_reg_d4 # 367


## regression of roundwise (vs. summary) choice on switch rate

fit_d4 <- brm(choose_roundwise_winner ~ 1 + r_switch + (1 | paper) , 
              data = reg_d4 , 
              family = bernoulli() , 
              chains = 6 , 
              cores = 6,
              iter = 4000, 
              seed = 7719)

summary(fit_d4)

## plot results  

fit_d4_post <- as_draws_array(fit_d4)
m_summary_d4 <- summarise_draws(fit_d4_post, default_summary_measures()) %>% 
  mutate(paper = if_else(grepl("r_paper", variable) ,
                         sub("r_paper\\[(.*),.*\\]", "\\1", variable) ,
                         NA_character_) ,
         variable = if_else(grepl("r_paper", variable) ,
                            sub("r_paper\\[.*,([^]]*)\\]", "\\1", variable) ,
                            variable)) %>%
  select(variable, paper, everything()) %>%
  filter(! variable %in% c("sd_paper__Intercept", "sd_paper__r_switch", "cor_paper__Intercept__r_switch", "sigma", "lprior", "lp__"))

# obtain regression lines

regline_mean <- m_summary_d4[1:2, "mean"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = mean) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))

regline_5 <- m_summary_d4[1:2, "q5"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = q5) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))


regline_95 <- m_summary_d4[1:2, "q95"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = q95) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))


prop_d4 <- d4 %>% 
  filter( (choose_summary_winner == 1 & choose_roundwise_winner == 0) | (choose_summary_winner == 0 & choose_roundwise_winner == 1) ) %>%  # keep trials with different predictions of comparison rules
  group_by(paper, id, subject) %>%
  mutate(nChoices = n() , 
         mSwitch = mean(r_switch) ,
         propRoundwise = sum(choose_roundwise_winner)/nChoices) %>% 
  ungroup() %>% 
  distinct(paper, id, subject, nChoices, mSwitch, propRoundwise)


p_d4 <- prop_d4 %>%
  ggplot() +
  geom_line(data = regline_mean, aes(x=rate, y=probRoundwise), linewidth = 1, color = "#9c179e") +
  geom_line(data = regline_5, aes(x=rate, y=probRoundwise), linewidth = .3, color = "#9c179e") +
  geom_line(data = regline_95, aes(x=rate, y=probRoundwise), linewidth = .3, color = "#9c179e") +
  geom_ribbon(data = regline_5, 
              aes(x = rate, ymin = probRoundwise, ymax = regline_95$probRoundwise), 
              fill = "#9c179e", alpha = 0.2) +
  geom_point(aes(x=mSwitch, y=propRoundwise, size=nChoices), alpha = .1) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1, .5)) + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1, .5)) + 
  labs(x = "Average Switching Frequency",
       y = "Proportion of Roundwise Choices",
       size = "Choices",
       color = "Paper",
       fill = "Paper") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom", 
        legend.box = "vertical") + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

p_d4_marginal <- ggMarginal(p_d4, type="density", fill = "gray")
p_d4_marginal


# Merge

reanalyis_app <- ggarrange(p_d2_marginal, p_d3_marginal, 
                           p_d5_marginal, p_d4_marginal,
                           nrow = 2, ncol = 2, labels = "AUTO", font.label = list(size = 22)) 
ggsave(reanalyis_app, file = "manuscript/figures/appendix/reanalysis_app.png", width = 14, height = 14)
