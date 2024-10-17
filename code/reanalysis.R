
# NOTE --------------------------------------------------------------------

'The dataset of Wulff et al. (2018) can be retrieved from: https://www.dirkwulff.org/#data)'

# preparation -------------------------------------------------------------

# load packages 
pacman::p_load(tidyverse, brms, posterior, scico, ggExtra, gridExtra)

# load data
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


# predictions -------------------------------------------------------------

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
                         r_mean_0 < r_mean_1 ~ 1 ,
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
  

# analysis ----------------------------------------------------------------

predictions_drop %>% distinct(paper, id, subject) %>% nrow() # N = 3598
predictions_win %>% distinct(paper, id, subject) %>% nrow()  # N = 3598
predictions_win %>% distinct(paper, id, subject, problem) %>% nrow()  # N = 23057
    
## overall consistency -----------------------------------------------------
    
predictions_win %>% nrow()  # N = 23057
predictions_win %>% filter(choose_summary_winner == TRUE | choose_roundwise_winner == TRUE) %>% nrow() # 18261
test <- predictions_win %>% filter(choose_summary_winner == TRUE | choose_roundwise_winner == TRUE)
test %>% filter(summary_winner  == roundwise_winner) %>% nrow() # 12194
    
    
## comparison rule ~ switch rate -------------------------------------------
    
# sampled option in incomplete rounds wins 
    
## logistic regression of roundwise (vs. summary) choice on switch rate 
    
reg_data <- predictions_win %>% 
  filter(summary_winner != roundwise_winner) %>% 
  mutate(choiceRoundwise = as.numeric(choose_roundwise_winner))
    
mfit <- brm(choiceRoundwise ~ 1 + r_switch + (1 + r_switch|paper) , 
            data = reg_data , 
            family = bernoulli() , 
            chains = 4 , 
            cores = 4)
    
summary(mfit)

## plot results  
    
mfit_post <- as_draws_array(mfit)
m_summary <- summarise_draws(mfit_post, default_summary_measures()) %>% 
  mutate(paper = if_else(grepl("r_paper", variable) ,
                         sub("r_paper\\[(.*),.*\\]", "\\1", variable) ,
                         NA_character_) ,
         variable = if_else(grepl("r_paper", variable) ,
                            sub("r_paper\\[.*,([^]]*)\\]", "\\1", variable) ,
                            variable)) %>%
  select(variable, paper, everything()) %>%
  filter(! variable %in% c("sd_paper__Intercept", "sd_paper__r_switch", "cor_paper__Intercept__r_switch", "sigma", "lprior", "lp__"))
    
    
# obtain regression lines

logistic <- function (x) {
      p <- 1/(1 + exp(-x))
      p <- ifelse(x == Inf, 1, p)
      p
}

regline_mean <- m_summary[1:2, "mean"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = mean) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3)) 
    
regline_5 <- m_summary[1:2, "q5"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = q5) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))
    
regline_95 <- m_summary[1:2, "q95"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = q95) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))
    
shortnames <- data %>% distinct(paper, short) 

props <- predictions_win %>% 
  filter(summary_winner != roundwise_winner) %>% 
  mutate(better_model = if_else(choose_roundwise_winner == T, 1, 0)) %>% 
  group_by(paper, subject) %>%
  mutate(nChoices = n() , 
         nRoundwise = sum(better_model, na.rm = TRUE) , 
         propRoundwise = round(nRoundwise/nChoices, 3) , 
         mSwitch = mean(r_switch, na.rm = TRUE)
         ) %>%
  ungroup() %>% 
  distinct(paper, subject, nChoices, mSwitch, propRoundwise) %>% 
  left_join(shortnames, by = join_by(paper))
    
p_win <- props %>%
  ggplot() +
  geom_point(aes(x=mSwitch, y=propRoundwise, size = nChoices), alpha = .1) +
  geom_density_2d_filled(aes(x=mSwitch, y=propRoundwise)) +
  #geom_line(data = regline_mean, aes(x=rate, y=probRoundwise), linewidth = 1) +
  #geom_line(data = regline_5, aes(x=rate, y=probRoundwise), linewidth = .8, linetype = "dotdash") +
  #geom_line(data = regline_95, aes(x=rate, y=probRoundwise), linewidth = .8, linetype = "dotdash") +
  scale_color_scico_d(palette = "managua") + 
  labs(x = "Average Switching Frequency",
       y = "Proportion of Roundwise Choices",
       size = "nChoices",
       color = "Paper",
       fill = "Paper") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom", 
        legend.box = "vertical") +
  guides(shape = guide_legend(order = 2, nrow = 1) , 
         color = guide_legend(order = 1, nrow = 3)
         )
    
pmarginal_win <- ggMarginal(p_win, type="density", fill = "gray")
ggsave(pmarginal_win, file = "manuscript/figures/reanalysis_win.png", width = 6, height = 6)
    
### Appendix: Incomplete Rounds Drop  ----------------------------------------------------------------

# sampled option in incomplete rounds wins 
    
## logistic regression of roundwise (vs. summary) choice on switch rate 
    
reg_data <- predictions_drop %>% 
  filter(summary_winner != roundwise_winner) %>% 
  mutate(choiceRoundwise = as.numeric(choose_roundwise_winner))
  
mfit <- brm(choiceRoundwise ~ 1 + r_switch + (1 + r_switch|paper) , 
            data = reg_data , 
            family = bernoulli() , 
            chains = 4 , 
            cores = 4)
    
# plot results  
    
mfit_post <- as_draws_array(mfit)
m_summary <- summarise_draws(mfit_post, default_summary_measures()) %>% 
  mutate(paper = if_else(grepl("r_paper", variable) ,
                         sub("r_paper\\[(.*),.*\\]", "\\1", variable) ,
                         NA_character_) ,
         variable = if_else(grepl("r_paper", variable) ,
                            sub("r_paper\\[.*,([^]]*)\\]", "\\1", variable) ,
                            variable)) %>%
  select(variable, paper, everything()) %>% 
  filter(! variable %in% c("sd_paper__Intercept", "sd_paper__r_switch", "cor_paper__Intercept__r_switch", "sigma", "lprior", "lp__"))
    
    
# obtain regression lines
    
regline_mean <- m_summary[1:2, "mean"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = mean) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3)) 
    
regline_5 <- m_summary[1:2, "q5"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = q5) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))
    
regline_95 <- m_summary[1:2, "q95"] %>% 
  mutate(variable=c("intercept", "slope")) %>% 
  pivot_wider(names_from = variable, values_from = q95) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(intercept + slope * rate), 3))
    
    
shortnames <- data %>% distinct(paper, short) 

props <- predictions_drop %>% 
  filter(summary_winner != roundwise_winner) %>% 
  mutate(better_model = if_else(choose_roundwise_winner == T, 1, 0)) %>% 
  group_by(paper, subject) %>%
  mutate(nChoices = n() , 
         nRoundwise = sum(better_model, na.rm = TRUE) , 
         propRoundwise = round(nRoundwise/nChoices, 3) , 
         mSwitch = mean(r_switch, na.rm = TRUE)
         ) %>%
  ungroup() %>% 
  distinct(paper, subject, nChoices, mSwitch, propRoundwise) %>% 
  left_join(shortnames, by = join_by(paper))
    
p_drop <- props %>% 
  ggplot() + 
  geom_point(aes(x=mSwitch, y=propRoundwise, size = nChoices), alpha = .1) +
  geom_line(data = regline_mean, aes(x=rate, y=probRoundwise), linewidth = 1) +
  geom_line(data = regline_5, aes(x=rate, y=probRoundwise), linewidth = .8, linetype = "dotdash") +
  geom_line(data = regline_95, aes(x=rate, y=probRoundwise), linewidth = .8, linetype = "dotdash") +
  scale_color_scico_d(palette = "managua") + 
  labs(x = "Average Switch Rate" ,
       y = "Proportion of Roundwise Choices" ,
       size = "nChoices" ,
       color = "Paper" ,
       fill = "Paper") + 
  theme_minimal(base_size = 16) + 
  theme(legend.position = "bottom" ,
        legend.box = "vertical") + 
  guides(shape = guide_legend(order = 2, nrow = 1) ,
         color = guide_legend(order = 1, nrow = 3)
         )

pmarginal_drop <- ggMarginal(p_drop, type="density", fill = "gray")
ggsave(pmarginal_win, file = "manuscript/figures/reanalysis_win.png", width = 6, height = 6)
    