# README --------------------------------------------------------------------

'The dataset of Wulff et al. (2018) can be retrieved from: https://www.dirkwulff.org/#data)'

# Preparation -------------------------------------------------------------

# load packages 
pacman::p_load(tidyverse, brms, posterior, scico, ggExtra, gridExtra, knitr)

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
         choice, # choice
         trial, option, outcome ) %>% # search data
  rename(sample = trial, # sample number
         attended = option) %>% # option from which sample was drawn (0/A, 1/B)
  group_by(paper, id, subject, problem) %>% # to compute search data separately for each trial
  mutate(
    n_sample = max(sample) , # total sample size
    n_sample_1 = sum(attended) , # sample size option 1/B
    n_sample_0 = n_sample - n_sample_1 , # sample size option 0/A
    switch = ifelse(attended != lag(attended), 1, 0) , # identify switches
    n_switch = sum(switch, na.rm = TRUE) , # number of switches
    r_switch = round(n_switch/(n_sample - 1), 2)  # switching frequency
    ) %>% 
  ungroup()


# compute predictions of summary and roundwise comparison rule

predictions <- 
  
  dat %>%
  mutate(attendedc = attended) %>% # copy to track attended option after pivot_wider()
  pivot_wider(names_from = attended, values_from = outcome, names_prefix = "out_") %>% # split samples for option 0/A and 1/B
  rename(attended = attendedc) %>% 
  group_by(paper, id, subject, problem) %>% 
  
  ## predictions for summary strategy
  
  mutate(
    
    # integrate sampled outcomes by summing (as assumed by sampling strategies)
    
    sum_0 = round(sum(out_0, na.rm = TRUE), 2) , # evidence for option 0/A
    sum_1 = round(sum(out_1, na.rm = TRUE), 2) , # evidence for option 1/B
    summary_winner = case_when(sum_0 > sum_1 ~ 0 , # predict choice
                               sum_0 < sum_1 ~ 1, 
                               sum_0 == sum_1 ~ NA) , 
    choose_summary_winner = case_when(summary_winner == choice ~ 1 , # compare predicted and observed choice
                                      summary_winner != choice ~ 0 ,
                                      is.na(summary_winner) ~ .5) , 
    
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
### see Appendix for predictions when incomplete rounds are not considered 

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
                                 round_tally == 0 ~ NA) , 
    choose_roundwise_winner = case_when(roundwise_winner == choice ~ 1 , # compare predicted and observed choice
                                        roundwise_winner != choice ~ 0 ,
                                        is.na(roundwise_winner) ~ .5) 
    ) %>%
  ungroup() %>%
  filter(round == n_round) %>% # keep only one row for each trial that summarizes all comparison rounds
  select(!c(sample, switch, attended, out_0, out_1, # drop uninformative sampling and round data
            complete, start, start_o, stop, new_round, round, r_mean_0, r_mean_1, r_winner))
  

# Analysis ----------------------------------------------------------------

predictions_win %>% 
  filter(
    dom == "Gain", cert = TRUE , # filter gain problems with safe option
    probA3 == 0 & probA4 == 0 & probA5 == 0 , # filter problems with maximum two outcomes
    probB3 == 0 & probB4 == 0 & probB5 == 0
   ) %>% 
  filter(n_sample_0 != 0 & n_sample_1 != 0) %>% # delete trials where only one option was sampled
  nrow()

predictions_win %>% distinct(paper, id, subject) %>% nrow()  # N = 3598
Ntrial <- predictions_win %>% distinct(paper, id, subject, problem) %>% nrow()  
Ntrial # N = 23057
    
## Description -----------------------------------------------------

consistency <- predictions_win %>% 
  group_by(choose_summary_winner, choose_roundwise_winner) %>% 
  summarise(N = n()) %>% 
  mutate(perc = round(N/Ntrial, 3)) %>% 
  ungroup()
sum(consistency$perc)

consistency %>% 
  select(!N) %>% 
  rename(Summary = choose_summary_winner, 
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
  arrange(Prediction, Summary) %>% 
  kable(format = "latex")


# correct
consistency %>% 
  filter(choose_summary_winner == TRUE | choose_roundwise_winner == TRUE) %>% 
  summarise(N = sum(N), 
            prop = sum(perc)) # 79.2%

# false
consistency %>% 
  filter(choose_summary_winner == FALSE & choose_roundwise_winner == FALSE) %>% 
  summarise(N = sum(N), prop = sum(perc)) # 13.6%

# indifference

consistency %>% 
  filter( (choose_summary_winner == .5 & choose_roundwise_winner == 0)  | (choose_summary_winner == 0 & choose_roundwise_winner == .5)  | (choose_summary_winner == .5 & choose_roundwise_winner == .5)   ) %>% 
  summarise(N = sum(N),
    prop = sum(perc)) # 7.2%


## Regression -------------------------------------------

# remove trials where both comparison rules made a false predictions
sub <- predictions_win %>% 
  filter(!(choose_summary_winner == FALSE & choose_roundwise_winner == FALSE))

set.seed(16381)
sub <- sub %>% 
  
  # for trials where strategies are indifferent, predict choice 
  mutate(
    choose_summary_winner_pred = case_when(summary_winner == choice ~ 1 ,
                                           summary_winner != choice ~ 0 ,
                                           is.na(summary_winner) ~ rbinom(n=1,size=1,p=.5)) , 
    choose_roundwise_winner_pred = case_when(roundwise_winner == choice ~ 1 ,
                                             roundwise_winner != choice ~ 0 ,
                                             is.na(roundwise_winner) ~ rbinom(n=1,size=1,p=.5))
    ) %>% 
  
  # consider only trials in which both strategies make different predictions
  filter(choose_summary_winner_pred != choose_roundwise_winner_pred) %>% 
  mutate(choiceRoundwise = as.numeric(choose_roundwise_winner_pred))


sub %>% distinct(paper, id, subject) %>% nrow() # 2120
sub %>% distinct(paper, id, subject, problem) %>% nrow() #5745

## logistic regression of roundwise (vs. summary) choice on switch rate

mfit <- brm(choiceRoundwise ~ 1 + r_switch + (1 + r_switch|paper) , 
                    data = sub , 
                    family = bernoulli() , 
                    chains = 4 , 
                    cores = 4)
    
summary(mfit)


## plot results  
    
mfit_post <- as_draws_array(mfit_NA_false)
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
    
props <- sub %>% 
  #filter(summary_winner != roundwise_winner) %>% 
  mutate(better_model = if_else(choose_roundwise_winner_pred == T, 1, 0)) %>% 
  group_by(paper, id, subject) %>%
  mutate(nChoices = n() , 
         nRoundwise = sum(better_model, na.rm = TRUE) , 
         propRoundwise = round(nRoundwise/nChoices, 3) , 
         mSwitch = mean(r_switch, na.rm = TRUE)
         ) %>%
  ungroup() %>% 
  distinct(paper, subject, nChoices, mSwitch, propRoundwise)

    
p_win <- props %>%
  ggplot() +
  geom_point(aes(x=mSwitch, y=propRoundwise, size = nChoices), alpha = .1) +
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



# Appendix ----------------------------------------------------------------

## Drop incomplete rounds ----------------------------------------------------------------

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


## Hills and Hertwig (2010) data -------------------------------------------------------

data %>% filter(paper %in% c("Hertwig04", "Hau08", "Ungemach09", "Hertwig10"))


## Hills and Hertwig (2010) approach ---------------------------------------------------------

# remove trials where both comparison rules made a false predictions
sub2 <- predictions_win %>% 
  filter(!(choose_summary_winner_mean == FALSE & choose_roundwise_winner == FALSE))


set.seed(1656381)
sub2 <- sub2 %>% 
  
  # for trials where strategies are indifferent, predict choice 
  mutate(
    choose_summary_winner_mean_pred = case_when(summary_winner_mean == choice ~ 1 ,
                                                summary_winner_mean != choice ~ 0 ,
                                                is.na(summary_winner_mean) ~ rbinom(n=1,size=1,p=.5)) , 
    choose_roundwise_winner_pred = case_when(roundwise_winner == choice ~ 1 ,
                                             roundwise_winner != choice ~ 0 ,
                                             is.na(roundwise_winner) ~ rbinom(n=1,size=1,p=.5))
  ) %>% 
  
  # consider only trials in which both strategies make different predictions
  filter(choose_summary_winner_mean_pred != choose_roundwise_winner_pred) %>% 
  mutate(choiceRoundwise = as.numeric(choose_roundwise_winner_pred))


## logistic regression of roundwise (vs. summary) choice on switch rate

mfit2 <- brm(choiceRoundwise ~ 1 + r_switch + (1 + r_switch|paper) , 
             data = sub2 , 
             family = bernoulli() , 
             chains = 4 , 
             cores = 4)

summary(mfit2)



    