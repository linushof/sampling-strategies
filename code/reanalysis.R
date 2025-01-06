# Preparation -------------------------------------------------------------

source("code/prepare_reanalysis.R") 
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
  arrange(Prediction, Summary) %>% 
  kable()



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
