pacman::p_load(brms, posterior, scico, ggExtra, gridExtra)

source("code/prepare_reanalysis_data.R")

predictions_drop %>% distinct(paper, id, subject) %>% nrow() # N = 3598
predictions_win %>% distinct(paper, id, subject) %>% nrow()  # N = 3598
predictions_win %>% distinct(paper, id, subject, problem) %>% nrow()  # N = 23057

# Overall consistency -----------------------------------------------------

predictions_win %>% nrow()  # N = 23057
predictions_win %>% filter(choose_summary_winner == TRUE | choose_roundwise_winner == TRUE) %>% nrow() # 18261
test <- predictions_win %>% filter(choose_summary_winner == TRUE | choose_roundwise_winner == TRUE)
test %>% filter(summary_winner  == roundwise_winner) %>% nrow() # 12194


# Comparison Rule ~ Switch Rate -------------------------------------------

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

## plot results  

mfit_post <- as_draws_array(mfit)
m_summary <- summarise_draws(mfit_post, default_summary_measures()) %>% 
  mutate(paper = if_else(grepl("r_paper", variable),
                         sub("r_paper\\[(.*),.*\\]", "\\1", variable),
                         NA_character_),
         variable = if_else(grepl("r_paper", variable),
                            sub("r_paper\\[.*,([^]]*)\\]", "\\1", variable),
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

cor(props$mSwitch, props$propRoundwise)

p_win <- props %>% 
  ggplot() + 
  geom_point(aes(x=mSwitch, y=propRoundwise, size = nChoices), alpha = .1) +
  geom_line(data = regline_mean, aes(x=rate, y=probRoundwise), linewidth = 1) +
  geom_line(data = regline_5, aes(x=rate, y=probRoundwise), linewidth = .8, linetype = "dotdash") +
  geom_line(data = regline_95, aes(x=rate, y=probRoundwise), linewidth = .8, linetype = "dotdash") +
  scale_color_scico_d(palette = "managua") + 
  labs(x = "Average Switch Rate",
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




# Appendix: Complete Rounds Drop ----------------------------------------------------------------


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

## plot results  

mfit_post <- as_draws_array(mfit)
m_summary <- summarise_draws(mfit_post, default_summary_measures()) %>% 
  mutate(paper = if_else(grepl("r_paper", variable),
                         sub("r_paper\\[(.*),.*\\]", "\\1", variable),
                         NA_character_),
         variable = if_else(grepl("r_paper", variable),
                            sub("r_paper\\[.*,([^]]*)\\]", "\\1", variable),
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
  labs(x = "Average Switch Rate",
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

pmarginal_drop <- ggMarginal(p_drop, type="density", fill = "gray")
ggsave(pmarginal_win, file = "manuscript/figures/reanalysis_win.png", width = 6, height = 6)
