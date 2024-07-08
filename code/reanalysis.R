pacman::p_load(brms, posterior, scico, ggExtra, gridExtra)

source("code/prepare_reanalysis_data.R")

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
mfit


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

hyper_a <- m_summary[[1, "mean"]]
hyper_b <- m_summary[[2, "mean"]]

# obtain regression lines
logistic <- function (x) {
  p <- 1/(1 + exp(-x))
  p <- ifelse(x == Inf, 1, p)
  p
}

shortnames <- data %>% distinct(paper, short) 

reglines <- m_summary[-c(1:2),] %>% 
  select(variable, paper, mean) %>% 
  pivot_wider(names_from = variable, values_from = mean) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(hyper_a + Intercept + (hyper_b+r_switch) * rate), 3)) %>% 
  left_join(shortnames, by = join_by(paper))

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

p <- props %>% 
  ggplot(aes(color = short)) + 
  geom_point(aes(x=mSwitch, y=propRoundwise, size = nChoices), alpha = .3) +
  geom_line(data = reglines, aes(x=rate, y=probRoundwise), linewidth = .6) +
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

check <- ggpubr::get_legend(p)

p_win <- p + theme(legend.position = "none")
pmarginal_win <- ggMarginal(p_win, type="density", groupFill = T, groupColour = T)

# incomplete rounds dropped

## logistic regression of roundwise (vs. summary) choice on switch rate 

reg_data_drop <- predictions_drop %>% 
  filter(summary_winner != roundwise_winner) %>% 
  mutate(choiceRoundwise = as.numeric(choose_roundwise_winner))

mfit_drop <- brm(choiceRoundwise ~ 1 + r_switch + (1 + r_switch|paper) , 
            data = reg_data_drop , 
            family = bernoulli() , 
            chains = 4 , 
            cores = 4)
mfit_drop


## plot results  

mfit_drop_post <- as_draws_array(mfit_drop)
m_drop_summary <- summarise_draws(mfit_drop_post, default_summary_measures()) %>% 
  mutate(paper = if_else(grepl("r_paper", variable),
                         sub("r_paper\\[(.*),.*\\]", "\\1", variable),
                         NA_character_),
         variable = if_else(grepl("r_paper", variable),
                            sub("r_paper\\[.*,([^]]*)\\]", "\\1", variable),
                            variable)) %>%
  select(variable, paper, everything()) %>% 
  filter(! variable %in% c("sd_paper__Intercept", "sd_paper__r_switch", "cor_paper__Intercept__r_switch", "sigma", "lprior", "lp__"))

hyper_a_drop <- m_drop_summary[[1, "mean"]]
hyper_b_drop <- m_drop_summary[[2, "mean"]]

reglines_drop <- m_drop_summary[-c(1:2),] %>% 
  select(variable, paper, mean) %>% 
  pivot_wider(names_from = variable, values_from = mean) %>% 
  expand_grid(rate = seq(0,1,.01)) %>% 
  mutate(probRoundwise = round(logistic(hyper_a_drop + Intercept + (hyper_b_drop+r_switch) * rate), 3)) %>% 
  left_join(shortnames, by = join_by(paper))

props_drop <- predictions_drop %>% 
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

p_drop <- props_drop %>% 
  ggplot(aes(color = short)) + 
  geom_point(aes(x=mSwitch, y=propRoundwise, size = nChoices), alpha = .3) +
  geom_line(data = reglines_drop, aes(x=rate, y=probRoundwise), linewidth = .6) +
  scale_color_scico_d(palette = "managua") + 
  labs(x = "Average Switch Rate",
       y = "Proportion of Roundwise Choices",
       size = "nChoices",
       color = "Paper",
       fill = "Paper") +
  theme_minimal(base_size = 16) +
  guides(shape = guide_legend(order = 2, nrow = 1) , 
         color = guide_legend(order = 1, nrow = 3)
  ) +
  theme(legend.position = "none", 
        legend.box = "vertical")

  
pmarginal_drop <- ggMarginal(p_drop, type="density", groupFill = T, groupColour = T)


# merge plot
merged <- plot_grid(pmarginal_win, pmarginal_drop)
plot_grid(merged, check, ncol = 1, rel_heights = c(1, .3))

ggsave(file = "manuscript/figures/reanalysis.png", width = 14, height = 10)