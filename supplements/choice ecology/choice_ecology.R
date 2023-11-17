# load packages 
pacman::p_load(here,
               tidyverse,
               scico, # for scientific color palettes
               viridis,
               latex2exp, # for LaTeX expressions in plots
               papaja,
               ggpubr,
               patchwork, 
               gganimate)


# load data
choices <- read_rds("data/choice_data.rds.bz2") 
problems <- read_rds("data/choice_problems.rds") 

# labels

# theta
label_theta <- function(string) {
  TeX(paste("$\\theta=$", string, sep = ""))
}

# psi
label_psi <- function(string) {
  TeX(paste("$\\psi=$", string, sep = ""))
}

# risk aversion analysis -----------------------------------------------------

# choice problems with better EV for safe vs. risky 

n_better_safe <- problems %>% filter(safe > r_ev) %>% nrow()
round(n_better_safe/nrow(problems), 2) # .53

n_better_safe_exp <- choices %>% filter(model == "roundwise", threshold == "relative", safe > mean_r) %>% nrow()
n_exp <- choices %>% filter(model == "roundwise", threshold == "relative") %>% nrow()
round(n_better_safe_exp/n_exp, 2) # .54

# proportion of choice from the safe option, conditional on rare event

## prepare data 
rates <- choices %>% 
  filter(!c(n_s == 0 | n_r == 0)) %>% # remove choices where an option was not attended 
  mutate(r_averse = ifelse(choice == "s", 1, 0)) %>% # risk averse choice
  group_by(model, psi, threshold, theta, rare, r_averse) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(r_averse == 0)) %>% 
  mutate(rare = case_when(rare == "attractive" ~ "Rare Desirable Outcome", 
                          rare == "unattractive" ~ "Rare Undesirable Outcome", 
                          rare == "none" ~ "No Rare Outcome"))

## plot data
r_averse_summary <- rates %>%  
  filter(model == "summary" & threshold == "relative") %>% 
  #filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_wrap(~rare, nrow = 3) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% Safe Choices",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

r_averse_roundwise <- rates %>%
  filter(model == "roundwise" & threshold == "relative") %>% 
  #filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
  facet_wrap(~rare, nrow = 3) + 
  scale_color_scico_d(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% Safe Choices",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

r_averse_summary + r_averse_roundwise + plot_annotation(tag_levels = "A") + plot_layout(ncol = 2)
ggsave(file = "supplements/choice ecology/figures/ecology_rates_risk_aversion.png", width = 14, height = 10)


# maximization analysis ---------------------------------------------------

# Frequency of diverging predictions

problems %>% 
  mutate(same_op = case_when( (p_r_high > .5) & (r_ev < safe)  ~ 0 ,
                              (p_r_high < .5) & (r_ev > safe)  ~ 0) ,
         same_op = ifelse(is.na(same_op), 1, same_op)) %>%
  group_by(same_op) %>% 
  summarise(count = n()) %>% 
  mutate(prop = round( count/sum(count), 2))  # n = 4 (p = 0.07)

choices %>% 
  filter(threshold == "relative") %>% 
  mutate(same_op = case_when( (ep_r_high > .5) & (mean_r < safe)  ~ 0 ,
                              (ep_r_high < .5) & (mean_r > safe)  ~ 0) ,
         same_op = ifelse(is.na(same_op), 1, same_op)) %>%
  group_by(same_op) %>% 
  summarise(count = n()) %>% 
  mutate(prop = round( count/sum(count), 2))  # n = 103630 (p = 0.09)

# prepare data 

## expected value

rates_EV <- choices %>%
  filter(!c(n_s == 0 | n_r == 0)) %>% # remove choices where an option was not attended 
  mutate(norm = case_when(r_ev/safe > 1 ~ "r", 
                          r_ev/safe < 1 ~ "s"), 
         norm2 = case_when(p_r_high > .5 ~ "r", 
                           p_r_high < .5 ~ "s")) %>% # determine option with higher sampled mean 
  filter(norm != norm2) %>% # drop options where predictions are the same 
  mutate(max = ifelse(norm == choice, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0))

## sampled average 
rates_EV_exp <- choices %>%
  filter(!c(n_s == 0 | n_r == 0)) %>% # remove choices where an option was not attended 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", 
                          mean_r/safe < 1 ~ "s"), 
         norm2 = case_when(ep_r_high > .5 ~ "r", 
                           ep_r_high < .5 ~ "s")) %>% # determine option with higher sampled mean 
  filter(norm != norm2) %>% # drop options where predictions are the same 
  mutate(max = ifelse(norm == choice, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0))


# Plot data

## expected value 

### Summary
max_EV_summary <- rates_EV %>%
  filter(model == "summary" & threshold == "relative") %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% EV Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

### Roundwise
max_EV_roundwise <- rates_EV %>%
  filter(model == "roundwise" & threshold == "relative") %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
  scale_color_scico_d(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% EV Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

### merge plots
max_EV <- ggarrange(max_EV_summary, max_EV_roundwise, nrow = 1)


## sampled average 

### Summary
max_EV_exp_summary <- rates_EV_exp %>%
  filter(model == "summary" & threshold == "relative") %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% Average Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

### Roundwise
max_EV_exp_roundwise <- rates_EV_exp %>%
  filter(model == "roundwise" & threshold == "relative") %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
  scale_color_scico_d(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% Average Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

### merge plots
max_EV_exp <- ggarrange(max_EV_exp_summary, max_EV_exp_roundwise, nrow = 1)

## merge and save plots
max_EV + max_EV_exp + 
  plot_layout(ncol = 1, guides = "collect") + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 24, face = "plain"))
ggsave(file = "supplements/choice ecology/figures/ecology_maximization.png", width = 14, height = 10)

