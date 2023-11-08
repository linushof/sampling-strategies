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

# labels

# theta
label_theta <- function(string) {
  TeX(paste("$\\theta=$", string, sep = ""))
}

# psi
label_psi <- function(string) {
  TeX(paste("$\\psi=$", string, sep = ""))
}



# ecological analysis -----------------------------------------------------

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
  mutate(rare = case_when(rare == "attractive" ~ "(0, .2)", 
                          rare == "unattractive" ~ "(.8, 1)", 
                          rare == "none" ~ "\\[.2, .8\\]"))

### compute proportions of choice problems, where safe option is larger than sampled mean of risky option
rates_obj <- choices %>%
  filter(!c(n_s == 0 | n_r == 0)) %>%
  mutate(sampled_ev_ratio = ifelse(safe > mean_r, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, rare, sampled_ev_ratio) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>% 
  filter(!(sampled_ev_ratio == 0)) %>% 
  mutate(rare = case_when(rare == "attractive" ~ "(0, .2)", 
                          rare == "unattractive" ~ "(.8, 1)", 
                          rare == "none" ~ "\\[.2, .8\\]"))

rates_obj_sr <- rates_obj %>% filter(model == "summary", threshold == "relative" ) 
rates_obj_rr <- rates_obj %>% filter(model == "roundwise", threshold == "relative" ) 

## plot data
r_averse_summary <- rates %>%  
  filter(model == "summary" & threshold == "relative") %>% 
  #filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_wrap(~rare, nrow = 1, labeller = labeller(rare = as_labeller(label_rare, default = label_parsed)))+
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary", 
       x = expression(paste("Switching Probability  ", psi)),
       y = "Proportion",
       color = "Threshold") +
  geom_line(data = rates_obj_sr, color = "gray", linewidth = 1, alpha = .3) +
  geom_point(data = rates_obj_sr, color = "gray", size = 3, alpha = .3) +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_apa(base_size = 20)

r_averse_roundwise <- rates %>%
  filter(model == "roundwise" & threshold == "relative") %>% 
  #filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
  facet_wrap(~rare, nrow = 1, labeller = labeller(rare = as_labeller(label_rare, default = label_parsed))) + 
  scale_color_scico_d(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Round-wise", 
       x = expression(paste("Switching Probability  ", psi)),
       y = "Proportion",
       color = "Threshold") +
  geom_line(data = rates_obj_rr, color = "gray", linewidth = 1, alpha = .3) +
  geom_point(data = rates_obj_rr, color = "gray", size = 3, alpha = .3) +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_apa(base_size = 20)

r_averse_summary + r_averse_roundwise + plot_annotation(tag_levels = "A") + plot_layout(ncol = 1)
ggsave(file = "manuscript/figures/rates_risk_aversion_rare.png", width = 14, height = 10)


# Maximization for trials where predictions for summary and round-wise comparison diverge 

# Frequency of diverging predictions
choices %>% 
  filter(threshold == "relative") %>% 
  mutate(same_op = case_when( (ep_r_high > .5) & (mean_r < safe)  ~ 1 ,
                              (ep_r_high < .5) & (mean_r > safe)  ~ 1) ,
         same_op = ifelse(is.na(same_op), 0, same_op)) %>%
  group_by(same_op) %>% 
  summarise(count = n(), 
            prop = round( count/1.2e6, 3)) # n = 103630 (p = 0.09)

# Prepare data: compute maximization rates for trials with diverging predictions
rates <- choices %>%
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

## Summary
max_summary <- rates %>%
  filter(model == "summary" & threshold == "relative") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary", 
       x = expression(paste("Switching Probability  ", psi)),
       y = "Proportion",
       color = "Threshold") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_apa(base_size = 20)

## Round-wise
max_roundwise <- rates %>%
  filter(model == "roundwise" & threshold == "relative") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
  scale_color_scico_d(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Round-wise", 
       x = expression(paste("Switching Probability  ", psi)),
       y = "Proportion",
       color = "Threshold") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_apa(base_size = 20)

## merge plots
max_summary + max_roundwise + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/rates_maximization_average_return_div.png", width = 14, height = 6)