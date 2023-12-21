#load pkgs 
pacman::p_load(tidyverse,
               scico, # for scientific color palettes
               latex2exp, # for LaTeX expressions in plots
               papaja,
               ggpubr,
               patchwork)


# load data
choices <- read_rds("data/choice_data.rds.bz2") 
cpt <- read_rds("data/cpt_estimates.rds") 

# plot labels

label_theta <- function(string) {
  TeX(paste("$\\theta=$", string, sep = ""))
}

label_psi <- function(string) {
  TeX(paste("$\\psi=$", string, sep = ""))
}

# propbability of high risky outcome
label_rare <- function(string) {
  TeX(paste("$\\p_{high}\\in$", string, sep = "")) 
}


# maximization ------------------------------------------------------------

# compute maximization rates

## sampled average
rates_EV_exp <- choices %>%
  filter(!c(n_s == 0 | n_r == 0)) %>% # remove choices where an option was not attended 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", 
                          mean_r/safe < 1 ~ "s")) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0))

## expected values
rates_EV <- choices %>%
  filter(!c(n_s == 0 | n_r == 0)) %>% # remove choices where an option was not attended 
  mutate(norm = case_when(r_ev/safe > 1 ~ "r", 
                          r_ev/safe < 1 ~ "s")) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0))

# plot data 

## sampled average

### summary
max_EV_exp_summary <- rates_EV_exp %>%
  filter(model == "summary" & threshold == "absolute") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(.4, 1), breaks = seq(.5, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% Average Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

### round-wise
max_EV_exp_roundwise <- rates_EV_exp %>%
  filter(model == "roundwise" & threshold == "absolute") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
  scale_color_scico_d(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(.4, 1), breaks = seq(.5, 1, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% Average Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

### merge plots
max_EV_exp <- ggarrange(max_EV_exp_summary, max_EV_exp_roundwise, nrow = 1)

## expected value

### summary
max_EV_summary <- rates_EV %>%
  filter(model == "summary" & threshold == "absolute") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(.4, 1), breaks = seq(.5, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% EV Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

### round-wise
max_EV_roundwise <- rates_EV %>%
  filter(model == "roundwise" & threshold == "absolute") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
  scale_color_scico_d(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(.4, 1), breaks = seq(.5, 1, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% EV Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)
max_EV <- ggarrange(max_EV_summary, max_EV_roundwise, nrow = 1)

### merge and save plots
max_EV + max_EV_exp + 
  plot_layout(ncol = 1, guides = "collect") + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 24, face = "plain"))
ggsave(file = "supplements/absolute thresholds/figures/absolute_maximization.png", width = 14, height = 10)


# risk aversion -----------------------------------------------------------


# prepare data 
rates <- choices %>% 
  filter(!c(n_s == 0 | n_r == 0)) %>% # remove choices where an option was not attended 
  mutate(r_averse = ifelse(choice == "s", 1, 0)) %>% # risk averse choice
  group_by(model, psi, threshold, theta, r_averse) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(r_averse == 0))

r_averse_summary <- rates %>%  
  filter(model == "summary" & threshold == "absolute") %>% 
  #filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
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
  filter(model == "roundwise" & threshold == "absolute") %>% 
  #filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
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

r_averse_summary + r_averse_roundwise + plot_annotation(tag_levels = "A")
ggsave(file = "supplements/absolute thresholds/figures/absolute_rates_risk_aversion.png", width = 14, height = 6)


# probability weighting function ---------------------------------------------------

# prepare data
weights <- cpt %>%
  select(model, psi, threshold, theta, parameter, mean) %>%
  pivot_wider(names_from = parameter, values_from = mean) %>% 
  select(-c(alpha, rho)) %>%
  expand_grid(p = seq(0, 1, .05)) %>% # create vector of sampled relative frequencies
  mutate(w = round(  (delta * p^gamma)/ ((delta * p^gamma)+(1-p)^gamma), 2)) # compute decision weights (see Goldstein & Einhorn, 1987) using the parameter estimates  

# plot data

## summary 

cpt_summary <- cpt %>% filter(model == "summary")
weights_summary <- weights %>% filter(model == "summary")

#### gamma
gamma_summary <- cpt_summary %>%
  filter(parameter == "gamma", threshold == "absolute") %>%
  ggplot(aes(psi, mean, color = psi)) + 
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) + 
  scale_y_continuous(limits =c(-.1, 2.1), breaks = seq(0,2, length.out = 3)) +
  labs(x = "Switching Probability (Search Rule)", 
       y = expression(paste("Curvature  ", gamma)),
       color = "Switching\nProbability") +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

#### delta
delta_summary <- cpt_summary %>%
  filter(parameter == "delta", threshold == "absolute") %>%
  ggplot(aes(psi, mean, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) +
  scale_y_continuous(limits = c(-0.1, 10.1), breaks = seq(0, 10, length.out = 3)) +
  labs(x = "Switching Probability (Search Rule)", 
       y = expression(paste("Elevation  ", delta)), 
       color = "Switching\nProbability") +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

####  probability weighting
wf_summary <- weights_summary %>% 
  filter(threshold == "absolute") %>% 
  ggplot(aes(p, w, group = psi, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  scale_x_continuous(breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 3)) +
  labs(x = TeX("$\\p_{high}$"),
       y = TeX("$w(\\p_{high})$"),
       color = "Switching\nProbability") +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

# merge and save plots

wf_summary + gamma_summary + delta_summary + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "supplements/absolute thresholds/figures/absolute_cpt_weighting_summary.png", width = 14, height = 10)

## roundwise

cpt_roundwise <- cpt %>% filter(model == "roundwise", !(psi == 1 & theta == 1))
weights_roundwise <- weights %>% filter(model == "roundwise", !(psi == 1 & theta == 1))

#### gamma
gamma_roundwise <- cpt_roundwise %>%
  filter(parameter == "gamma", threshold == "absolute") %>%
  ggplot(aes(psi, mean, color = psi)) + 
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) + 
  scale_y_continuous(limits =c(-.1, 2.1), breaks = seq(0,2, length.out = 3)) +
  labs(x = "Switching Probability (Search Rule)", 
       y = expression(paste("Curvature  ", gamma)),
       color = "Switching\nProbability") +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

#### delta
delta_roundwise <- cpt_roundwise %>%
  filter(parameter == "delta", threshold == "absolute") %>%
  ggplot(aes(psi, mean, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) +
  scale_y_continuous(limits = c(-0.1, 2.1), breaks = seq(0,2, length.out = 3)) +
  labs(x = "Switching Probability (Search Rule)", 
       y = expression(paste("Elevation  ", delta)), 
       color = "Switching\nProbability") +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

####  probability weighting
wf_roundwise <- weights_roundwise %>% 
  filter(threshold == "absolute") %>% 
  ggplot(aes(p, w, group = psi, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  scale_x_continuous(breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 3)) +
  labs(x = TeX("$\\p_{high}$"),
       y = TeX("$w(\\p_{high})$"), 
       color = "Switching\nProbability") +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

# merge and save plots

wf_roundwise + gamma_roundwise + delta_roundwise + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "supplements/absolute thresholds/figures/absolute_cpt_weighting_roundwise.png", width = 14, height = 10)


# value function ----------------------------------------------------------


# prepare data

values <- cpt %>%
  select(model, psi, threshold, theta, parameter, mean) %>%
  pivot_wider(names_from = parameter, values_from = mean) %>%
  select(-c(gamma, delta, rho)) %>%
  expand_grid(x = seq(0, 20, .5)) %>%  # create vector of possible outcomes
  mutate(v = round(x^alpha, 2)) # compute subjective values on the basis of estimated parameters

# plot data 

## summary

values_summary <- values %>% filter(model == "summary")

### alpha
alpha_summary <- cpt_summary %>%
  filter(parameter == "alpha", threshold == "absolute") %>% 
  ggplot(aes(psi, mean, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0,1, length.out = 3)) + 
  scale_y_continuous(limits = c(-0.1, 2.1), breaks = seq(0,2, length.out = 3)) + 
  labs(x = "Switching Probability (Search Rule)", 
       y = expression(paste("Outcome Sensitivity  ", alpha)),
       color = "Switching\nProbability") +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

### value function 
vf_summary <- values_summary %>% 
  filter(threshold == "absolute") %>% 
  ggplot(aes(x, v, group = psi, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  scale_x_continuous(limits = c(-1, 21), breaks = seq(0, 20, length.out = 3)) +
  scale_y_continuous(limits = c(-1, 21), breaks = seq(0, 20, length.out = 3)) +
  labs(x = "Sampled Outcome",
       y = "Subjective Value",
       color = "Switching\nProbability") +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

# merge and save plots
vf_summary + alpha_summary + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "supplements/absolute_thresholds/figures/absolute_cpt_value_summary.png", width = 14, height = 7)

## round-wise

values_roundwise <- values %>% filter(model == "roundwise", !(psi == 1 & theta == 1))

### alpha
alpha_roundwise <- cpt_roundwise %>%
  filter(parameter == "alpha", threshold == "absolute") %>% 
  ggplot(aes(psi, mean, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0,1, length.out = 3)) + 
  scale_y_continuous(limits = c(-0.1, 2.1), breaks = seq(0,2, length.out = 3)) + 
  labs(x = "Switching Probability (Search Rule)", 
       y = expression(paste("Outcome Sensitivity  ", alpha)),
       color = "Switching\nProbability") +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

### value function 
vf_roundwise <- values_roundwise %>% 
  filter(threshold == "absolute") %>% 
  ggplot(aes(x, v, group = psi, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  scale_x_continuous(limits = c(-1, 21), breaks = seq(0, 20, length.out = 3)) +
  scale_y_continuous(limits = c(-1, 21), breaks = seq(0, 20, length.out = 3)) +
  labs(x = "Sampled Outcome",
       y = "Subjective Value",
       color = "Switching\nProbability") +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

# merge and save plots
vf_roundwise + alpha_roundwise + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "supplements/absolute thresholds/figures/absolute_cpt_value_roundwise.png", width = 14, height = 7)