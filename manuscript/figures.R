here::i_am("manuscript/figures.R")

# load packages 
pacman::p_load(here,
               tidyverse,
               scico, # for scientific color palettes
               latex2exp, # for LaTeX expressions in plots
               papaja,
               patchwork)


# load data
choices <- read_rds(here("data", "choice_data.rds.bz2")) 
cpt <- read_rds(here("data", "cpt_estimates.rds")) 

# maximization rates
  
## compute maximization rates for EV and sampled mean

### expected value
rates_ev <- choices %>%
  filter(!c(n_s == 0 | n_r == 0)) %>% # drop trials where only one option was attended
  mutate(norm = case_when(ev_ratio > 1 ~ "r", 
                          ev_ratio < 1 ~ "s")) %>% # determine higher-EV prospect (normative choice)
  filter(!is.na(norm)) %>% # drop problems without normative choice
  group_by(model, psi, threshold, theta, rare, norm, choice) %>% 
  summarise(n = n()) %>% # number of correct/false risky and correct/false safe choices
  mutate(rate = round(n/sum(n), 2)) %>% # rates of correct/false risky and correct/false safe choices
  ungroup() %>%
  complete(model, psi, threshold, theta, rare, norm, choice, fill = list(n = 0, rate = 0)) %>% # include choices where n = 0
  filter(!(model == "roundwise" & theta > 5), !(model == "summary" & theta < 15)) %>% # drop rows with invalid parameter combinations
  mutate(type = case_when(norm == "r" & choice == "r" ~ "Risky",
                          norm == "s" & choice == "s" ~ "Safe")) %>%
  filter(!is.na(type)) %>% # drop false choices
  select(-c(norm, choice, n)) %>% 
  mutate(norm = "EV")


###  sampled mean
rates_mean <- choices %>%
  filter(!c(n_s == 0 | n_r == 0)) %>% 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", 
                          mean_r/safe < 1 ~ "s")) %>% # determine prospect with higher sampled mean 
  filter(!is.na(norm)) %>%
  group_by(model, psi, threshold, theta, rare, norm, choice) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  complete(model, psi, threshold, theta, rare, norm, choice, fill = list(n = 0, rate = 0)) %>%
  filter(!(model == "roundwise" & theta > 5), !(model == "summary" & theta < 15)) %>% 
  mutate(type = case_when(norm == "r" & choice == "r" ~ "Risky",
                          norm == "s" & choice == "s" ~ "Safe")) %>%
  filter(!is.na(type)) %>% 
  select(-c(norm, choice, n)) %>% 
  mutate(norm = "Mean")

rates <- bind_rows(rates_ev, rates_mean) 
rates <- rates %>%  mutate(rare = case_when(rare == "none" ~ "No",
                                            rare == "attractive" ~ "High",
                                            rare == "unattractive" ~ "Low"),
                           threshold = case_when(threshold == "absolute" ~ "Absolute",
                                                 threshold == "relative" ~ "Relative"))


## plots

### round-wise

rates_round_r <- rates %>%  filter(model == "roundwise" & threshold == "Relative") %>% 
  filter(psi > .9 | psi == .5 | psi == (1-.9)) # only show rates for psi = c(.1, .5, 1) 

rates %>%
  filter(model == "roundwise" & threshold == "Absolute") %>% 
  filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate)) +
  facet_grid(rare~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  labs(x = expression(paste("Switching Probability ", psi)),
       y = "Maximization Rate",
       linetype = "Option",
       color = "Norm",
       shape = "Threshold") +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, .5)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, .5)) +
  geom_line(aes(linetype = type, color = norm), size = 1, alpha = .7) + 
  geom_point(aes(shape = threshold, color = norm), size = 4, alpha = .7) +
  geom_line(data = rates_round_r, aes(linetype = type, color = norm), size = 1, alpha = .7) + 
  geom_point(data = rates_round_r, aes(shape = threshold, color = norm), size = 4, alpha = .7) +
  scale_color_scico_d(palette = "bam", begin = .2, end = .8) + 
  theme_apa(base_size = 20) + 
  theme(legend.position = "top")
ggsave(file = "manuscript/figures/maximization_roundwise.png", width = 14, height = 12)


### summary

rates_summary_r <- rates %>%  
  filter(model == "summary" & threshold == "Relative") %>% 
  filter(psi > .9 | psi == .5 | psi == (1-.9)) 

rates %>%
  filter(model == "summary" & threshold == "Absolute") %>% 
  filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate)) +
  facet_grid(rare~theta, labeller = labeller(rare = as_labeller(label_rare, default = label_parsed),
                                             theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  labs(x = expression(paste("Switching Probability ", psi)),
       y = "Maximization Rate",
       linetype = "Option", 
       color = "Norm",
       shape = "Threshold") +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, .5)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, .5)) +
  geom_line(aes(linetype = type, color = norm), size = 1, alpha = .7) + 
  geom_point(aes(shape = threshold, color = norm), size = 4, alpha = .7) +
  geom_line(data = rates_summary_r, aes(linetype = type, color = norm), size = 1, alpha = .7) + 
  geom_point(data = rates_summary_r, aes(shape = threshold, color = norm), size = 4, alpha = .7) +
  scale_color_scico_d(palette = "bam", begin = .2, end = .8) + 
  theme_apa(base_size = 20) + 
  theme(legend.position = "top")
ggsave(file = "manuscript/figures/maximization_summary.png", width = 14, height = 12)


# cumulative prospect theory

## round-wise

cpt_roundwise <- cpt %>% filter(model == "roundwise")

### probability weighting 

#### gamma

gamma_relative_roundwise <- cpt_roundwise %>% filter(parameter == "gamma", threshold == "relative")

gamma_roundwise <- cpt_roundwise %>%
  filter(parameter == "gamma", threshold == "absolute") %>%
  ggplot(aes(psi, mean, color = psi)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(0,1.1), breaks = seq(0,1,.5)) + 
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Curvature  ", gamma)),
       color = expression(psi)) +
  geom_errorbar(data = gamma_relative_roundwise, aes(ymin=`2.5%`, ymax=`97.5%`), color = "gray") + 
  geom_point(data = gamma_relative_roundwise, color = "gray") +
  geom_line(data = gamma_relative_roundwise, color = "gray") +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), size = 1) + 
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  theme_apa(base_size = 20)

#### delta

delta_relative_roundwise <- cpt_roundwise %>% filter(parameter == "delta", threshold == "relative")

delta_roundwise <- cpt_roundwise %>%
  filter(parameter == "delta", threshold == "absolute") %>%
  ggplot(aes(psi, mean, color = psi)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(0,1.1), breaks = seq(0,1,.5)) + 
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Elevation  ", delta)),
       color = expression(psi)) +
  geom_errorbar(data = delta_relative_roundwise, aes(ymin=`2.5%`, ymax=`97.5%`), color = "gray") + 
  geom_point(data = delta_relative_roundwise, color = "gray") +
  geom_line(data = delta_relative_roundwise, color = "gray") +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), size = 1) + 
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  theme_apa(base_size = 20)

####  weighting function

weights <- cpt %>%
  select(model, psi, threshold, theta, parameter, mean) %>%
  pivot_wider(names_from = parameter, values_from = mean) %>%
  select(-c(alpha, rho)) %>%
  expand_grid(p = seq(0, 1, .05)) %>% # create vector of sampled relative frequencies
  mutate(w = round(  (delta * p^gamma)/ ((delta * p^gamma)+(1-p)^gamma), 2)) # compute decision weights (see Goldstein & Einhorn, 1987) using the parameter estimates  

weights_roundwise <- weights %>% filter(model == "roundwise")
weights_relative_roundwise <- weights_roundwise %>% filter(threshold == "relative")

wf_roundwise <- weights_roundwise %>% 
  filter(threshold == "absolute") %>% 
  ggplot(aes(p, w, group = psi, color = psi)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") + 
  labs(x = "Sampled Relative Frequency",
       y = expression(paste("Decision Weight  ", pi)),
       color = expression(psi)) +
  scale_x_continuous(breaks = seq(0, 1, .5)) +
  scale_y_continuous(breaks = seq(0, 1, .5)) +
  geom_line(data = weights_relative_roundwise, color = "gray") + 
  geom_line(size = 1, alpha = .7) +
  scale_color_scico(palette = "tokyo", end = .8) +
  theme_apa(base_size = 20)

wf_roundwise + gamma_roundwise + delta_roundwise + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/probability-weighting_roundwise.png", width = 14, height = 12)

### value function and logit choice rule

#### alpha

alpha_relative_roundwise <- cpt_roundwise %>% filter(parameter == "alpha", threshold == "relative")

alpha_roundwise <- cpt_roundwise %>%
  filter(parameter == "alpha", threshold == "absolute") %>% 
  ggplot(aes(psi, mean, color = psi)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(0,1.1), breaks = seq(0,1,.5)) + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.5)) + 
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Concavity  ", alpha)),
       color = expression(psi)) +
  geom_errorbar(data = alpha_relative_roundwise, aes(ymin=`2.5%`, ymax=`97.5%`), color = "gray") + 
  geom_point(data = alpha_relative_roundwise, color = "gray") +
  geom_line(data = alpha_relative_roundwise, color = "gray") +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), size = 1) + 
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  theme_apa(base_size = 20)

#### value function 

values <- cpt %>%
  select(model, psi, threshold, theta, parameter, mean) %>%
  pivot_wider(names_from = parameter, values_from = mean) %>%
  select(-c(gamma, delta, rho)) %>%
  expand_grid(x = seq(0, 20, .5)) %>%  # create vector of possible outcomes
  mutate(v = round(x^alpha, 2)) # compute subjective values on the basis of estimated parameters

values_roundwise <- values %>% filter(model == "roundwise")
values_relative_roundwise <- values_roundwise %>% filter(threshold == "relative")

vf_roundwise <- values_roundwise %>% 
  filter(threshold == "absolute") %>% 
  ggplot(aes(x, v, group = psi, color = psi)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") + 
  labs(x = "Sampled Outcome",
       y = "Subjective Value",
       color = expression(psi)) +
  scale_x_continuous(breaks = seq(0, 20, 10)) +
  scale_y_continuous(breaks = seq(0, 20, 10)) +
  geom_line(data = values_relative_roundwise, color = "gray") + 
  geom_line(size = 1, alpha = .7) +
  scale_color_scico(palette = "tokyo", end = .8) +
  theme_apa(base_size = 20)

#### rho  

rho_relative_roundwise <- cpt_roundwise %>% filter(parameter == "rho", threshold == "relative")

rho_roundwise <- cpt_roundwise %>%
  filter(parameter == "rho", threshold == "absolute") %>% 
  ggplot(aes(psi, mean, color = psi)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(0,1.1), breaks = seq(0,1,.5)) + 
  scale_y_continuous(limits = c(0, 5), breaks = seq(0,5,1)) + 
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Choice Consistency  ", rho)),
       color = expression(psi)) +
  geom_errorbar(data = rho_relative_roundwise, aes(ymin=`2.5%`, ymax=`97.5%`), color = "gray") + 
  geom_point(data = rho_relative_roundwise, color = "gray") +
  geom_line(data = rho_relative_roundwise, color = "gray") +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), size = 1) + 
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  theme_apa(base_size = 20)

vf_roundwise + alpha_roundwise + rho_roundwise + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/outcome-sensitivity_roundwise.png", width = 14, height = 12)


## summary 

cpt_summary <- cpt %>% filter(model == "summary")

### probability weighting 

#### gamma

gamma_relative_summary <- cpt_summary %>% filter(parameter == "gamma", threshold == "relative")

gamma_summary <- cpt_summary %>%
  filter(parameter == "gamma", threshold == "absolute") %>%
  ggplot(aes(psi, mean, color = psi)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(0,1.1), breaks = seq(0,1,.5)) + 
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Curvature  ", gamma)),
       color = expression(psi)) +
  geom_errorbar(data = gamma_relative_summary, aes(ymin=`2.5%`, ymax=`97.5%`), color = "gray") + 
  geom_point(data = gamma_relative_summary, color = "gray") +
  geom_line(data = gamma_relative_summary, color = "gray") +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), size = 1) + 
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  theme_apa(base_size = 20)

#### delta

delta_relative_summary <- cpt_summary %>% filter(parameter == "delta", threshold == "relative")

delta_summary <- cpt_summary %>%
  filter(parameter == "delta", threshold == "absolute") %>%
  ggplot(aes(psi, mean, color = psi)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(0,1.1), breaks = seq(0,1,.5)) + 
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Elevation  ", delta)),
       color = expression(psi)) +
  geom_errorbar(data = delta_relative_summary, aes(ymin=`2.5%`, ymax=`97.5%`), color = "gray") + 
  geom_point(data = delta_relative_summary, color = "gray") +
  geom_line(data = delta_relative_summary, color = "gray") +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), size = 1) + 
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  theme_apa(base_size = 20)

#### weighting function

weights <- cpt %>%
  select(model, psi, threshold, theta, parameter, mean) %>%
  pivot_wider(names_from = parameter, values_from = mean) %>%
  select(-c(alpha, rho)) %>%
  expand_grid(p = seq(0, 1, .05)) %>%
  mutate(w = round(  (delta * p^gamma)/ ((delta * p^gamma)+(1-p)^gamma), 2))

weights_summary <- weights %>% filter(model == "summary")
weights_relative_summary <- weights_summary %>% filter(threshold == "relative")

wf_summary <- weights_summary %>% 
  filter(threshold == "absolute") %>% 
  ggplot(aes(p, w, group = psi, color = psi)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") + 
  labs(x = "Sampled Relative Frequency",
       y = expression(paste("Decision Weight  ", pi)),
       color = expression(psi)) +
  scale_x_continuous(breaks = seq(0, 1, .5)) +
  scale_y_continuous(breaks = seq(0, 1, .5)) +
  geom_line(data = weights_relative_summary, color = "gray") + 
  geom_line(size = 1, alpha = .7) +
  scale_color_scico(palette = "tokyo", end = .8) +
  theme_apa(base_size = 20)

wf_summary + gamma_summary + delta_summary + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/probability-weighting_summary.png", width = 14, height = 12)

### value function

#### alpha 

alpha_relative_summary <- cpt_summary %>% filter(parameter == "alpha", threshold == "relative")

alpha_summary <- cpt_summary %>%
  filter(parameter == "alpha", threshold == "absolute") %>% 
  ggplot(aes(psi, mean, color = psi)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(0,1.1), breaks = seq(0,1,.5)) + 
  scale_y_continuous(limits = c(0,1.1), breaks = seq(0,1,.5)) + 
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Concavity  ", alpha)),
       color = expression(psi)) +
  geom_errorbar(data = alpha_relative_summary, aes(ymin=`2.5%`, ymax=`97.5%`), color = "gray") + 
  geom_point(data = alpha_relative_summary, color = "gray") +
  geom_line(data = alpha_relative_summary, color = "gray") +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), size = 1) + 
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  theme_apa(base_size = 20)

#### value function

values <- cpt %>%
  select(model, psi, threshold, theta, parameter, mean) %>%
  pivot_wider(names_from = parameter, values_from = mean) %>%
  select(-c(gamma, delta, rho)) %>%
  expand_grid(x = seq(0, 20, .5)) %>%  
  mutate(v = round(x^alpha, 2)) 

values_summary <- values %>% filter(model == "summary")
values_relative_summary <- values_summary %>% filter(threshold == "relative")

vf_summary <- values_summary %>% 
  filter(threshold == "absolute") %>% 
  ggplot(aes(x, v, group = psi, color = psi)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") + 
  labs(x = "Sampled Outcome",
       y = "Subjective Value",
       color = expression(psi)) +
  scale_x_continuous(breaks = seq(0, 20, 10)) +
  scale_y_continuous(breaks = seq(0, 20, 10)) +
  geom_line(data = values_relative_summary, color = "gray") + 
  geom_line(size = 1, alpha = .7) +
  scale_color_scico(palette = "tokyo", end = .8) +
  theme_apa(base_size = 20)

#### rho 

rho_relative_summary <- cpt_summary %>% filter(parameter == "rho", threshold == "relative")

rho_summary <- cpt_summary %>%
  filter(parameter == "rho", threshold == "absolute") %>% 
  ggplot(aes(psi, mean, color = psi)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(0,1.1), breaks = seq(0,1,.5)) + 
  scale_y_continuous(limits = c(0, 5), breaks = seq(0,5,1)) + 
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Choice Consistency  ", rho)),
       color = expression(psi)) +
  geom_errorbar(data = rho_relative_summary, aes(ymin=`2.5%`, ymax=`97.5%`), color = "gray") + 
  geom_point(data = rho_relative_summary, color = "gray") +
  geom_line(data = rho_relative_summary, color = "gray") +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), size = 1) + 
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  theme_apa(base_size = 20)

vf_summary + alpha_summary + rho_summary + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/outcome-sensitivity_summary.png", width = 14, height = 12)

