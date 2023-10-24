# preparation -------------------------------------------------------------

# load pkgs 
pacman::p_load(tidyverse,
               scico, # for scientific color palettes
               latex2exp, # for LaTeX expressions in plots
               papaja,
               ggpubr,
               patchwork)


# load data
cpt <- read_rds("supplements/gamma_analysis/cpt_estimates_2.rds") 


# plot labels

label_theta <- function(string) {
  TeX(paste("$\\theta=$", string, sep = ""))
}

label_psi <- function(string) {
  TeX(paste("$\\psi=$", string, sep = ""))
}

# switching probability psi
label_psi <- function(string) {
  TeX(paste("$\\psi=$", string, sep = ""))
}

# propbability of high risky outcome
label_rare <- function(string) {
  TeX(paste("$\\p_{high}\\in$", string, sep = "")) 
}


# weighting function ---------------------------------------------------------


# check MCMC statistics / convergence 

# scale reduction factor 
max(cpt$Rhat) # 1.0014 ----> < 1.00 
min(cpt$n.eff) # 17,000

# prepare data
weights <- cpt %>%
  select(model, psi, threshold, theta, parameter, mean) %>%
  pivot_wider(names_from = parameter, values_from = mean) %>% 
  select(-c(alpha, rho)) %>%
  expand_grid(p = seq(0, 1, .05)) %>% # create vector of sampled relative frequencies
  mutate(w = round(  (delta * p^gamma)/ ((delta * p^gamma)+(1-p)^gamma), 2)) # compute decision weights (see Goldstein & Einhorn, 1987) using the parameter estimates  


cpt_roundwise <- cpt %>% filter(model == "roundwise", !(psi == 1 & theta == 1))
weights_roundwise <- weights %>% filter(model == "roundwise", !(psi == 1 & theta == 1))


#### gamma
gamma_roundwise <- cpt_roundwise %>%
  filter(parameter == "gamma", threshold == "relative") %>%
  ggplot(aes(psi, mean, color = psi)) + 
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  labs(x = "Switching Probability (Search Rule)", 
       y = expression(paste("Curvature  ", gamma)),
       color = "Switching\nProbability") +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

#### delta
delta_roundwise <- cpt_roundwise %>%
  filter(parameter == "delta", threshold == "relative") %>%
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
  filter(threshold == "relative") %>% 
  ggplot(aes(p, w, group = psi, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  scale_x_continuous(breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 3)) +
  geom_line(linewidth = 1)

# merge and save plots

wf_roundwise + gamma_roundwise + delta_roundwise + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/cpt_weighting_roundwise.png", width = 14, height = 10)


# value function ---------------------------------------------------------

# prepare data

values <- cpt %>%
  select(model, psi, threshold, theta, parameter, mean) %>%
  pivot_wider(names_from = parameter, values_from = mean) %>%
  select(-c(gamma, delta, rho)) %>%
  expand_grid(x = seq(0, 20, .5)) %>%  # create vector of possible outcomes
  mutate(v = round(x^alpha, 2)) # compute subjective values on the basis of estimated parameters

# plot data 

################################## choice proportions as a function of outcome magnitude

choices %>% 
  filter(!c(n_s == 0 | n_r == 0)) %>% 
  mutate(exp_ev_diff = mean_r - safe) %>% 
  group_by(model, psi, threshold, theta, exp_ev_diff, choice) %>%  
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n), 2), 
         prop2 = if_else(choice == "s", 1-prop, prop)) %>%
  filter(model == "summary", threshold == "relative", theta >= 15) %>% 
  # filter(ep_r_high != 0, ep_r_high != 1) %>% 
  # mutate(bias = if_else(ep_r_high == 1, "p=0 or p=1", if_else(ep_r_high == 0, "p=0 or p=1", "0 < p < 1"))) %>%
  ggplot(aes(x=exp_ev_diff, y = prop2)) +
  geom_point(size = 3, color = "gray", alpha = .05) +
  geom_vline(xintercept = 0) + 
  #scale_color_manual(values = c("gray", "black")) + 
  #scale_alpha_manual(values = c(.5, 1)) + 
  # geom_smooth(color = "black", se = FALSE) + 
  facet_grid(psi~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed), 
                                            psi = as_labeller(label_psi, default = label_parsed))) + 
  labs(title = "Sampling Strategies" , 
       x = "Experienced EV diff", 
       y = "Proportion of Risky Choices") +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0,1,.5)) + 
  theme_minimal(base_size = 20) + 
  theme(legend.position = "top")
ggsave("housekeeping/notes/risky_choice_proportions_sampling.png", width = 14, height = 14)

postpred %>% 
  mutate(exp_ev_diff = mean_r - safe) %>% 
  group_by(model, psi, threshold, theta, exp_ev_diff, choice_pp) %>%  
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n), 2), 
         prop2 = if_else(choice_pp == 1, 1-prop, prop)) %>%
  filter(model == "summary", threshold == "relative", theta >= 15) %>% 
  # filter(ep_r_high != 0, ep_r_high != 1) %>% 
  # mutate(bias = if_else(ep_r_high == 1, "p=0 or p=1", if_else(ep_r_high == 0, "p=0 or p=1", "0 < p < 1"))) %>%
  ggplot(aes(x=exp_ev_diff, y = prop2)) +
  geom_point(size = 3, color = "gray", alpha = .05) +
  geom_vline(xintercept = 0) + 
  #scale_color_manual(values = c("gray", "black")) + 
  #scale_alpha_manual(values = c(.5, 1)) + 
  # geom_smooth(color = "black", se = FALSE) + 
  facet_grid(psi~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed), 
                                            psi = as_labeller(label_psi, default = label_parsed))) + 
  labs(title = "Posterior Predictive" , 
       x = "Experienced EV diff", 
       y = "Proportion of Risky Choices") +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0,1,.5)) + 
  theme_minimal(base_size = 20) + 
  theme(legend.position = "top")
ggsave("housekeeping/notes/risky_choice_proportions_cpt.png", width = 14, height = 14)

##################################################################

## summary

values_summary <- values %>% filter(model == "summary")

### alpha
alpha_summary <- cpt_summary %>%
  filter(parameter == "alpha", threshold == "relative") %>% 
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
  filter(threshold == "relative") %>% 
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
ggsave(file = "manuscript/figures/cpt_value_summary.png", width = 14, height = 7)

## round-wise

values_roundwise <- values %>% filter(model == "roundwise", !(psi == 1 & theta == 1))

### alpha
alpha_roundwise <- cpt_roundwise %>%
  filter(parameter == "alpha", threshold == "relative") %>% 
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
  filter(threshold == "relative") %>% 
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
ggsave(file = "manuscript/figures/cpt_value_roundwise.png", width = 14, height = 7)


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

# appendix absolute thresholds --------------------------------------------

# appendix choice rule ----------------------------------------------------

cpt_summary <- cpt %>% filter(model == "summary")
rho_summary <- cpt_summary %>%
  filter(parameter == "rho", threshold == "relative") %>% 
  ggplot(aes(psi, mean)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0,1,length.out = 3)) + 
  scale_y_continuous(limits = c(-.5, 5.5), breaks = seq(0,5, length.out = 3)) + 
  labs(title = "Summary", 
       x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Choice Consistency  ", phi))) +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_apa(base_size = 20)

cpt_roundwise <- cpt %>% filter(model == "roundwise")
rho_roundwise <- cpt_roundwise %>%
  filter(parameter == "rho", threshold == "relative") %>% 
  ggplot(aes(psi, mean)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0, 1, length.out = 3)) + 
  scale_y_continuous(limits = c(-.5, 5.5), breaks = seq(0,5, length.out = 3)) + 
  labs(title = "Round-wise", 
       x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Choice Consistency  ", phi))) +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_apa(base_size = 20)

### combine plots
rho_summary + rho_roundwise + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/choice_rule.png", width = 14, height = 7)


# appendix ground truth --------------------------------------------


## maximization rates for expected values

rates_ev <- choices %>%
  filter(!c(n_s == 0 | n_r == 0)) %>% # remove choices where an option was not attended 
  mutate(norm = case_when(ev_ratio > 1 ~ "r", 
                          ev_ratio < 1 ~ "s")) %>% # determine option with higher ev 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0)) %>% 
  mutate(type = "EV")

## prepare data (compute maximization rates)
rates_av <- choices %>%
  filter(!c(n_s == 0 | n_r == 0)) %>% # remove choices where an option was not attended 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", 
                          mean_r/safe < 1 ~ "s")) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0)) %>% 
  mutate(type = "Average")


### summary
rates_av_s <- rates_av %>% filter(model == "summary" & threshold == "relative")
max_summary <- rates_ev %>%
  filter(model == "summary" & threshold == "relative") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, color = type)) +
  facet_wrap(~theta, nrow = 1,  labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_color_scico_d(palette = "berlin") +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary", 
       x = expression(paste("Switching Probability  ", psi)),
       y = "Proportion",
       color = "Maximized\nProperty") +
  geom_line(data = rates_av_s, linewidth = 1) + 
  geom_point(data = rates_av_s, size = 3) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  theme_apa(base_size = 20)

### round-wise
rates_av_r <- rates_av %>% filter(model == "roundwise" & threshold == "relative")
max_roundwise <- rates_ev %>% 
  filter(model == "roundwise" & threshold == "relative") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, color = type)) +
  facet_wrap(~theta, nrow = 1,  labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_color_scico_d(palette = "berlin") + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Round-wise", 
       x = expression(paste("Switching Probability  ", psi)),
       y = "Proportion",
       color = "Maximized\nProperty") +
  geom_line(data = rates_av_r, linewidth = 1) + 
  geom_point(data = rates_av_r, size = 3) +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_apa(base_size = 20)


### merge and save plots
max_summary + max_roundwise + plot_annotation(tag_levels = "A") + plot_layout(ncol = 1, guides = "collect")
ggsave(file = "manuscript/figures/rates_maximization_ev.png", width = 14, height = 10)


