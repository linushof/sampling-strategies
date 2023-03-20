here::i_am("manuscript/figures.R")

# load packages 
pacman::p_load(here,
               tidyverse,
               scico, # for scientific color palettes
               latex2exp, # for LaTeX expressions in plots
               papaja,
               ggpubr,
               patchwork)


# load data
choices <- read_rds(here("data", "choice_data.rds.bz2")) 
cpt <- read_rds(here("data", "cpt_estimates.rds")) 
round <- read_rds("data/simulation_roundwise.rds.bz2") 
summary <- read_rds("data/simulation_summary.rds.bz2")

# plot labels

label_theta <- function(string) {
  TeX(paste("$\\theta=$", string, sep = ""))
}

# switching probability psi
label_psi <- function(string) {
  TeX(paste("$\\psi=$", string, sep = ""))
}

# propbability of high risky outcome
label_rare <- function(string) {
  TeX(paste("$\\p_{High}$", string, sep = "")) 
}

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

# trajectories 

## data wrangling 
round <- round %>% 
  mutate(psi = 1-(psi+.5)) %>% # recode psi
  filter(threshold == "relative") %>% 
  group_by(psi, threshold, theta, problem, agent) %>% # group by trial
  mutate(smp_no = row_number(), # assign sample numbers
         diff = if_else(smp_no == 1, 0, diff)) %>% # fill missing values
  fill(diff)

summary <- summary %>% 
  mutate(psi = 1-(psi+.5)) %>% # recode psi
  filter(threshold == "relative") %>% 
  group_by(psi, threshold, theta, problem, agent) %>% # group by trial
  mutate(smp_no = row_number(), # assign sample numbers
         diff = if_else(smp_no == 1, 0, diff)) %>% # fill missing values
  fill(diff)

## plots

problem_number <- 43
round_boundary <- 3
summary_boundary <- 45

### round-wise model

# compute median for each number of sampled outcomes
round_median <- round %>% 
  group_by(psi, threshold, theta, problem, smp_no) %>% 
  summarise(count = n(), 
            median = median(diff)) %>% 
  slice(seq_len(min(which(median %in% c(-round_boundary, round_boundary)), n()))) %>% 
  filter(psi %in% c((1-.9), .5, 1),
         theta == round_boundary, 
         problem == problem_number) %>% 
  mutate(agent = "Median")

round_trajectories_median <- round %>% 
  filter(psi %in% c((1-.9), .5, 1),
         theta == round_boundary, 
         problem == problem_number) %>% 
  ggplot(aes(x = smp_no, y = diff, group_by = agent, color = as.factor(psi))) + 
  facet_wrap(~psi, nrow = 3, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed)), scales = "free_x") + 
  scale_color_scico_d(palette = "lajolla", begin = .3, end = .7) + 
  # scale_x_continuous(limits = c(0,50), breaks = seq(0, 50, 25)) +
  scale_y_continuous(limits = c(-round_boundary, round_boundary), breaks = seq(-round_boundary,round_boundary, round_boundary)) +
  labs(x = "Number of Sampled Outcomes",
       y = expression(paste(Delta, " Wins")),
       alpha = "Agent\nCount") +
  guides(color = "none") +
  geom_hline(yintercept = c(-round_boundary, 0, round_boundary), linetype = "dashed") +
  geom_line(position = position_dodge(width = .3), size = .1, color = "gray", alpha = .3) + 
  geom_line(data = round_median, aes(y = median, alpha = count), size = 1) + 
  theme_apa() 

### summary model

summary_median <- summary %>% 
  group_by(psi, threshold, theta, problem, smp_no) %>% 
  summarise(count = n(), 
            median = median(diff)) %>% 
  slice(seq_len(min(which(median <= -summary_boundary | median >= summary_boundary), n()))) %>% 
  filter(psi %in% c((1-.9), .5, 1),
         theta == summary_boundary, 
         problem == problem_number) %>%
  mutate(median = case_when(median < -summary_boundary ~ -summary_boundary, 
                            median > summary_boundary ~ summary_boundary, 
                            median >= -summary_boundary & median <= summary_boundary ~ median),
         agent = "Median") 

ann_risky <- data.frame(psi=(1-.9), smp_no = 45, agent = "Label", diff=40, label="Risky Threshold")
ann_safe <- data.frame(psi=(1-.9), smp_no = 45, agent = "Label", diff=-40, label="Safe Threshold")

summary_trajectories_median <- summary %>% 
  filter(psi %in% c((1-.9), .5, 1),
         theta == summary_boundary, 
         problem == problem_number) %>% 
  mutate(diff = case_when(diff < -summary_boundary ~ -summary_boundary, 
                          diff > summary_boundary ~ summary_boundary, 
                          diff >= -summary_boundary & diff <= summary_boundary ~ diff)) %>% 
  ggplot(aes(x = smp_no, y = diff, group_by = agent, color = as.factor(psi))) + 
  facet_wrap(~psi, nrow = 3, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed)), scales = "free_x") + 
  scale_color_scico_d(palette = "lajolla", begin = .3, end = .7) + 
  # scale_x_continuous(limits = c(0,50), breaks = seq(0, 50, 25)) +
  scale_y_continuous(limits = c(-summary_boundary, summary_boundary), breaks = seq(-summary_boundary, summary_boundary, summary_boundary)) +
  labs(x = "Number of Sampled Outcomes",
       y = expression(paste(Delta, " Sum")),
       color = expression(psi),
       alpha = "Agent\nCount") +
  guides(color = "none") +
  geom_hline(yintercept = c(-summary_boundary, 0, summary_boundary), linetype = "dashed") + 
  geom_line(position = position_dodge(width = .3), size = .1, color = "gray", alpha = .3) + 
  geom_line(data = summary_median, aes(y = median, alpha = count), size = 1) + 
  theme_apa() + 
  geom_text(data = ann_risky, label=ann_risky$label, color = "black") + 
  geom_text(data = ann_safe, label=ann_safe$label, color = "black")

### both

ggarrange(round_trajectories_median, summary_trajectories_median, ncol = 2, nrow = 1, common.legend = TRUE, legend = "right", labels = c("Round-wise", "Summary"))
ggsave(file = "manuscript/figures/trajectories_6.png", width = 10, height = 10)
