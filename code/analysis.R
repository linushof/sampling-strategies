# preparation -------------------------------------------------------------

# load packages 
pacman::p_load(tidyverse,
               scico, # for scientific color palettes
               latex2exp, # for LaTeX expressions in plots
               papaja,
               ggpubr,
               patchwork, 
               readxl,
               viridis,
               viridisLite)


# load data
problems <- read_xlsx("data/choice_problems.xlsx") 
choices <- read_rds("data/choice_data.rds.bz2")
choices <- left_join(choices, problems, by=join_by(id))
cpt <- read_rds("data/cpt_estimates.rds") 
round <- read_rds("data/simulation_roundwise.rds.bz2") 
summary <- read_rds("data/simulation_summary.rds.bz2")

# plot labels

label_theta <- function(string) {
  TeX(paste("$\\theta=$", string, sep = ""))
}

label_psi <- function(string) {
  TeX(paste("$\\psi=$", string, sep = ""))
}

# probability of high risky outcome
label_rare <- function(string) {
  TeX(paste("$\\p_{high}\\in$", string, sep = "")) 
}

# Behavioral --------------------------------------------------------------

choices <- left_join(choices, problems, by=join_by(id))

## Trajectories -----------------------------------------------

# select and prepare data

## problems 5 and 19 for illustration
round_sub <- round %>% filter(id %in% c(5, 19))
summary_sub <- summary %>% filter(id %in% c(5, 19))

##  medium thresholds for illustration
summary_boundary <- 150
round_boundary <- 3

## compute median trajectory for each sampling strategy
summary_median <- summary_sub %>% 
  group_by(psi, theta, id, smp) %>% 
  summarise(count = n(), # number of sampling agents
            median = median(D)) %>% # median evidence across sampling agents
  slice(seq_len(min(which(median <= -theta | median >= theta), n()))) # remove samples after median hit the threshold

round_median <- round_sub %>% 
  group_by(psi, theta, id, smp) %>% 
  summarise(count = n(), # number of sampling agents
            median = median(D)) %>% # median evidence across sampling agents
  slice(seq_len(min(which(median %in% c(-theta, theta)), n()))) # remove samples after median hit the threshold


## data for problem 5
problem_number <- 5

summary_sub_5 <- summary_sub %>% 
  filter(psi %in% c(.1, .5, 1), 
         theta == summary_boundary, 
         id == problem_number) %>% 
  mutate(D = case_when(D < -summary_boundary ~ -summary_boundary , 
                       D > summary_boundary ~ summary_boundary , 
                       D >= -summary_boundary & D <= summary_boundary ~ D))

round_sub_5 <- round_sub %>%
  filter(psi %in% c(.1, .5, 1), 
         theta == round_boundary, 
         id == problem_number)

summary_median_sub_5 <- summary_median %>%  
  filter(psi %in% c(.1, .5, 1), 
         theta == summary_boundary, 
         id == problem_number) %>%
  mutate(median = case_when(median < -summary_boundary ~ -summary_boundary, 
                            median > summary_boundary ~ summary_boundary, 
                            median >= -summary_boundary & median <= summary_boundary ~ median))

round_median_sub_5 <- round_median %>% 
  filter(psi %in% c(.1, .5, 1),
         theta == round_boundary, 
         id == problem_number)

## data for problem 19
problem_number <- 19

summary_sub_19 <- summary_sub %>% 
  filter(psi %in% c(.1, .5, 1), 
         theta == summary_boundary, 
         id == problem_number) %>% 
  mutate(D = case_when(D < -summary_boundary ~ -summary_boundary , 
                          D > summary_boundary ~ summary_boundary , 
                          D >= -summary_boundary & D <= summary_boundary ~ D))

round_sub_19 <- round_sub %>%
  filter(psi %in% c(.1, .5, 1), 
         theta == round_boundary, 
         id == problem_number)


summary_median_sub_19 <- summary_median %>%  
  filter(psi %in% c(.1, .5, 1), 
         theta == summary_boundary, 
         id == problem_number) %>%
  mutate(median = case_when(median < -summary_boundary ~ -summary_boundary, 
                            median > summary_boundary ~ summary_boundary, 
                            median >= -summary_boundary & median <= summary_boundary ~ median))


round_median_sub_19 <- round_median %>% 
  filter(psi %in% c(.1, .5, 1),
         theta == round_boundary, 
         id == problem_number)


# plot data 

## problem 5

# create EV labels
ann_risky_5 <- data.frame(psi=.1, smp = 50, D=115, label="Risky Threshold \n EV = 40")
ann_safe_5 <- data.frame(psi=.1, smp = 50, D=-115, label="Safe Threshold \n EV = 43")

# summary comparison
summary_trajectories_5 <- summary_sub_5 %>% 
  ggplot(aes(x = smp, y = D)) + 
  facet_wrap(~psi, nrow = 3, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed)), scales = "free_x") + 
  scale_y_continuous(limits = c(-summary_boundary, summary_boundary), 
                     breaks = seq(-summary_boundary, summary_boundary, summary_boundary)) +
  labs(title = "Summary Comparison", 
       x = "Number of Sampled Outcomes",
       y = "Decision Variable",
       color = expression(psi),
       alpha = "Agent\nCount") +
  geom_hline(yintercept = c(-summary_boundary, 0, summary_boundary), linetype = "dashed") + 
  geom_text(data = ann_risky_5, label=ann_risky_5$label, size = 5) + 
  geom_text(data = ann_safe_5, label=ann_safe_5$label, size = 5) +
  geom_line(aes(group = agent), position = position_dodge(width = .3), linewidth = .3, alpha = .1, color = "gray") + 
  geom_line(data = summary_median_sub_5, aes(y = median, alpha = count), linewidth = 1, color = "#9c179e") +
  theme_minimal(base_size = 20) + 
  theme(panel.grid = element_blank())

# roundwise comparison
round_trajectories_5 <- round_sub_5 %>%
  ggplot(aes(x = smp, y = D)) + 
  facet_wrap(~psi, nrow = 3, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed)), scales = "free_x") + 
  scale_y_continuous(limits = c(-round_boundary, round_boundary), 
                     breaks = seq(-round_boundary, round_boundary, round_boundary)) +
  labs(title = "Roundwise Comparison", 
       x = "Number of Sampled Outcomes",
       y = "Decision Variable",
       color = expression(psi),
       alpha = "Agent\nCount") +
  geom_hline(yintercept = c(-round_boundary, 0, round_boundary), linetype = "dashed") + 
  geom_line(aes(group = agent), position = position_dodge(width = .3), linewidth = .3, alpha = .1, color = "gray") + 
  geom_line(data = round_median_sub_5, aes(y = median, alpha = count), linewidth = 1, color = "#9c179e") +
  theme_minimal(base_size = 20) + 
  theme(panel.grid = element_blank())

# merge and save plot
summary_trajectories_5 + round_trajectories_5 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
ggsave(file = "manuscript/figures/accumulation_problem_5.png", width = 14, height = 14)


##  problem 19

# create EV labels
ann_risky_19 <- data.frame(psi=.1, smp = 50, D=115, label="Risky Threshold \n EV = 39")
ann_safe_19 <- data.frame(psi=.1, smp = 50, D=-115, label="Safe Threshold \n EV = 36")

# summary comparison
summary_trajectories_19 <- summary_sub_19 %>% 
  ggplot(aes(x = smp, y = D)) + 
  facet_wrap(~psi, nrow = 3, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed)), scales = "free_x") + 
  scale_y_continuous(limits = c(-summary_boundary, summary_boundary), 
                     breaks = seq(-summary_boundary, summary_boundary, summary_boundary)) +
  labs(title = "Summary Comparison", 
       x = "Number of Sampled Outcomes",
       y = "Decision Variable",
       color = expression(psi),
       alpha = "Agent\nCount") +
  geom_hline(yintercept = c(-summary_boundary, 0, summary_boundary), linetype = "dashed") + 
  geom_text(data = ann_risky_19, label=ann_risky_19$label, size = 5) + 
  geom_text(data = ann_safe_19, label=ann_safe_19$label, size = 5) +
  geom_line(aes(group = agent), position = position_dodge(width = .3), linewidth = .3, alpha = .1, color = "gray") + 
  geom_line(data = summary_median_sub_19, aes(y = median, alpha = count), linewidth = 1, color = "#9c179e") +
  theme_minimal(base_size = 20) + 
  theme(panel.grid = element_blank())

# roundwise comparison
round_trajectories_19 <- round_sub_19 %>%
  ggplot(aes(x = smp, y = D)) + 
  facet_wrap(~psi, nrow = 3, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed)), scales = "free_x") + 
  scale_y_continuous(limits = c(-round_boundary, round_boundary), 
                     breaks = seq(-round_boundary, round_boundary, round_boundary)) +
  labs(title = "Roundwise Comparison", 
       x = "Number of Sampled Outcomes",
       y = "Decision Variable",
       color = expression(psi),
       alpha = "Agent\nCount") +
  geom_hline(yintercept = c(-round_boundary, 0, round_boundary), linetype = "dashed") + 
  geom_line(aes(group = agent), position = position_dodge(width = .3), linewidth = .3, alpha = .1, color = "gray") + 
  geom_line(data = round_median_sub_19, aes(y = median, alpha = count), linewidth = 1, color = "#9c179e") +
  theme_minimal(base_size = 20) + 
  theme(panel.grid = element_blank())

# merge and save
summary_trajectories_19 + round_trajectories_19 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
ggsave(file = "manuscript/figures/accumulation_problem_19.png", width = 14, height = 14)

## Maximization  ------------------------------------------------------

# compute maximization rates

## sampled average
rates_EV_exp <- choices %>%
  mutate(norm = case_when(avg_r/safe > 1 ~ "r", 
                          avg_r/safe < 1 ~ "s")) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice, 1, 0)) %>% 
  group_by(model, psi, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0))

## expected values
rates_EV <- choices %>%
  mutate(norm = case_when(ev_risky/safe > 1 ~ "r", 
                          ev_risky/safe < 1 ~ "s")) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice, 1, 0)) %>% 
  group_by(model, psi, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0))

# plot data 

## sampled average

### summary
max_EV_exp_summary <- rates_EV_exp %>%
  filter(model == "summary") %>% 
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
  filter(model == "roundwise") %>% 
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
  filter(model == "summary") %>% 
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
  filter(model == "roundwise") %>% 
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
ggsave(file = "manuscript/figures/maximization.png", width = 14, height = 10)


## Undersampling -----------------------------------------------------

# prepare data 
round <- left_join(round, problems, by=join_by(id))

## compute trial-level and round-level frequencies

### higher risky outcome
freq  <- round %>% 
  filter(psi %in% c(.1, .5, 1)) %>% 
  group_by(psi, theta, id, agent) %>%
  mutate(n_smp = n() , # number of samples
         smp_s = sum(is.na(out_r)) , # number of samples safe option
         smp_r = n_smp - smp_s , # number of samples risky option
         sp_r_1 = round(sum(if_else(out_r == r_1, 1, 0), na.rm = TRUE)/smp_r, 2) , # sampled probability risky outcome 1
         sp_r_2 = round(1 - sp_r_1, 2)) %>% 
  ungroup() %>%
  group_by(psi, theta, id, agent, round) %>% 
  mutate(n_smp_round = n() , 
         smp_round_s = sum(is.na(out_r)) ,
         smp_round_r = n_smp_round - smp_round_s ,
         round_sp_r_1 = round(sum(if_else(out_r == r_1, 1, 0), na.rm = TRUE)/smp_round_r, 2),
         round_sp_r_2 = round(1 - round_sp_r_1, 2)
  ) 


## compute median round-level frequencies ...

### ... for each sampled frequency on the trial level

freq_trial_1 <- freq %>% 
  distinct(psi, theta, id, agent, round, sp_r_1, round_sp_r_1) %>% # drop redundant row
  select(psi, theta, id, agent, round, sp_r_1, round_sp_r_1) %>% 
  rename(sp = "sp_r_1", round_sp = "round_sp_r_1")

freq_trial_2 <- freq %>% 
  distinct(psi, theta, id, agent, round, sp_r_2, round_sp_r_2) %>% # drop redundant row
  select(psi, theta, id, agent, round, sp_r_2, round_sp_r_2) %>% 
  rename(sp = "sp_r_2", round_sp = "round_sp_r_2")

freq_trial_median <- bind_rows(freq_trial_1, freq_trial_2) %>% 
  group_by(psi, theta, sp) %>%
  summarise(median_round_sp = median(round_sp, na.rm = TRUE)) # compute median round-level frequencies for each parameter combination and trial-level frequency

### ... for each latent probability

freq_latent_1 <- freq %>% distinct(psi, theta, id, agent, round, p_r_1, round_sp_r_1) %>% 
  select(psi, theta, id, agent, round, p_r_1, round_sp_r_1) %>% 
  rename(p = "p_r_1", round_sp = "round_sp_r_1")

freq_latent_2 <- freq %>% distinct(psi, theta, id, agent, round, p_r_2, round_sp_r_2) %>% 
  select(psi, theta, id, agent, round, p_r_2, round_sp_r_2) %>% 
  rename(p = "p_r_2", round_sp = "round_sp_r_2")

freq_latent_median <- bind_rows(freq_latent_1, freq_latent_2) %>% 
  group_by(psi, theta, p) %>%
  summarise(median_round_sp = median(round_sp, na.rm = TRUE)) 


# plot data

## median for trial level frequencies
undersampling_trial <- freq_trial_median %>% 
  ggplot(aes(x = sp, y = median_round_sp, color = as.factor(theta))) +
  geom_jitter(size = 2) + 
  facet_wrap(~psi, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed))) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, .5)) + 
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, .5)) + 
  labs(x = "Sampled Probability Across Choice Trial",
       y = "Sampled Probability\nWithin Comparison Round", 
       color = "Threshold\n(Stopping Rule)") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  scale_color_scico_d(palette = "imola", alpha = .7) + 
  theme_minimal(base_size = 20)

## median for latent probabilities
undersampling_latent <- freq_latent_median %>% 
  ggplot(aes(x = p, y = median_round_sp, color = as.factor(theta))) +
  geom_jitter(size = 2) + 
  facet_wrap(~psi, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed))) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, .5)) + 
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, .5)) + 
  labs(x = "Ground-Truth Probability",
       y = "Sampled Probability\nWithin Comparison Round", 
       color = "Threshold\n(Stopping Rule)") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  scale_color_scico_d(palette = "imola", alpha = .7) + 
  theme_minimal(base_size = 20)

# merge and save plot
undersampling_latent + undersampling_trial + 
  plot_layout(ncol = 1, guides = "collect") + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 24, face = "plain"))
ggsave(file = "manuscript/figures/undersampling.png", width = 14, height = 10)


## Risk  -----------------------------------------------------------

# prepare data 
rates <- choices %>% 
  mutate(safe_choice = ifelse(choice == "s", 1, 0)) %>% # risk averse choice
  group_by(model, psi, theta, safe_choice) %>% 
  summarise(n = n()) %>% 
  mutate(rate = n/sum(n)) %>% 
  ungroup() %>%
  filter(!(safe_choice == 0))

r_averse_summary <- rates %>%  
  filter(model == "summary") %>% 
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
  filter(model == "roundwise") %>% 
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
ggsave(file = "manuscript/figures/rates_risk_aversion.png", width = 14, height = 6)



# Computational ---------------------------------------------------------------------

# check convergence 

max(cpt$Rhat) # 1.04
min(cpt$n.eff) # 1

# only consider strategies/models that converged (Rhat <= 1.01)

converged <- cpt %>% 
  filter(parameter!="deviance", !(Rhat > 1.01)) %>% 
  group_by(model, psi, theta) %>% 
  summarise(count = n())

cpt <- left_join(cpt, converged, by=join_by(model, psi, theta)) %>% 
  filter(count == 4)


## Probability Weighting ---------------------------------------------------------

# prepare data
weights <- cpt %>%
  select(model, psi, theta, parameter, mean) %>%
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
  filter(parameter == "gamma") %>%
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
  filter(parameter == "delta") %>%
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
ggsave(file = "manuscript/figures/cpt_weighting_summary.png", width = 14, height = 10)

## roundwise

cpt_roundwise <- cpt %>% filter(model == "roundwise", !(psi == 1 & theta == 1))
weights_roundwise <- weights %>% filter(model == "roundwise", !(psi == 1 & theta == 1))

#### gamma
gamma_roundwise <- cpt_roundwise %>%
  filter(parameter == "gamma") %>%
  ggplot(aes(psi, mean, color = psi)) + 
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) + 
  scale_y_continuous(limits =c(-.1,2.1), breaks = seq(0,2, length.out = 3)) +
  labs(x = "Switching Probability (Search Rule)", 
       y = expression(paste("Curvature  ", gamma)),
       color = "Switching\nProbability") +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

#### delta
delta_roundwise <- cpt_roundwise %>%
  filter(parameter == "delta") %>%
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
ggsave(file = "manuscript/figures/cpt_weighting_roundwise.png", width = 14, height = 10)


## Outcome Sensitivity ---------------------------------------------------------

# prepare data

values <- cpt %>%
  select(model, psi, theta, parameter, mean) %>%
  pivot_wider(names_from = parameter, values_from = mean) %>%
  select(-c(gamma, delta, rho)) %>%
  expand_grid(x = seq(0, 20, 1)) %>%  # create vector of possible outcomes
  mutate(v = round(x^alpha, 2)) # compute subjective values on the basis of estimated parameters

# plot data 

## summary

values_summary <- values %>% filter(model == "summary")

### alpha
alpha_summary <- cpt_summary %>%
  filter(parameter == "alpha") %>% 
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
  ggplot(aes(x, v, group = psi, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  scale_x_continuous(limits = c(-1, 21), breaks = seq(0, 20, length.out = 3)) +
  scale_y_continuous(limits = c(-1, 100), breaks = seq(0, 100, length.out = 3)) +
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
  filter(parameter == "alpha") %>% 
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
  ggplot(aes(x, v, group = psi, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  scale_x_continuous(limits = c(-1, 21), breaks = seq(0, 20, length.out = 3)) +
  scale_y_continuous(limits = c(-1, 100), breaks = seq(0, 100, length.out = 3)) +
  labs(x = "Sampled Outcome",
       y = "Subjective Value",
       color = "Switching\nProbability") +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

# merge and save plots
vf_roundwise + alpha_roundwise + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/cpt_value_roundwise.png", width = 14, height = 7)


# Ecological  ----------------------------------------------------------

## Maximization  ---------------------------------------------------

## expected values
rates_EV_rare <- choices %>%
  mutate(norm = case_when(ev_risky/safe > 1 ~ "r", 
                          ev_risky/safe < 1 ~ "s")) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice, 1, 0)) %>% 
  group_by(model, psi, theta, rare, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0))

# plot data 

## summary
max_EV_rare_summary <- rates_EV_rare %>%
  filter(model == "summary") %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_wrap(~factor(rare, levels = c("none", "attractive", "unattractive")), nrow = 3) +
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

## roundwise
max_EV_rare_roundwise <- rates_EV_rare %>%
  filter(model == "roundwise") %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
  facet_wrap(~factor(rare, levels = c("none", "attractive", "unattractive")), nrow = 3) +
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

ggarrange(max_EV_rare_summary, max_EV_rare_roundwise, nrow = 1)
ggsave(file = "manuscript/figures/maximization_rare.png", width = 14, height = 10)


## Risk -----------------------------------------------------

# prepare data 
rates_rare <- choices %>% 
  mutate(safe_choice = ifelse(choice == "s", 1, 0)) %>% # risk averse choice
  group_by(model, psi, theta, rare, safe_choice) %>% 
  summarise(n = n()) %>% 
  mutate(rate = n/sum(n)) %>% 
  ungroup() %>%
  filter(!(safe_choice == 0))

r_averse_rare_summary <- rates_rare %>%  
  filter(model == "summary") %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  facet_wrap(~factor(rare, levels = c("none", "attractive", "unattractive")), nrow = 3) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% Safe Choices",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

r_averse_rare_roundwise <- rates_rare %>%
  filter(model == "roundwise") %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
  scale_color_scico_d(palette = "imola", alpha = .7) + 
  facet_wrap(~factor(rare, levels = c("none", "attractive", "unattractive")), nrow = 3) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% Safe Choices",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

r_averse_rare_summary + r_averse_rare_roundwise + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/rates_risk_aversion_rare.png", width = 14, height = 10)



# Efficiency --------------------------------------------------------------

## Sample Size -------------------------------------------------------------

effort_summary <- choices %>% 
  filter(model == "summary") %>% 
  group_by(psi, theta) %>% 
  summarise(mean_smp = mean(smp), 
            median_smp = median(smp)) %>% 
  ggplot(aes(x=psi, y=median_smp, group=theta, color = theta)) + 
  scale_y_continuous(limits = c(0,200), breaks = seq(0,200, length.out = 5)) +
  scale_color_scico(palette = "imola") + 
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "Median Sample Size",
       color = "Threshold\n(Stopping Rule)") +
  geom_point(size = 3) + 
  geom_line(linewidth = 1) + 
  theme_minimal(base_size = 20)

effort_roundwise <- choices %>% 
  filter(model == "roundwise") %>% 
  group_by(psi, theta) %>% 
  summarise(mean_smp= mean(smp), 
            median_smp = median(smp)) %>% 
  ggplot(aes(x=psi, y=median_smp, group=theta, color = as.factor(theta))) + 
  scale_y_continuous(limits = c(0,200), breaks = seq(0,200, length.out = 5)) +
  scale_color_scico_d(palette = "imola") + 
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "Median Sample Size",
       color = "Threshold\n(Stopping Rule)") +
  geom_point(size = 3) + 
  geom_line(linewidth = 1) + 
  theme_minimal(base_size = 20)

effort_summary + effort_roundwise + plot_layout(ncol = 2) + plot_annotation(tag_levels = "A")
ggsave("manuscript/figures/sample_size.png", width = 14, height = 6)

## Reward Rate --------------------------------------------

# prepare data

## Maximization as a function of comparison strategies and sample size
max_n <- choices %>% 
  mutate(norm = case_when(ev_risky/safe > 1 ~ "r", 
                          ev_risky/safe < 1 ~ "s")) %>%
  mutate(max = ifelse(norm == choice, 1, 0)) %>% 
  group_by(model, psi, theta) %>% 
  summarise(n = n(), 
            median_n = median(smp) , 
            max_prop = sum(max)/n)

## reward rate
reward_rates <- choices %>%  
  mutate(chosen_option_EV = ifelse(choice == "s", safe, ev_risky) ,
         rr_EV = chosen_option_EV/smp) %>% 
  select(model, psi, theta, rr_EV)


# plot data

## Maximization as a function of comparison strategies and sample size

### summary
max_n_summary <- max_n %>% 
  filter(model == "summary") %>% 
  ggplot(aes(x=median_n, y=max_prop, group = psi, color = psi)) +
  scale_color_scico(palette = "tokyo", alpha = .7, end = .8) +
  labs(title = "Summary Comparison", 
       x = "Median Sample Size",
       y = "% EV Maximization",
       color = "Switching\nProbability\n(Search Rule)") +
  geom_point(size = 2) + 
  geom_line(linewidth = 1) + 
  scale_y_continuous(limits = c(.4,1), breaks = seq(.4,1, length.out = 6)) +
  theme_minimal(base_size = 20)

### roundwise
max_n_roundwise <- max_n %>% 
  filter(model == "roundwise") %>% 
  ggplot(aes(x=median_n, y=max_prop, group = psi,  color = psi)) +
  scale_color_scico(palette = "tokyo", alpha = .7, end = .8) +
  labs(title = "Roundwise Comparison", 
       x = "Median Sample Size",
       y = "% EV Maximization", 
       color = "Switching\nProbability\n(Search Rule)") +
  geom_point(size = 2) + 
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(.4,1), breaks = seq(.4,1, length.out = 6)) +
  theme_minimal(base_size = 20)

### merge
max_n_EV <- ggarrange(max_n_summary, max_n_roundwise, nrow = 1, common.legend = TRUE, legend = "right")

## reward rate

### summary
rr_summary <- reward_rates %>% 
  group_by(model, psi, theta) %>% 
  summarise(mean_rr_EV = mean(rr_EV, na.rm = TRUE)) %>% 
  filter(model == "summary") %>%  
  ggplot(aes(x = psi, y = mean_rr_EV, group = theta, color = theta)) + 
  scale_color_scico(palette = "imola") + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  labs(x = "Switching Probability\n(Search Rule)", 
       y = "Mean Reward Rate",
       color = "Threshold\n(Stopping Rule)") + 
  scale_y_continuous(limits = c(0, 23), breaks = seq(0,23,length.out = 6)) + 
  theme_minimal(base_size = 20)

#### roundwise
rr_roundwise <- reward_rates %>% 
  group_by(model, psi, theta) %>% 
  summarise(mean_rr_EV = mean(rr_EV, na.rm = TRUE)) %>% 
  filter(model == "roundwise") %>%  
  ggplot(aes(x = psi, y = mean_rr_EV, group = theta, color = as.factor(theta))) + 
  scale_color_scico_d(palette = "imola") + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  labs(x = "Switching Probability\n(Search Rule)", 
       y = "Mean Reward Rate",
       color = "Threshold\n(Stopping Rule)") + 
  scale_y_continuous(limits = c(0, 23), breaks = seq(0,23,length.out = 6)) + 
  theme_minimal(base_size = 20)

#### merge and save
rrates <- ggarrange(rr_summary, rr_roundwise, nrow = 1)
max_n_EV + rrates + 
  plot_layout(ncol = 1, guides = "auto", ) + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 24, face = "plain"))
ggsave(file = "manuscript/figures/efficiency.png", width = 16, height = 10)



# Appendix ----------------------------------------------------------------

## Initial Bias ------------------------------------------------------------

summary <- left_join(summary, problems, by=join_by(id)) # add problem features

choices_summary <- summary %>%
  group_by(psi, theta, id, agent) %>%
  mutate(start = ifelse(smp == 1 & at == "r", "r", ifelse(smp == 1 & at == "s", "s", NA)), 
         start_o = ifelse(is.na(start), first(start), start),
         n_smp = n(), # number of samples
         smp_s = sum(is.na(out_r)), # number of samples safe option
         smp_r = n_smp - smp_s, # number of samples risky option
         sp_r_1 = round(sum(if_else(out_r == r_1, 1, 0), na.rm = TRUE)/smp_r, 2), # sampled probability risky outcome 1
         sp_r_2 = round(1 - sp_r_1, 2), # sampled probability risky outcome 2
         avg_r = round(mean(out_r, na.rm = TRUE), 2)) %>% # sampled average risky option
  ungroup() %>%
  filter(!is.na(choice)) %>% 
  select(psi, theta, id, agent, start_o, n_smp, smp_s, smp_r, sp_r_1, sp_r_2, avg_r, choice) 

# starting bias sample size 

choices_summary <- choices_summary %>% 
  mutate(n_starting = ifelse(start_o == "r", smp_r, smp_s), 
         n_second = ifelse(start_o == "r", smp_s, smp_r ), 
         n_start_sec_diff = n_starting - n_second, 
         start_advantage = if_else(n_start_sec_diff > 0, "Bias", "No Bias"))

choices_summary %>%
  ggplot(aes(x=n_start_sec_diff, fill = start_advantage)) + 
  geom_histogram(center = 0, binwidth = 1) + 
  scale_fill_viridis_d() + 
  facet_grid(psi~theta, scales = "free", labeller = labeller(theta = as_labeller(label_theta, default = label_parsed), 
                                                             psi = as_labeller(label_psi, default = label_parsed))) +
  labs(x = TeX("Difference in Sample Size ($\\N_{Initial \\, Option} - \\N_{Second \\,  Option} $)"), 
       y = "Frequency") +
  theme_minimal(base_size = 20) +
  theme(legend.position =  "none") 

ggsave("manuscript/figures/appendix/initial_bias_sample_sizes.png", width = 14, height = 14)


# starting bias choice

choices_summary %>% 
  mutate(bias_choice = if_else(start_o == choice, TRUE, FALSE)) %>% 
  group_by(psi, theta, bias_choice) %>%  
  summarize(count = n()) %>% 
  mutate(prop = count/sum(count)) %>% 
  filter(bias_choice == TRUE) %>% 
  ggplot(aes(x=psi, y=prop, color = theta, group = theta)) + 
  geom_point(alpha = .7, size = 3) +
  geom_line(linewidth = 1) + 
  scale_color_scico(palette = "imola") + 
  scale_y_continuous(n.breaks = 3) + 
  scale_x_continuous(breaks = seq(0,1,.5)) + 
  labs(x = "Switching Probability (Search Rule)",
       y = "Proportion of Starting Option Choices",
       color = "Threshold\n(Stopping Rule)") +
  theme_minimal(base_size = 14)

ggsave("manuscript/figures/appendix/initial_bias_choices.png", width = 8, height = 6)


# starting bias sampled probabilities 

custom_labels <- c("r" = "Starting Option: Risky", "s" = "Starting Option: Safe")

choices_summary %>% 
  mutate(extreme = if_else((sp_r_1 == 1 | sp_r_1 == 0), TRUE, FALSE)) %>% 
  group_by(psi, theta, start_o, extreme) %>%  
  summarize(count = n()) %>% 
  mutate(prop_extreme = count/sum(count)) %>% 
  filter(extreme == TRUE) %>% 
  ggplot(aes(x=psi, y=prop_extreme, color = theta, group = theta)) + 
  facet_wrap(~start_o, labeller = as_labeller(custom_labels)) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) + 
  scale_color_scico(palette = "imola") + 
  scale_y_continuous(limits = c(-.1,1.1), breaks = c(0, 1, .5)) + 
  scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0,1,.5)) + 
  labs(x = "Switching Probability (Search Rule)",
       y = "Proportion of Choices Without\nSampling Both Risky Outcomes",
       color = "Threshold\n(Stopping Rule)") +
  theme_minimal(base_size = 20)

ggsave("manuscript/figures/appendix/initial_bias_sampled_probabilities.png", width = 14, height = 6)

# starting option bias risk aversion 

# prepare data 
rates <- choices_summary %>% 
  filter(!c(sp_r_1 == 0 | sp_r_2 == 0)) %>% # remove choices where the risky option was not experienced as such
  mutate(safe_choice = ifelse(choice == "s", 1, 0)) %>% # risk averse choice
  group_by(psi, theta, safe_choice) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(safe_choice == 0))

rates %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Switching Probability\n(Search Rule)",
       y = "% Safe Choices",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

ggsave(file = "manuscript/figures/appendix/initial_bias_risk_seeking.png", width = 8, height = 6)


# starting bias risky choice proportions

left_join(choices_summary, problems, by=join_by(id)) %>% 
  mutate(choice_r = if_else(choice == "r", 1, 0) , # predict choice of risky option
         r_low = if_else(r_1 < r_2, r_1, r_2) ,
         r_high = if_else(r_1 > r_2, r_1, r_2) ,         
         sp_r_low = if_else(r_low == r_1, sp_r_1, sp_r_2) , 
         sp_r_high = if_else(r_high == r_1, sp_r_1, sp_r_2)) %>% 
  group_by(psi, theta, sp_r_high, choice) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  filter(choice == "r") %>% 
  mutate(bias = if_else(sp_r_high == 1, "p=0 or p=1", if_else(sp_r_high == 0, "p=0 or p=1", "0 < p < 1"))) %>% 
  ggplot(aes(x=sp_r_high, y = prop)) +
  geom_point(aes(color = bias), size = 1) +
  scale_color_manual(values = c("gray", "black")) + 
  scale_alpha_manual(values = c(.5, 1)) + 
  facet_grid(psi~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed), 
                                            psi = as_labeller(label_psi, default = label_parsed))) + 
  labs(x = TeX("$\\p_{high}$"), 
       y = "Proportion of Risky Choices", 
       color = "", 
       alpha = "") +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0,1,.5)) + 
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0,1,.5)) + 
  theme_minimal(base_size = 20) + 
  theme(legend.position = "top")
ggsave("manuscript/figures/appendix/initial_bias_risky_choice_proportions.png", width = 14, height = 14)



## Posterior Predictive Checks ----------------------------------------------------

# pre-processing

cpt_clean <- cpt %>%  
  select(model:mean) %>%
  filter(parameter != "deviance") %>% 
  pivot_wider(names_from = parameter, values_from = mean) %>% 
  mutate(across(alpha:rho, ~round(., 2)))

ppset <- choices %>%
  left_join(cpt_clean, by = c("model", "psi", "theta")) %>% 
  filter(!c(smp_s == 0 | smp_r == 0)) %>% 
  mutate(choice_obs = if_else(choice == "s", 1,0) ,
         r_low = if_else(r_1 < r_2, r_1, r_2) ,
         r_high = if_else(r_1 > r_2, r_1, r_2) ,
         sp_r_low = if_else(r_low == r_1, sp_r_1, sp_r_2) ,
         sp_r_high = if_else(r_high == r_1, sp_r_1, sp_r_2))

# make  predictions

set.seed(681271)
postpred <- ppset %>% mutate(
  w_high = round( (delta * sp_r_high^gamma) / ( (delta*sp_r_high^gamma)+(1-sp_r_high)^gamma ), 2) , 
  w_low = 1 - w_high , 
  v_high = r_high^alpha , 
  v_low = r_low^alpha , 
  v_safe = safe^alpha , 
  V_safe = v_safe , 
  V_risky = (w_high * v_high) + (w_low * v_low) ,
  V_safe_scaled = V_safe^(1/alpha) , 
  V_risky_scaled = V_risky^(1/alpha) ,
  V_diff = V_safe_scaled-V_risky_scaled , 
  p_safe_risky = round(1 / ( 1 + exp(-rho*V_diff) ) , 2) ,
  choice_pp = rbinom(n=nrow(ppset), size=1, prob=p_safe_risky))
# write_rds(postpred, "data/posterior_predictions.rds.bz2", compress = "bz2")

# post-processing

## compute observed choice rates 

### risky choice proportions

riskprop_obs <- choices %>% 
  mutate(choice_cmp = ifelse(choice == "r", 0, 1), 
         r_low = if_else(r_1 < r_2, r_1, r_2) ,
         r_high = if_else(r_1 > r_2, r_1, r_2) ,
         sp_r_low = if_else(r_low == r_1, sp_r_1, sp_r_2) ,
         sp_r_high = if_else(r_high == r_1, sp_r_1, sp_r_2)) %>% 
  group_by(model, psi, theta, sp_r_high, choice_cmp) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n),2) , 
         bias = if_else(sp_r_high == 1, "p=0 or p=1", if_else(sp_r_high == 0, "p=0 or p=1", "0 < p < 1")) , 
         type = "Sampling Strategy") %>% 
  filter(choice_cmp == 0)

riskprop_pp <- postpred %>% 
  mutate(choice_cmp = choice_pp) %>% 
  group_by(model, psi, theta, sp_r_high, choice_cmp) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n), 2), 
         bias = if_else(sp_r_high == 1, "p=0 or p=1", if_else(sp_r_high == 0, "p=0 or p=1", "0 < p < 1")) , 
         type = "Posterior Predictive") %>% 
  filter(choice_cmp == 0)

riskprop <- bind_rows(riskprop_obs, riskprop_pp)

### maximization rates

max_rates_obs <- choices %>% 
  mutate(norm = case_when(avg_r/safe > 1 ~ "r", avg_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  mutate(norm = ifelse(norm == "r", "Risky Option", "Safe Option")) %>% 
  group_by(model, psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1) %>% 
  mutate(type = "Sampling Strategy")

max_rates_pp <- postpred %>% 
  mutate(norm = case_when(avg_r/safe > 1 ~ "r", avg_r/safe < 1 ~ "s") , 
         choice_pp = ifelse(choice_pp == 0, "r", "s"), 
         norm_choice = ifelse(choice_pp == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  mutate(norm = ifelse(norm == "r", "Risky Option", "Safe Option")) %>% 
  group_by(model, psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1) %>% 
  mutate(type = "CPT Posterior Predictive")

max_rates <- bind_rows(max_rates_obs, max_rates_pp)

## compute predictive accuracy

pp_acc <- postpred %>% 
  select(model, psi, theta, alpha, gamma, delta, rho, id, agent, choice_obs, choice_pp) %>% 
  mutate(match = if_else(choice_obs == choice_pp, 1, 0)) %>% 
  group_by(model, psi, theta, alpha, gamma, delta, rho, match) %>% 
  summarise(count = n()) %>% 
  mutate(perc = round(count/sum(count), 3)) %>% 
  filter(match != 0) %>% 
  ungroup()

## compute DIC

dic <- cpt %>% 
  filter(parameter == "deviance") %>% 
  select(model:sd) %>% 
  mutate(var = sd^2 ,
         pD = var/2 , 
         DIC = round(pD + mean, 1))

# plot data 

## summary comparison

riskprop_summary <- riskprop %>% filter(model == "summary")
max_rates_summary <- max_rates %>% filter(model == "summary")
pp_acc_summary <- pp_acc %>% filter(model == "summary")
dic_summary <- dic %>% filter(model == "summary")

riskprop_summary %>% 
  ggplot(aes(x=sp_r_high, y = prop)) +
  geom_point(aes(shape = type, color = bias, alpha = bias), size = 3) +
  scale_shape_manual(values = c(4, 16)) + 
  scale_color_manual(values = c("gray", "black")) + 
  scale_alpha_manual(values = c(.5, 1)) + 
  facet_grid(psi~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed), 
                                            psi = as_labeller(label_psi, default = label_parsed))) + 
  labs(x = TeX("$\\p_{high}$"), 
       y = "Proportion of Risky Choices", 
       shape = "", 
       color = "", 
       alpha = "") +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0,1,.5)) + 
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0,1,.5)) + 
  theme_minimal(base_size = 20) + 
  theme(legend.position='top') + 
  geom_text(data = dic_summary, aes(label=paste("DIC=", as.character(DIC)), x = .7, y = -.1)) 
ggsave("manuscript/figures/appendix/ppc_summary_riskprop.png", width = 14, height = 16)

max_rates_summary_p <- max_rates_summary %>% 
  ggplot(aes(x=psi, y=prop, group = norm, color = norm)) + 
  facet_grid(type~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_point(aes(shape = type), size = 3) + 
  geom_line() + 
  scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0,1,length.out = 3)) + 
  scale_y_continuous(limits = c(.4, 1.1), breaks = seq(.5, 1, length.out = 3)) +
  scale_shape_manual(values = c(4, 19)) + 
  labs(x = "Switching Probability (Search Rule)" ,
       y = "Proportion of\nMaximizing Choices" ,
       color = "Better Average",
       shape = "") + 
  theme_minimal(base_size = 20) + 
  theme(strip.text.y = element_blank()) + 
  guides(shape = "none")

pp_acc_summary <- pp_acc %>%  
  filter(model == "summary") %>% 
  ggplot(aes(x = psi, y = perc)) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(.4, 1.1), breaks = seq(.5, 1, length.out = 3)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_point(size = 3, shape = 4) + 
  labs(x = "Switching Probability (Search Rule)" , 
       y = "Proportion of\nCorrect Predictions", 
       color = "Choice Consistency") + 
  scale_color_viridis(option = "C") + 
  theme_minimal(base_size = 20)

max_rates_summary_p + pp_acc_summary + 
  plot_layout(nrow = 2, guides = "collect") + 
  plot_annotation(tag_levels = "A") & 
  theme(legend.position='top')
ggsave(file = "manuscript/figures/appendix/ppc_summary_max_acc.png", width = 14, height = 10)


## roundwise comparison 

riskprop_roundwise <- riskprop %>% filter(model == "roundwise")
max_rates_roundwise <- max_rates %>% filter(model == "roundwise")
pp_acc_roundwise <- pp_acc %>% filter(model == "roundwise")
dic_roundwise <- dic %>% filter(model == "roundwise")

riskprop_roundwise %>% 
  ggplot(aes(x=sp_r_high, y = prop)) +
  geom_point(aes(shape = type, color = bias, alpha = bias), size = 3) +
  scale_shape_manual(values = c(4, 16)) + 
  scale_color_manual(values = c("gray", "black")) + 
  scale_alpha_manual(values = c(.5, 1)) + 
  facet_grid(psi~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed), 
                                            psi = as_labeller(label_psi, default = label_parsed))) + 
  labs(x = TeX("$\\p_{high}$"), 
       y = "Proportion of Risky Choices", 
       shape = "", 
       color = "", 
       alpha = "") +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0,1,.5)) + 
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0,1,.5)) + 
  theme_minimal(base_size = 20) + 
  theme(legend.position='top') + 
  geom_text(data = dic_roundwise, aes(label=paste("DIC=", as.character(DIC)), x = .7, y = -.1)) 
ggsave("manuscript/figures/appendix/ppc_roundwise_riskprop.png", width = 14, height = 16)

max_rates_roundwise_p <- max_rates_roundwise %>% 
  ggplot(aes(x=psi, y=prop, group = norm, color = norm)) + 
  facet_grid(type~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_point(aes(shape = type), size = 3) + 
  geom_line() + 
  scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0,1,length.out = 3)) + 
  scale_y_continuous(limits = c(.4, 1.1), breaks = seq(.5, 1, length.out = 3)) +
  scale_shape_manual(values = c(4, 19)) + 
  labs(x = "Switching Probability (Search Rule)" ,
       y = "Proportion of\nMaximizing Choices" ,
       color = "Better Average",
       shape = "") + 
  theme_minimal(base_size = 20) + 
  theme(strip.text.y = element_blank()) + 
  guides(shape = "none")

pp_acc_roundwise <- pp_acc %>%  
  filter(model == "roundwise") %>% 
  ggplot(aes(x = psi, y = perc)) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(.4, 1.1), breaks = seq(.5, 1, length.out = 3)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_point(size = 3, shape = 4) + 
  labs(x = "Switching Probability (Search Rule)" , 
       y = "Proportion of\nCorrect Predictions", 
       color = "Choice Consistency") + 
  scale_color_viridis(option = "C") + 
  theme_minimal(base_size = 20)

max_rates_roundwise_p + pp_acc_roundwise + 
  plot_layout(nrow = 2, guides = "collect") + 
  plot_annotation(tag_levels = "A") & 
  theme(legend.position='top')
ggsave(file = "manuscript/figures/appendix/ppc_roundwise_max_acc.png", width = 14, height = 10)
