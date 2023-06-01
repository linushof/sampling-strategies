# preparation -------------------------------------------------------------

# load pkgs 
pacman::p_load(tidyverse,
               scico, # for scientific color palettes
               latex2exp, # for LaTeX expressions in plots
               papaja,
               ggpubr,
               patchwork)


# load data
choices <- read_rds("data/choice_data.rds.bz2") 
cpt <- read_rds("data/cpt_estimates.rds") 
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

# accumulation trajectories -----------------------------------------------

# prepare data

## summary

### evidence for each number of sampled outcomes

#### summary
summary <- summary %>% 
  mutate(psi = 1-(psi+.5)) %>% # recode psi
  filter(threshold == "relative") %>% 
  group_by(psi, threshold, theta, problem, agent) %>% # group by trial
  mutate(smp_no = row_number(), # assign sample numbers
         diff = if_else(smp_no == 1, 0, diff)) %>% 
  fill(diff) # fill missing values

#### round
round <- round %>% 
  mutate(psi = 1-(psi+.5)) %>% # recode psi
  filter(threshold == "relative") %>% 
  group_by(psi, threshold, theta, problem, agent) %>% # group by trial
  mutate(smp_no = row_number(), # assign sample numbers
         diff = if_else(smp_no == 1, 0, diff)) %>% # fill missing values
  fill(diff)

### median evidence

#### summary
summary_median <- summary %>% 
  group_by(psi, threshold, theta, problem, smp_no) %>% 
  summarise(count = n(), 
            median = median(diff)) %>% 
  slice(seq_len(min(which(median <= -theta | median >= theta), n())))

#### round
round_median <- round %>% 
  group_by(psi, threshold, theta, problem, smp_no) %>% 
  summarise(count = n(), 
            median = median(diff)) %>% 
  slice(seq_len(min(which(median %in% c(-theta, theta)), n())))


### subset of selected trials (relative)

### get problems where sampling strategies should drive towards different thresholds
choice_problems %>% mutate(id = row_number()) %>% filter(r_ev > safe & p_r_low > p_r_high) # 7, 35 
choice_problems %>% mutate(id = row_number()) %>% filter(r_ev < safe & p_r_low < p_r_high) # 43 , 47

#### 43

problem_number <- 43
round_boundary <- 5
summary_boundary <- 75  

##### summary
summary_sub <- summary %>% 
  filter(psi %in% c((1-.9), .5, 1), theta == summary_boundary, problem == problem_number) %>% 
  mutate(diff = case_when(diff < -summary_boundary ~ -summary_boundary, 
                          diff > summary_boundary ~ summary_boundary, 
                          diff >= -summary_boundary & diff <= summary_boundary ~ diff))

summary_median_sub <- summary_median %>%  
  filter(psi %in% c((1-.9), .5, 1), theta == summary_boundary, problem == problem_number) %>%
  mutate(median = case_when(median < -summary_boundary ~ -summary_boundary, 
                            median > summary_boundary ~ summary_boundary, 
                            median >= -summary_boundary & median <= summary_boundary ~ median))

##### round
round_sub <- round %>%
  filter(psi %in% c((1-.9), .5, 1),
         theta == round_boundary, 
         problem == problem_number)

round_median_sub <- round_median %>% 
  filter(psi %in% c((1-.9), .5, 1),
         theta == round_boundary, 
         problem == problem_number)

##### plot data

###### summary

### labels for evidence thresholds
ann_risky <- data.frame(psi=(1-.9), smp_no = 40, diff=65, label="Risky Threshold \n 8.6 (19%) or 17.36 (81%)")
ann_safe <- data.frame(psi=(1-.9), smp_no = 40, diff=-65, label="Safe Threshold \n 15.70 (100%)")

summary_trajectories <- summary_sub %>% 
  ggplot(aes(x = smp_no, y = diff)) + 
  facet_wrap(~psi, nrow = 3, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed)), scales = "free_x") + 
  scale_y_continuous(limits = c(-summary_boundary, summary_boundary), 
                     breaks = seq(-summary_boundary, summary_boundary, summary_boundary)) +
  labs(title = "Summary", 
       x = "Number of Sampled Outcomes",
       y = "Difference in Cumulative Sums",
       color = expression(psi),
       alpha = "Agent\nCount") +
  geom_hline(yintercept = c(-summary_boundary, 0, summary_boundary), linetype = "dashed") + 
  geom_text(data = ann_risky, label=ann_risky$label) + 
  geom_text(data = ann_safe, label=ann_safe$label) +
  geom_line(aes(group = agent), position = position_dodge(width = .3), size = .3, alpha = .5, color = "gray") + 
  geom_line(data = summary_median_sub, aes(y = median, alpha = count), size = 1, color = "#9c179e") +
  theme_apa()

###### round-wise

round_trajectories <- round_sub %>%
  ggplot(aes(x = smp_no, y = diff)) + 
  facet_wrap(~psi, nrow = 3, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed)), scales = "free_x") + 
  scale_y_continuous(limits = c(-round_boundary, round_boundary), 
                     breaks = seq(-round_boundary, round_boundary, round_boundary)) +
  labs(title = "Round-wise", 
       x = "Number of Sampled Outcomes",
       y = "Difference in Round Wins",
       color = expression(psi),
       alpha = "Agent\nCount") +
  geom_hline(yintercept = c(-round_boundary, 0, round_boundary), linetype = "dashed") + 
  geom_line(aes(group = agent), position = position_dodge(width = .3), size = .3, alpha = .5, color = "gray") + 
  geom_line(data = round_median_sub, aes(y = median, alpha = count), size = 1, color = "#9c179e") +
  theme_apa()

## merge and save plots
summary_trajectories + round_trajectories + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
ggsave(file = "manuscript/figures/accumulation_problem_43.png", width = 14, height = 14)

#### 35

problem_number <- 35
round_boundary <- 5
summary_boundary <- 75  

##### summary
summary_sub <- summary %>% 
  filter(psi %in% c((1-.9), .5, 1), theta == summary_boundary, problem == problem_number) %>% 
  mutate(diff = case_when(diff < -summary_boundary ~ -summary_boundary, 
                          diff > summary_boundary ~ summary_boundary, 
                          diff >= -summary_boundary & diff <= summary_boundary ~ diff))

summary_median_sub <- summary_median %>%  
  filter(psi %in% c((1-.9), .5, 1), theta == summary_boundary, problem == problem_number) %>%
  mutate(median = case_when(median < -summary_boundary ~ -summary_boundary, 
                            median > summary_boundary ~ summary_boundary, 
                            median >= -summary_boundary & median <= summary_boundary ~ median))

##### round
round_sub <- round %>%
  filter(psi %in% c((1-.9), .5, 1),
         theta == round_boundary, 
         problem == problem_number)

round_median_sub <- round_median %>% 
  filter(psi %in% c((1-.9), .5, 1),
         theta == round_boundary, 
         problem == problem_number)

##### plot data

###### summary

### labels for evidence thresholds
ann_risky <- data.frame(psi=(1-.9), smp_no = 50, diff=65, label="Risky Threshold \n 10.44 (84%) or 12.33 (16%)")
ann_safe <- data.frame(psi=(1-.9), smp_no = 50, diff=-65, label="Safe Threshold \n 10.51 (100%)")

summary_trajectories <- summary_sub %>% 
  ggplot(aes(x = smp_no, y = diff)) + 
  facet_wrap(~psi, nrow = 3, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed)), scales = "free_x") + 
  scale_y_continuous(limits = c(-summary_boundary, summary_boundary), 
                     breaks = seq(-summary_boundary, summary_boundary, summary_boundary)) +
  labs(title = "Summary", 
       x = "Number of Sampled Outcomes",
       y = "Difference in Cumulative Sums",
       color = expression(psi),
       alpha = "Agent\nCount") +
  geom_hline(yintercept = c(-summary_boundary, 0, summary_boundary), linetype = "dashed") + 
  geom_text(data = ann_risky, label=ann_risky$label) + 
  geom_text(data = ann_safe, label=ann_safe$label) +
  geom_line(aes(group = agent), position = position_dodge(width = .3), size = .3, alpha = .5, color = "gray") + 
  geom_line(data = summary_median_sub, aes(y = median, alpha = count), size = 1, color = "#9c179e") +
  theme_apa()

####### round-wise
round_trajectories <- round_sub %>%
  ggplot(aes(x = smp_no, y = diff)) + 
  facet_wrap(~psi, nrow = 3, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed)), scales = "free_x") + 
  scale_y_continuous(limits = c(-round_boundary, round_boundary), 
                     breaks = seq(-round_boundary, round_boundary, round_boundary)) +
  labs(title = "Round-wise", 
       x = "Number of Sampled Outcomes",
       y = "Difference in Round Wins",
       color = expression(psi),
       alpha = "Agent\nCount") +
  geom_hline(yintercept = c(-round_boundary, 0, round_boundary), linetype = "dashed") + 
  geom_line(aes(group = agent), position = position_dodge(width = .3), size = .3, alpha = .5, color = "gray") + 
  geom_line(data = round_median_sub, aes(y = median, alpha = count), size = 1, color = "#9c179e") +
  theme_apa()

## merge and save plots
summary_trajectories + round_trajectories + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
ggsave(file = "manuscript/figures/accumulation_problem_35.png", width = 14, height = 14)

# sampled frequencies -----------------------------------------------------

# prepare data 
round_freq  <- round %>% 
  filter(psi %in% c((1-.9), .5, 1),
         theta == round_boundary) %>% 
  group_by(psi, threshold, theta, problem, agent) %>%
  mutate(n_sample = n(), # compute trial-level n 
         n_s = sum(is.na(r)), 
         n_r = n_sample - n_s, 
         freq_r_high = round(sum(if_else(r == r_high, 1, 0), na.rm = TRUE)/n_r, 2)) %>% # compute trial-level frequencies
  ungroup() %>%
  group_by(psi, threshold, theta, problem, agent, round) %>% 
  mutate(n_round = n(), 
         n_round_s = sum(is.na(r)),
         n_round_r = n_round - n_round_s,
         freq_round_r_high = round(sum(if_else(r == r_high, 1, 0), na.rm = TRUE)/n_round_r, 2)) %>% # compute round-level frequencies
  distinct(psi, threshold, theta, problem, agent, round, freq_r_high, freq_round_r_high) # drop redundant rows

round_freq_median <- round_freq %>% 
  group_by(psi, threshold, theta, freq_r_high) %>% 
  summarise(median_freq_round_r_high = median(freq_round_r_high, na.rm = TRUE)) # compute median round-level frequencies for each parameter combination and trial-level frequency

# plot data
round_freq %>% 
  ggplot(aes(x = freq_r_high, y = freq_round_r_high)) +
  facet_wrap(~psi, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed))) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, .5)) + 
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, .5)) + 
  labs(x = "Trial-Level Frequency",
       y = "Round-Level Frequency") +
  geom_density_2d_filled() +
  scale_fill_scico_d(palette = "acton") +
  geom_point(data = round_freq_median, aes(y = median_freq_round_r_high), size = .7, color = "white") + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "white") + 
  theme_apa() + 
  theme(legend.position = "none")

# save plot
ggsave(file = "manuscript/figures/frequencies.png", width = 10, height = 4)

# maximization rates ------------------------------------------------------

## prepare data (compute maximization rates)
rates <- choices %>%
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


## plot data 


### summary
max_summary <- rates %>%
  filter(model == "summary" & threshold == "relative") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary", 
       x = expression(paste("Switching Probability  ", psi)),
       y = "Maximization Rate",
       color = "Threshold") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_apa(base_size = 16)

### round-wise
max_roundwise <- rates %>%
  filter(model == "roundwise" & threshold == "relative") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
  scale_color_scico_d(palette = "imola", alpha = .7, direction = -1) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Round-wise", 
       x = expression(paste("Switching Probability  ", psi)),
       y = "Maximization Rate",
       color = "Threshold") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_apa(base_size = 16)

### merge and save plots
max_summary + max_roundwise + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/rates_maximization.png", width = 14, height = 6)


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
  filter(model == "summary" & threshold == "relative") %>% 
  #filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary", 
       x = expression(paste("Switching Probability  ", psi)),
       y = "Proportion of Safe Choices",
       color = "Threshold") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_apa(base_size = 16)

r_averse_roundwise <- rates %>%
  filter(model == "roundwise" & threshold == "relative") %>% 
  #filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
  scale_color_scico_d(palette = "imola", alpha = .7, direction = - 1) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Round-wise", 
       x = expression(paste("Switching Probability  ", psi)),
       y = "Proportion of Safe Choices",
       color = "Threshold") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_apa(base_size = 16)

r_averse_summary + r_averse_roundwise + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/rates_risk_aversion.png", width = 14, height = 6)



# weighting function ---------------------------------------------------------


# check MCMC statistics / convergence 

# scale reduction factor 
max(cpt$Rhat) # 1.0014 ----> <= 1.001 
min(cpt$n.eff) # 17,000

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
  filter(parameter == "gamma", threshold == "relative") %>%
  ggplot(aes(psi, mean, color = psi)) + 
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) + 
  scale_y_continuous(limits =c(-.1, 2.1), breaks = seq(0,2, length.out = 3)) +
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Curvature  ", gamma)),
       color = expression(psi)) +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_apa(base_size = 16)

#### delta
delta_summary <- cpt_summary %>%
  filter(parameter == "delta", threshold == "relative") %>%
  ggplot(aes(psi, mean, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) +
  scale_y_continuous(limits = c(-0.1, 10.1), breaks = seq(0, 10, length.out = 3)) +
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Elevation  ", delta)), 
       color = expression(psi)) +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_apa(base_size = 16)

####  probability weighting
wf_summary <- weights_summary %>% 
  filter(threshold == "relative") %>% 
  ggplot(aes(p, w, group = psi, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  scale_x_continuous(breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Sampled Relative Frequency",
       y = expression(paste("Decision Weight  ", pi)),
       color = expression(psi)) +
  geom_line(linewidth = 1) +
  theme_apa(base_size = 16)

# merge and save plots

wf_summary + gamma_summary + delta_summary + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/cpt_weighting_summary.png", width = 14, height = 10)

## round-wise

cpt_roundwise <- cpt %>% filter(model == "roundwise")
weights_roundwise <- weights %>% filter(model == "roundwise")

#### gamma
gamma_roundwise <- cpt_roundwise %>%
  filter(parameter == "gamma", threshold == "relative") %>%
  ggplot(aes(psi, mean, color = psi)) + 
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) + 
  scale_y_continuous(limits =c(-.1, 2.1), breaks = seq(0,2, length.out = 3)) +
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Curvature  ", gamma)),
       color = expression(psi)) +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_apa(base_size = 16)

#### delta
delta_roundwise <- cpt_roundwise %>%
  filter(parameter == "delta", threshold == "relative") %>%
  ggplot(aes(psi, mean, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) +
  scale_y_continuous(limits = c(-0.1, 10.1), breaks = seq(0,10, length.out = 3)) +
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Elevation  ", delta)), 
       color = expression(psi)) +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_apa(base_size = 16)

####  probability weighting
wf_roundwise <- weights_roundwise %>% 
  filter(threshold == "relative") %>% 
  ggplot(aes(p, w, group = psi, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  scale_x_continuous(breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Sampled Relative Frequency",
       y = expression(paste("Decision Weight  ", pi)),
       color = expression(psi)) +
  geom_line(linewidth = 1) +
  theme_apa(base_size = 16)

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
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Concavity  ", alpha)),
       color = expression(psi)) +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_apa(base_size = 16)
alpha_summary


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
       color = expression(psi)) +
  geom_line(linewidth = 1) +
  theme_apa(base_size = 16)

# merge and save plots
vf_summary + alpha_summary + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/cpt_value_summary.png", width = 14, height = 7)


## round-wise

values_roundwise <- values %>% filter(model == "roundwise")

### alpha
alpha_roundwise <- cpt_roundwise %>%
  filter(parameter == "alpha", threshold == "relative") %>% 
  ggplot(aes(psi, mean, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0,1, length.out = 3)) + 
  scale_y_continuous(limits = c(-0.1, 2.1), breaks = seq(0,2, length.out = 3)) + 
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Concavity  ", alpha)),
       color = expression(psi)) +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_apa(base_size = 16)

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
       color = expression(psi)) +
  geom_line(linewidth = 1) +
  theme_apa(base_size = 16)

# merge and save plots
vf_roundwise + alpha_roundwise + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/cpt_value_roundwise.png", width = 14, height = 7)


# appendix ----------------------------------------------------------------

# choice rule  

rho_summary <- cpt_summary %>%
  filter(parameter == "rho", threshold == "relative") %>% 
  ggplot(aes(psi, mean)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0,1,length.out = 3)) + 
  scale_y_continuous(limits = c(-.5, 5.5), breaks = seq(0,5, length.out = 3)) + 
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Choice Consistency  ", rho))) +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_apa(base_size = 16)

rho_roundwise <- cpt_roundwise %>%
  filter(parameter == "rho", threshold == "relative") %>% 
  ggplot(aes(psi, mean)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0, 1, length.out = 3)) + 
  scale_y_continuous(limits = c(-.5, 5.5), breaks = seq(0,5, length.out = 3)) + 
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Choice Consistency  ", rho))) +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_apa(base_size = 16)

### combine plots
rho_summary + rho_roundwise + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/choice_rule.png", width = 14, height = 7)

# absolute thresholds

## HERE: Add analyses

