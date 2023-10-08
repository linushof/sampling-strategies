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

# accumulation trajectories -----------------------------------------------

# prepare data

## compute evidence conditional on number of sampled outcomes

### summary
summary <- summary %>% 
  mutate(psi = 1-(psi+.5)) %>% # recode psi
  filter(threshold == "relative") %>% 
  group_by(psi, threshold, theta, problem, agent) %>% # group by trial
  mutate(smp_no = row_number(), # assign sample numbers
         diff = if_else(smp_no == 1, 0, diff)) %>% 
  fill(diff) # fill missing values

####  median evidence
summary_median <- summary %>% 
  group_by(psi, threshold, theta, problem, smp_no) %>% 
  summarise(count = n(), 
            median = median(diff)) %>% 
  slice(seq_len(min(which(median <= -theta | median >= theta), n())))

### round
round <- round %>% 
  mutate(psi = 1-(psi+.5)) %>% # recode psi
  filter(threshold == "relative") %>% 
  group_by(psi, threshold, theta, problem, agent) %>% # group by trial
  mutate(smp_no = row_number(), # assign sample numbers
         diff = if_else(smp_no == 1, 0, diff)) %>% # fill missing values
  fill(diff)

#### median evidence
round_median <- round %>% 
  group_by(psi, threshold, theta, problem, smp_no) %>% 
  summarise(count = n(), 
            median = median(diff)) %>% 
  slice(seq_len(min(which(median %in% c(-theta, theta)), n())))

## find illustrative problems where option with higher EV is not the option that returns the higher outcome most of the time 
choice_problems %>% mutate(id = row_number()) %>% filter(r_ev > safe & p_r_low > p_r_high) # 7, 35 
choice_problems %>% mutate(id = row_number()) %>% filter(r_ev < safe & p_r_low < p_r_high) # 43 , 47


# plot problem 43
problem_number <- 43

## summary

summary_boundary <- 75  
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

ann_risky <- data.frame(psi=(1-.9), smp_no = 40, diff=60, label="Risky Threshold \n 8.6 (19%) or 17.36 (81%)")
ann_safe <- data.frame(psi=(1-.9), smp_no = 40, diff=-60, label="Safe Threshold \n 15.70 (100%)")

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
  geom_text(data = ann_risky, label=ann_risky$label, size = 5) + 
  geom_text(data = ann_safe, label=ann_safe$label, size = 5) +
  geom_line(aes(group = agent), position = position_dodge(width = .3), size = .3, alpha = .5, color = "gray") + 
  geom_line(data = summary_median_sub, aes(y = median, alpha = count), size = 1, color = "#9c179e") +
  theme_apa(base_size = 20)

## round-wise

round_boundary <- 5
round_sub <- round %>%
  filter(psi %in% c((1-.9), .5, 1), theta == round_boundary, problem == problem_number)

round_median_sub <- round_median %>% 
  filter(psi %in% c((1-.9), .5, 1),
         theta == round_boundary, 
         problem == problem_number)

round_trajectories <- round_sub %>%
  ggplot(aes(x = smp_no, y = diff)) + 
  facet_wrap(~psi, nrow = 3, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed)), scales = "free_x") + 
  scale_y_continuous(limits = c(-round_boundary, round_boundary), 
                     breaks = seq(-round_boundary, round_boundary, round_boundary)) +
  labs(title = "Roundwise", 
       x = "Number of Sampled Outcomes",
       y = "Difference in Round Wins",
       color = expression(psi),
       alpha = "Agent\nCount") +
  geom_hline(yintercept = c(-round_boundary, 0, round_boundary), linetype = "dashed") + 
  geom_line(aes(group = agent), position = position_dodge(width = .3), size = .3, alpha = .5, color = "gray") + 
  geom_line(data = round_median_sub, aes(y = median, alpha = count), size = 1, color = "#9c179e") +
  theme_apa(base_size = 20)

### merge and save plots
summary_trajectories + round_trajectories + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
ggsave(file = "manuscript/figures/accumulation_problem_43.png", width = 14, height = 14)

# plot problem 35
problem_number <- 35

## summary
summary_boundary <- 75  
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

ann_risky <- data.frame(psi=(1-.9), smp_no = 50, diff=60, label="Risky Threshold \n 10.44 (84%) or 12.33 (16%)")
ann_safe <- data.frame(psi=(1-.9), smp_no = 50, diff=-60, label="Safe Threshold \n 10.51 (100%)")

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
  geom_text(data = ann_risky, label=ann_risky$label, size = 5) + 
  geom_text(data = ann_safe, label=ann_safe$label, size = 5) +
  geom_line(aes(group = agent), position = position_dodge(width = .3), size = .3, alpha = .5, color = "gray") + 
  geom_line(data = summary_median_sub, aes(y = median, alpha = count), size = 1, color = "#9c179e") +
  theme_apa(base_size = 20)

## round-wise
round_boundary <- 5
round_sub <- round %>%
  filter(psi %in% c((1-.9), .5, 1),
         theta == round_boundary, 
         problem == problem_number)

round_median_sub <- round_median %>% 
  filter(psi %in% c((1-.9), .5, 1),
         theta == round_boundary, 
         problem == problem_number)

round_trajectories <- round_sub %>%
  ggplot(aes(x = smp_no, y = diff)) + 
  facet_wrap(~psi, nrow = 3, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed)), scales = "free_x") + 
  scale_y_continuous(limits = c(-round_boundary, round_boundary), 
                     breaks = seq(-round_boundary, round_boundary, round_boundary)) +
  labs(title = "Roundwise", 
       x = "Number of Sampled Outcomes",
       y = "Difference in Round Wins",
       color = expression(psi),
       alpha = "Agent\nCount") +
  geom_hline(yintercept = c(-round_boundary, 0, round_boundary), linetype = "dashed") + 
  geom_line(aes(group = agent), position = position_dodge(width = .3), size = .3, alpha = .5, color = "gray") + 
  geom_line(data = round_median_sub, aes(y = median, alpha = count), size = 1, color = "#9c179e") +
  theme_apa(base_size = 20)

### merge and save plots
summary_trajectories + round_trajectories + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
ggsave(file = "manuscript/figures/accumulation_problem_35.png", width = 14, height = 14)

# maximization rates experienced EV ------------------------------------------------------

## prepare data (compute maximization rates)
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


## plot data 

### summary
max_EV_exp_summary <- rates_EV_exp %>%
  filter(model == "summary" & threshold == "relative") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "Proportion of Choices\nMaximizing the Sampled Average",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_apa(base_size = 20)

### round-wise
max_EV_exp_roundwise <- rates_EV_exp %>%
  filter(model == "roundwise" & threshold == "relative") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
  scale_color_scico_d(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "Proportion of Choices\nMaximizing the Sampled Average",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_apa(base_size = 20)

### merge and save plots
max_EV_exp_summary + max_EV_exp_roundwise + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/maximization_average.png", width = 14, height = 6)

# maximization rates EV (ground truth) ------------------------------------

## prepare data (compute maximization rates)
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

## plot data 

### summary
max_EV_summary <- rates_EV %>%
  filter(model == "summary" & threshold == "relative") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "Proportion of Choices\nMaximizing the Expected Value",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_apa(base_size = 20)

### round-wise
max_EV_roundwise <- rates_EV %>%
  filter(model == "roundwise" & threshold == "relative") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
  scale_color_scico_d(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "Proportion of Choices\nMaximizing the Expected Value",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_apa(base_size = 20)

### merge and save plots
max_EV_summary + max_EV_roundwise + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/maximization_EV.png", width = 14, height = 6)

###
max_EV_exp <- ggarrange(max_EV_exp_summary, max_EV_exp_roundwise)
max_EV <- ggarrange(max_EV_summary, max_EV_roundwise)
max_EV_exp + max_EV + plot_layout(nrow = 2) + plot_annotation(tag_levels = "A")
###

# effort (sample size) and reward rate ---------------------------------------------

# sampling effort

effort_summary <- choices %>% 
  filter(model == "summary", threshold == "relative") %>% 
  group_by(psi, theta) %>% 
  summarise(mean_n = mean(n_sample), 
            median_n = median(n_sample)) %>% 
  ggplot(aes(x=psi, y=median_n, group=theta, color = theta)) + 
  scale_color_scico(palette = "imola") + 
  labs(title = "Summary Comparison", 
       x = expression(paste("Switching Probability  ", psi)),
       y = "Average Sample Size",
       color = "Threshold") +
  geom_point(size = 3) + 
  geom_line(linewidth = 1) + 
  theme_apa(base_size = 20)

effort_roundwise <- choices %>% 
  filter(model == "roundwise", threshold == "relative") %>% 
  group_by(psi, theta) %>% 
  summarise(mean_n = mean(n_sample), 
            median_n = median(n_sample)) %>% 
  ggplot(aes(x=psi, y=median_n, group=theta, color = as.factor(theta))) + 
  scale_color_scico_d(palette = "imola") + 
  labs(title = "Roundwise Comparison", 
       x = expression(paste("Switching Probability  ", psi)),
       y = "Average Sample Size",
       color = "Threshold") +
  geom_point(size = 3) + 
  geom_line(linewidth = 1) + 
  theme_apa(base_size = 20)

effort_summary + effort_roundwise + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/sample_sizes.png", width = 14, height = 6)

# reward rate 

reward_rates <- choices %>%  
  mutate(higher_EV_option = ifelse(ev_ratio > 1, "r", ifelse(ev_ratio < 1 , "s", NA)) ,
         higher_EV_exp_option = ifelse(mean_r > safe, "r", ifelse(mean_r < safe, "s", NA)) ,
         higher_EV_choice = choice == higher_EV_option , 
         higher_EV_exp_choice = choice == higher_EV_exp_option , 
         chosen_option_ev = ifelse(choice == "s", safe, ifelse(choice == "r", r_ev, NA)) ,
         chosen_option_ev_exp = ifelse(choice == "s", safe, ifelse(choice == "r", mean_r, NA)) , 
         rr_ev = chosen_option_ev/n_sample , 
         rr_ev_exp = chosen_option_ev_exp/n_sample)

# maximization performance based on number of samples

reward_rates %>% 
  group_by(model, psi, threshold, n_sample) %>% 
  summarise(max_ev_rate = mean(higher_EV_choice, na.rm = TRUE), 
            max_ev_exp_rate = mean(higher_EV_exp_choice, na.rm = TRUE)
  ) %>% 
  filter(model == "summary", threshold == "relative") %>% 
  ggplot(aes(x = n_sample, y = max_ev_rate, group = psi, color = psi)) + 
  geom_point() +
  geom_smooth(se = F) + 
  scale_x_continuous(limits = c(0,50)) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  theme_minimal()

reward_rates %>% 
  group_by(model, psi, threshold, n_sample) %>% 
  summarise(max_ev_rate = mean(higher_EV_choice, na.rm = TRUE), 
            max_ev_exp_rate = mean(higher_EV_exp_choice, na.rm = TRUE)
  ) %>% 
  filter(model == "roundwise", threshold == "relative") %>% 
  ggplot(aes(x = n_sample, y = max_ev_rate, group = psi, color = psi)) + 
  geom_point() +
  geom_smooth(se = F) + 
  scale_x_continuous(limits = c(0,50)) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  theme_minimal()

reward_rates %>% 
  group_by(model, psi, threshold, n_sample) %>% 
  summarise(max_ev_rate = mean(higher_EV_choice, na.rm = TRUE), 
            max_ev_exp_rate = mean(higher_EV_exp_choice, na.rm = TRUE)
  ) %>% 
  filter(model == "summary", threshold == "relative", n_sample < 50) %>% 
  ggplot(aes(x = n_sample, y = max_ev_rate)) + 
  geom_density_2d_filled() +
  facet_wrap(~psi, nrow = 2) + 
  scale_x_continuous(limits = c(0,50)) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  theme_minimal()

reward_rates %>% 
  group_by(model, psi, threshold, n_sample) %>% 
  summarise(max_ev_rate = mean(higher_EV_choice, na.rm = TRUE), 
            max_ev_exp_rate = mean(higher_EV_exp_choice, na.rm = TRUE)
  ) %>% 
  filter(model == "roundwise", threshold == "relative", n_sample < 50) %>% 
  ggplot(aes(x = n_sample, y = max_ev_rate)) + 
  geom_density_2d_filled() +
  facet_wrap(~psi, nrow = 2) + 
  scale_x_continuous(limits = c(0,50)) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  theme_minimal()


reward_rates %>% 
  group_by(model, psi, threshold, n_sample) %>% 
  summarise(max_ev_rate = mean(higher_EV_choice, na.rm = TRUE), 
            max_ev_exp_rate = mean(higher_EV_exp_choice, na.rm = TRUE)
  ) %>% 
  filter(model == "summary", threshold == "relative") %>% 
  ggplot(aes(x = n_sample, y = max_ev_exp_rate, group = psi, color = psi)) + 
  geom_point() +
  geom_smooth(se = F) + 
  scale_x_continuous(limits = c(0,50)) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  theme_minimal()

reward_rates %>% 
  group_by(model, psi, threshold, n_sample) %>% 
  summarise(max_ev_rate = mean(higher_EV_choice, na.rm = TRUE), 
            max_ev_exp_rate = mean(higher_EV_exp_choice, na.rm = TRUE)
  ) %>% 
  filter(model == "roundwise", threshold == "relative") %>% 
  ggplot(aes(x = n_sample, y = max_ev_exp_rate, group = psi, color = psi)) + 
  geom_point() +
  geom_smooth(se = F) + 
  scale_x_continuous(limits = c(0,50)) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  theme_minimal()


# reward rates (obtained option's ev/exp ev normalized by n_samples)
rr_roundwise <- reward_rates %>% 
  group_by(model, psi, threshold, theta) %>% 
  summarise(mean_rr_ev = mean(rr_ev, na.rm = TRUE), 
            mean_rr_ev_exp = mean(rr_ev_exp, na.rm = TRUE)
  ) %>% 
  pivot_longer(cols = c(mean_rr_ev, mean_rr_ev_exp), names_to = "metric", values_to = "values") %>% 
  filter(model == "roundwise", threshold == "relative") %>%  
  ggplot(aes(x = psi, y = values, color = metric)) + 
  geom_point(size = 3, alpha = .5) +
  geom_line(linewidth = 1, alpha = .5) + 
  facet_grid(~theta) + 
  labs(title = "Roundwise") + 
  theme_minimal()

rr_roundwise <- reward_rates %>% 
  group_by(model, psi, threshold, theta) %>% 
  summarise(mean_rr_ev = mean(rr_ev, na.rm = TRUE), 
            mean_rr_ev_exp = mean(rr_ev_exp, na.rm = TRUE)
  ) %>% 
  pivot_longer(cols = c(mean_rr_ev, mean_rr_ev_exp), names_to = "metric", values_to = "values") %>% 
  filter(model == "roundwise", threshold == "relative") %>%  
  ggplot(aes(x = psi, y = values, color = metric)) + 
  scale_color_discrete(labels=c('EV', 'Average Outcome')) + 
  geom_point(size = 3, alpha = .5) +
  geom_line(linewidth = 1, alpha = .5) + 
  facet_grid(~theta) + 
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)", 
       y = "Mean Reward Rate",
       color = "Accuracy Metric") + 
  theme_minimal()

rr_summary <- reward_rates %>% 
  group_by(model, psi, threshold, theta) %>% 
  summarise(mean_rr_ev = mean(rr_ev, na.rm = TRUE), 
            mean_rr_ev_exp = mean(rr_ev_exp, na.rm = TRUE)
  ) %>% 
  pivot_longer(cols = c(mean_rr_ev, mean_rr_ev_exp), names_to = "metric", values_to = "values") %>% 
  filter(model == "summary", threshold == "relative") %>%  
  ggplot(aes(x = psi, y = values, color = metric)) + 
  scale_color_discrete(labels=c('EV', 'Average Outcome')) + 
  geom_point(size = 3, alpha = .5) +
  geom_line(linewidth = 1, alpha = .5) + 
  facet_grid(~theta) + 
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)", 
       y = "Mean Reward Rate",
       color = "Accuracy Metric") + 
  theme_minimal()

rr_summary + rr_roundwise + plot_annotation(tag_levels = "A") + plot_layout(nrow = 2)
ggsave(file = "manuscript/figures/reward_rates.png", width = 14, height = 8)

# proportion correct/n_samples 
reward_rates %>% 
  group_by(model, psi, threshold, theta) %>% 
  summarise(norm_max_ev_rate = mean(higher_EV_choice/n_sample, na.rm = TRUE), 
            norm_max_ev_exp_rate = mean(higher_EV_exp_choice/n_sample, na.rm = TRUE)) %>% 
  pivot_longer(cols = c(norm_max_ev_rate, norm_max_ev_exp_rate), names_to = "metric", values_to = "values") %>% 
  filter(model == "summary", threshold == "relative") %>%  
  ggplot(aes(x = psi, y = values, color = metric)) + 
  geom_point(size = 3, alpha = .5) +
  geom_line(linewidth = 1, alpha = .5) + 
  facet_grid(~theta) + 
  theme_minimal()

reward_rates %>% 
  group_by(model, psi, threshold, theta) %>% 
  summarise(norm_max_ev_rate = mean(higher_EV_choice/n_sample, na.rm = TRUE), 
            norm_max_ev_exp_rate = mean(higher_EV_exp_choice/n_sample, na.rm = TRUE)) %>% 
  pivot_longer(cols = c(norm_max_ev_rate, norm_max_ev_exp_rate), names_to = "metric", values_to = "values") %>% 
  filter(model == "roundwise", threshold == "relative") %>%  
  ggplot(aes(x = psi, y = values, color = metric)) + 
  geom_point(size = 3, alpha = .5) +
  geom_line(linewidth = 1, alpha = .5) + 
  facet_grid(~theta) + 
  theme_minimal()


# sampled frequencies -----------------------------------------------------

# prepare data 
round_freq_high  <- round %>% 
  mutate(psi = 1-(psi+.5)) %>% # recode psi
  filter(psi %in% c((1-.9), .5, 1),
         threshold == "relative",
         theta == 5) %>% 
  group_by(psi, problem, agent) %>%
  mutate(n_sample = n(), # compute trial-level n 
         n_s = sum(is.na(r)), 
         n_r = n_sample - n_s, 
         trial_freq = round(sum(if_else(r == r_high, 1, 0), na.rm = TRUE)/n_r, 2)) %>% # compute trial-level frequencies
  ungroup() %>%
  group_by(psi, problem, agent, round) %>% 
  mutate(n_round = n(), 
         n_round_s = sum(is.na(r)),
         n_round_r = n_round - n_round_s,
         round_freq = round(sum(if_else(r == r_high, 1, 0), na.rm = TRUE)/n_round_r, 2)) %>% # compute round-level frequencies
  distinct(psi, threshold, theta, problem, agent, round, trial_freq, round_freq) # drop redundant rows

round_freq_low  <- round %>% 
  mutate(psi = 1-(psi+.5)) %>% # recode psi
  filter(psi %in% c((1-.9), .5, 1),
         threshold == "relative",
         theta == 5) %>% 
  group_by(psi, problem, agent) %>%
  mutate(n_sample = n(), # compute trial-level n 
         n_s = sum(is.na(r)), 
         n_r = n_sample - n_s, 
         trial_freq = round(sum(if_else(r == r_low, 1, 0), na.rm = TRUE)/n_r, 2)) %>% # compute trial-level frequencies
  ungroup() %>%
  group_by(psi, problem, agent, round) %>% 
  mutate(n_round = n(), 
         n_round_s = sum(is.na(r)),
         n_round_r = n_round - n_round_s,
         round_freq = round(sum(if_else(r == r_low, 1, 0), na.rm = TRUE)/n_round_r, 2)) %>% # compute round-level frequencies
  distinct(psi, threshold, theta, problem, agent, round, trial_freq, round_freq) # drop redundant rows

round_freq <- bind_rows(round_freq_low, round_freq_high)

round_freq_median <- round_freq %>% 
  group_by(psi, trial_freq) %>%
  summarise(median_round_freq = median(round_freq, na.rm = TRUE)) # compute median round-level frequencies for each parameter combination and trial-level frequency


# plot data
round_freq %>% 
  ggplot(aes(x = trial_freq, y = round_freq)) +
  facet_wrap(~psi, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed))) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, .5)) + 
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, .5)) + 
  labs(x = "Trial-Level Frequency",
       y = "Round-Level Frequency") +
  geom_density_2d_filled() +
  scale_fill_scico_d(palette = "devon") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "white") + 
  geom_point(data = round_freq_median, aes(y = median_round_freq), size = 1, color = "white") + 
  theme_apa(base_size = 20) + 
  theme(legend.position = "none")

# save plot
ggsave(file = "manuscript/figures/undersampling.png", width = 14, height = 5)


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

# compute proportions of choice problems, where safe option is larger than sampled mean of risky option
rates_obj <- choices %>%
  filter(!c(n_s == 0 | n_r == 0)) %>%
  mutate(sampled_ev_ratio = ifelse(safe > mean_r, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, sampled_ev_ratio) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>% 
  filter(!(sampled_ev_ratio == 0))
rates_obj_sr <- rates_obj %>% filter(model == "summary", threshold == "relative" ) 
rates_obj_rr <- rates_obj %>% filter(model == "roundwise", threshold == "relative" ) 

r_averse_summary <- rates %>%  
  filter(model == "summary" & threshold == "relative") %>% 
  #filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
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

r_averse_summary + r_averse_roundwise + plot_annotation(tag_levels = "A")
ggsave(file = "manuscript/figures/rates_risk_aversion.png", width = 14, height = 6)


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

# plot data

## summary 

cpt_summary <- cpt %>% filter(model == "summary")
weights_summary <- weights %>% filter(model == "summary")


#### choice proportion as function of p_high

choices %>% 
  filter(model == "summary", threshold == "relative") %>% 
  ggplot(aes(x=ep_r_high)) + 
  geom_histogram(fill = "gray", color = "black", bins = 30) +
  facet_grid(psi~theta) +
  theme_minimal()

choices %>% group_by(model, psi, threshold, theta, ep_r_high, choice) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  filter(model == "summary", threshold == "relative", choice == "r") %>% 
  # filter(ep_r_high != 0, ep_r_high != 1) %>% 
  mutate(bias = if_else(ep_r_high == 1, "p=0 or p=1", if_else(ep_r_high == 0, "p=0 or p=1", "0 < p < 1"))) %>% 
  ggplot(aes(x=ep_r_high, y = prop)) +
  geom_point(aes(color = bias, alpha = bias), size = 3) +
  scale_color_manual(values = c("gray", "black")) + 
  scale_alpha_manual(values = c(.5, 1)) + 
  # geom_smooth(color = "black", se = FALSE) + 
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
ggsave("manuscript/figures/risky_choice_rates_summary.png", width = 14, height = 14)


#### gamma
gamma_summary <- cpt_summary %>%
  filter(parameter == "gamma", threshold == "relative") %>%
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
  filter(parameter == "delta", threshold == "relative") %>%
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
  filter(threshold == "relative") %>% 
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
  filter(parameter == "gamma", threshold == "relative") %>%
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
  filter(parameter == "delta", threshold == "relative") %>%
  ggplot(aes(psi, mean, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) +
  scale_y_continuous(limits = c(-0.1, 10.1), breaks = seq(0,10, length.out = 3)) +
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
  labs(x = TeX("$\\p_{high}$"),
       y = TeX("$w(\\p_{high})$"), 
       color = "Switching\nProbability") +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

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


