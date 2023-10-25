# packages
pacman::p_load(tidyverse, scico, ggbeeswarm)

# data

## pre-processing
choice_sr_ext <- read_rds("supplements/starting option bias/choices_sr_ext.rds.bz2") 

'summary <- read_rds("data/simulation_summary.rds.bz2") 
choice_sr_ext <- summary %>%
  group_by(psi, threshold, theta, problem, agent) %>%
  mutate(sample = row_number(), 
         start = ifelse(sample == 1 & attended == "r", "r", ifelse(sample == 1 & attended == "s", "s", NA)), 
         start_o = ifelse(is.na(start), first(start), start),
         n_sample = n(), # total number of single samples
         n_s = sum(is.na(r)), # number of single samples drawn from safe option
         n_r = n_sample - n_s, # number of single samples drawn from risky option
         ep_r_high = round(sum(if_else(r == r_high, 1, 0), na.rm = TRUE)/n_r, 2), # experienced probability of higher risky outcome
         ep_r_low = round(1 - ep_r_high, 2), # experienced probability of lower risky outcome
         mean_r = round(mean(r, na.rm = TRUE), 2)) %>% # sampled mean risky prospect
  ungroup() %>%
  filter(!is.na(choice), threshold == "relative") %>% # discard single samples
  mutate(psi = 1-(psi+.5)) %>% # to interpret psi as switching probability
  select(psi:problem, rare, p_r_low:ev_ratio, agent, n_sample, sample, start_o, n_s, n_r, ep_r_low, ep_r_high, mean_r, r_sum, s_sum, diff, choice)

write_rds(choice_sr_ext, "supplements/starting option bias/choices_sr_ext.rds.bz2", compress = "bz2")'

# labels 

label_theta <- function(string) {
  TeX(paste("$\\theta=$", string, sep = ""))
}

label_psi <- function(string) {
  TeX(paste("$\\psi=$", string, sep = ""))
}

# starting bias sample size 

choice_sr_ext <- choice_sr_ext %>% 
  mutate(n_starting = ifelse(start_o == "r", n_r, n_s), 
         n_second = ifelse(start_o == "r", n_s, n_r ), 
         n_start_sec_diff = n_starting - n_second, 
         start_advantage = if_else(n_start_sec_diff > 0, "Bias", "No Bias"))

choice_sr_ext %>%
  ggplot(aes(x=n_start_sec_diff, fill = n_start_sec_diff > 0)) + 
  geom_histogram(center = 0, binwidth = 1) + 
  scale_fill_scico_d(palette = "lisbon", begin = .2, end = .8) + 
  facet_grid(psi~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed), 
                                            psi = as_labeller(label_psi, default = label_parsed))) +
  scale_y_continuous(trans = "log10") + 
  labs(x = TeX("Difference in Sample Size ($\\N_{Start \\, Option} - \\N_{Second \\,  Option} $)"), 
       y =  expression(log[10](Frequency))) +
  theme_minimal(base_size = 20) + 
  theme(legend.position =  "none") 
ggsave("supplements/starting option bias/figures/bias_sample_sizes.png", width = 14, height = 14)


# starting bias choice

choice_sr_ext %>% 
  mutate(bias_choice = if_else(start_o == choice, TRUE, FALSE)) %>% 
  group_by(psi, theta, bias_choice) %>%  
  summarize(count = n()) %>% 
  mutate(prop = count/sum(count)) %>% 
  filter(bias_choice == TRUE) %>% 
  ggplot(aes(x=psi, y=prop, color = theta, group = theta)) + 
  geom_point(alpha = .7, size = 3) +
  geom_line(linewidth = 1) + 
  scale_color_scico(palette = "imola") + 
  scale_y_continuous(limits = c(.4,1.1), breaks = c(.5, .75, 1)) + 
  scale_x_continuous(breaks = seq(0,1,.5)) + 
  labs(x = "Switching Probability (Search Rule)",
       y = "Proportion of Starting Option Choices",
       color = "Threshold\n(Stopping Rule)") +
  theme_minimal(base_size = 14)

ggsave("supplements/starting option bias/figures/bias_choices.png", width = 8, height = 6)


# starting bias sampled probabilities 

custom_labels <- c("r" = "Starting Option: Risky", "s" = "Starting Option: Safe")

choice_sr_ext %>% 
  mutate(extreme = if_else((ep_r_high == 1 | ep_r_high == 0), TRUE, FALSE)) %>% 
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
ggsave("supplements/starting option bias/figures/bias_sampled_probabilities.png", width = 14, height = 6)

  
# starting bias risky choice proportions

choice_sr_ext %>% group_by(psi, threshold, theta, ep_r_high, choice) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  filter(choice == "r") %>% 
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
ggsave("supplements/starting option bias/figures/bias_risky_choice_proportions.png", width = 14, height = 14)

