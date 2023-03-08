# load packages
pacman::p_load(tidyverse, 
               scico, 
               papaja, 
               ggpubr, 
               latex2exp)

# load data
round <- read_rds("data/simulation_roundwise.rds.bz2") 
summary <- read_rds("data/simulation_summary.rds.bz2")

# data wrangling 
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

# plots

problem_number <- 43
round_boundary <- 3
summary_boundary <- 45
# switching probability psi
label_psi <- function(string) {
  TeX(paste("$\\psi=$", string, sep = ""))
}

## central tendencies

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
  facet_wrap(~psi, nrow = 3, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed))) + 
  scale_color_scico_d(palette = "lajolla", begin = .3, end = .7) + 
  scale_x_continuous(limits = c(0,50), breaks = seq(0, 50, 25)) +
  scale_y_continuous(limits = c(-round_boundary, round_boundary), breaks = seq(-round_boundary,round_boundary, round_boundary)) +
  labs(x = "Sampled Outcomes",
       y = expression(paste(Delta, " Wins")),
       color = expression(psi),
       alpha = "") +
  geom_line(position = position_dodge(width = .3), size = .1, color = "gray", alpha = .3) + 
  geom_line(data = round_median, aes(y = median, alpha = count), size = 1) + 
  geom_hline(yintercept = c(-round_boundary, round_boundary), linetype = "dashed") + 
  theme_apa() + 
  guides(alpha = "none")  + 
  annotate("text", x = 48, y= 2.7, label = "Risky", size = 2.5) + 
  annotate("text", x = 48, y= -2.7, label = "Safe", size = 2.5)

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

summary_trajectories_median <- summary %>% 
  filter(psi %in% c((1-.9), .5, 1),
         theta == summary_boundary, 
         problem == problem_number) %>% 
  mutate(diff = case_when(diff < -summary_boundary ~ -summary_boundary, 
                          diff > summary_boundary ~ summary_boundary, 
                          diff >= -summary_boundary & diff <= summary_boundary ~ diff)) %>% 
  ggplot(aes(x = smp_no, y = diff, group_by = agent, color = as.factor(psi))) + 
  facet_wrap(~psi, nrow = 3, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed))) + 
  scale_color_scico_d(palette = "lajolla", begin = .3, end = .7) + 
  scale_x_continuous(limits = c(0,50), breaks = seq(0, 50, 25)) +
  scale_y_continuous(limits = c(-summary_boundary, summary_boundary), breaks = seq(-summary_boundary, summary_boundary, summary_boundary)) +
  labs(x = "Sampled Outcomes",
       y = expression(paste(Delta, " Sum")),
       color = expression(psi),
       alpha = "") +
  geom_line(position = position_dodge(width = .3), size = .1, color = "gray", alpha = .3) + 
  geom_line(data = summary_median, aes(y = median, alpha = count), size = 1) + 
  geom_hline(yintercept = c(-summary_boundary, summary_boundary), linetype = "dashed") + 
  theme_apa() + 
  guides(alpha = "none") + 
  annotate("text", x = 48, y= 40, label = "Risky", size = 2.5) + 
  annotate("text", x = 48, y= -40, label = "Safe", size = 2.5)

ggarrange(round_trajectories_median, summary_trajectories_median, ncol = 2, nrow = 1, common.legend = TRUE, legend = "right", labels = "AUTO")





  
  
