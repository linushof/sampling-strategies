library(pacman)
p_load(tidyverse, scico, patchwork)

#data 
data_summary <- read_rds("rerun/data/rerun_lite_results_summary.rds")
data_roundwise <- read_rds("rerun/data/rerun_lite_results_roundwise.rds")


View(problems)

# maximization ------------------------------------------------------------

# summary 

## aggregate 
max_summary <- data_summary %>% 
  mutate(maxprop = if_else(better_ev == "safe", prop, 1-prop)) %>% 
  group_by(theta, psi) %>% 
  summarise(rate = mean(maxprop)) %>% 
  mutate(theta = as.double(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% EV Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()

## rare event conditions 
max_summary_rare <- data_summary %>% 
  mutate(maxprop = if_else(better_ev == "safe", prop, 1-prop)) %>% 
  group_by(theta, psi, rare) %>% 
  summarise(rate = mean(maxprop)) %>% 
  mutate(theta = as.double(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_wrap(~rare, nrow = 3) + 
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  #scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% EV Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()


# roundwise

## aggregate
max_roundwise <- data_roundwise %>% 
  mutate(maxprop = if_else(better_ev == "safe", prop, 1-prop)) %>% 
  group_by(theta, psi) %>% 
  summarise(rate = mean(maxprop)) %>% 
  mutate(theta = as.factor(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico_d(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% EV Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()


## rare event conditions
max_roundwise_rare <- data_roundwise %>% 
  mutate(maxprop = if_else(better_ev == "safe", prop, 1-prop)) %>% 
  group_by(theta, psi, rare) %>% 
  summarise(rate = mean(maxprop)) %>% 
  mutate(theta = as.factor(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_wrap(~rare, nrow = 3) + 
  scale_color_scico_d(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  #scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% EV Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()



max_EV <- max_summary + max_roundwise
max_EV_rare <- max_summary_rare + max_roundwise_rare



# risk aversion -----------------------------------------------------------

# summary

## aggregate
risk_summary <- data_summary %>% 
  group_by(theta, psi) %>% 
  summarise(rate = mean(prop)) %>% 
  mutate(theta = as.double(theta) , 
         psi = as.double(psi)) %>% 
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
  theme_minimal()

## rare event conditions 
risk_summary_rare <- data_summary %>% 
  group_by(theta, psi, rare) %>% 
  summarise(rate = mean(prop)) %>% 
  mutate(theta = as.double(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_wrap(~rare, nrow = 3) + 
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% Safe Choices",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()

# roundwise

## aggregate
risk_roundwise <- data_roundwise %>% 
  group_by(theta, psi) %>% 
  summarise(rate = mean(prop)) %>% 
  mutate(theta = as.factor(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico_d(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% Safe Choices",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()

## aggregate
risk_roundwise_rare <- data_roundwise %>% 
  group_by(theta, psi, rare) %>% 
  summarise(rate = mean(prop)) %>% 
  mutate(theta = as.factor(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_wrap(~rare, nrow=3) + 
  scale_color_scico_d(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% Safe Choices",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()


risk <- risk_summary + risk_roundwise
risk_rare <- risk_summary_rare + risk_roundwise_rare


# problem level -----------------------------------------------------------

 
data_summary %>%
  mutate(theta = as.factor(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(psi, prop, group = theta, color = theta)) +
  facet_wrap(~problem, nrow=6) + 
  scale_color_scico_d(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% Safe Choices",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()




