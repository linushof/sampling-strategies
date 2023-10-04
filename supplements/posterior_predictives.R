# load packages
pacman::p_load(tidyverse , 
               R2jags ,
               digest , 
               viridis , 
               ggbeeswarm, 
               scico, 
               patchwork, 
               ggpubr, 
               papaja, 
               latex2exp)


# load data
cpt <- read_rds("data/cpt_estimates.rds")
choices <- read_rds("data/choice_data.rds.bz2")


cpt %>% filter(parameter == "deviance")
# largest deviances for summary model with low switching probabilities 

cpt_clean <- cpt %>%  
  select(model:mean) %>%
  filter(parameter != "deviance") %>% 
  pivot_wider(names_from = parameter, values_from = mean) %>% 
  mutate(across(alpha:rho, ~round(., 2)))

ppset <- choices %>%
  left_join(cpt_clean, by = c("model", "psi", "threshold", "theta")) %>% 
  filter(!c(n_s == 0 | n_r == 0)) %>% 
  mutate(choice_obs = if_else(choice == "s", 1,0))

## generate posterior predictions 

set.seed(114981)
postpred <- ppset %>% mutate(
  w_high = round( (delta * ep_r_high^gamma) / ( (delta*ep_r_high^gamma)+(1-ep_r_high)^gamma ), 2) , 
  w_low = 1 - w_high , 
  v_high = r_high^alpha , 
  v_low = r_low^alpha , 
  v_safe = safe^alpha , 
  V_safe = v_safe , 
  V_risky = (w_high * v_high) + (w_low * v_low) ,
  V_safe_scaled = V_safe^(1/alpha) , 
  V_risky_scaled = V_risky^(1/alpha) ,
  V_diff = V_safe_scaled-V_risky_scaled , 
  p_safe_risky = round(1 / ( 1 + exp(-rho*V_diff) ),2) ,
  choice_pp = rbinom(n=nrow(ppset), size=1, prob=p_safe_risky))
write_rds(postpred, "supplements/posterior_predictives.rds.bz2", compress = "bz2")

## results

postpred <- read_rds("supplements/posterior_predictives.rds.bz2")

### prepare data 

#### compute maximization rates (qualitative pattern)

##### sampling strategies
obs_ev_exp_max <- postpred %>%
  mutate(norm = case_when(mean_r/safe > 1 ~ 0, 
                          mean_r/safe < 1 ~ 1)) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice_obs, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0)) %>% 
  mutate(type = "Sampling Strategy")

##### CPT posterior predictive
pp_ev_exp_max <- postpred %>%
  mutate(norm = case_when(mean_r/safe > 1 ~ 0, 
                          mean_r/safe < 1 ~ 1)) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice_pp, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0)) %>% 
  mutate(type = "CPT Posterior Predictive")

##### compute proportion of correctly predicted choices on trial level
postpred_acc <- postpred %>% 
  select(model, psi, threshold, theta, alpha, gamma, delta, rho, problem, agent, choice_obs, choice_pp ) %>% 
  mutate(match = if_else(choice_obs == choice_pp, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, alpha, gamma, delta, rho, match) %>% 
  summarise(count = n()) %>% 
  mutate(perc = round(count/sum(count), 3)) %>% 
  filter(match != 0) %>% 
  ungroup()

### plot data 

label_theta <- function(string) {
  TeX(paste("$\\theta=$", string, sep = ""))
}

#### summary comparison

pp_ev_exp_max_sr <- pp_ev_exp_max %>% filter(model == "summary", threshold == "relative") 
pp_max_summary <- obs_ev_exp_max %>%
  filter(model == "summary", threshold == "relative") %>% 
  ggplot(aes(psi, rate, shape = type, group = theta)) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Switching Probability\n(Search Rule)",
       y = "Proportion of\n Maximizing Choices",
       shape = "") +
  facet_wrap(~theta, nrow = 1, labeller =  labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_point(size = 2) +
  geom_point(data = pp_ev_exp_max_sr, size = 3) +
  scale_shape_manual(values = c(4, 19)) + 
  scale_color_scico(palette = "imola") +
  theme_minimal() + 
  theme_apa(base_size = 20)

pp_acc_summary <- postpred_acc %>%  
  filter(model == "summary", threshold == "relative") %>% 
  ggplot(aes(x = psi, y = perc)) + 
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_hline(yintercept = .7, linetype = "dashed") + 
  scale_y_continuous(limits = c(.5,1), breaks = seq(.5,1,.1)) + 
  geom_point(size = 3, shape = 4) + 
  labs(x = "Switching Probability\n(Search Rule)" , 
       y = "Proportion of\nCorrect Predictions", 
       color = "Choice Consistency") + 
  scale_color_viridis(option = "C") + 
  theme_apa(base_size = 20)

ggarrange(pp_max_summary, pp_acc_summary, nrow = 2, common.legend = T, legend = "right", labels = "AUTO")
ggsave(file = "manuscript/figures/posterior_predictive_summary.png", width = 14, height = 7)

#### roundwise comparison

#### summary comparison

pp_ev_exp_max_rr <- pp_ev_exp_max %>% filter(model == "roundwise", threshold == "relative") 
pp_max_roundwise <- obs_ev_exp_max %>%
  filter(model == "roundwise", threshold == "relative") %>% 
  ggplot(aes(psi, rate, shape = type, group = theta)) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Switching Probability\n(Search Rule)",
       y = "Proportion of\n Maximizing Choices",
       shape = "") +
  facet_wrap(~theta, nrow = 1, labeller =  labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_point(size = 2) +
  geom_point(data = pp_ev_exp_max_rr, size = 3) +
  scale_shape_manual(values = c(4, 19)) + 
  scale_color_scico(palette = "imola") +
  theme_minimal() + 
  theme_apa(base_size = 20)

pp_acc_roundwise <- postpred_acc %>%  
  filter(model == "roundwise", threshold == "relative") %>% 
  ggplot(aes(x = psi, y = perc)) + 
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_hline(yintercept = .7, linetype = "dashed") + 
  scale_y_continuous(limits = c(.5,1), breaks = seq(.5,1,.1)) + 
  geom_point(size = 3, shape = 4) + 
  labs(x = "Switching Probability\n(Search Rule)" , 
       y = "Proportion of\nCorrect Predictions", 
       color = "Choice Consistency") + 
  scale_color_viridis(option = "C") + 
  theme_apa(base_size = 20)

ggarrange(pp_max_roundwise, pp_acc_roundwise, nrow = 2, common.legend = T, legend = "right", labels = "AUTO")
ggsave(file = "manuscript/figures/posterior_predictive_roundwise.png", width = 14, height = 7)

# the lower psi and the higher the elevation, the more risky choices are predicted
# i.e., with low switching probabilities, small but systematic CPT bias towards the risky option 

## difference in CPT valuations

postpred %>% 
  filter(model == "summary", threshold == "relative") %>%  
  group_by(psi, theta) %>% 
  summarise(m_V_diff = mean(V_diff, na.rm = T)) %>% 
  ggplot(aes(x=psi, y = m_V_diff, color = theta, group = theta)) +
  geom_point(size = 3) + 
  geom_line(linewidth = 1) +
  scale_color_viridis() + 
  theme_minimal() + 
  scale_y_continuous(limits = c(-2,2))

## probability of choosing the safe option

postpred %>% 
  filter(model == "summary", threshold == "relative") %>%  
  group_by(psi, theta) %>% 
  summarise(m_p_safe = mean(p_safe_risky, na.rm = T)) %>% 
  ggplot(aes(x=psi, y = m_p_safe, color = theta, group = theta)) +
  geom_point(size = 3) + 
  geom_line(linewidth = 1) +
  scale_color_viridis() + 
  theme_minimal()

## proportion of risky choices

postpred %>% 
  filter(model == "summary", threshold == "relative") %>%  
  group_by(psi, theta, choice_pp) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n/sum(n), 2)) %>% 
  filter(choice_pp == 0) %>% 
  ggplot(aes(x=psi, y = prop, color = theta, group = theta)) +
  geom_point(size = 3, alpha = .5) + 
  geom_line(linewidth = 1, alpha = .5) +
  scale_color_viridis() + 
  theme_minimal() + 
  scale_y_continuous(limits = c(.3,.8))

postpred %>% 
  filter(model == "summary", threshold == "relative") %>%  
  group_by(psi, theta, choice_obs) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n/sum(n), 2)) %>% 
  filter(choice_obs == 0) %>% 
  ggplot(aes(x=psi, y = prop, color = theta, group = theta)) +
  geom_point(size = 3, alpha = .5) + 
  geom_line(linewidth = 1, alpha = .5) +
  scale_color_viridis() + 
  theme_minimal() + 
  scale_y_continuous(limits = c(.3,.8))


# test --------------------------------------------------------------------

# for low psi, bias towards safe option is larger

test <- choices %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)

p1 <- test %>% 
  ggplot(aes(x=psi, y=prop, group = norm, color = norm)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line() +
  scale_y_continuous(limits = c(.5,1))


test2 <- postpred %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(choice_pp_2 = ifelse(choice_pp == 0, "r", "s"), 
         norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice_pp_2 == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)


p2 <- test2 %>% 
  ggplot(aes(x=psi, y=prop, group = norm, color = norm)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(limits = c(.5,1))


p1 + p2 + plot_layout(nrow = 2)


test3 <- choices %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)

p3 <- test3 %>% 
  ggplot(aes(x=psi, y=prop)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line()

test4 <- postpred %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(choice_pp_2 = ifelse(choice_pp == 0, "r", "s"), 
         norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice_pp_2 == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)

p4 <- test4 %>% 
  ggplot(aes(x=psi, y=prop)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line()

p3 + p4 + plot_layout(nrow = 2)




test3 <- choices %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1) %>% 
  mutate(norm = "Free")

test5 <- choices %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)

p5 <- test5 %>% 
  ggplot(aes(x=psi, y=prop, group = norm, color = norm)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line() + 
  geom_line(data = test3) + 
  geom_point(data = test3) + 
  labs(title = "Sampling Strategy Maximization") + 
  theme_minimal()

p5


test4 <- postpred %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(choice_pp_2 = ifelse(choice_pp == 0, "r", "s"), 
         norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice_pp_2 == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1) %>% 
  mutate(norm = "Free")


test6 <- postpred %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(choice_pp_2 = ifelse(choice_pp == 0, "r", "s"), 
         norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice_pp_2 == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)


p6 <- test6 %>% 
  ggplot(aes(x=psi, y=prop, group = norm, color = norm)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line() + 
  geom_line(data = test4) + 
  geom_point(data = test4) + 
  labs(title = "Posterior Predictive Maximization") +
  theme_minimal()

p5 + p6 + plot_layout(nrow = 2)



cpt %>% filter(parameter == "deviance") %>% 
  group_by(model, psi, threshold, theta) %>% 
  summarize(mdev = mean) %>% 
  View()






test <- choices %>% 
  filter(model == "roundwise", threshold == "relative") %>% 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)

p1 <- test %>% 
  ggplot(aes(x=psi, y=prop, group = norm, color = norm)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(limits = c(.5,1))


test2 <- postpred %>% 
  filter(model == "roundwise", threshold == "relative") %>% 
  mutate(choice_pp_2 = ifelse(choice_pp == 0, "r", "s"), 
         norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice_pp_2 == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)


p2 <- test2 %>% 
  ggplot(aes(x=psi, y=prop, group = norm, color = norm)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(limits = c(.5,1))

p1 + p2 + plot_layout(nrow = 2)