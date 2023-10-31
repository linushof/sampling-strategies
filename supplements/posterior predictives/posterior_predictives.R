# load packages
pacman::p_load(tidyverse , 
               viridis , 
               scico, 
               patchwork, 
               ggpubr, 
               papaja, 
               latex2exp)

# load data
cpt <- read_rds("data/cpt_estimates.rds")
choices <- read_rds("data/choice_data.rds.bz2")
# postpred <- read_rds("supplements/posterior predictives/posterior_predictives.rds.bz2")

label_theta <- function(string) {
  TeX(paste("$\\theta=$", string, sep = ""))
}

label_psi <- function(string) {
  TeX(paste("$\\psi=$", string, sep = ""))
}

# posterior predictions ---------------------------------------------------

cpt_clean <- cpt %>%  
  select(model:mean) %>%
  filter(parameter != "deviance") %>% 
  pivot_wider(names_from = parameter, values_from = mean) %>% 
  mutate(across(alpha:rho, ~round(., 2)))

ppset <- choices %>%
  left_join(cpt_clean, by = c("model", "psi", "threshold", "theta")) %>% 
  filter(!c(n_s == 0 | n_r == 0)) %>% 
  mutate(choice_obs = if_else(choice == "s", 1,0))

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

# analyses ----------------------------------------------------------------

# prepare data 

## DICs 
dic <- cpt %>% 
  filter(parameter == "deviance") %>% 
  select(model:sd) %>% 
  mutate(var = sd^2 ,
         pD = var/2 , 
         DIC = round(pD + mean, 1)
         )

## risky choice proportions 

### summary comparison

#### observed
riskprop_smp_sr <- choices %>% 
  mutate(choice_cmp = ifelse(choice == "r", 0, 1)) %>% 
  group_by(model, psi, threshold, theta, ep_r_high, choice_cmp) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  filter(model == "summary", threshold == "relative", choice_cmp == 0) %>% 
  mutate(bias = if_else(ep_r_high == 1, "p=0 or p=1", if_else(ep_r_high == 0, "p=0 or p=1", "0 < p < 1")), 
         type = "Sampling Strategy") 

#### CPT posterior predictives
riskprop_pp_sr <- postpred %>% 
  mutate(choice_cmp = choice_pp) %>% 
  group_by(model, psi, threshold, theta, ep_r_high, choice_cmp) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n), 2)) %>% 
  filter(model == "summary", threshold == "relative", choice_cmp == 0) %>% 
  mutate(bias = if_else(ep_r_high == 1, "p=0 or p=1", if_else(ep_r_high == 0, "p=0 or p=1", "0 < p < 1")), 
         type = "Posterior Predictive")

#### merge data
riskprop_sr <- bind_rows(riskprop_smp_sr, riskprop_pp_sr)

### roundwise comparison

#### observed
riskprop_smp_rr <- choices %>% 
  mutate(choice_cmp = ifelse(choice == "r", 0, 1)) %>% 
  group_by(model, psi, threshold, theta, ep_r_high, choice_cmp) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  filter(model == "roundwise", threshold == "relative", choice_cmp == 0) %>% 
  mutate(bias = if_else(ep_r_high == 1, "p=0 or p=1", if_else(ep_r_high == 0, "p=0 or p=1", "0 < p < 1")), 
         type = "Sampling Strategy") 

#### posterior predictives
riskprop_pp_rr <- postpred %>% 
  mutate(choice_cmp = choice_pp) %>% 
  group_by(model, psi, threshold, theta, ep_r_high, choice_cmp) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n), 2)) %>% 
  filter(model == "roundwise", threshold == "relative", choice_cmp == 0) %>% 
  mutate(bias = if_else(ep_r_high == 1, "p=0 or p=1", if_else(ep_r_high == 0, "p=0 or p=1", "0 < p < 1")), 
         type = "Posterior Predictive")

#### merge data
riskprop_rr <- bind_rows(riskprop_smp_rr, riskprop_pp_rr)

## maximization rates, conditional on better average option (*_max_1)

### summary comparison

#### observed
obs_max_1_sr <- choices %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  mutate(norm = ifelse(norm == "r", "Risky Option", "Safe Option")) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1) %>% 
  mutate(type = "Sampling Strategy")

#### posterior predictive
pp_max_1_sr <- postpred %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s") , 
         choice_pp = ifelse(choice_pp == 0, "r", "s"), 
         norm_choice = ifelse(choice_pp == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  mutate(norm = ifelse(norm == "r", "Risky Option", "Safe Option")) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1) %>% 
  mutate(type = "CPT Posterior Predictive")

#### merge data
max_1_sr <- bind_rows(obs_max_1_sr, pp_max_1_sr)

### roundwise comparison

#### observed
obs_max_1_rr <- choices %>% 
  filter(model == "roundwise", threshold == "relative") %>% 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  mutate(norm = ifelse(norm == "r", "Risky Option", "Safe Option")) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1) %>% 
  mutate(type = "Sampling Strategy")

#### posterior predictive
pp_max_1_rr <- postpred %>% 
  filter(model == "roundwise", threshold == "relative") %>% 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s") ,
         choice_pp = ifelse(choice_pp == 0, "r", "s") , 
         norm_choice = ifelse(choice_pp == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  mutate(norm = ifelse(norm == "r", "Risky Option", "Safe Option")) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1) %>% 
  mutate(type = "CPT Posterior Predictive")

#### merge
max_1_rr <- bind_rows(obs_max_1_rr, pp_max_1_rr)

## proportion of correctly predicted choices on trial level
pp_acc <- postpred %>% 
  select(model, psi, threshold, theta, alpha, gamma, delta, rho, problem, agent, choice_obs, choice_pp) %>% 
  mutate(match = if_else(choice_obs == choice_pp, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, alpha, gamma, delta, rho, match) %>% 
  summarise(count = n()) %>% 
  mutate(perc = round(count/sum(count), 3)) %>% 
  filter(match != 0) %>% 
  ungroup()

# plot data 

## risky choice proportions (ppc 1)

### summary comparison

dic_sr <- dic %>% filter(model == "summary", threshold == "relative")

riskprop_sr %>% 
  ggplot(aes(x=ep_r_high, y = prop)) +
  geom_point(aes(shape = type, color = bias, alpha = bias), size = 3) +
  scale_shape_manual(values = c(4, 16)) + 
  scale_color_manual(values = c("gray", "black")) + 
  scale_alpha_manual(values = c(.5, 1)) + 
  # geom_smooth(color = "black", se = FALSE) + 
  # geom_hline(yintercept = .5) +
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
  geom_text(data = dic_sr, aes(label=paste("DIC=", as.character(DIC)), x = .7, y = -.1)) 
ggsave("supplements/posterior predictives/figures/ppc_summary_1.png", width = 14, height = 16)

### roundwise comparison

dic_rr <- dic %>% filter(model == "roundwise", threshold == "relative")
riskprop_rr %>% 
  ggplot(aes(x=ep_r_high, y = prop)) +
  geom_point(aes(shape = type, color = bias, alpha = bias), size = 3) +
  scale_shape_manual(values = c(4, 16)) + 
  scale_color_manual(values = c("gray", "black")) + 
  scale_alpha_manual(values = c(.5, 1)) + 
  # geom_smooth(color = "black", se = FALSE) + 
  # geom_hline(yintercept = .5) +
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
  geom_text(data = dic_rr, aes(label=paste("DIC=", as.character(DIC)), x = .7, y = -.1)) 
ggsave("supplements/posterior predictives/figures/ppc_roundwise_1.png", width = 14, height = 16)

## maximization rates and trial-level accuracy (ppc 2)

### summary comparison

max_1_summary <- max_1_sr %>% 
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
  filter(model == "summary", threshold == "relative") %>% 
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

max_1_summary + pp_acc_summary + 
  plot_layout(nrow = 2, guides = "collect") + 
  plot_annotation(tag_levels = "A") & 
  theme(legend.position='top')
ggsave(file = "supplements/posterior predictives/figures/ppc_summary_2.png", width = 14, height = 10)

### roundwise comparison

max_1_roundwise <- max_1_rr %>% 
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
  filter(model == "roundwise", threshold == "relative") %>% 
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

max_1_roundwise + pp_acc_roundwise + 
  plot_layout(nrow = 2, guides = "collect") + 
  plot_annotation(tag_levels = "A") & 
  theme(legend.position='top')
ggsave(file = "supplements/posterior predictives/figures/ppc_roundwise_2.png", width = 14, height = 10)

# additional analyses -----------------------------------------------------

# maximization rates, unconditional on better average option (*_max_2)

## prepare data 

### observed
obs_max_2 <- choices %>%
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", 
                          mean_r/safe < 1 ~ "s")) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(norm_choice = ifelse(norm == choice, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, norm_choice) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(norm_choice == 0)) %>% 
  mutate(type = "Sampling Strategy")

### posterior predictives
pp_max_2 <- postpred %>%
  mutate(norm = case_when(mean_r/safe > 1 ~ 0, 
                          mean_r/safe < 1 ~ 1)) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(norm_choice = ifelse(norm == choice_pp, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, norm_choice) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(norm_choice == 0)) %>% 
  mutate(type = "CPT Posterior Predictive")

## plot data 

### summary
pp_max_2_sr <- pp_max_2 %>% filter(model == "summary", threshold == "relative") 
max_2_summary <- obs_max_2 %>%
  filter(model == "summary", threshold == "relative") %>% 
  ggplot(aes(psi, rate, shape = type, group = theta)) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(.4, 1.1), breaks = seq(.5, 1, length.out = 3)) +
  labs(x = "Switching Probability (Search Rule)",
       y = "Proportion of\n Maximizing Choices",
       shape = "") +
  facet_wrap(~theta, nrow = 1, labeller =  labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_point(size = 2) +
  geom_point(data = pp_max_2_sr, size = 3) +
  scale_shape_manual(values = c(4, 19)) + 
  scale_color_scico(palette = "imola") +
  theme_minimal() + 
  theme_apa(base_size = 20)

### roundwise
pp_max_2_rr <- pp_max_2 %>% filter(model == "roundwise", threshold == "relative") 
max_2_roundwise <- obs_max_2 %>%
  filter(model == "roundwise", threshold == "relative") %>% 
  ggplot(aes(psi, rate, shape = type, group = theta)) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(.4, 1.1), breaks = seq(.5, 1, length.out = 3)) +
  labs(x = "Switching Probability (Search Rule)",
       y = "Proportion of\n Maximizing Choices",
       shape = "") +
  facet_wrap(~theta, nrow = 1, labeller =  labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_point(size = 2) +
  geom_point(data = pp_max_2_rr, size = 3) +
  scale_shape_manual(values = c(4, 19)) + 
  scale_color_scico(palette = "imola") +
  theme_minimal() + 
  theme_apa(base_size = 20)
