# prepare data 
risk_rates <- choices %>% 
  filter(!c(n_s == 0 | n_r == 0)) %>% # remove choices where an option was not attended 
  mutate(risk = case_when(choice == "s" & mean_r > safe ~ "averse" ,
                          choice == "r" & mean_r < safe ~ "seeking" ,
                          choice == "s" & mean_r <= safe ~ "neutral" , 
                          choice == "r" & mean_r >= safe ~ "neutral"), 
         risk = as.factor(risk)) %>% # risk attitude
  group_by(model, psi, threshold, theta, risk) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup()


# granular pattern 

risk_rates <- choices %>% 
  filter(!c(n_s == 0 | n_r == 0)) %>% # remove choices where an option was not attended 
  mutate(risk = case_when(choice == "s" & ev_r > safe ~ "averse" ,
                          choice == "r" & ev_r < safe ~ "seeking" ,
                          choice == "s" & ev_r <= safe ~ "neutral" , 
                          choice == "r" & ev_r >= safe ~ "neutral"), 
         risk = as.factor(risk)) %>% # risk attitude
  group_by(model, psi, threshold, theta, risk) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup()


risk_rates %>% 
  filter(model=="summary", threshold=="relative", risk != "neutral") %>% 
  ggplot(aes(x=risk, y=rate, fill=risk)) +
  facet_grid(theta~psi, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed), 
                                            psi = as_labeller(label_psi, default = label_parsed))) +
  geom_bar(stat="identity", position = "stack") + 
  geom_point() + 
  scale_y_continuous(limits = c(0, .3), breaks = seq(0,.3,.05)) +
  labs(x="Risk Attitude",
       y="Proportion", 
       fill="Risk Attitude",
       title = "Proportion of Risk-averse vs. Risk-seeking Choices",
       subtitle = "Summary Comparison Rule") + 
  theme_apa()
ggsave("supplements/risk attitudes/risk_attitudes_summary.png", width=16,height=9)

risk_rates %>% 
  filter(model=="roundwise", threshold=="relative", risk != "neutral") %>% 
  ggplot(aes(x=risk, y=rate, fill=risk)) +
  facet_grid(theta~psi, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed), 
                                            psi = as_labeller(label_psi, default = label_parsed))) +
  geom_bar(stat="identity", position = "stack") + 
  geom_point() + 
  scale_y_continuous(limits = c(0, .3), breaks = seq(0,.3,.05)) +
  labs(x="Risk Attitude",
       y="Proportion", 
       fill="Risk Attitude",
       title = "Proportion of Risk-averse vs. Risk-seeking Choices",
       subtitle = "Roundwise Comparison Rule") + 
  theme_apa()
ggsave("supplements/risk attitudes/risk_attitudes_roundwise.png", width=16,height=9)


# Overall pattern

## expected number of safe choices under risk neutrality
problems <- choices <- read_rds("data/choice_problems.rds") 
neutral_rates <- problems %>% 
  mutate(better_safe = case_when(safe > r_ev ~ 1, 
                                 safe < r_ev ~ 0, 
                                 safe == r_ev ~ NA)) %>% 
  group_by(better_safe) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count/sum(count))
safe_prop <- round(neutral_rates[[2,3]], 3)
## Safe proportion = .533

rates <- choices %>% 
  filter(!c(n_s == 0 | n_r == 0)) %>% # remove choices where an option was not attended 
  mutate(r_averse = ifelse(choice == "s", 1, 0)) %>% # risk averse choice
  group_by(model, psi, threshold, theta, r_averse) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>% 
  filter(!(r_averse == 0)) %>% 
  mutate(rate_EV_control = (rate -safe_prop)*100)


r_averse_summary <- rates %>%  
  filter(model == "summary" & threshold == "relative") %>% 
  #filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate_EV_control, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 1)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "% Safe Choices \n Observed - Expected under EV max.",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  theme_minimal(base_size = 20)

r_averse_roundwise <- rates %>%
  filter(model == "roundwise" & threshold == "relative") %>% 
  #filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate_EV_control, group = theta, color = as.factor(theta))) +
  scale_color_scico_d(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 1)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "% Safe Choices \n Observed - Expected under EV max.",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  theme_minimal(base_size = 20)

r_averse_summary + r_averse_roundwise + plot_annotation(tag_levels = "A")
ggsave(file = "supplements/risk attitudes/rates_safe_control.png", width = 14, height = 6)



