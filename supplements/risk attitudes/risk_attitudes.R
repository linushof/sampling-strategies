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
  labs(x="Risk Attitude",
       y="Proportion", 
       fill="Risk Attitude",
       title = "Proportion of Risk-averse vs. Risk-seeking Choices",
       subtitle = "Roundwise Comparison Rule") + 
  theme_apa()
ggsave("supplements/risk attitudes/risk_attitudes_roundwise.png", width=16,height=9)

