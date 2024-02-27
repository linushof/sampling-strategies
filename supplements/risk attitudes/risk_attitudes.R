# load packages 
pacman::p_load(tidyverse, scico, psych, rethinking)

# load data
problems <- read_rds("data/choice_problems.rds")
choices <- read_rds("data/choice_data.rds.bz2")

# Risk Attitudes: Control for ORDINAL EV differences ----------------------

# prepare data

## expected* proportion of safe choices (under risk neutrality)
expected_rates <- problems %>% 
  mutate(better_safe = case_when(safe > r_ev ~ 1, 
                                 safe < r_ev ~ 0, 
                                 safe == r_ev ~ NA)) %>% 
  group_by(better_safe) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count/sum(count))
safe_prop <- round(expected_rates[[2,3]], 3)
safe_prop ## Safe proportion = .533

## observed - expected proportion of safe choices (under risk neutrality)
deviation_rates <- choices %>% 
  filter(!c(n_s == 0 | n_r == 0)) %>% 
  mutate(safe_choice = ifelse(choice == "s", 1, 0)) %>% # risk averse choice
  group_by(model, psi, threshold, theta, safe_choice) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>% 
  filter(!(safe_choice == 0)) %>% 
  mutate(deviation_rate = (rate - safe_prop)*100)

#Proportion or risk averse/seeking/neutral choices
risk_rates <- choices %>% 
  filter(!c(n_s == 0 | n_r == 0)) %>% # remove choices where an option was not attended
  mutate(risk = case_when(choice == "s" & r_ev > safe ~ "averse" ,
                          choice == "r" & r_ev < safe ~ "seeking" ,
                          choice == "s" & r_ev <= safe ~ "neutral" , 
                          choice == "r" & r_ev >= safe ~ "neutral") ,
         risk = as.factor(risk)) %>% # risk attitude
  group_by(model, psi, threshold, theta, risk) %>% 
  summarise(n = n()) %>% 
  mutate(risk_rate = round(n/sum(n), 2)) %>% 
  ungroup()

# summary comparison

## deviation rate 
deviation_rates %>%  
  filter(model == "summary" & threshold == "relative") %>% 
  ggplot(aes(x=psi, y=deviation_rate, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 5)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "% Safe Choices\nObserved vs. Expected",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  theme_minimal(base_size = 20)

#### Logistic Regression ####

dat <- choices %>%
  filter(threshold=="relative") %>% 
  select(model, theta, psi, choice, ev_diff) %>%
  group_by(model, theta, psi) %>%
  mutate(strategy = cur_group_id()) %>%
  ungroup() %>% 
  mutate(choice = if_else(choice == "s",1,0),
         better_ev = if_else(ev_diff < 0, 1, 0),
         strategy = as.factor(strategy))

d <- list(
  S = dat$strategy , 
  C = dat$choice , 
  EV_1 = dat$better_ev
  )

m1 <- alist( 
  C ~ dbern(theta) , 
  logit(theta) <- a[S] + b*EV_1 , 
  a[S] ~ dnorm(0,2) ,
  b ~ dnorm(0,2)
)

m1.fit <- ulam(m1, data=d, chains=8, cores=8, iter = 500, cmdstan = TRUE)
traceplot(m1.fit)
precis(m1.fit, depth = 2)
plot(m1.fit, depth = 2)


# store data
strategies <- dat %>% distinct(model, theta, psi, strategy)
intercepts <- tibble(strategy = as.factor(1:100) ,                   intercept = coef(m1.fit)[1:100])
by <- join_by(strategy)
logreg <- left_join(strategies, intercepts, by=by)
write_rds(logreg, "supplements/risk attitudes/risk_aversion_intercepts.rds")

#### Logistic Regression ####


## risk rate 
risk_rates %>% 
  filter(model=="summary", threshold=="relative", risk != "neutral") %>% 
  ggplot(aes(x=risk, y=risk_rate, fill=risk)) +
  scale_fill_viridis_d() + 
  facet_grid(theta~psi, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed), 
                                            psi = as_labeller(label_psi, default = label_parsed))) +
  geom_bar(stat="identity", position = "stack") + 
  scale_y_continuous(limits = c(0,.3), breaks = seq(0, .3, .05)) +
  labs(x="Risk Attitude",
       y="Proportion", 
       fill="Risk Attitude",
       title = "Proportion of Risk-averse vs. Risk-seeking Choices",
       subtitle = "Summary Comparison Rule") + 
  theme_minimal()


# roundwise comparison

## deviation rate 
deviation_rates %>%
  filter(model == "roundwise" & threshold == "relative") %>% 
  ggplot(aes(x=psi, y=deviation_rate, group=theta, color=as.factor(theta))) +
  scale_color_scico_d(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 5)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "% Safe Choices\nObserved vs. Expected",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  theme_minimal(base_size = 20)
# Run logistic regression (since Figure too complicated)

# risk rate 
risk_rates %>% 
  filter(model=="roundwise", threshold=="relative", risk != "neutral") %>% 
  ggplot(aes(x=risk, y=risk_rate, fill=risk)) +
  scale_fill_viridis_d() + 
  facet_grid(theta~psi, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed), 
                                            psi = as_labeller(label_psi, default = label_parsed))) +
  geom_bar(stat="identity", position = "stack") + 
  scale_y_continuous(limits = c(0, .3), breaks = seq(0,.3,.05)) +
  labs(x="Risk Attitude",
       y="Proportion", 
       fill="Risk Attitude",
       title = "Proportion of Risk-averse vs. Risk-seeking Choices",
       subtitle = "Roundwise Comparison Rule") + 
  theme_minimal()

# Pattern Explanation --------------------------------------------------


# Summary comparison: risk aversion


# Skewness of outcomes
names(problems)
skew <- problems %>% mutate(ad.safe = safe - r_low , 
                            ad.risky = abs(safe - r_high) , 
                            ad.diff = round(ad.safe - ad.risky, 3)
                            )


skew %>% filter(rare=="unattractive") %>% describe() # -.45
skew %>% filter(rare=="attractive") %>% describe() # -.74
skew %>% filter(rare=="none") %>% describe() # -.1

names(problems)
problems %>% filter(r_ev > safe) %>% summarise(m = mean(ev_diff)) # advantage risky: 2.99 
problems %>% filter(r_ev < safe) %>% summarise(m = mean(ev_diff)) # advantage safe: 3.41
skew %>% filter(rare=="unattractive") %>% summarise(m = mean(ev_diff)) # 2.44
skew %>% filter(rare=="attractive") %>% summarise(m = mean(ev_diff)) # -4.26
skew %>% filter(rare=="none") %>% summarise(m = mean(ev_diff)) # .548


ggplot(problems, aes(x=ev_diff)) + 
  facet_wrap(~rare, nrow = 3) +
  theme_minimal() + 
  geom_density()

ggplot(skew, aes(x=ad.diff)) + 
  facet_wrap(~rare, nrow = 3) +
  theme_minimal() + 
  geom_density()


# granular 


neutral_rates_attractive <- problems %>% 
  filter(rare == "attractive") %>% 
  mutate(better_safe = case_when(safe > r_ev ~ 1, 
                                 safe < r_ev ~ 0, 
                                 safe == r_ev ~ NA)) %>% 
  group_by(better_safe) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count/sum(count))
safe_prop_attractive <- round(neutral_rates_attractive[[2,3]], 3)

neutral_rates_unattractive <- problems %>% 
  filter(rare == "unattractive") %>% 
  mutate(better_safe = case_when(safe > r_ev ~ 1, 
                                 safe < r_ev ~ 0, 
                                 safe == r_ev ~ NA)) %>% 
  group_by(better_safe) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count/sum(count))
safe_prop_unattractive <- round(neutral_rates_unattractive[[2,3]], 3)

neutral_rates_none <- problems %>% 
  filter(rare == "none") %>% 
  mutate(better_safe = case_when(safe > r_ev ~ 1, 
                                 safe < r_ev ~ 0, 
                                 safe == r_ev ~ NA)) %>% 
  group_by(better_safe) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count/sum(count))
safe_prop_none <- round(neutral_rates_none[[2,3]], 3)

safe_prop_attractive #.95 with randomness, risk seeking is expected
safe_prop_unattractive #.1 with randomness, risk aversion is expected
safe_prop_none #.55 with randomness, risk seeking is expected

# according to the above logic, risk seeking should be stronger
# but below, it is the other way around ... 
#why is the risk aversion markedly stronger???
# why do we obtain the U-Shape on the aggregate?


rates_rare_event <- choices %>% 
  filter(!c(n_s == 0 | n_r == 0)) %>% # remove choices where an option was not attended 
  mutate(r_averse = ifelse(choice == "s", 1, 0)) %>% # risk averse choice
  group_by(model, psi, threshold, theta, rare, r_averse) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>% 
  filter(!(r_averse == 0))

rates_rare_event <-  rates_rare_event %>% 
  mutate(rate_EV_control = case_when(rare == "attractive" ~ (rate-safe_prop_attractive)*100 ,
                                     rare == "unattractive" ~ (rate-safe_prop_unattractive)*100 , 
                                     rare == "none" ~ (rate-safe_prop_none)*100
                                     )
         )


r_averse_summary_rare_event <- rates_rare_event %>%  
  filter(model == "summary" & threshold == "relative") %>% 
  #filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate_EV_control, group = theta, color = theta)) +
  facet_wrap(~rare) + 
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, 10)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "% Safe Choices \n Observed - Expected under EV max.",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  theme_minimal(base_size = 20)


r_averse_roundwise_rare_event <- rates_rare_event %>%  
  filter(model == "roundwise" & threshold == "relative") %>% 
  #filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate_EV_control, group = theta, color = theta)) +
  facet_wrap(~rare) + 
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, 10)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "% Safe Choices \n Observed - Expected under EV max.",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  theme_minimal(base_size = 20)



## Risk-seeking of roundwise comparison rule -> more trials where risky option provides better frequent outcome?

problems %>% mutate(risk_better_freq = case_when(p_r_high > .5 ~ 1 ,
                                                 p_r_high < .5 ~ 0 ,
                                                 p_r_high == .5 ~ NA)) %>% 
  group_by(risk_better_freq) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count/sum(count))
# Based on the ground-truth, the safe option offers the better frequent outcome in more problems - cannot explain the difference
