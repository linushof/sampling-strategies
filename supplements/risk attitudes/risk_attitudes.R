
# Setup -------------------------------------------------------------

# load packages 
pacman::p_load(tidyverse, scico, psych, rethinking, latex2exp, ggpubr)

# load data
problems <- read_rds("data/choice_problems.rds")
choices <- read_rds("data/choice_data.rds.bz2")
logreg <- read_rds("supplements/risk attitudes/logreg.rds")

# labels
label_theta <- function(string) {
  TeX(paste("$\\theta=$", string, sep = ""))
}

label_psi <- function(string) {
  TeX(paste("$\\psi=$", string, sep = ""))
}

# probability of high risky outcome
label_rare <- function(string) {
  TeX(paste("$\\p_{high}\\in$", string, sep = "")) 
}



# Pre-processing --------------------------------------------------------------------

# expected* proportion of safe choices (under risk neutrality)

expected_rates <- problems %>% 
  mutate(better_safe = case_when(safe > r_ev ~ 1, 
                                 safe < r_ev ~ 0, 
                                 safe == r_ev ~ NA)) %>% 
  group_by(better_safe) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count/sum(count))
safe_prop <- round(expected_rates[[2,3]], 3)
safe_prop ## Safe proportion = .533


# observed - expected proportion of safe choices (under risk neutrality)

deviation_rates <- choices %>% 
  filter(!c(n_s == 0 | n_r == 0)) %>% 
  mutate(safe_choice = ifelse(choice == "s", 1, 0)) %>% # risk averse choice
  group_by(model, psi, threshold, theta, safe_choice) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>% 
  filter(!(safe_choice == 0)) %>% 
  mutate(deviation_rate = (rate - safe_prop)*100)


# logistic regression: probability of safe choice, controlling for ordinal differences in EV 

dat <- choices %>%
  filter(threshold=="relative") %>% 
  mutate(choice = if_else(choice == "s",1,0), 
         better_ev = if_else(safe > r_ev, 1, 0)) %>% 
  select(model, theta, psi, choice, problem, ev_diff, better_ev) %>%
  group_by(model, theta, psi) %>%
  mutate(strategy = as.factor(cur_group_id())) %>% 
  group_by(strategy, problem, ev_diff, better_ev, .add=T) %>% 
  summarise(n_choice = sum(choice)) %>% 
  mutate(ev_diff_a = abs(ev_diff))

d <- list(
  S = dat$strategy , 
  nC = dat$n_choice , 
  EV_1 = dat$better_ev ,
  EV_2 = dat$ev_diff_a ,
  EV_3 = dat$ev_diff
)


## fit model
m1 <- alist( 
  nC ~ dbinom(200, theta) , 
  logit(theta) <- a[S] + b*EV_1 , 
  a[S] ~ dnorm(0,2) ,
  b ~ dnorm(0,2)
)
m1.fit <- ulam(m1, data=d, chains=8, cores=8, cmdstan = TRUE)

#traceplot(m1.fit) # check convergence
#precis(m1.fit, depth = 2)
#plot(m1.fit, depth = 2)

## store results
strategies <- dat %>% distinct(model, theta, psi, strategy)
intercepts_m1 <- tibble(strategy = as.factor(1:100) ,
                        intercept = coef(m1.fit)[1:100])
logreg1 <- left_join(strategies, intercepts_m1, by=join_by(strategy))
logreg1 <- logreg1 %>% mutate(p_false_safe = round(exp(intercept) / (1+exp(intercept)), 3))


## Proportion of risk averse/seeking/neutral choices
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


# Summary Comparison ------------------------------------------------------


## Pattern -----------------------------------------------------------------

# deviation rate 

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

# predicted probability of false safe 

p1 <- logreg1 %>%  
  filter(model == "summary") %>% 
  ggplot(aes(x=psi, y=p_false_safe, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  #scale_y_continuous(limits = c(0, .4), breaks = seq(0, .4, .1)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "Probability of False Safe Choice",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

# risk rate 

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


## Explanation -------------------------------------------------------------

# below, I check the explanation that the choice difficulty differs 
# for gambles where the safe or the risky option has the higher expectation

diff <- problems %>% 
  mutate(problem = row_number() , 
         better_ev = as.factor(if_else(safe > r_ev, 1, 0)) ,
         ev_diff_a = abs(ev_diff)) %>% 
  select(problem, rare, better_ev, ev_diff, ev_diff_a, everything())

means <- aggregate(ev_diff_a ~ better_ev, data = diff, FUN = mean)
ggplot(diff, aes(x=ev_diff_a, group = better_ev, fill = better_ev, color = better_ev)) +
  geom_density() + 
  geom_vline(data = means, aes(xintercept = ev_diff_a, color = better_ev), linetype = "dashed", linewidth = 2) +
  scale_color_viridis_d() +
  scale_fill_viridis_d(alpha = .5) +
  theme_minimal()

# EV differences are somewhat larger if safe option has a better EV
# are probabilities smaller if EV differences are controlled for? 

#### M2 ##### THIS DOESN'T WORK
'm2 <- alist( 
  nC ~ dbinom(200, theta) , 
  logit(theta) <- a[S] + b[S]*EV_1, 
  a[S] ~ dnorm(0,2) ,
  b[S] ~ dnorm(0,2)
)
m2.fit <- ulam(m2, data=d, chains=8, cores=8, iter = 1000, cmdstan = TRUE)

#traceplot(m2.fit) # check convergence
#precis(m2.fit, depth = 2) # b=3.35 (3.34, 3.36)
plot(m2.fit, depth = 2)

## store results
intercepts_m2 <- tibble(strategy = as.factor(1:100) ,
                        intercept = coef(m2.fit)[1:100])
logreg2 <- left_join(strategies, intercepts_m2, by=join_by(strategy))
logreg2 <- logreg2 %>% mutate(p_false_safe = round(exp(intercept) / (1+exp(intercept)), 3))


p2 <- logreg2 %>%  
  filter(model == "summary") %>% 
  ggplot(aes(x=psi, y=p_false_safe, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  #scale_y_continuous(limits = c(0, .4), breaks = seq(0, .4, .1)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "Probability of False Safe Choice",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)'



#### M3 #### This works as M1
m3 <- alist( 
  nC ~ dbinom(200, theta) , # binomial 
  logit(theta) <- a[S] + b*EV_3, 
  a[S] ~ dnorm(0,2) ,
  b ~ dnorm(0,2)
)

m3.fit <- ulam(m3, data=d, chains=8, cores=8, iter = 1000, cmdstan = TRUE)

traceplot(m3.fit) # check convergence
precis(m3.fit, depth = 2)
plot(m3.fit, depth = 2)

intercepts_m3 <- tibble(strategy = as.factor(1:100) ,
                        intercept = coef(m3.fit)[1:100])
logreg3 <- left_join(strategies, intercepts_m3, by=join_by(strategy))
logreg3 <- logreg3 %>% mutate(p_false_safe = round(exp(intercept) / (1+exp(intercept)), 3))

p3 <- logreg3 %>%  
  filter(model == "summary") %>% 
  ggplot(aes(x=psi, y=p_false_safe, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  #scale_y_continuous(limits = c(0, .4), breaks = seq(0, .4, .1)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "Probability of Safe Choice",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

### M4 
m4 <- alist( 
  nC ~ dbinom(200, theta) , # binomial 
  logit(theta) <- a[S] + b1*EV_1 + b2*EV_2, 
  a[S] ~ dnorm(0,2) ,
  b1 ~ dnorm(0,2) , 
  b2 ~ dnorm(0,2)
)
m4.fit <- ulam(m4, data=d, chains=8, cores=8, iter = 1000, cmdstan = TRUE)

intercepts_m4 <- tibble(strategy = as.factor(1:100) ,
                        intercept = coef(m4.fit)[1:100])
logreg4 <- left_join(strategies, intercepts_m4, by=join_by(strategy))
logreg4 <- logreg4 %>% mutate(p_false_safe = round(exp(intercept) / (1+exp(intercept)), 3))

p4 <- logreg4 %>%  
  filter(model == "summary") %>% 
  ggplot(aes(x=psi, y=p_false_safe, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  #scale_y_continuous(limits = c(0, .4), breaks = seq(0, .4, .1)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "Probability of Safe Choice",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)


p3

# for different rare event conditions 
ggplot(diff, aes(x=ev_diff_a, group = better_ev, fill = better_ev, color = better_ev)) +
  facet_wrap(~rare) + 
  geom_density() +
  scale_color_viridis_d() +
  scale_fill_viridis_d(alpha = .5) + 
  theme_minimal() 


deviation_rates

logreg

#problems %>% filter(rare=="unattractive") %>% summarise(m = mean(ev_diff)) # 2.44
#problems %>% filter(rare=="attractive") %>% summarise(m = mean(ev_diff)) # -4.26
#problems %>% filter(rare=="none") %>% summarise(m = mean(ev_diff)) # .548
View(choices)

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



'skew <- problems %>% mutate(ad.safe = safe - r_low , 
                            ad.risky = abs(safe - r_high) , 
                            ad.diff = round(ad.safe - ad.risky, 3)
)


skew %>% filter(rare=="unattractive") %>% describe() # -.45
skew %>% filter(rare=="attractive") %>% describe() # -.74
skew %>% filter(rare=="none") %>% describe() # -.1


ggplot(problems, aes(x=ev_diff)) + 
  facet_wrap(~rare, nrow = 3) +
  theme_minimal() + 
  geom_density()

ggplot(skew, aes(x=ad.diff)) + 
  facet_wrap(~rare, nrow = 3) +
  theme_minimal() + 
  geom_density()'




# Roundwise Comparison ----------------------------------------------------


## Pattern -----------------------------------------------------------------

# deviation rate 

deviation_rates %>%  
  filter(model == "roundwise" & threshold == "relative") %>% 
  ggplot(aes(x=psi, y=deviation_rate, group=as.factor(theta), color=as.factor(theta))) +
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

# predicted probability of false safe 

pr2 <- logreg2 %>%  
  filter(model == "roundwise") %>% 
  ggplot(aes(x=psi, y=p_false_safe, group=as.factor(theta), color=as.factor(theta))) +
  scale_color_scico_d(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(0, .4), breaks = seq(0, .4, .1)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "Probability of False Safe Choice",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

ggarrange(p2, pr2, nrow = 2)

risk_rates %>% 
  filter(model=="roundwise", threshold=="relative", risk != "neutral") %>% 
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
       subtitle = "Roundwise Comparison Rule") + 
  theme_minimal()


## Explanation --------------------------------------------------

## Risk-seeking of roundwise comparison rule -> more trials where risky option provides better frequent outcome?

problems %>% mutate(risk_better_freq = case_when(p_r_high > .5 ~ 1 ,
                                                 p_r_high < .5 ~ 0 ,
                                                 p_r_high == .5 ~ NA)) %>% 
  group_by(risk_better_freq) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count/sum(count))
# Based on the ground-truth, the safe option offers the better frequent outcome in more problems - cannot explain the difference
