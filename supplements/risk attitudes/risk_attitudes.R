
# Setup -------------------------------------------------------------

# load packages 
pacman::p_load(tidyverse, scico, psych, rethinking, latex2exp, ggpubr)

# load data
problems <- read_rds("data/choice_problems.rds")
choices <- read_rds("data/choice_data.rds.bz2")

# labels
label_theta <- function(string) {
  TeX(paste("$\\theta=$", string, sep = ""))
}

label_psi <- function(string) {
  TeX(paste("$\\psi=$", string, sep = ""))
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
  mutate(rate = round(n/sum(n), 3)) %>% 
  ungroup() %>% 
  filter(!(safe_choice == 0)) %>% 
  mutate(deviation_rate = (rate - safe_prop)*100)

## Proportion of risk averse/seeking/neutral choices
risk_rates <- choices %>% 
  filter(!c(n_s == 0 | n_r == 0)) %>% # 
  mutate(risk = case_when(choice == "s" & r_ev > safe ~ "averse" ,
                          choice == "r" & r_ev < safe ~ "seeking" ,
                          choice == "s" & r_ev <= safe ~ "neutral" , 
                          choice == "r" & r_ev >= safe ~ "neutral") ,
         risk = as.factor(risk)) %>% # risk attitude
  group_by(model, psi, threshold, theta, risk) %>% 
  summarise(n = n()) %>% 
  mutate(risk_rate = round(n/sum(n), 4)) %>% 
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
       y =  "% Safe Choices\nObserved - Expected",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  theme_minimal()


# predicted probability of false safe choice (dummy coding)

dat <- choices %>%
  filter(model == "summary", threshold=="relative") %>% 
  mutate(choice = if_else(choice == "s",1,0) , 
         EV_dummy = if_else(safe > r_ev, 1, 0), 
         EV_effect = if_else(safe > r_ev, 1, -1)) %>% 
  select(model, theta, psi, choice, problem, ev_diff, EV_dummy, EV_effect) %>%
  group_by(model, theta, psi) %>%
  mutate(strategy = as.factor(cur_group_id())) 

dat_dummy <- dat %>% 
  group_by(strategy, problem, ev_diff, EV_dummy, .add=T) %>% 
  summarise(n_choice = sum(choice))

d_dummy <- list(
  S = dat_dummy$strategy , 
  nC = dat_dummy$n_choice , 
  EV_dummy = dat_dummy$EV_dummy 
)


## fit model
m_dummy <- alist( 
  nC ~ dbinom(200, theta) , 
  logit(theta) <- a[S] + b*EV_dummy, 
  a[S] ~ dnorm(0,2) ,
  b ~ dnorm(0,2)
)
m_dummy_fit <- ulam(m_dummy, data=d_dummy, chains=8, cores=8, cmdstan = TRUE)


## store results
strategies <- dat %>% distinct(theta, psi, strategy)
intercepts_m_dummy <- tibble(strategy = as.factor(1:50) ,
                        intercept = coef(m_dummy_fit)[1:50])

## plot results
left_join(strategies, intercepts_m_dummy, by=join_by(strategy)) %>% 
  mutate(p_false_safe = round(exp(intercept) / (1+exp(intercept)), 3)) %>% 
  ggplot(aes(x=psi, y=p_false_safe, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       subtitle = "Dummy: Probability of false safe choice" , 
       x = "Switching Probability\n(Search Rule)",
       y =  "Probability of False Safe Choice",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()


# predicted probability of safe choice, given no EV differences (effect coding)

dat_effect <- dat %>% 
  group_by(strategy, problem, ev_diff, EV_effect, .add=T) %>% 
  summarise(n_choice = sum(choice))

d_effect <- list(
  S = dat_effect$strategy , 
  nC = dat_effect$n_choice , 
  EV_effect = dat_effect$EV_effect 
)

## fit model
m_effect <- alist( 
  nC ~ dbinom(200, theta) , 
  logit(theta) <- a[S] + b*EV_effect, 
  a[S] ~ dnorm(0,2) ,
  b ~ dnorm(0,2)
)
m_effect_fit <- ulam(m_effect, data=d_effect, chains=8, cores=8, cmdstan = TRUE)

## store results
intercepts_m_effect <- tibble(strategy = as.factor(1:50) ,
                        intercept = coef(m_effect_fit)[1:50])

## plot results
left_join(strategies, intercepts_m_effect, by=join_by(strategy)) %>% 
  mutate(p_safe = round(exp(intercept) / (1+exp(intercept)), 3)) %>% 
  ggplot(aes(x=psi, y=p_safe, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       subtitle = "Effect: Probability of safe choice, given no (ordinal) EV differences" ,
       x = "Switching Probability\n(Search Rule)",
       y =  "Probability of Safe Choice",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()

# risk rate 

risk_rates %>% 
  filter(model=="summary", threshold=="relative", risk != "neutral") %>%
  select(-n) %>% 
  pivot_wider(values_from = risk_rate, names_from = risk) %>% 
  mutate(diff = averse - seeking) %>%
  ggplot(aes(x=psi, y=diff, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  # scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 5)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "% Diff Risk Averse - Seeking",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

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


## conditioned on problems with better safe/risky EV

diff <- problems %>% 
  mutate(problem = row_number() , 
         better_ev = as.factor(if_else(safe > r_ev, "safe", "risky")) ,
         ev_diff_a = abs(ev_diff)) %>% 
  select(problem, rare, better_ev, ev_diff, ev_diff_a, everything())

means <- aggregate(ev_diff_a ~ better_ev, data = diff, FUN = mean)
ggplot(diff, aes(x=ev_diff_a, group = better_ev, fill = better_ev, color = better_ev)) +
  geom_density() + 
  geom_vline(data = means, aes(xintercept = ev_diff_a, color = better_ev), linetype = "dashed", linewidth = 1.5) +
  scale_color_viridis_d() +
  scale_fill_viridis_d(alpha = .5) +
  labs(x="|EV Differences|" ,
       y="Density",
       color = "Better EV Option", 
       fill = "Better EV Option") + 
  theme_minimal()

## conditioned on problems with better safe/risky EV & attractive/unattractive/no rare event

diff <- diff %>% 
  mutate(rare = case_when(rare == "attractive" ~ "Rare Desirable Outcome", 
                        rare == "unattractive" ~ "Rare Undesirable Outcome", 
                        rare == "none" ~ "No Rare Outcome"))

### absolute EV differences
means <- aggregate(ev_diff_a ~ rare, data = diff, FUN = mean)
ggplot(diff, aes(x=ev_diff_a, group = better_ev, fill = better_ev, color = better_ev)) +
  facet_wrap(~rare, nrow = 3) + 
  geom_density() +
  geom_vline(data = means, aes(xintercept = ev_diff_a), linetype = "dashed", linewidth = 1.5) +
  scale_color_viridis_d() +
  scale_fill_viridis_d(alpha = .5) + 
  labs(x="|EV Differences|" ,
       y="Density",
       color = "Better EV Option", 
       fill = "Better EV Option") + 
  theme_minimal() 


### positive/negative EV differences
means <- aggregate(ev_diff ~ rare, data = diff, FUN = mean)
ggplot(diff, aes(x=ev_diff)) + 
  facet_wrap(~rare, nrow = 3) +
  theme_minimal() + 
  geom_density() + 
  geom_vline(data = means, aes(xintercept = ev_diff, color = rare), linetype = "dashed", linewidth = 1.5) +
  geom_vline(xintercept = 0, linewidth = 1) + 
  labs(x="EV Differences" ,
       y="Density",
       color = "Mean")  


### Statistical control for differences in EV magnitude

dat_diff <- dat %>% 
  group_by(strategy, problem, ev_diff, .add=T) %>% 
  summarise(n_choice = sum(choice))

d_diff <- list(
  S = dat_diff$strategy , 
  nC = dat_diff$n_choice , 
  EV_diff = dat_diff$ev_diff 
)


## fit model
m_diff <- alist( 
  nC ~ dbinom(200, theta) , 
  logit(theta) <- a[S] + b*EV_diff, 
  a[S] ~ dnorm(0,2) ,
  b ~ dnorm(0,2)
)
m_diff_fit <- ulam(m_diff, data=d_diff, chains=8, cores=8, cmdstan = TRUE)

## store results
intercepts_m_diff <- tibble(strategy = as.factor(1:50) ,
                            intercept = coef(m_diff_fit)[1:50])

## plot results
left_join(strategies, intercepts_m_diff, by=join_by(strategy)) %>% 
  mutate(p_safe = round(exp(intercept) / (1+exp(intercept)), 3)) %>% 
  ggplot(aes(x=psi, y=p_safe, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       subtitle = "Effect: Probability of safe choice, given no (metric) EV differences" ,
       x = "Switching Probability\n(Search Rule)",
       y =  "Probability of Safe Choice",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()





###### old

d <- list(
  S = dat$strategy , 
  nC = dat$n_choice , 
  EV = dat$better_ev ,
  EV_diff = abs(dat$ev_diff) 
)

m2 <- alist( 
  nC ~ dbinom(200, theta) , 
  logit(theta) <- a[S] + b*EV , 
  a[S] ~ dnorm(0,2) ,
  b ~ dnorm(0,2) 
  
)
m2.fit <- ulam(m2, data=d, chains=8, cores=8, cmdstan = TRUE)

strategies <- dat %>% distinct(model, theta, psi, strategy)
intercepts_m2 <- tibble(strategy = as.factor(1:100) ,
                        intercept = coef(m2.fit)[1:100])
logreg2 <- left_join(strategies, intercepts_m2, by=join_by(strategy))
logreg2 <- logreg2 %>% mutate(p_false_risky = round(exp(intercept) / (1+exp(intercept)), 3))


logreg2 %>%  
  filter(model == "summary") %>% 
  ggplot(aes(x=psi, y=p_false_risky, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "Probability of False Risky Choice",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)


logreg1 <- as_data_frame(logreg1) 
test <- as.double(logreg2$p_false_risky)
comp <- logreg1 %>% mutate(p_false_risky = test)
comp <- comp %>% mutate(diff = p_false_safe - p_false_risky)

comp %>%  
  filter(model == "summary") %>% 
  ggplot(aes(x=psi, y=diff, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "Difference False Safe - False Risky",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)



# safe

dat <- choices %>%
  filter(threshold=="relative") %>% 
  mutate(choice = if_else(choice == "s",1,0), 
         better_ev = if_else(safe > r_ev, 1, 0)) %>% 
  select(model, theta, psi, choice, problem, ev_diff, better_ev) %>%
  group_by(model, theta, psi) %>%
  mutate(strategy = as.factor(cur_group_id())) %>% 
  group_by(strategy, problem, ev_diff, better_ev, .add=T) %>% 
  summarise(n_choice = sum(choice))

d <- list(
  S = dat$strategy , 
  nC = dat$n_choice , 
  EV = dat$better_ev ,
  EV_diff = dat$ev_diff
)

m3 <- alist( 
  nC ~ dbinom(200, theta) , 
  logit(theta) <- a[S] + b*EV_diff , 
  a[S] ~ dnorm(0,2) ,
  b ~ dnorm(0,2)
)

m3.fit <- ulam(m3, data=d, chains=8, cores=8, cmdstan = TRUE)

strategies <- dat %>% distinct(model, theta, psi, strategy)
intercepts_m3 <- tibble(strategy = as.factor(1:100) ,
                        intercept = coef(m3.fit)[1:100])
logreg3 <- left_join(strategies, intercepts_m3, by=join_by(strategy))
logreg3 <- logreg3 %>% mutate(p_safe = round(exp(intercept) / (1+exp(intercept)), 3))
logreg3 %>% filter(model == "summary") %>% View()


# risky

dat <- choices %>%
  filter(threshold=="relative") %>% 
  mutate(choice = if_else(choice == "r",1,0), 
         better_ev = if_else(r_ev > safe, 1, 0)) %>% 
  select(model, theta, psi, choice, problem, ev_diff, better_ev) %>%
  group_by(model, theta, psi) %>%
  mutate(strategy = as.factor(cur_group_id())) %>% 
  group_by(strategy, problem, ev_diff, better_ev, .add=T) %>% 
  summarise(n_choice = sum(choice))

d <- list(
  S = dat$strategy , 
  nC = dat$n_choice , 
  EV = dat$better_ev ,
  EV_diff = dat$ev_diff 
)

m4 <- alist( 
  nC ~ dbinom(200, theta) , 
  logit(theta) <- a[S] + b*EV_diff , 
  a[S] ~ dnorm(0,2) ,
  b ~ dnorm(0,2)
)

m4.fit <- ulam(m4, data=d, chains=8, cores=8, cmdstan = TRUE)

strategies <- dat %>% distinct(model, theta, psi, strategy)
intercepts_m4 <- tibble(strategy = as.factor(1:100) ,
                        intercept = coef(m4.fit)[1:100])
logreg4 <- left_join(strategies, intercepts_m4, by=join_by(strategy))
logreg4 <- logreg4 %>% mutate(p_risky = round(exp(intercept) / (1+exp(intercept)), 3))

logreg3 <- as_data_frame(logreg3) 
p_risky <- as.double(logreg4$p_risky)
comp <- logreg3 %>% mutate(p_risky = p_risky)
comp <- comp %>% mutate(diff = p_safe - p_risky)

comp %>% filter(model=="summary") %>% View()
comp %>%  
  filter(model == "summary") %>% 
  ggplot(aes(x=psi, y=diff, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "Difference False Safe - False Risky",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

comp %>% filter(model=="summary") %>% View()

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



View(problems)
problems %>% 
  mutate(better_ev = if_else(safe > r_ev, 1, 0),
         ev_diff_a = abs(ev_diff)) %>% 
  View()


dat <- choices %>%
  filter(threshold=="relative", rare == "none") %>% 
  mutate(choice = if_else(choice == "s",1,0), 
         better_ev = if_else(safe > r_ev, 1, 0)) %>% 
  select(model, theta, psi, choice, problem, ev_diff, better_ev) %>%
  group_by(model, theta, psi) %>%
  mutate(strategy = as.factor(cur_group_id())) %>% 
  group_by(strategy, problem, ev_diff, better_ev, .add=T) %>% 
  summarise(n_choice = sum(choice))

d <- list(
  S = dat$strategy , 
  nC = dat$n_choice , 
  EV = dat$better_ev ,
  EV_diff = dat$ev_diff 
)


## fit model
m <- alist( 
  nC ~ dbinom(200, theta) , 
  logit(theta) <- a[S] + b*EV, 
  a[S] ~ dnorm(0,2) ,
  b ~ dnorm(0,2)
)
m.fit <- ulam(m, data=d, chains=8, cores=8, cmdstan = TRUE)

## store results
strategies <- dat %>% distinct(model, theta, psi, strategy)
intercepts_m <- tibble(strategy = as.factor(1:100) ,
                        intercept = coef(m.fit)[1:100])
logreg <- left_join(strategies, intercepts_m, by=join_by(strategy))
logreg <- logreg %>% mutate(p_false_safe = round(exp(intercept) / (1+exp(intercept)), 3))



logreg %>%  
  filter(model == "summary") %>% 
  ggplot(aes(x=psi, y=p_false_safe, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "Difference False Safe - False Risky",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)

dat <- choices %>%
  filter(threshold=="relative", rare == "none") %>% 
  mutate(choice = if_else(choice == "r",1,0), 
         better_ev = if_else(r_ev > safe, 1, 0)) %>% 
  select(model, theta, psi, choice, problem, ev_diff, better_ev) %>%
  group_by(model, theta, psi) %>%
  mutate(strategy = as.factor(cur_group_id())) %>% 
  group_by(strategy, problem, ev_diff, better_ev, .add=T) %>% 
  summarise(n_choice = sum(choice))

d <- list(
  S = dat$strategy , 
  nC = dat$n_choice , 
  EV = dat$better_ev ,
  EV_diff = dat$ev_diff 
)


## fit model
m10 <- alist( 
  nC ~ dbinom(200, theta) , 
  logit(theta) <- a[S] + b*EV, 
  a[S] ~ dnorm(0,2) ,
  b ~ dnorm(0,2)
)
m10.fit <- ulam(m10, data=d, chains=8, cores=8, cmdstan = TRUE)

## store results
strategies <- dat %>% distinct(model, theta, psi, strategy)
intercepts_m10 <- tibble(strategy = as.factor(1:100) ,
                       intercept = coef(m10.fit)[1:100])
logreg10 <- left_join(strategies, intercepts_m10, by=join_by(strategy))
logreg10 <- logreg10 %>% mutate(p_false_risky = round(exp(intercept) / (1+exp(intercept)), 3))

logreg10 %>%  
  filter(model == "summary") %>% 
  ggplot(aes(x=psi, y=p_false_risky, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "Difference False Safe - False Risky",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)


logreg <- as_data_frame(logreg) 
p_risky <- as.double(logreg10$p_false_risky)
comp <- logreg %>% mutate(p_false_risky = p_risky)
comp <- comp %>% mutate(diff = p_false_safe - p_false_risky)

comp %>%  
  filter(model == "summary") %>% 
  ggplot(aes(x=psi, y=diff, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y =  "Difference False Safe - False Risky",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)




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



