pacman::p_load(tidyverse, scico)
problems <- read_rds("supplements/risk attitudes/rerun2/problems.rds")

test <- problems %>% mutate(max = if_else(ev_diff < 0, 1, 0)) 
mean(test$max)


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


###### Sampling strategy with summary comparison rule

# Define tested parameter values
psi.store <- c(.1,.3,.5,.7,.9)
theta.store <- c(15,45,75)

# list entry for each problem
choice.store <- matrix(NA, ncol=length(psi.store),nrow = length(theta.store))
nProblems <- nrow(problems)
problem.store <- param_list <- vector("list", nProblems) 

for (m in seq_len(nProblems)){
  
  #Properties of the lottery problem
  out.safe <- problems[[m, "safe"]]
  p.safe <- 1

  out.risky <- c(problems[[m,"r_low"]], problems[[m,"r_high"]])
  p.risky <- c(problems[[m,"p_r_low"]], problems[[m,"p_r_high"]])
  
  
  for (k in 1:length(theta.store)){
    for (j in 1:length(psi.store)){
      psi <- psi.store[j] #Switching probability
      theta <- theta.store[k]
      nRuns <- 100
      choice <- vector(length = nRuns)
      
      for (i in 1:nRuns){
        
        #Initialize variables
        currentOption <- NA
        DV.safe <- 0
        DV.risky <- 0
        nSamples.safe <- 0
        nSamples.risky <- 0 
        
        # Determine from which option samples are drawn first
        initial.option <- rbinom(1,1,.5) # 1=safe,0=risky 
        if (initial.option == 1) {
          currentOption <- 1 #safe option
        } else {
          currentOption <- 0 #risky option
        }
        
        #Start sampling
        if (currentOption==1){
          nSamples.safe <- nSamples.safe + 1
          DV.safe <- DV.safe + out.safe[rbinom(1,1,p.safe)]
        } else {
          nSamples.risky <- nSamples.risky + 1
          DV.risky <- DV.risky + out.risky[rbinom(1,1, p.risky[2])+1] 
        }

        while (abs(DV.safe-DV.risky)<theta){
          # Determine whether to switch or stay
          switch <- rbinom(1,1,psi) 
          if (switch==1){
            currentOption <- abs(currentOption-1)
          } else {
            currentOption <- currentOption
          }
          
          
          # Draw sample
          if (currentOption==1){
            nSamples.safe <- nSamples.safe + 1
            DV.safe <- DV.safe + out.safe[rbinom(1,1,p.safe)]
          } else {
            nSamples.risky <- nSamples.risky + 1
            DV.risky <- DV.risky + out.risky[rbinom(1,1,p.risky[2])+1]
          }
        }
        
        if (DV.safe>DV.risky){
          choice[i] <- 1 #safe option chosen
        } else {
          choice[i] <- 0 #risky option chosen
        }
      }
      choice.store[k,j] <- mean(choice) # proportion of choosing the safe option
    }
  }
  problem.store[[m]] <- choice.store
  print(paste("\u2713 Problem No. ", m, " finished!"))
}

# store results
for (set in 1:length(problem.store)){ 
  dimnames(problem.store[[set]]) <- list(c("15", "45", "75"), c(".1", ".3", ".5", ".7", ".9"))
  problem.store[[set]] <- as.data.frame(problem.store[[set]])
  
  problem.store[[set]] <- problem.store[[set]] %>% 
    mutate(theta = c(15, 45, 75),
           problem = set) %>% 
    pivot_longer(names_to = "psi", values_to = "prop", cols = `.1`:`.9`) %>% 
    select(problem, theta, psi, prop)
}

results <- bind_rows(problem.store)
problems <- problems %>% mutate(problem = row_number())
data <- left_join(problems, results, by=join_by(problem)) %>% 
  select(problem, everything())
write_rds(data, "supplements/risk attitudes/rerun2/results_summary_rerun2_lite.rds")
data <- read_rds("supplements/risk attitudes/rerun2/results_summary_rerun2_lite.rds")

# plot results 

## maximization
data %>% 
  mutate(max = if_else(ev_diff < 0, "safe", "risky"), 
         maxprop = if_else(max == "safe", prop, 1-prop)) %>% 
  group_by(theta, psi) %>% 
  summarise(rate = mean(maxprop)) %>% 
  mutate(theta = as.factor(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico_d(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  # scale_y_continuous(limits = c(.4, .6), breaks = seq(.4, .6, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% EV Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()


## risk aversion 

### aggregate
data %>% 
  group_by(theta, psi) %>% 
  summarise(rate = mean(prop)) %>% 
  mutate(theta = as.factor(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  #facet_wrap(~rare) + 
  scale_color_scico_d(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(.4, .6), breaks = seq(.4, .6, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% Safe Choices",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()

### rare event
data %>% 
  group_by(rare, theta, psi) %>% 
  summarise(rate = mean(prop)) %>% 
  mutate(theta = as.factor(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_wrap(~rare, nrow = 3) + 
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




data %>% 
  group_by(theta, psi) %>% 
  summarise(rate = mean(prop)) %>% 
  mutate(deviation_rate =  rate-mean(test$max), 
         theta = as.factor(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(x=psi, y=deviation_rate, group=theta, color=theta)) +
  scale_color_scico_d(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 5)) +
  labs(title = "Summary Comparison",
       x = "Switching Probability\n(Search Rule)",
       y =  "% Safe Choices\nObserved - Expected",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  #geom_hline(yintercept = 0) +
  theme_minimal()



View(data)

dat <- data %>%
  mutate(EV_dummy = if_else(safe > r_ev, 1, 0), 
         EV_effect = if_else(safe > r_ev, 1, -1)) %>% 
  select(theta, psi, prop, problem, r_ev, safe, ev_diff, EV_dummy, EV_effect) %>%
  group_by(theta, psi) %>%
  mutate(strategy = as.factor(cur_group_id()),
         n_choice = prop*100) 


d_dummy <- list(
  S = dat_dummy$strategy , 
  nC = dat_dummy$n_choice , 
  EV_dummy = dat_dummy$EV_dummy ,
  EV_abs_diff = abs(dat_dummy$ev_diff)
)


## fit model
m_dummy <- alist( 
  nC ~ dbinom(200, theta) , 
  logit(theta) <- a[S] + b[S]*EV_dummy, 
  a[S] ~ dnorm(0,2) ,
  b[S] ~ dnorm(0,2)
)
m_dummy_fit <- ulam(m_dummy, data=d_dummy, chains=8, cores=8, cmdstan = TRUE)
precis(m_dummy_fit, depth = 2)
plot(m_dummy_fit, depth = 2)

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
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       subtitle = "Dummy: Probability of false safe choice" , 
       x = "Switching Probability\n(Search Rule)",
       y =  "Probability of False Safe Choice",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()


# predicted probability of safe choice, given no EV differences (effect coding)


d_effect <- list(
  S = dat$strategy , 
  nC = dat$n_choice , 
  EV_effect = dat$EV_effect 
)

## fit model
m_effect <- alist( 
  nC ~ dbinom(100, theta) , 
  logit(theta) <- a[S] + b*EV_effect, 
  a[S] ~ dnorm(0,2) ,
  b ~ dnorm(0,2)
)
m_effect_fit <- ulam(m_effect, data=d_effect, chains=8, cores=8, cmdstan = TRUE)
precis(m_effect_fit, depth = 2)

## store results
intercepts_m_effect <- tibble(strategy = as.factor(1:15) ,
                              intercept = coef(m_effect_fit)[1:15])
intercepts_m_effect
## plot results
left_join(strategies, intercepts_m_effect, by=join_by(strategy)) %>% 
  mutate(p_safe = round(exp(intercept) / (1+exp(intercept)), 3)) %>% 
  ggplot(aes(x=as.double(psi), y=p_safe, group=theta, color=theta)) +
  scale_color_scico(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       subtitle = "Effect: Probability of safe choice, given no (ordinal) EV differences" ,
       x = "Switching Probability\n(Search Rule)",
       y =  "Probability of Safe Choice",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()

