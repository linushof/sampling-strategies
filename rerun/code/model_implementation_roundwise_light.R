pacman::p_load(tidyverse, scico, readxl)
problems <- read_rds("rerun/data/choice_problems_balanced.rds")

# Define tested parameter values
psi.store <- c(.1, .3, .5, .7, .9) 
theta.store <- c(1, 3, 5, 10)

# list entry for each problem
choice.store <- matrix(NA, ncol=length(psi.store),nrow = length(theta.store))
nProblems <- nrow(problems)
problem.store <- param_list <- vector("list", nProblems) 


for (m in seq_len(nProblems)){
  
  #Properties of the lottery problem
  out.safe <- problems[[m, "safe"]]
  p.safe <- 1
  
  out.risky <- c(problems[[m,"r_1"]], problems[[m,"r_2"]])
  p.risky <- c(problems[[m,"p_r_1"]], problems[[m,"p_r_2"]])
  
  for (k in 1:length(theta.store)){
    for (j in 1:length(psi.store)){
    psi <- psi.store[j] #Switching probability
    theta <- theta.store[k] #Decision threshold
    
    nRuns <- 1000
    choice <- vector(length = nRuns)
    
    for (i in 1:nRuns){
      
      #Initialize variables
      currentOption <- NA
      samples.safe <- 0
      samples.risky <- 0
      DV <- 0
      nSamples.safe <- 0
      nSamples.risky <- 0 
      nRoundSamples.safe <- 0
      nRoundSamples.risky <- 0 
      
      # Determine from which option samples are drawn first
      initial.option <- rbinom(1,1,.5) # 1=safe,0=risky 
      if (initial.option == 1) {
        currentOption <- 1 #safe option
      } else {
        currentOption <- 0 #risky option
      }
      
      # Start sampling and evidence accumulation
      while (abs(DV)<theta){
        
        while (currentOption==initial.option){
          
          # Draw samples from initial option
          if (currentOption==1){
            nSamples.safe <- nSamples.safe + 1
            nRoundSamples.safe <- nRoundSamples.safe + 1
            samples.safe <- samples.safe + out.safe[rbinom(1,1,p.safe)] 
          } else {
            nSamples.risky <- nSamples.risky + 1
            nRoundSamples.risky <- nRoundSamples.risky + 1
            samples.risky <- samples.risky + out.risky[rbinom(1,1,p.risky[2])+1] 
          }
          
          # Determine whether to switch or stay
          switch <- rbinom(1,1,psi) 
          if (switch==1){
            currentOption <- abs(currentOption-1) # after switch: currentOption != initial.option
          } else {
            currentOption <- currentOption
          }
        }
        
        # draw samples from the other (second) option
        while (currentOption!=initial.option){
          
          # Draw samples from initial option
          if (currentOption==1){
            nSamples.safe <- nSamples.safe + 1
            nRoundSamples.safe <- nRoundSamples.safe + 1
            samples.safe <- samples.safe + out.safe[rbinom(1,1,p.safe)] 
          } else {
            nSamples.risky <- nSamples.risky + 1
            nRoundSamples.risky <- nRoundSamples.risky + 1
            samples.risky <- samples.risky + out.risky[rbinom(1,1,p.risky[2])+1] 
          }
          
          # Determine whether to switch or stay
          switch <- rbinom(1,1,psi) 
          if (switch==1){
            currentOption <- abs(currentOption-1) #after switch: currentOption == initial.option
          } else {
            currentOption <- currentOption
          }
        }
        
        # Make roundwise comparison
        DV <- DV + (((samples.safe/nRoundSamples.safe)-(samples.risky/nRoundSamples.risky))>0)*1 + (((samples.safe/nRoundSamples.safe)-(samples.risky/nRoundSamples.risky))<0)*-1

        
        # Reset roundwise values and loop until abs(DV) == theta
        nRoundSamples.safe <- 0
        samples.safe <- 0
        nRoundSamples.risky <- 0
        samples.risky <- 0
        

      }
      
      if (DV>0){
        choice[i] <- 1 #safe option chosen
      } else {
        choice[i] <- 0 #risky option chosen
      }
    }
    choice.store[k,j] <- mean(choice)
  }
  }
  problem.store[[m]] <- choice.store
  print(paste("\u2713 Problem No. ", m, " finished!"))
}

# store results
for (set in 1:length(problem.store)){ 
  dimnames(problem.store[[set]]) <- list(c("1", "3", "5", "10"), c(".1", ".3", ".5", ".7", ".9"))
  problem.store[[set]] <- as.data.frame(problem.store[[set]])
  
  problem.store[[set]] <- problem.store[[set]] %>% 
    mutate(theta = c(1, 3, 5, 10),
           problem = set) %>% 
    pivot_longer(names_to = "psi", values_to = "prop", cols = `.1`:`.9`) %>% 
    select(problem, theta, psi, prop)
}

results <- bind_rows(problem.store)
problems <- problems %>% mutate(problem = row_number())
data <- left_join(problems, results, by=join_by(problem)) %>% 
  select(problem, everything())
#write_rds(data, "supplements/risk attitudes/rerun/results_comparison_rerun_lite.rds")

# plot results 

## maximization
data %>% 
  mutate(maxprop = if_else(better_ev == "safe", prop, 1-prop)) %>% 
  group_by(theta, psi) %>% 
  summarise(rate = mean(maxprop)) %>% 
  mutate(theta = as.factor(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico_d(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  # scale_y_continuous(limits = c(.4, .6), breaks = seq(.4, .6, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% EV Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()


data %>% 
  mutate(maxprop = if_else(better_ev == "safe", prop, 1-prop)) %>% 
  group_by(theta, psi, rare) %>% 
  summarise(rate = mean(maxprop)) %>% 
  mutate(theta = as.factor(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_wrap(~rare, nrow=3) + 
  scale_color_scico_d(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  #scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% EV Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) 


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
  labs(title = "Roundwise Comparison", 
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
  facet_wrap(~rare) + 
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


### problem-wise
data %>% 
  mutate(theta = as.factor(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(psi, prop, group = theta, color = theta)) +
  facet_wrap(~problem, nrow=6) + 
  scale_color_scico_d(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  # scale_y_continuous(limits = c(.4, .6), breaks = seq(.4, .6, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% Safe Choices",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal()
## asymmetric: 21, 22, 23, 24, 26, 27, 28, 29
##  safe outcomes remain the same in the different environments
## attractive rare event: better safe: in most cases, the safe outcome is markedly better than the probable risky outcome (in the direction of ev diff)
## attractive rare event: better risky: in most cases, the safe outcome is markedly better than the probable risky outcome (against the direction ev differences -> under low thresholds, sampling error would cause false safe choices)
## attractive rare event: from randomness to more systematic sampling error; where safe outcome is not much better, the binomial distribution is most skewed

## unattractive rare event: better safe: in most cases, the attractive common event is only slightly better than the safe outcome
## unattractive rare event: better risky: in most cases, the attractive common event is substantially better than the safe outcome

problems %>% mutate(no = row_number()) %>% View()

#test
