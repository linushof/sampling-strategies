library(tidyverse)
problems <- read_rds("data/choice_problems_balanced.rds")

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

  out.risky <- c(problems[[m,"r_1"]], problems[[m,"r_2"]])
  p.risky <- c(problems[[m,"p_r_1"]], problems[[m,"p_r_2"]])
  
  
  for (k in 1:length(theta.store)){
    for (j in 1:length(psi.store)){
      psi <- psi.store[j] #Switching probability
      theta <- theta.store[k]
      nRuns <- 100000
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
          DV.risky <- DV.risky + out.risky[rbinom(1,1,p.risky[2])+1] 
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
write_rds(data, "supplements/risk attitudes/rerun/results_summary_rerun_lite.rds")



# plot results 

## maximization
data %>% 
  mutate(maxprop = if_else(max == "safe", prop, 1-prop)) %>% 
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
  theme_minimal(base_size = 20)


## risk aversion 

data %>% 
  group_by(rare, theta, psi) %>% 
  summarise(rate = mean(prop)) %>% 
  mutate(theta = as.factor(theta) , 
         psi = as.double(psi)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_wrap(~rare) + 
  scale_color_scico_d(palette = "imola", alpha = .7) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  # scale_y_continuous(limits = c(.4, .6), breaks = seq(.4, .6, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switching Probability\n(Search Rule)",
       y = "% Safe Choices",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 20)
