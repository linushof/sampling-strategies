pacman::p_load(tidyverse, scico, readxl)
problems <- read_rds("rerun/data/choice_problems_balanced.rds")

# Define tested parameter values
psi.store <- seq(.1, 1, .1) 
theta.store <- seq(1, 10, 1)

# list entry for each problem
nProblems <- nrow(problems)

## choices
problem.store.choices <- vector("list", nProblems) 
choice.store <- matrix(NA, ncol=length(psi.store),nrow = length(theta.store))

## samples
problem.store.samples <- vector("list", nProblems) 
sample.store <- matrix(NA, ncol=length(psi.store),nrow = length(theta.store))


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
    
    nRuns <- 10000
    choice <- vector(length = nRuns)
    samples <- vector(length = nRuns)
    
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
          #switch <- sample(1:0, size=1,replace=T, prob = c(psi, 1-psi))
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
          #switch <- sample(1:0, size=1,replace=T, prob = c(psi, 1-psi))
          if (switch==1){
            currentOption <- abs(currentOption-1) #after switch: currentOption == initial.option
          } else {
            currentOption <- currentOption
          }
        }
        
        # Make roundwise comparison
        DV <- DV + (((samples.safe/nRoundSamples.safe)-(samples.risky/nRoundSamples.risky))>0)*1 + (((samples.safe/nRoundSamples.safe)-(samples.risky/nRoundSamples.risky))<0)*-1
        #print(paste(m, i, theta, psi, initial.option, nRoundSamples.safe, nRoundSamples.risky)) 
        
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
      
      samples[i] <- nSamples.safe + nSamples.risky
      
    }
    choice.store[k,j] <- mean(choice)
    sample.store[k,j] <- mean(samples)
  }
  }
  problem.store.choices[[m]] <- choice.store
  problem.store.samples[[m]] <-  sample.store
  print(paste("\u2713 Problem No. ", m, " finished!"))
}

# store results


## choices
for (set in 1:length(problem.store.choices)){ 
  dimnames(problem.store.choices[[set]]) <- list(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                         c(".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1"))
  problem.store.choices[[set]] <- as.data.frame(problem.store.choices[[set]])
  
  problem.store.choices[[set]] <- problem.store.choices[[set]] %>% 
    mutate(theta = seq(1, 10, 1),
           problem = set) %>% 
    pivot_longer(names_to = "psi", values_to = "prop", cols = `.1`:`1`) %>% 
    select(problem, theta, psi, prop)
}
results.choices <- bind_rows(problem.store.choices)

## samples
for (set in 1:length(problem.store.samples)){ 
  dimnames(problem.store.samples[[set]]) <- list(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                                 c(".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1"))
  problem.store.samples[[set]] <- as.data.frame(problem.store.samples[[set]])
  
  problem.store.samples[[set]] <- problem.store.samples[[set]] %>% 
    mutate(theta = seq(1, 10, 1),
           problem = set) %>%
    pivot_longer(names_to = "psi", values_to = "samples", cols = `.1`:`1`) %>% 
    select(problem, theta, psi, samples)
}
results.samples <- bind_rows(problem.store.samples)

results <- left_join(results.choices, results.samples, by=join_by(problem, theta, psi))
problems <- problems %>% mutate(problem = row_number())
data <- left_join(problems, results, by=join_by(problem)) %>% 
  select(problem, everything())

write_rds(data, "rerun/data/rerun_lite_results_roundwise.rds")
