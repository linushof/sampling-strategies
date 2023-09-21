# packages
pacman::p_load(tidyverse)

# data
cpt <- read_rds("data/cpt_estimates.rds")
problems <- read_rds("data/choice_problems.rds")

# parameter recovery simulation 

consistencies <- cpt %>% filter(model == "summary" & threshold == "relative" & parameter == "rho")
consistencies


# simulate data 

## CPT core model 

### weighting function
w_high <- round(  (delta * p_r_high^gamma)/ ((delta * p_r_high^gamma)+(1-p_r_high)^gamma), 2)
w_low <- 1 - w_high

### value function
v_high <- r_high^alpha
v_low <- r_low^alpha 
v_safe <- safe^alpha

### valuations
V_safe <- v_safe
V_risky <- (w_high * v_high) + (w_low * v_low)
  
## choice rule
p_safe <- 1 / ( 1 + exp(-rho*(V_safe - V_risky)) )

## predict choices
choice <- rbinom(p_safe) # safe option is 1 


# posterior predictive check

