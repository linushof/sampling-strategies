# packages
pacman::p_load(tidyverse)

# data
cpt <- read_rds("data/cpt_estimates.rds")
problems <- read_rds("data/choice_problems.rds")

# parameter recovery simulation ------------------------------------------

# specify parameter values for simulation
cases <- cpt %>% filter(model == "summary" & threshold == "relative" & parameter == "rho")
rho_estimates <- sort(cases$mean) # means of posterior distributions of choice consistency parameter rho
params <- expand_grid(rho = unique(round(rho_estimates, 2)) , 
                     alpha = seq(0, 2, length.out = 5) ,
                     gamma = seq(0, 2, length.out = 5) ,
                     delta = seq(0, 10, length.out = 5)
                     )
params <- params %>% mutate(set_no = row_number()) %>% select(set_no, everything()) 
problems <- problems %>% mutate(problem_no = row_number()) %>% select(problem_no, everything())

sim_cases <- expand_grid(params, problems) 
sim_cases <- sim_cases %>% mutate(safe_total = NA)

N <- 10 # synthetic agents 


# simulation 
set.seed(51712)
for(i in seq_len(nrow(sim_cases))){
    
    # CPT core model 
    
    ## weighting function
    w_high <- round(  (sim_cases[[i, "delta"]] * sim_cases[[i, "p_r_high"]]^sim_cases[[i, "gamma"]]) / 
                        ((sim_cases[[i, "delta"]] * sim_cases[[i, "p_r_high"]]^sim_cases[[i, "gamma"]])+(1-sim_cases[[i, "p_r_high"]])^sim_cases[[i, "gamma"]]), 2)
    w_low <- 1 - w_high
      
    ## value function
    v_high <- sim_cases[[i, "r_high"]]^sim_cases[[i, "alpha"]]
    v_low <- sim_cases[[i, "r_low"]]^sim_cases[[i, "alpha"]] 
    v_safe <- sim_cases[[i, "safe"]]^sim_cases[[i, "alpha"]]
      
    ## valuations
    V_safe <- v_safe
    V_risky <- (w_high * v_high) + (w_low * v_low)
      
    ## choice rule
    p_safe_risky <- 1 / ( 1 + exp(-sim_cases[[i, "rho"]]*(V_safe - V_risky)) )
      
    ## predict choices
    sim_cases[[i, safe_total]] <- rbinom(n=1, size=N, prob=p_safe_risky) # safe option is 1 
    
}

# posterior predictive check ----------------------------------------------


