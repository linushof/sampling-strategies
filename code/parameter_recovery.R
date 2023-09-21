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

# simulate data 
set.seed(210923)
predictions <- sim_cases %>% mutate(
  w_high = round( (delta * p_r_high^gamma) / ( (delta*p_r_high^gamma)+(1-p_r_high)^gamma ), 2) , 
  w_low = 1 - w_high , 
  v_high = r_high^alpha , 
  v_low = r_low^alpha , 
  v_safe = safe^alpha , 
  V_safe = v_safe , 
  V_risky = (w_high * v_high) + (w_low * v_low) ,
  V_safe_scaled = V_safe^(1/alpha) , 
  V_risky_scaled = V_risky^(1/alpha) ,
  V_diff = V_safe_scaled-V_risky_scaled , 
  p_safe_risky = round(1 / ( 1 + exp(-rho*V_diff) ),2) ,
  N = 100 , 
  safe_total = rbinom(n=nrow(sim_cases), size=N, prob=p_safe_risky) , 
  safe_perc = safe_total/N
)

# check CPT behavior 

## sensibility of (aggregated choices) to difference in Valuation depending on choice sensitivity parameter
predictions %>% 
  ggplot(aes(x = V_diff, y = safe_perc)) + 
  geom_jitter(alpha = .05) +
  scale_x_continuous(limits = c(-3, 3)) +
  facet_wrap(~rho) + 
  theme_minimal()

## parameter recovery 




## posterior predictive check ---------------------------------------------------------------------



