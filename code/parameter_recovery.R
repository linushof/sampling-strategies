# packages
pacman::p_load(tidyverse, viridis)

# data
cpt <- read_rds("data/cpt_estimates.rds")
problems <- read_rds("data/choice_problems.rds")

# parameter recovery simulation ------------------------------------------

# specify parameter values for simulation
cases <- cpt %>% filter(model == "summary" & threshold == "relative" & parameter == "rho")
rho_estimates <- sort(cases$mean) # means of posterior distributions of choice consistency parameter rho
params <- expand_grid(rho = unique(round(rho_estimates, 2)) , 
                     alpha = seq(.5, 1.5, length.out = 5) ,
                     gamma = seq(.5, 1.5, length.out = 5) ,
                     delta = seq(.5, 1.5, length.out = 5)
                     )
nrow(params)
params <- expand_grid(rho = seq(.2, 1, length.out = 5) , 
                      alpha = seq(.5, 1.5, length.out = 5) ,
                      gamma = seq(.5, 1.5, length.out = 5) ,
                      delta = seq(.5, 1.5, length.out = 5)
)
nrow(params)
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
  scale_x_continuous(limits = c(-5, 5)) +
  facet_wrap(~rho) + 
  theme_minimal()

predictions %>%  
  ggplot(aes(x = gamma, y = safe_perc)) + 
  geom_jitter(alpha = .05) + 
  facet_wrap(~rho) + 
  theme_minimal()
  
predictions %>% 
  mutate(EV_max_perc = ifelse(ev_ratio < 0, safe_perc, 1 - safe_perc)) %>%
  ggplot(aes(x = gamma, y = EV_max_perc)) + 
  geom_jitter(alpha = .05) + 
  facet_wrap(~rho) + 
  theme_minimal()


test <- predictions %>% 
  mutate(EV_max_perc = ifelse(ev_ratio < 0, safe_perc, 1 - safe_perc)) %>%
  group_by(rho, gamma, delta, alpha) %>% 
  summarize(mean_EV_max_perc = round( mean(EV_max_perc), 2 ))

test %>% 
  filter(alpha == 1) %>% 
  ggplot(aes(x = delta, y = mean_EV_max_perc, group = gamma, color = gamma)) + 
  geom_point(size = 2) + 
  geom_line(size = 1) + 
  facet_wrap(~rho) + 
  theme_minimal() + 
  scale_color_viridis()

test %>% 
  filter(alpha == 1) %>% 
  ggplot(aes(x = gamma, y = mean_EV_max_perc, group = delta, color = delta)) + 
  geom_point(size = 2) + 
  geom_line(size = 1) + 
  facet_wrap(~rho) + 
  theme_minimal() + 
  scale_color_viridis()

test %>% 
  filter(delta == 1 & gamma == 1) %>% 
  ggplot(aes(x = alpha, y = mean_EV_max_perc)) + 
  geom_point(size = 2) + 
  geom_line(size = 1) + 
  facet_wrap(~rho) + 
  theme_minimal()


test2 <- predictions %>% 
  group_by(rho, gamma, delta, alpha) %>% 
  summarize(mean_safe_perc = round( mean(safe_perc), 2 ))

test2 %>% 
  filter(delta == 1 & gamma == 1) %>% 
  ggplot(aes(x = alpha, y = mean_safe_perc)) + 
  geom_point(size = 2) + 
  geom_line(size = 1) + 
  facet_wrap(~rho) + 
  theme_minimal()


test2 %>% 
  filter(alpha == 1) %>% 
  ggplot(aes(x = delta, y = mean_safe_perc, group = gamma, color = gamma)) + 
  geom_point(size = 2) + 
  geom_line(size = 1) + 
  facet_wrap(~rho) + 
  theme_minimal() + 
  scale_color_viridis()

test2 %>% 
  filter(alpha == 1) %>% 
  ggplot(aes(x = gamma, y = mean_safe_perc, group = delta, color = delta)) + 
  geom_point(size = 2) + 
  geom_line(size = 1) + 
  facet_wrap(~rho) + 
  theme_minimal() + 
  scale_color_viridis()

## parameter recovery 






## posterior predictive check ---------------------------------------------------------------------



