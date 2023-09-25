# load packages
pacman::p_load(tidyverse , 
               R2jags ,
               digest , 
               viridis , 
               ggbeeswarm, 
               scico)

# read data and source scripts
problems <- read_rds("data/choice_problems.rds")
cpt <- read_rds("data/cpt_estimates.rds")
source("code/helper_functions/fun_initialize_MCMC.R") # call function creating initial values for MCMC

# specify parameter values for simulation (rho -> based on estimates from model fits)
cases <- cpt %>% filter(model == "summary" & threshold == "relative" & parameter == "rho")
rho_estimates <- sort(cases$mean)
summary(rho_estimates)
params <- expand_grid(rho.sim = seq(.18, 1.2, length.out = 5) ,  
                      alpha.sim = seq(.5, 1.5, length.out = 5) ,
                      gamma.sim = seq(.5, 1.5, length.out = 5) ,
                      delta.sim = seq(.5, 1.5, length.out = 5)
)
params <- params %>% mutate(set_no = row_number()) %>% select(set_no, everything())
problems <- problems %>% mutate(problem_no = row_number()) %>% select(problem_no, everything())
sim_cases <- expand_grid(params, problems) 

# simulate CPT choice behavior, given specified parameter values and original problems 
set.seed(210923)
N <- 200
predictions <- sim_cases %>% mutate(
  w_high = round( (delta.sim * p_r_high^gamma.sim) / ( (delta.sim*p_r_high^gamma.sim)+(1-p_r_high)^gamma.sim ), 2) , 
  w_low = 1 - w_high , 
  v_high = r_high^alpha.sim , 
  v_low = r_low^alpha.sim , 
  v_safe = safe^alpha.sim , 
  V_safe = v_safe , 
  V_risky = (w_high * v_high) + (w_low * v_low) ,
  V_safe_scaled = V_safe^(1/alpha.sim) , 
  V_risky_scaled = V_risky^(1/alpha.sim) ,
  V_diff = V_safe_scaled-V_risky_scaled , 
  p_safe_risky = round(1 / ( 1 + exp(-rho.sim*V_diff) ),2) ,
  N = N , 
  safe_total = rbinom(n=nrow(sim_cases), size=N, prob=p_safe_risky) , 
  safe_perc = safe_total/N
)

# Fit CPT models to test parameter recovery 

## group choices by generative CPT parameter combinations
params_sim <- predictions %>% distinct(rho.sim, alpha.sim, gamma.sim, delta.sim)
choices_grouped <- vector("list", nrow(params_sim))
for(set in seq_len(nrow(params_sim))){
  choices_grouped[[set]] <- predictions %>%
    filter(rho.sim == params_sim[[set, "rho.sim"]] & alpha.sim == params_sim[[set, "alpha.sim"]] & gamma.sim == params_sim[[set, "gamma.sim"]] & delta.sim == params_sim[[set, "delta.sim"]]) %>%
    mutate(i = row_number()) # assign trial numbers
}

## allocate space for JAGS output
estimates_cpt <- vector("list", nrow(params_sim)) # posterior summary and MCMC diagnostics
posterior_cpt <- vector("list", nrow(params_sim)) # full posterior

## fit models
params_cpt <- c("alpha", "gamma", "delta", "rho")
for(set in seq_len(nrow(params_sim))){
  
  ### get choices associated with distinct CPT parameter combinations
  current_trials <- list(N = N , 
                         safe_total = choices_grouped[[set]]$safe_total ,
                         r_low = choices_grouped[[set]]$r_low ,
                         r_high = choices_grouped[[set]]$r_high ,
                         safe = choices_grouped[[set]]$safe ,
                         p_r_low = choices_grouped[[set]]$p_r_low , 
                         p_r_high = choices_grouped[[set]]$p_r_high ,
                         start = min(choices_grouped[[set]]$i) ,
                         stop = max(choices_grouped[[set]]$i))
  
  ### sample from posterior distributions using MCMC
  current_sample <- jags.parallel(data = current_trials ,
                                  parameters.to.save = params_cpt ,
                                  model.file = "code/CPT_model_recovery.txt" ,
                                  n.chains = 4 ,
                                  n.iter = 20000 ,
                                  n.burnin = 10000 ,
                                  n.cluster = 10 , # run chains on different cores
                                  DIC = TRUE ,
                                  jags.seed = 220923)
  
  ### store posterior summary and MCMC diagnostics
  current_summary <- current_sample$BUGSoutput$summary %>% as_tibble(rownames = "parameter")
  estimates_cpt[[set]] <- expand_grid(params_sim[set, ], current_summary)
  
  ## store full posterior
  current_posterior <- current_sample$BUGSoutput$sims.matrix %>% as_tibble()
  posterior_cpt[[set]] <- expand_grid(params_sim[set, ], current_posterior)
  
  ## progress status
  print(paste("\u2713 Parameter Set No. ", set, " estimated!"))
}

estimates_cpt <- estimates_cpt %>% bind_rows()
posterior_cpt <- posterior_cpt %>% bind_rows()
write_rds(estimates_cpt, "supplements/recovery_estimates.rds")
write_rds(posterior_cpt, "supplements/recovery_posteriors.rds")

# results 
estimates <- read_rds("supplements/recovery_estimates.rds")
round(max(estimates$Rhat), 3) # 1.006
min(estimates$n.eff) # 2000

dat <- estimates %>% 
  select(rho.sim:mean) %>%  
  pivot_wider(names_from = "parameter", values_from = "mean") %>% 
  select(-deviance) %>% 
  mutate(across(alpha:rho, ~ round(., 2)))

# gamma
sims <- expand_grid(gamma = unique(dat$gamma.sim) , 
                    rho.sim = unique(dat$rho.sim)) %>%
  mutate(gamma.sim = gamma)

## recovered gamma values across other CPT parameters
dat %>% 
  ggplot(aes(x=gamma.sim, y=gamma, group=gamma.sim)) + 
  facet_wrap(~rho.sim, nrow = 1, labeller = "label_both") +
  geom_violin(width = .5) + 
  geom_jitter(width = .1, alpha = .5, color = "gray") + 
  geom_boxplot(width = .1, linewidth = .8) +
  geom_point(data=sims, color = "red", size = 2) + 
  scale_x_continuous(breaks = unique(dat$gamma.sim)) + 
  scale_y_continuous(breaks = unique(dat$gamma.sim)) + 
  theme_minimal() + 
  labs(title = "Parameter Recovery for Gamma", 
       x = "Simulated Parameter Value", 
       y = "Recovered Parameter Value")
ggsave("supplements/figures/gamma1.png", width = 14, height = 6)

## recovered gamma values conditional on other CPT parameters 
dat %>% 
  ggplot(aes(x=gamma.sim, y=gamma)) +
  facet_grid(delta.sim~alpha.sim~rho.sim, labeller = "label_both") +
  geom_abline(intercept = 0, slope = 1, linewidth = 1) + 
  geom_line(color = "darkorchid1", linewidth=1) + 
  geom_point(size = 3, color = "darkorchid1") + 
  scale_x_continuous(breaks = unique(dat$gamma.sim)) + 
  scale_y_continuous(breaks = unique(dat$gamma.sim)) + 
  theme_minimal() + 
  labs(title = "Parameter Recovery for Gamma", 
       x = "Simulated Parameter Value", 
       y = "Recovered Parameter Value")
ggsave("supplements/figures/gamma2.png", width = 10, height = 18)

# delta 
sims <- expand_grid(delta = unique(dat$delta.sim) , 
                    rho.sim = unique(dat$rho.sim)) %>%
  mutate(delta.sim = delta)

## recovered delta values across other CPT parameters
dat %>% 
  ggplot(aes(x=delta.sim, y=delta, group=delta.sim)) + 
  facet_wrap(~rho.sim, nrow = 1, labeller = "label_both") +
  geom_violin(width = .5) + 
  geom_jitter(width = .1, alpha = .5, color = "gray") + 
  geom_boxplot(width = .1, linewidth = .8) +
  geom_point(data=sims, color = "red", size = 2) + 
  scale_x_continuous(breaks = unique(dat$delta.sim)) + 
  scale_y_continuous(breaks = unique(dat$delta.sim)) + 
  theme_minimal() + 
  labs(title = "Parameter Recovery for Delta", 
       x = "Simulated Parameter Value", 
       y = "Recovered Parameter Value")
ggsave("supplements/figures/delta1.png", width = 14, height = 6)

## recovered delta values conditional on other CPT parameters 
dat %>% 
  ggplot(aes(x=delta.sim, y=delta)) +
  facet_grid(gamma.sim~alpha.sim~rho.sim, labeller = "label_both") +
  geom_abline(intercept = 0, slope = 1, linewidth = 1) + 
  geom_line(color = "darkorchid1", linewidth=1) + 
  geom_point(size = 3, color = "darkorchid1") + 
  scale_x_continuous(breaks = unique(dat$delta.sim)) + 
  scale_y_continuous(breaks = unique(dat$delta.sim)) + 
  theme_minimal() + 
  labs(title = "Parameter Recovery for Delta", 
       x = "Simulated Parameter Value", 
       y = "Recovered Parameter Value")
ggsave("supplements/figures/delta2.png", width = 10, height = 18)

# alpha
sims <- expand_grid(alpha = unique(dat$alpha.sim) , 
                    rho.sim = unique(dat$rho.sim)) %>%
  mutate(alpha.sim = alpha)

## recovered alpha values across other CPT parameters
dat %>% 
  ggplot(aes(x=alpha.sim, y=alpha, group=alpha.sim)) + 
  facet_wrap(~rho.sim, nrow = 1, labeller = "label_both") +
  geom_violin(width = .25) + 
  geom_jitter(width = .1, alpha = .5, color = "gray") + 
  geom_boxplot(width = .1, linewidth = .8) +
  geom_point(data=sims, color = "red", size = 2) + 
  scale_x_continuous(breaks = unique(dat$alpha.sim)) + 
  scale_y_continuous(breaks = unique(dat$alpha.sim)) + 
  theme_minimal() + 
  labs(title = "Parameter Recovery for Alpha", 
       x = "Simulated Parameter Value", 
       y = "Recovered Parameter Value")
ggsave("supplements/figures/alpha1.png", width = 14, height = 6)


## recovered alpha values conditional on other CPT parameters 
dat %>% 
  ggplot(aes(x=alpha.sim, y=alpha)) +
  facet_grid(gamma.sim~delta.sim~rho.sim, labeller = "label_both") +
  geom_abline(intercept = 0, slope = 1, linewidth = 1) + 
  geom_line(color = "darkorchid1", linewidth=1) + 
  geom_point(size = 3, color = "darkorchid1") + 
  scale_x_continuous(breaks = unique(dat$alpha.sim)) + 
  scale_y_continuous(breaks = unique(dat$alpha.sim)) + 
  theme_minimal() + 
  labs(title = "Parameter Recovery for Alpha", 
       x = "Simulated Parameter Value", 
       y = "Recovered Parameter Value")
ggsave("supplements/figures/alpha2.png", width = 10, height = 18)


# parameter recovery simulation ------------------------------------------

# specify parameter values for simulation
cases <- cpt %>% filter(model == "summary" & threshold == "relative" & parameter == "rho")
rho_estimates <- sort(cases$mean) # means of posterior distributions of choice consistency parameter rho
params <- expand_grid(rho = unique(round(rho_estimates, 2)) , 
                     alpha = seq(.5, 1.5, length.out = 5) ,
                     gamma = seq(.5, 1.5, length.out = 5) ,
                     delta = seq(.5, 1.5, length.out = 5)
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


# posterior predictive check ---------------------------------------------------------------------



