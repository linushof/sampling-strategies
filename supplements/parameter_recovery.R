# load packages
pacman::p_load(tidyverse , 
               R2jags ,
               digest , 
               viridis , 
               ggbeeswarm, 
               scico, 
               patchwork)

# read data and source scripts
problems <- read_rds("data/choice_problems.rds")
cpt <- read_rds("data/cpt_estimates.rds")
choices <- read_rds("data/choice_data.rds.bz2")
source("code/helper_functions/fun_initialize_MCMC.R") # call function creating initial values for MCMC

# CPT parameter recovery simulation ------------------------------------------

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



# CPT generative simulation ---------------------------------------------------

# specify parameter values for simulation
cases <- cpt %>% filter(model == "summary" & threshold == "relative" & parameter == "rho")
rho_estimates <- sort(cases$mean) # means of posterior distributions of choice consistency parameter rho
params <- expand_grid(rho.sim = unique(round(rho_estimates, 2)) , 
                     alpha.sim = seq(.5, 1.5, length.out = 5) ,
                     gamma.sim = seq(.5, 1.5, length.out = 5) ,
                     delta.sim = seq(.5, 1.5, length.out = 5)
                     )
params <- params %>% mutate(set_no = row_number()) %>% select(set_no, everything()) 
problems <- problems %>% mutate(problem_no = row_number()) %>% select(problem_no, everything())
sim_cases <- expand_grid(params, problems) 

# simulate data 
set.seed(250923)
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
  N = 100 , 
  safe_total = rbinom(n=nrow(sim_cases), size=N, prob=p_safe_risky) , 
  safe_perc = safe_total/N
)

# results 

## mapping CPT valuations onto choice proportions
predictions %>% 
  ggplot(aes(x = V_diff, y = safe_perc, color = rho.sim)) + 
  facet_wrap(~rho.sim, labeller = "label_both") + 
  geom_jitter(alpha = .05) +
  scale_x_continuous(limits = c(-5, 5)) +
  scale_color_scico(palette = "acton") + 
  labs(title = "Predicted Safe Choices as a Function CPT Valuations" , 
       y = "Proportion of Safe Choices" , 
       x = "CPT(Safe) - CPT(Risky)") + 
  theme_minimal()
ggsave("supplements/figures/CPT_pred_1.png", width = 12, height = 8)

## mapping CPT valuations onto choice proportions

### gamma, delta 

evmax_gamma_delta <- predictions %>% 
  mutate(ev_max = ifelse(ev_ratio < 0, safe_perc, 1 - safe_perc)) %>%
  group_by(rho.sim, gamma.sim, delta.sim) %>% 
  summarize(mean_ev_max = round( mean(ev_max), 2 ))

evmax_gamma_delta %>% 
  ggplot(aes(x = gamma.sim, y = mean_ev_max, group = as.factor(delta.sim), color = as.factor(delta.sim))) +
  facet_wrap(~rho.sim, labeller = "label_both") + 
  geom_point(size = 3) + 
  geom_line(size = 1) + 
  scale_color_viridis_d() + 
  theme_minimal() + 
  labs(title = "EV Maximization as a Function of Gamma, Delta and Rho", 
       x = "Simulated Gamma", 
       y = "Proportions of EV Maximization", 
       color = "Simulated\nDelta")
ggsave("supplements/figures/CPT_pred_2.png", width = 12, height = 8)

evmax_gamma_delta %>% 
  ggplot(aes(x = delta.sim, y = mean_ev_max, group = as.factor(gamma.sim), color = as.factor(gamma.sim))) +
  facet_wrap(~rho.sim, labeller = "label_both") + 
  geom_point(size = 3) + 
  geom_line(size = 1) + 
  scale_color_viridis_d() + 
  theme_minimal() + 
  labs(title = "EV Maximization as a Function of Gamma, Delta and Rho", 
       x = "Simulated Delta", 
       y = "Proportions of EV Maximization", 
       color = "Simulated\nGamma")
ggsave("supplements/figures/CPT_pred_3.png", width = 12, height = 8)

### alpha

evmax_alpha <- predictions %>% 
  mutate(ev_max = ifelse(ev_ratio < 0, safe_perc, 1 - safe_perc)) %>%
  group_by(rho.sim, alpha.sim) %>% 
  summarize(mean_ev_max = round( mean(ev_max), 2 ))

evmax_alpha %>% 
  ggplot(aes(x = alpha.sim, y = mean_ev_max)) +
  facet_wrap(~rho.sim, labeller = "label_both") + 
  geom_point(size = 2) + 
  geom_line(size = 1) + 
  theme_minimal() + 
  labs(title = "EV Maximization as a Function of Alpha and Rho", 
       x = "Simulated Alpha", 
       y = "Proportions of EV Maximization")
ggsave("supplements/figures/CPT_pred_4.png", width = 12, height = 8)

# posterior predictive check ---------------------------------------------------------------------

cpt_clean <- cpt %>%  
  select(model:mean) %>%
  filter(parameter != "deviance") %>% 
  pivot_wider(names_from = parameter, values_from = mean) %>% 
  mutate(across(alpha:rho, ~round(., 2)))

ppset <- choices %>%
  left_join(cpt_clean, by = c("model", "psi", "threshold", "theta")) %>% 
  filter(!c(n_s == 0 | n_r == 0)) %>% 
  mutate(choice_obs = if_else(choice == "s", 1,0))

## generate posterior predictions 

set.seed(114981)
postpred <- ppset %>% mutate(
  w_high = round( (delta * ep_r_high^gamma) / ( (delta*ep_r_high^gamma)+(1-ep_r_high)^gamma ), 2) , 
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
  choice_pp = rbinom(n=nrow(ppset), size=1, prob=p_safe_risky))
write_rds(postpred, "supplements/posterior_predictives.rds.bz2", compress = "bz2")

## results

postpred <- read_rds("supplements/posterior_predictives.rds.bz2")
postpred <- postpred %>% 
  select(model, psi, threshold, theta, alpha, gamma, delta, rho, problem, agent, choice_obs, choice_pp )
postpred_results <- postpred %>% 
  mutate(match = if_else(choice_obs == choice_pp, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, alpha, gamma, delta, rho, match) %>% 
  summarise(count = n()) %>% 
  mutate(perc = round(count/sum(count), 3)) %>% 
  filter(match != 0) %>% 
  ungroup()


### predictive accuracy

postpred_results %>%  ggplot(aes(x = rho, y = perc, color = psi)) + 
  facet_grid(threshold~model) + 
  geom_hline(yintercept = c(.5, .55, .9), linetype = "dashed") + 
  scale_y_continuous(limits = c(.5,1), breaks = seq(.5,1,.1)) + 
  geom_point(size = 3) + 
  labs(title = "Posterior Predictive Accuracy of CPT", 
       x = "Choice Consistency" , 
       y = "Proportion of Correct Predictions", 
       color = "Switching\nProbability") + 
  scale_color_viridis(option = "D") + 
  theme_minimal()
ggsave("supplements/figures/post_pred_1.png", width = 10, height = 8)

### qualitative patterns (maximization rates)

#### prepare data (compute maximization rates)
rates_pp <- postpred %>%
  mutate(norm = case_when(mean_r/safe > 1 ~ 0, 
                          mean_r/safe < 1 ~ 1)) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice_pp, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0))

rates_obs <- postpred %>%
  mutate(norm = case_when(mean_r/safe > 1 ~ 0, 
                          mean_r/safe < 1 ~ 1)) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice_obs, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0))


## plot data 

### summary
rates_obs_sr <- rates_obs %>%
  filter(model == "summary" & threshold == "relative")
max_summary <- rates_pp %>%
  filter(model == "summary" & threshold == "relative") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Switching Probability\n(Search Rule)",
       y = "Proportion of Choices\nMaximizing the Sampled Average",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  geom_point(data = rates_obs_sr, shape = 17, size = 3) + 
  geom_line(data = rates_obs_sr, linewidth = 1) + 
  theme_minimal()

### round-wise
rates_obs_rr <- rates_obs %>%
  filter(model == "roundwise" & threshold == "relative")
max_roundwise <- rates_pp %>%
  filter(model == "roundwise" & threshold == "relative") %>% 
  # filter(psi > .9 | psi == .5 | psi == (1-.9)) %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
  scale_color_scico_d(palette = "imola", alpha = .7) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Switching Probability\n(Search Rule)",
       y = "Proportion of Choices\nMaximizing the Sampled Average",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  geom_point(data = rates_obs_rr, shape = 17, size = 3) + 
  geom_line(data = rates_obs_rr, linewidth = 1) + 
  theme_minimal()

max_summary + max_roundwise + plot_annotation(tag_levels = "A")
ggsave("supplements/figures/post_pred_2.png", width = 14, height = 6)

