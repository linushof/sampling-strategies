# load packages
pacman::p_load(tidyverse , 
               R2jags ,
               digest , 
               viridis , 
               ggbeeswarm, 
               scico, 
               patchwork, 
               ggpubr, 
               papaja, 
               latex2exp)

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

### prepare data 

#### compute maximization rates (qualitative pattern)

##### sampling strategies
obs_ev_exp_max <- postpred %>%
  mutate(norm = case_when(mean_r/safe > 1 ~ 0, 
                          mean_r/safe < 1 ~ 1)) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice_obs, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0)) %>% 
  mutate(type = "Sampling Strategy")

##### CPT posterior predictive
pp_ev_exp_max <- postpred %>%
  mutate(norm = case_when(mean_r/safe > 1 ~ 0, 
                          mean_r/safe < 1 ~ 1)) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice_pp, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0)) %>% 
  mutate(type = "CPT Posterior Predictive")

##### compute proportion of correctly predicted choices on trial level
postpred_acc <- postpred %>% 
  select(model, psi, threshold, theta, alpha, gamma, delta, rho, problem, agent, choice_obs, choice_pp ) %>% 
  mutate(match = if_else(choice_obs == choice_pp, 1, 0)) %>% 
  group_by(model, psi, threshold, theta, alpha, gamma, delta, rho, match) %>% 
  summarise(count = n()) %>% 
  mutate(perc = round(count/sum(count), 3)) %>% 
  filter(match != 0) %>% 
  ungroup()

### plot data 

label_theta <- function(string) {
  TeX(paste("$\\theta=$", string, sep = ""))
}

#### summary comparison

pp_ev_exp_max_sr <- pp_ev_exp_max %>% filter(model == "summary", threshold == "relative") 
pp_max_summary <- obs_ev_exp_max %>%
  filter(model == "summary", threshold == "relative") %>% 
  ggplot(aes(psi, rate, shape = type, group = theta)) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Switching Probability\n(Search Rule)",
       y = "Proportion of\n Maximizing Choices",
       shape = "") +
  facet_wrap(~theta, nrow = 1, labeller =  labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_point(size = 2) +
  geom_point(data = pp_ev_exp_max_sr, size = 3) +
  scale_shape_manual(values = c(4, 19)) + 
  scale_color_scico(palette = "imola") +
  theme_minimal() + 
  theme_apa(base_size = 20)

pp_acc_summary <- postpred_acc %>%  
  filter(model == "summary", threshold == "relative") %>% 
  ggplot(aes(x = psi, y = perc)) + 
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_hline(yintercept = .7, linetype = "dashed") + 
  scale_y_continuous(limits = c(.5,1), breaks = seq(.5,1,.1)) + 
  geom_point(size = 3, shape = 4) + 
  labs(x = "Switching Probability\n(Search Rule)" , 
       y = "Proportion of\nCorrect Predictions", 
       color = "Choice Consistency") + 
  scale_color_viridis(option = "C") + 
  theme_apa(base_size = 20)

ggarrange(pp_max_summary, pp_acc_summary, nrow = 2, common.legend = T, legend = "right", labels = "AUTO")
ggsave(file = "manuscript/figures/posterior_predictive_summary.png", width = 14, height = 7)

#### roundwise comparison

#### summary comparison

pp_ev_exp_max_rr <- pp_ev_exp_max %>% filter(model == "roundwise", threshold == "relative") 
pp_max_roundwise <- obs_ev_exp_max %>%
  filter(model == "roundwise", threshold == "relative") %>% 
  ggplot(aes(psi, rate, shape = type, group = theta)) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Switching Probability\n(Search Rule)",
       y = "Proportion of\n Maximizing Choices",
       shape = "") +
  facet_wrap(~theta, nrow = 1, labeller =  labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_point(size = 2) +
  geom_point(data = pp_ev_exp_max_rr, size = 3) +
  scale_shape_manual(values = c(4, 19)) + 
  scale_color_scico(palette = "imola") +
  theme_minimal() + 
  theme_apa(base_size = 20)

pp_acc_roundwise <- postpred_acc %>%  
  filter(model == "roundwise", threshold == "relative") %>% 
  ggplot(aes(x = psi, y = perc)) + 
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_hline(yintercept = .7, linetype = "dashed") + 
  scale_y_continuous(limits = c(.5,1), breaks = seq(.5,1,.1)) + 
  geom_point(size = 3, shape = 4) + 
  labs(x = "Switching Probability\n(Search Rule)" , 
       y = "Proportion of\nCorrect Predictions", 
       color = "Choice Consistency") + 
  scale_color_viridis(option = "C") + 
  theme_apa(base_size = 20)

ggarrange(pp_max_roundwise, pp_acc_roundwise, nrow = 2, common.legend = T, legend = "right", labels = "AUTO")
ggsave(file = "manuscript/figures/posterior_predictive_roundwise.png", width = 14, height = 7)

# the lower psi and the higher the elevation, the more risky choices are predicted
# i.e., with low switching probabilities, small but systematic CPT bias towards the risky option 

## difference in CPT valuations

postpred %>% 
  filter(model == "summary", threshold == "relative") %>%  
  group_by(psi, theta) %>% 
  summarise(m_V_diff = mean(V_diff, na.rm = T)) %>% 
  ggplot(aes(x=psi, y = m_V_diff, color = theta, group = theta)) +
  geom_point(size = 3) + 
  geom_line(linewidth = 1) +
  scale_color_viridis() + 
  theme_minimal() + 
  scale_y_continuous(limits = c(-2,2))

## probability of choosing the safe option

postpred %>% 
  filter(model == "summary", threshold == "relative") %>%  
  group_by(psi, theta) %>% 
  summarise(m_p_safe = mean(p_safe_risky, na.rm = T)) %>% 
  ggplot(aes(x=psi, y = m_p_safe, color = theta, group = theta)) +
  geom_point(size = 3) + 
  geom_line(linewidth = 1) +
  scale_color_viridis() + 
  theme_minimal()

## proportion of risky choices

postpred %>% 
  filter(model == "summary", threshold == "relative") %>%  
  group_by(psi, theta, choice_pp) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n/sum(n), 2)) %>% 
  filter(choice_pp == 0) %>% 
  ggplot(aes(x=psi, y = prop, color = theta, group = theta)) +
  geom_point(size = 3, alpha = .5) + 
  geom_line(linewidth = 1, alpha = .5) +
  scale_color_viridis() + 
  theme_minimal() + 
  scale_y_continuous(limits = c(.3,.8))

postpred %>% 
  filter(model == "summary", threshold == "relative") %>%  
  group_by(psi, theta, choice_obs) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n/sum(n), 2)) %>% 
  filter(choice_obs == 0) %>% 
  ggplot(aes(x=psi, y = prop, color = theta, group = theta)) +
  geom_point(size = 3, alpha = .5) + 
  geom_line(linewidth = 1, alpha = .5) +
  scale_color_viridis() + 
  theme_minimal() + 
  scale_y_continuous(limits = c(.3,.8))


# test --------------------------------------------------------------------

# for low psi, bias towards safe option is larger

test <- choices %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)

p1 <- test %>% 
  ggplot(aes(x=psi, y=prop, group = norm, color = norm)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line() +
  scale_y_continuous(limits = c(.5,1))


test2 <- postpred %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(choice_pp_2 = ifelse(choice_pp == 0, "r", "s"), 
         norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice_pp_2 == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)


p2 <- test2 %>% 
  ggplot(aes(x=psi, y=prop, group = norm, color = norm)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(limits = c(.5,1))


p1 + p2 + plot_layout(nrow = 2)


test3 <- choices %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)

p3 <- test3 %>% 
  ggplot(aes(x=psi, y=prop)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line()

test4 <- postpred %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(choice_pp_2 = ifelse(choice_pp == 0, "r", "s"), 
         norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice_pp_2 == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)

p4 <- test4 %>% 
  ggplot(aes(x=psi, y=prop)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line()

p3 + p4 + plot_layout(nrow = 2)




test3 <- choices %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1) %>% 
  mutate(norm = "Free")

test5 <- choices %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)

p5 <- test5 %>% 
  ggplot(aes(x=psi, y=prop, group = norm, color = norm)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line() + 
  geom_line(data = test3) + 
  geom_point(data = test3) + 
  labs(title = "Sampling Strategy Maximization") + 
  theme_minimal()
  
p5


test4 <- postpred %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(choice_pp_2 = ifelse(choice_pp == 0, "r", "s"), 
         norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice_pp_2 == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1) %>% 
  mutate(norm = "Free")


test6 <- postpred %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(choice_pp_2 = ifelse(choice_pp == 0, "r", "s"), 
         norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice_pp_2 == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)


p6 <- test6 %>% 
  ggplot(aes(x=psi, y=prop, group = norm, color = norm)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line() + 
  geom_line(data = test4) + 
  geom_point(data = test4) + 
  labs(title = "Posterior Predictive Maximization") +
  theme_minimal()

p5 + p6 + plot_layout(nrow = 2)



cpt %>% filter(parameter == "deviance") %>% 
  group_by(model, psi, threshold, theta) %>% 
  summarize(mdev = mean) %>% 
  View()






test <- choices %>% 
  filter(model == "roundwise", threshold == "relative") %>% 
  mutate(norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)

p1 <- test %>% 
  ggplot(aes(x=psi, y=prop, group = norm, color = norm)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(limits = c(.5,1))


test2 <- postpred %>% 
  filter(model == "roundwise", threshold == "relative") %>% 
  mutate(choice_pp_2 = ifelse(choice_pp == 0, "r", "s"), 
         norm = case_when(mean_r/safe > 1 ~ "r", mean_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice_pp_2 == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  group_by(psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1)


p2 <- test2 %>% 
  ggplot(aes(x=psi, y=prop, group = norm, color = norm)) + 
  facet_wrap(~theta, nrow = 1) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous(limits = c(.5,1))


p1 + p2 + plot_layout(nrow = 2)




# test2 -------------------------------------------------------------------

cpt %>% filter(parameter == "deviance") %>% View()

choices %>% group_by(model, psi, threshold, theta, ep_r_high, choice) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  filter(model == "summary", threshold == "relative") %>% 
  ggplot(aes(x=ep_r_high, y = prop, color = choice)) +
  geom_point(alpha = .5) + 
  geom_smooth() + 
  facet_grid(psi~theta) + 
  theme_minimal()

choices %>% group_by(model, psi, threshold, theta, ep_r_low, choice) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  filter(model == "summary", threshold == "relative") %>% 
  ggplot(aes(x=ep_r_low, y = prop, color = choice)) +
  geom_point(alpha = .5) + 
  geom_smooth() + 
  facet_grid(psi~theta) + 
  theme_minimal()



choices %>% group_by(model, psi, threshold, theta, ep_r_high, choice) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  filter(model == "summary", threshold == "relative") %>% 
  View()
ggplot(aes(x=ep_r_high, y = prop, color = choice)) +
  geom_point(alpha = .5) + 
  geom_smooth() + 
  facet_grid(psi~theta) + 
  theme_minimal()


choices %>% group_by(model, psi, threshold, theta, ep_r_high, choice) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  filter(model == "summary", threshold == "relative", choice == "s") %>% 
  ggplot(aes(x=ep_r_high, y = prop)) +
  geom_density_2d_filled() + 
  facet_grid(psi~theta) + 
  theme_minimal()


choices %>% group_by(model, psi, threshold, theta, ep_r_high, choice) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  filter(model == "summary", threshold == "relative") %>% 
  ggplot(aes(x=ep_r_high, y = prop, color = choice)) +
  geom_jitter(alpha = .5) + 
  facet_grid(psi~theta)


choices %>% group_by(model, psi, threshold, theta, ep_r_high, choice) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  filter(model == "summary", threshold == "relative", choice == "r") %>% 
  ggplot(aes(x=ep_r_high, y = prop)) +
  geom_jitter(alpha = .5) + 
  facet_grid(psi~theta)


choices %>% group_by(model, psi, threshold, theta, choice) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  filter(model == "summary", threshold == "relative") %>% 
  View()
ggplot(aes(x=choice, y = prop)) +
  geom_bar(stat = "identity") + 
  facet_grid(psi~theta)
















