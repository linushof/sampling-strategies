# Preparation -------------------------------------------------------------

# load packages
pacman::p_load(tidyverse , 
               R2jags , 
               digest , 
               readxl ,
               scico , # for scientific color palettes
               latex2exp , # for LaTeX expressions in plots
               patchwork)

# load data
problems <- as.data.frame(read_xlsx("data/choice_problems.xlsx"))
choices <- read_rds("data/choice_data.rds.bz2") 


# prepare data for CPT model (requires rank-ordered outcomes and probabilities)
choices <- left_join(choices, problems, by=join_by(id)) %>% 
  mutate(choice_r = if_else(choice == "r", 1, 0) , # predict choice of risky option
         r_low = if_else(r_1 < r_2, r_1, r_2) ,
         r_high = if_else(r_1 > r_2, r_1, r_2) ,         
         sp_r_low = if_else(r_low == r_1, sp_r_1, sp_r_2) , 
         sp_r_high = if_else(r_high == r_1, sp_r_1, sp_r_2)) %>% 
  filter(model == "roundwise")

# to model choices of each strategy separately, group choices
params_sim <- choices %>% distinct(model, psi, theta) # retrieve all strategies
choices_grouped <- vector("list", nrow(params_sim))
for(set in seq_len(nrow(params_sim))){
  choices_grouped[[set]] <- choices %>%
    filter(model == params_sim[[set, "model"]] & psi == params_sim[[set, "psi"]] & theta == params_sim[[set, "theta"]]) %>%
    mutate(i = row_number()) # assign trial numbers for JAGS loop
}

# allocate space for JAGS output
estimates_cpt <- vector("list", nrow(params_sim)) # posterior summary and MCMC diagnostics
posterior_cpt <- vector("list", nrow(params_sim)) # all posterior samples

# Model fitting -----------------------------------------------------------

# JAGS settings
params_cpt <- c("alpha", "gamma", "delta", "rho") # free parameters
source("code/helper_functions/fun_initialize_MCMC.R") # calls function to create starting values for MCMC

# loop over the different sampling strategies
for(set in seq_len(nrow(params_sim))){
  
  # get trial data
  current_trials <- list(choice = choices_grouped[[set]]$choice_r ,
                         r_low = choices_grouped[[set]]$r_low ,
                         r_high = choices_grouped[[set]]$r_high ,
                         safe = choices_grouped[[set]]$safe ,
                         ep_r_low = choices_grouped[[set]]$sp_r_low ,
                         ep_r_high = choices_grouped[[set]]$sp_r_high ,
                         start = min(choices_grouped[[set]]$i) ,
                         stop = max(choices_grouped[[set]]$i)
  )
  
  ## sample from posterior distributions using MCMC
  current_sample <- jags.parallel(data = current_trials ,
                                  inits = inits_MCMC ,
                                  parameters.to.save = params_cpt ,
                                  model.file = "code/CPT_model.txt" ,
                                  n.chains = 6 ,
                                  n.iter = 1000 ,
                                  n.burnin = 500 ,
                                  n.thin = 1 ,
                                  n.cluster = 6 , # compute MCMC chains in parallel
                                  DIC = TRUE ,
                                  jags.seed = 83176)
  
  ## posterior summary and MCMC diagnostics
  current_summary <- current_sample$BUGSoutput$summary %>% as_tibble(rownames = "parameter")
  estimates_cpt[[set]] <- expand_grid(params_sim[set, ], current_summary)
  
  ## full posterior
  current_posterior <- current_sample$BUGSoutput$sims.matrix %>% as_tibble()
  posterior_cpt[[set]] <- expand_grid(params_sim[set, ], current_posterior)
  
  ## status
  print(paste("\u2713 Parameter Set No. ", set, " estimated!"))
} # close strategy loop

cpt_gamma <- estimates_cpt %>% bind_rows()
cpt_gamma_posterior <- posterior_cpt %>% bind_rows()

# Storage -----------------------------------------------------------------

# write_rds(cpt_gamma, "supplements/supp_gamma_estimates.rds")
# write_rds(posterior_cpt, "supplements/supp_gamma_posteriors.rds.bz2", compress = "bz2") 


# Analysis ----------------------------------------------------------------

cpt_gamma <- readRDS("supplements/gamma/supp_gamma_estimates.rds")

label_theta <- function(string) {
  TeX(paste("$\\theta=$", string, sep = ""))
}


label_psi <- function(string) {
  TeX(paste("$\\psi=$", string, sep = ""))
}


## convergence --------------------------------------------------------------

max(cpt_gamma$Rhat)
min(cpt_gamma$n.eff) 


## weighting function ---------------------------------------------------------

# prepare data
weights <- cpt_gamma %>%
  select(model, psi, theta, parameter, mean) %>%
  pivot_wider(names_from = parameter, values_from = mean) %>% 
  select(-c(alpha, rho)) %>%
  expand_grid(p = seq(0, 1, .05)) %>% # create vector of sampled relative frequencies
  mutate(w = round(  (delta * p^gamma)/ ((delta * p^gamma)+(1-p)^gamma), 2)) # compute decision weights (see Goldstein & Einhorn, 1987) using the parameter estimates  

#### gamma
gamma_roundwise <- cpt_gamma %>%
  filter(parameter == "gamma", !(psi == 1 & theta ==1)) %>%
  ggplot(aes(psi, mean, color = psi)) + 
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  labs(x = "Switching Probability (Search Rule)", 
       y = expression(paste("Curvature  ", gamma)),
       color = "Switching\nProbability") +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_minimal()

#### delta
delta_roundwise <- cpt_gamma %>%
  filter(parameter == "delta", !(psi == 1 & theta ==1)) %>%
  ggplot(aes(psi, mean, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) +
  scale_y_continuous(limits = c(-0.1, 5.1), breaks = seq(0,5, length.out = 3)) +
  geom_hline(yintercept = 1) + 
  labs(x = "Switching Probability (Search Rule)", 
       y = expression(paste("Elevation  ", delta)), 
       color = "Switching\nProbability") +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_minimal()

####  probability weighting
wf_roundwise <- weights %>% 
  filter(!(psi == 1 & theta ==1)) %>%
  ggplot(aes(p, w, group = psi, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  scale_x_continuous(breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 3)) +
  geom_line(linewidth = 1) + 
  labs(x = TeX("$\\p_{high}$"),
       y = TeX("$w(\\p_{high})$"),
       color = "Switching\nProbability") +
  theme_minimal()

# merge and save plots

wf_roundwise + gamma_roundwise + delta_roundwise + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
#ggsave(file = "supplements/gamma/figures/supp_gamma_weighting.png", width = 14, height = 10)

## value function ---------------------------------------------------------

# prepare data

values <- cpt_gamma %>%
  select(model, psi, theta, parameter, mean) %>%
  pivot_wider(names_from = parameter, values_from = mean) %>%
  select(-c(gamma, delta, rho)) %>%
  expand_grid(x = seq(0, 20, .5)) %>%  # create vector of possible outcomes
  mutate(v = round(x^alpha, 2)) # compute subjective values on the basis of estimated parameters

## round-wise

values_roundwise <- values %>% filter(!(psi == 1 & theta == 1))

### alpha
alpha_roundwise <- cpt_gamma %>%
  filter(parameter == "alpha", !(psi == 1 & theta == 1)) %>% 
  ggplot(aes(psi, mean, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0,1, length.out = 3)) + 
  scale_y_continuous(limits = c(-0.1, 2.1), breaks = seq(0,2, length.out = 3)) + 
  labs(x = "Switching Probability (Search Rule)", 
       y = expression(paste("Outcome Sensitivity  ", alpha)),
       color = "Switching\nProbability") +
  geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_minimal()

### value function 
vf_roundwise <- values %>% 
  filter(!(psi == 1 & theta == 1)) %>% 
  ggplot(aes(x, v, group = psi, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  scale_x_continuous(limits = c(-1, 21), breaks = seq(0, 20, length.out = 3)) +
  scale_y_continuous(limits = c(-1, 100), breaks = seq(0, 100, length.out = 3)) +
  labs(x = "Sampled Outcome",
       y = "Subjective Value",
       color = "Switching\nProbability") +
  geom_line(linewidth = 1) +
  theme_minimal()

# merge and save plots
vf_roundwise + alpha_roundwise + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
#ggsave(file = "supplements/gamma/figures/supp_gamma_value", width = 14, height = 7)


## posterior predictive checks ---------------------------------------------


# pre-processing

cpt_clean <- cpt_gamma %>%  
  select(model:mean) %>%
  filter(parameter != "deviance") %>% 
  pivot_wider(names_from = parameter, values_from = mean) %>% 
  mutate(across(alpha:rho, ~round(., 2)))

ppset <- choices %>%
  left_join(cpt_clean, by = c("model", "psi", "theta")) %>% 
  filter(!c(smp_s == 0 | smp_r == 0)) %>% 
  mutate(choice_obs = if_else(choice == "s", 1,0) ,
         r_low = if_else(r_1 < r_2, r_1, r_2) ,
         r_high = if_else(r_1 > r_2, r_1, r_2) ,
         sp_r_low = if_else(r_low == r_1, sp_r_1, sp_r_2) ,
         sp_r_high = if_else(r_high == r_1, sp_r_1, sp_r_2))

# make  predictions

set.seed(54321)
postpred <- ppset %>% mutate(
  w_high = round( (delta * sp_r_high^gamma) / ( (delta*sp_r_high^gamma)+(1-sp_r_high)^gamma ), 2) , 
  w_low = 1 - w_high , 
  v_high = r_high^alpha , 
  v_low = r_low^alpha , 
  v_safe = safe^alpha , 
  V_safe = v_safe , 
  V_risky = (w_high * v_high) + (w_low * v_low) ,
  V_safe_scaled = V_safe^(1/alpha) , 
  V_risky_scaled = V_risky^(1/alpha) ,
  V_diff = V_safe_scaled-V_risky_scaled , 
  p_safe_risky = round(1 / ( 1 + exp(-rho*V_diff) ) , 2) ,
  choice_pp = rbinom(n=nrow(ppset), size=1, prob=p_safe_risky))

# post-processing

## compute observed choice rates 

### risky choice proportions

riskprop_obs <- choices %>% 
  mutate(choice_cmp = ifelse(choice == "r", 0, 1), 
         r_low = if_else(r_1 < r_2, r_1, r_2) ,
         r_high = if_else(r_1 > r_2, r_1, r_2) ,
         sp_r_low = if_else(r_low == r_1, sp_r_1, sp_r_2) ,
         sp_r_high = if_else(r_high == r_1, sp_r_1, sp_r_2)) %>% 
  group_by(model, psi, theta, sp_r_high, choice_cmp) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n),2) , 
         bias = if_else(sp_r_high == 1, "p=0 or p=1", if_else(sp_r_high == 0, "p=0 or p=1", "0 < p < 1")) , 
         type = "Sampling Strategy") %>% 
  filter(choice_cmp == 0)

riskprop_pp <- postpred %>% 
  mutate(choice_cmp = choice_pp) %>% 
  group_by(model, psi, theta, sp_r_high, choice_cmp) %>% 
  summarize(n = n()) %>% 
  mutate(prop = round(n/sum(n), 2), 
         bias = if_else(sp_r_high == 1, "p=0 or p=1", if_else(sp_r_high == 0, "p=0 or p=1", "0 < p < 1")) , 
         type = "Posterior Predictive") %>% 
  filter(choice_cmp == 0)

riskprop <- bind_rows(riskprop_obs, riskprop_pp)

### maximization rates

max_rates_obs <- choices %>% 
  mutate(norm = case_when(avg_r/safe > 1 ~ "r", avg_r/safe < 1 ~ "s"), 
         norm_choice = ifelse(choice == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  mutate(norm = ifelse(norm == "r", "Risky Option", "Safe Option")) %>% 
  group_by(model, psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1) %>% 
  mutate(type = "Sampling Strategy")

max_rates_pp <- postpred %>% 
  mutate(norm = case_when(avg_r/safe > 1 ~ "r", avg_r/safe < 1 ~ "s") , 
         choice_pp = ifelse(choice_pp == 0, "r", "s"), 
         norm_choice = ifelse(choice_pp == norm, 1, 0)) %>% 
  filter(!is.na(norm)) %>% 
  mutate(norm = ifelse(norm == "r", "Risky Option", "Safe Option")) %>% 
  group_by(model, psi, theta, norm, norm_choice) %>% 
  summarise(n_norm = n()) %>% 
  mutate(prop = round(n_norm/sum(n_norm),2)) %>% 
  filter(norm_choice == 1) %>% 
  mutate(type = "CPT Posterior Predictive")

max_rates <- bind_rows(max_rates_obs, max_rates_pp)

## compute predictive accuracy

pp_acc <- postpred %>% 
  select(model, psi, theta, alpha, gamma, delta, rho, id, agent, choice_obs, choice_pp) %>% 
  mutate(match = if_else(choice_obs == choice_pp, 1, 0)) %>% 
  group_by(model, psi, theta, alpha, gamma, delta, rho, match) %>% 
  summarise(count = n()) %>% 
  mutate(perc = round(count/sum(count), 3)) %>% 
  filter(match != 0) %>% 
  ungroup()

## compute DIC

dic <- cpt_gamma %>% 
  filter(parameter == "deviance") %>% 
  select(model:sd) %>% 
  mutate(var = sd^2 ,
         pD = var/2 , 
         DIC = round(pD + mean, 1))

# plot data 

riskprop %>% 
  ggplot(aes(x=sp_r_high, y = prop)) +
  geom_point(aes(shape = type, color = bias, alpha = bias), size = 3) +
  scale_shape_manual(values = c(4, 16)) + 
  scale_color_manual(values = c("gray", "black")) + 
  scale_alpha_manual(values = c(.5, 1)) + 
  facet_grid(psi~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed), 
                                            psi = as_labeller(label_psi, default = label_parsed))) + 
  labs(x = TeX("$\\p_{high}$"), 
       y = "Proportion of Risky Choices", 
       shape = "", 
       color = "", 
       alpha = "") +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0,1,.5)) + 
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0,1,.5)) + 
  theme_minimal(base_size = 20) + 
  theme(legend.position='top') + 
  geom_text(data = dic, aes(label=paste("DIC=", as.character(DIC)), x = .7, y = -.1)) 
# ggsave("manuscript/figures/appendix/ppc_roundwise_riskprop.png", width = 14, height = 16)

max_rates_p <- max_rates %>% 
  ggplot(aes(x=psi, y=prop, group = norm, color = norm)) + 
  facet_grid(type~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_point(aes(shape = type), size = 3) + 
  geom_line() + 
  scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0,1,length.out = 3)) + 
  scale_y_continuous(limits = c(.4, 1.1), breaks = seq(.5, 1, length.out = 3)) +
  scale_shape_manual(values = c(4, 19)) + 
  labs(x = "Switching Probability (Search Rule)" ,
       y = "Proportion of\nMaximizing Choices" ,
       color = "Better Average",
       shape = "") + 
  theme_minimal(base_size = 20) + 
  theme(strip.text.y = element_blank()) + 
  guides(shape = "none")

pp_acc_p <- pp_acc %>%  
  filter(model == "roundwise") %>% 
  ggplot(aes(x = psi, y = perc)) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(.4, 1.1), breaks = seq(.5, 1, length.out = 3)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_point(size = 3, shape = 4) + 
  labs(x = "Switching Probability (Search Rule)" , 
       y = "Proportion of\nCorrect Predictions", 
       color = "Choice Consistency") + 
  scale_color_viridis(option = "C") + 
  theme_minimal(base_size = 20)

max_rates_p + pp_acc_p + 
  plot_layout(nrow = 2, guides = "collect") + 
  plot_annotation(tag_levels = "A") & 
  theme(legend.position='top')
#ggsave(file = "manuscript/figures/appendix/ppc_roundwise_max_acc.png", width = 14, height = 10)


