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
