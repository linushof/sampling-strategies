pacman::p_load(tidyverse, R2jags)
source("code/helper_functions/fun_initialize_MCMC.R") # call function creating initial values for MCMC

# read data
choice_data <- read_rds("data/choice_data.rds.bz2")

# prepare data
choice_data <- choice_data %>% 
  filter(!c(n_s == 0 | n_r == 0)) %>%  # omit trials where only one option was attended 
  mutate(choice_r = if_else(choice == "r", 1, 0)) %>%  # to apply logit choice rule
  filter(model == "roundwise", threshold == "relative")

# group trials of distinct sampling strategies (unique combinations of model + parameters)
params_sim <- choice_data %>% distinct(model, psi, threshold, theta) # to get distinct parameter combinations
choices_grouped <- vector("list", nrow(params_sim))
for(set in seq_len(nrow(params_sim))){
  choices_grouped[[set]] <- choice_data %>%
    filter(model == params_sim[[set, "model"]] & psi == params_sim[[set, "psi"]] & threshold == params_sim[[set, "threshold"]] & theta == params_sim[[set, "theta"]]) %>%
    mutate(i = row_number()) # assign trial numbers
}

# allocate space for JAGS output
estimates_cpt <- vector("list", nrow(params_sim)) # posterior summary and MCMC diagnostics
posterior_cpt <- vector("list", nrow(params_sim)) # full posterior

# MCMC simulation 

params_cpt <- c("alpha", "gamma", "delta", "rho")
n_chains <- 20

for(set in seq_len(nrow(params_sim))){

  ## get trials of the respective parameter combination
  current_trials <- list(choice = choices_grouped[[set]]$choice_r,
                         r_low = choices_grouped[[set]]$r_low,
                         r_high = choices_grouped[[set]]$r_high,
                         safe = choices_grouped[[set]]$safe,
                         ep_r_low = choices_grouped[[set]]$ep_r_low, # use relative frequencies (experienced probabilities) to account for sampling error
                         ep_r_high = choices_grouped[[set]]$ep_r_high,
                         p_safe = choices_grouped[[set]]$p_safe,
                         start = min(choices_grouped[[set]]$i),
                         stop = max(choices_grouped[[set]]$i))

  ## sample from posterior distributions using MCMC
  current_sample <- jags.parallel(data = current_trials,
                                  inits = inits_MCMC,
                                  parameters.to.save = params_cpt,
                                  model.file = "code/cpt_model_ext_gamma.txt",
                                  n.chains = 20,
                                  n.iter = 60000,
                                  n.burnin = 10000,
                                  n.thin = 20,
                                  n.cluster = 20, # run chains on different cores
                                  DIC = TRUE,
                                  jags.seed = 617251)

  ## posterior summary and MCMC diagnostics
  current_summary <- current_sample$BUGSoutput$summary %>% as_tibble(rownames = "parameter")
  estimates_cpt[[set]] <- expand_grid(params_sim[set, ], current_summary)
  
  ## full posterior
  current_posterior <- current_sample$BUGSoutput$sims.matrix %>% as_tibble()
  posterior_cpt[[set]] <- expand_grid(params_sim[set, ], current_posterior)
  
  ## status
  print(paste("\u2713 Parameter Set No. ", set, " estimated!"))
}

# save data
estimates_cpt <- estimates_cpt %>% bind_rows()
posterior_cpt <- posterior_cpt %>% bind_rows()

write_rds(estimates_cpt, "data/cpt_estimates_ext_gamma.rds")
write_rds(posterior_cpt, "data/cpt_posteriors_ext_gamma.rds.bz2", compress = "bz2")
