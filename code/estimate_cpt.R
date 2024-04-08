pacman::p_load(tidyverse, R2jags, digest)
source("code/helper_functions/fun_initialize_MCMC.R") # call function creating initial values for MCMC

# read data
choice_data <- read_rds("data/choice_data_balanced.rds.bz2")

# prepare data
choice_data <- choice_data %>% 
  mutate(choice_r = if_else(choice_trace == "risky", 1, 0), 
         r_low = if_else(r_1 < r_2, r_1, r_2), 
         r_high = if_else(r_1 > r_2, r_1, r_2),         
         ep_r_low = if_else(r_1 < r_2, ep_r_1, ep_r_2), 
         ep_r_high = if_else(r_1 > r_2, ep_r_1, ep_r_2),
         )

# group trials of distinct sampling strategies (unique combinations of model + parameters)
params_sim <- choice_data %>% distinct(model, psi, theta) # to get distinct parameter combinations
choices_grouped <- vector("list", nrow(params_sim))
for(set in seq_len(nrow(params_sim))){
  choices_grouped[[set]] <- choice_data %>%
    filter(model == params_sim[[set, "model"]] & psi == params_sim[[set, "psi"]] & theta == params_sim[[set, "theta"]]) %>%
    mutate(i = row_number()) # assign trial numbers
}

# allocate space for JAGS output
estimates_cpt <- vector("list", nrow(params_sim)) # posterior summary and MCMC diagnostics
posterior_cpt <- vector("list", nrow(params_sim)) # full posterior

# MCMC simulation 

params_cpt <- c("alpha", "gamma", "delta", "rho")

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
                                  model.file = "code/CPT_model.txt",
                                  n.chains = 20,
                                  n.iter = 4000,
                                  n.burnin = 2000,
                                  n.thin = 4,
                                  n.cluster = 20, # run chains on different cores
                                  DIC = TRUE,
                                  jags.seed = 11617123)

  ## posterior summary and MCMC diagnostics
  current_summary <- current_sample$BUGSoutput$summary %>% as_tibble(rownames = "parameter")
  estimates_cpt[[set]] <- expand_grid(params_sim[set, ], current_summary)
  
  ## full posterior
  #current_posterior <- current_sample$BUGSoutput$sims.matrix %>% as_tibble()
  #posterior_cpt[[set]] <- expand_grid(params_sim[set, ], current_posterior)
  
  ## status
  print(paste("\u2713 Parameter Set No. ", set, " estimated!"))
}

estimates_cpt <- estimates_cpt %>% bind_rows()
posterior_cpt <- posterior_cpt %>% bind_rows()

# save data

##  full data sets
checksum_estimates_cpt <- digest(estimates_cpt, "sha256")
write_rds(estimates_cpt, "data/cpt_estimates_balanced.rds")

checksum_posterior_cpt <- digest(posterior_cpt, "sha256")
write_rds(posterior_cpt, "data/cpt_posteriors.rds.bz2", compress = "bz2")