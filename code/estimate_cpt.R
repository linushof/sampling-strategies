# Preparation -------------------------------------------------------------

# load packages
pacman::p_load(tidyverse, R2jags, digest, readxl)

# load data
problems <- as.data.frame(read_xlsx("data/choice_problems_balanced_refined.xlsx"))
choices <- read_rds("data/choice_data_balanced_refined.rds.bz2")

# prepare data for CPT model (requires rank-ordered outcomes and probabilities)
choices <- left_join(choices, problems, by=join_by(id)) %>% 
  mutate(choice_r = if_else(choice == "r", 1, 0) , # predict choice of risky option
         r_low = if_else(r_1 < r_2, r_1, r_2) ,
         r_high = if_else(r_1 > r_2, r_1, r_2) ,         
         sp_r_low = if_else(r_low == r_1, sp_r_1, sp_r_2) , 
         sp_r_high = if_else(r_high == r_1, sp_r_1, sp_r_2))

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
                                  n.chains = 4 ,
                                  n.iter = 1000 ,
                                  n.burnin = 500 ,
                                  n.thin = 1 ,
                                  n.cluster = 4 , # compute MCMC chains in parallel
                                  DIC = TRUE ,
                                  jags.seed = 2345)
  
  ## posterior summary and MCMC diagnostics
  current_summary <- current_sample$BUGSoutput$summary %>% as_tibble(rownames = "parameter")
  estimates_cpt[[set]] <- expand_grid(params_sim[set, ], current_summary)
  
  ## full posterior
  current_posterior <- current_sample$BUGSoutput$sims.matrix %>% as_tibble()
  posterior_cpt[[set]] <- expand_grid(params_sim[set, ], current_posterior)
  
  ## status
  print(paste("\u2713 Parameter Set No. ", set, " estimated!"))
} # close strategy loop

estimates_cpt <- estimates_cpt %>% bind_rows()
posterior_cpt <- posterior_cpt %>% bind_rows()

# Storage -----------------------------------------------------------------

# checksums
checksum_estimates_cpt <- digest(estimates_cpt, "sha256")
checksum_posterior_cpt <- digest(posterior_cpt, "sha256")

write_rds(estimates_cpt, "data/cpt_estimates_balanced_refined.rds")
write_rds(posterior_cpt, "data/cpt_posteriors_balanced_refined.rds.bz2", compress = "bz2") 