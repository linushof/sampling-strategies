rm(list = ls())

# Preparation -------------------------------------------------------------

# load packages
pacman::p_load(tidyverse, rstan, digest)

# load data
dir <- "data/choices/"
choice_files <- list.files(dir, pattern='choices')
choices <- lapply(paste0(dir, choice_files), read_rds)
names(choices) <- choice_files |> str_remove(".rds.bz2")

# Stan settings
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## initial values for MCMC
set_inits <- function(chain_id = 1) {
  list(alpha_pre=.5, gamma_pre=.5, delta_pre=.5, phi_pre=.01 )
}
n_chains <- 4 # number of chains
inits <- lapply(1:n_chains, function(id) set_inits(chain_id = id))


# Fit CPT -----------------------------------------------------------------
# For each simulation, a CPT model is fitted separately for each sampling strategy in that simulation

for (sim in 1:length(choices)){
  
  # get simulation info
  simname <- names(choices)[[sim]]
  newfile <- paste0("data/cpt/", "cpt_", simname,".rds")
  
  # group data by sampling strategies (CPT is fitted to each strategy separately)
  model <- regmatches(simname,  gregexpr("summary|summary_decreasing|roundwise|roundwise_decreasing", simname))[[1]]
  params <- grepl("decreasing", model)
  if(params==FALSE) { # constant switch rate
    params_sim <- choices[[sim]] %>% distinct(model, psi, theta) # retrieve all strategies
    estimates_cpt <- vector("list", nrow(params_sim)) # posterior summary and MCMC diagnostics
    dat <- vector("list", nrow(params_sim))
    for(set in seq_len(nrow(params_sim))){
      
      dat[[set]] <- choices[[sim]] %>%
        filter(model == params_sim[[set, "model"]] &
                 psi == params_sim[[set, "psi"]] &
                 theta == params_sim[[set, "theta"]])
      }
    } else { # decreasing switch rate
      
        params_sim <- choices[[sim]] %>% distinct(model, base, rate, theta)
        estimates_cpt <- vector("list", nrow(params_sim)) # posterior summary and MCMC diagnostics
        dat <- vector("list", nrow(params_sim))
        for(set in seq_len(nrow(params_sim))){
          dat[[set]] <- choices[[sim]] %>%
            filter(model==params_sim[[set, "model"]] & 
                     base==params_sim[[set, "base"]] & 
                     rate==params_sim[[set, "rate"]] & 
                     theta==params_sim[[set, "theta"]]
                   )
        }
    }
  
  # fit CPT model
  # loop over the different sampling strategies
  
  for(set in seq_len(nrow(params_sim))){
    
    # create data list for Stan
    dat_stan <- list(
      N = nrow(dat[[set]]) , 
      choice = if_else(dat[[set]][["choice"]]=="o1",1,0) ,
      o1_low = if_else(dat[[set]][["o1_1"]]<dat[[set]][["o1_2"]], dat[[set]][["o1_1"]], dat[[set]][["o1_2"]]) ,
      o1_sp_low = if_else(dat[[set]][["o1_1"]]<dat[[set]][["o1_2"]], dat[[set]][["o1_sp1"]], dat[[set]][["o1_sp2"]]) , 
      o1_high = if_else(dat[[set]][["o1_1"]]>dat[[set]][["o1_2"]], dat[[set]][["o1_1"]], dat[[set]][["o1_2"]]) , 
      o1_sp_high = if_else(dat[[set]][["o1_1"]]>dat[[set]][["o1_2"]], dat[[set]][["o1_sp1"]], dat[[set]][["o1_sp2"]]) ,
      o2_low = if_else(dat[[set]][["o2_1"]]<dat[[set]][["o2_2"]], dat[[set]][["o2_1"]], dat[[set]][["o2_2"]]) ,
      o2_sp_low = if_else(dat[[set]][["o2_1"]]<dat[[set]][["o2_2"]], dat[[set]][["o2_sp1"]], dat[[set]][["o2_sp2"]]) , 
      o2_high = if_else(dat[[set]][["o2_1"]]>dat[[set]][["o2_2"]], dat[[set]][["o2_1"]], dat[[set]][["o2_2"]]) , 
      o2_sp_high = if_else(dat[[set]][["o2_1"]]>dat[[set]][["o2_2"]], dat[[set]][["o2_sp1"]], dat[[set]][["o2_sp2"]]) 
    )
    
    # fit model
    cpt_fit <- stan(
      file = "code/models/CPT.stan",  # Stan program
      init = inits,
      data = dat_stan,    # named list of data
      chains = n_chains,             # number of Markov chains
      warmup = 10000,          # number of warmup iterations per chain
      iter = 5000,            # total number of iterations per chain
      refresh = 1000          # show progress every 'refresh' iterations
    )
    
    
    ## posterior summary and MCMC diagnostics
    pars <- cpt_fit@model_pars
    fit_summary <- summary(cpt_fit)$summary |> 
      as_tibble() |> 
      mutate("parameter"=pars) |> 
      select(parameter,everything())
    
    estimates_cpt[[set]] <- expand_grid(params_sim[set, ], fit_summary)
    
    print(paste("\u2713 Parameter Set No. ", set, " estimated!"))
  } # close strategy loop
  
  estimates_cpt <- estimates_cpt %>% bind_rows()
  write_rds(estimates_cpt, newfile)

} # close simulation loop
