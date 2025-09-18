# preparation -------------------------------------------------------------
rm(list = ls())

# import models 
model_files <- list.files(path='code/models', pattern='simulate', full.names = T)
lapply(model_files, source)

# simulation settings ----------------------------------------------------------------

# number of synthetic agents
n_agents <- 3e2

# problems
SR_large <-  as.data.frame(readRDS("data/problems/SR_large.rds"))
SR_small <- as.data.frame(readRDS("data/problems/SR_small.rds"))
RR <-  as.data.frame(readRDS("data/problems/RR.rds"))
RR2 <-  as.data.frame(readRDS("data/problems/RR2.rds"))

# parameters
psi <- seq(.1, 1, .3) # switch rate
theta_summary <- 1:3 # thresholds 
theta_roundwise <- 1:3 # thresholds 

summary_param_constant <- expand.grid(psi=psi, theta=theta_summary)
roundwise_param_constant <- expand.grid(psi=psi, theta=theta_roundwise)

# SR large ----------------------------------------------------------------

## summary -----------------------------------------------------------------
set.seed(8172) # seed random number generator to make simulations reproducible
simulation_summary_SR_large <- simulate_summary(problems=SR_large, param=summary_param_constant, n_agents)
write_rds(simulation_summary_SR_large, "data/simulations/simulation_summary_SR_large.rds.bz2", compress = "bz2")

## roundwise ---------------------------------------------------------------
set.seed(9821)
simulation_roundwise_SR_large <- simulate_roundwise(problems=SR_large, param=roundwise_param_constant, n_agents)
write_rds(simulation_roundwise_SR_large, "data/simulations/simulation_roundwise_SR_large.rds.bz2", compress = "bz2")

# SR small ----------------------------------------------------

## summary -----------------------------------------------------------------
set.seed(36151) # seed random number generator to make simulations reproducible
simulation_summary_SR_small <- simulate_summary(problems=SR_small, param=summary_param_constant, n_agents)
write_rds(simulation_summary_SR_small, "data/simulations/simulation_summary_SR_small.rds.bz2", compress = "bz2")

## roundwise ---------------------------------------------------------------
set.seed(6535)
simulation_roundwise_SR_small <- simulate_roundwise(problems=SR_small, param=roundwise_param_constant, n_agents)
write_rds(simulation_roundwise_SR_small, "data/simulations/simulation_roundwise_SR_small.rds.bz2", compress = "bz2")

## RR -------------------------------------------------------------

## summary -----------------------------------------------------------------
set.seed(5612) # seed random number generator to make simulations reproducible
simulation_summary_RR <- simulate_summary(problems=RR, param=summary_param_constant, n_agents)
write_rds(simulation_summary_RR, "data/simulations/simulation_summary_RR.rds.bz2", compress = "bz2")


## roundwise ---------------------------------------------------------------
set.seed(9371)
simulation_roundwise_RR <- simulate_roundwise(problems=RR, param=roundwise_param_constant, n_agents)
write_rds(simulation_roundwise_RR, "data/simulations/simulation_roundwise_RR.rds.bz2", compress = "bz2")

# RR2 ---------------------------------------------------------------------

## summary -----------------------------------------------------------------
set.seed(76178) # seed random number generator to make simulations reproducible
simulation_summary_RR2 <- simulate_summary(problems=RR2, param=summary_param_constant, n_agents)
write_rds(simulation_summary_RR2, "data/simulations/simulation_summary_RR2.rds.bz2", compress = "bz2")

## roundwise ---------------------------------------------------------------
set.seed(78162)
simulation_roundwise_RR2 <- simulate_roundwise(problems=RR2, param=roundwise_param_constant, n_agents)
write_rds(simulation_roundwise_RR2, "data/simulations/simulation_roundwise_RR2.rds.bz2", compress = "bz2")
