# preparation -------------------------------------------------------------
rm(list = ls())

library(readxl)

# import models 
model_files <- list.files(path='code/models', pattern='simulate', full.names = T)
lapply(model_files, source)

# simulation settings ----------------------------------------------------------------

# number of synthetic agents
n_agents <- 2 #1e3

# problems
SR_large <-  as.data.frame(read_xlsx("data/problems/SR_large_sys.xlsx"))
SR_small <- as.data.frame(read_xlsx("data/problems/SR_small_sys.xlsx"))
RR <-  as.data.frame(read_rds("data/problems/RR_large_rand.rds"))

# parameters
psi <- seq(.1, 1, .1) # switch rate
theta_summary <- seq(100, 300, 100) # threshold summary comparison
theta_roundwise <- 1:3 # thresholds roundwise comparison

# when switch rates should decrease over time, replace psi by:
base <- seq(.2,.8,.2) # minimum switch rate
rate <- seq(.1, .5, .2) # rate of change

summary_param_constant <- expand.grid(psi=psi, theta=theta_summary)
roundwise_param_constant <- expand.grid(psi=psi, theta=theta_roundwise)

summary_param_decreasing <- expand.grid(base=base, rate=rate, theta=theta_summary)
roundwise_param_decreasing <- expand.grid(base=base, rate=rate, theta=theta_roundwise)

# Constant switch rate (original) -------------------------------------------------------------

## safe-risky  -------------------------------------------------------------

### large EV differences ----------------------------------------------------

# summary comparison
set.seed(8172) # seed random number generator to make simulations reproducible
simulation_summary_SR_large <- simulate_summary(problems=SR_large, param=summary_param_constant, n_agents)
#checksum_simulation_summary <- digest(simulation_summary, "sha256")
write_rds(simulation_summary_SR_large, "data/simulations/temp/simulation_summary_SR_large.rds.bz2", compress = "bz2")

# roundwise comparison
set.seed(9821)
simulation_roundwise_SR_large <- simulate_roundwise(problems=SR_large, param=roundwise_param_constant, n_agents)
#checksum_simulation_roundwise <- digest(simulation_roundwise, "sha256")
write_rds(simulation_roundwise_SR_large, "data/simulations/temp/simulation_roundwise_SR_large.rds.bz2", compress = "bz2")

### small EV differences ----------------------------------------------------

# summary comparison
set.seed(36151) # seed random number generator to make simulations reproducible
simulation_summary_SR_small <- simulate_summary(problems=SR_small, param=summary_param_constant, n_agents)
#checksum_simulation_summary <- digest(simulation_summary, "sha256")
write_rds(simulation_summary_SR_small, "data/simulations/temp/simulation_summary_SR_small.rds.bz2", compress = "bz2")

# roundwise comparison
set.seed(6535)
simulation_roundwise_SR_small <- simulate_roundwise(problems=SR_small, param=roundwise_param_constant, n_agents)
#checksum_simulation_roundwise <- digest(simulation_roundwise, "sha256")
write_rds(simulation_roundwise_SR_small, "data/simulations/temp/simulation_roundwise_SR_small.rds.bz2", compress = "bz2")


## risky-risky -------------------------------------------------------------
# we use large EV differences to demonstrate the effect of problems with only risky gambles

set.seed(5612) # seed random number generator to make simulations reproducible
simulation_summary_RR <- simulate_summary(problems=RR, param=summary_param_constant, n_agents)
#checksum_simulation_summary <- digest(simulation_summary, "sha256")
write_rds(simulation_summary_RR, "data/simulations/temp/simulation_summary_RR.rds.bz2", compress = "bz2")

# roundwise comparison
set.seed(9371)
simulation_roundwise_RR <- simulate_roundwise(problems=RR, param=roundwise_param_constant, n_agents)
#checksum_simulation_roundwise <- digest(simulation_roundwise, "sha256")
write_rds(simulation_roundwise_RR, "data/simulations/temp/simulation_roundwise_RR.rds.bz2", compress = "bz2")


# Decreasing switch rate --------------------------------------------------
# we use safe-risky problems with large EV differences to demonstrate the effect of decreasing switch rates

simulation_summary_decreasing <- simulate_summary_decreasing(problems=RR, param=summary_param_decreasing, n_agents)
write_rds(simulation_summary_decreasing, 'data/simulations/temp/simulation_summary_decreasing_RR.rds.bz2', compress = "bz2")

# roundwise
simulation_roundwise_decreasing <- simulate_roundwise_decreasing(problems=RR, param=roundwise_param_decreasing, n_agents)
write_rds(simulation_roundwise_decreasing, 'data/simulations/temp/simulation_roundwise_decreasing_RR.rds.bz2', compress = "bz2")
