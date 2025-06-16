rm(list = ls())

# preparation -------------------------------------------------------------

# load packages
library(readxl)

# import models 
model_files <- list.files(path='code/models', pattern='simulate', full.names = T)
lapply(model_files, source)

# simulation settings ----------------------------------------------------------------

# problem sets
problems <- as.data.frame(read_xlsx("data/choice_problems_general.xlsx"))
#problems <- as.data.frame(read_rds("data/choice_problems.rds"))

# parameter sets
# ...

# number of synthetic agents
n_agents <- 2 #1e3

# Constant switch rate (original) -------------------------------------------------------------

## safe-risky  -------------------------------------------------------------


### large EV differences ----------------------------------------------------



### small EV differences ----------------------------------------------------

#### summary comparison ------------------------------------------------------
# specify parameters for search rule (psi; switch rate) and stopping rule (theta; threshold)
param <- expand.grid(psi = seq(.1, 1, .1) , theta = seq(100, 300, 50))
set.seed(36151) # seed random number generator to make simulations reproducible
simulation_summary <- simulate_summary(problems,param,n_agents)
# checksum_simulation_summary <- digest(simulation_summary, "sha256")
#write_rds(simulation_summary, "data/simulation_summary.rds.bz2", compress = "bz2")

#### roundwise comparison ----------------------------------------------------
param <- expand.grid(psi = seq(.1, 1, .1), theta = 1:5)
set.seed(6535)
simulation_roundwise <- simulate_roundwise(problems,param,n_agents)
# checksum_simulation_roundwise <- digest(simulation_roundwise, "sha256")
write_rds(simulation_roundwise, "data/simulation_roundwise.rds.bz2", compress = "bz2")



## risky-risky -------------------------------------------------------------


### large EV differences ----------------------------------------------------



### small EV differences ----------------------------------------------------




# Decreasing switch rate --------------------------------------------------
# we use safe-risky problems with large EV differences to demonstrate the effect of decreasing switch rates

param <- expand.grid(base = seq(.2,.8,.2) , 
                     rate = seq(.1, .5, .2) ,
                     theta = seq(100, 300, 100))
simulation_summary_decreasing <- simulate_summary_decreasing(problems, param=param, n_agents )
#write_rds(simulation_summary, 'revision/data/simulation_summary_original_decreasing.rds')

# roundwise
param <- expand.grid(base = seq(.2,.8,.2) , 
                     rate = seq(.1, .5, .2) ,
                     theta = 1:3)
simulation_roundwise_decreasing <- simulate_roundwise_decreasing(problems, param=param, n_agents )
write_rds(simulation_roundwise, 'revision/data/simulation_roundwise_original_decreasing.rds')
