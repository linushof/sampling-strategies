rm(list = ls())

# load packages
pacman::p_load(tidyverse, digest, readxl)

# call function that pre-processes simulation data sets
source("code/helper_functions/fun_prepare_choice_data.R")

# get simulation files 
simulation_files <- list.files(path='data/simulations', pattern='simulation', full.names = T)

for(file in 1:length(simulation_files)){
  result <- prepare_choice_data(file=simulation_files[file])
  print(result)
}
