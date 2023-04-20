# load pkgs
pacman::p_load(tidyverse)

# Function for computing an option's expected value 

ev <- function(p_r_low, r_low, r_high) {
  # p_r_low: probability of the smaller risky outcome
  # r_low: smaller risky outcome
  # r_high: higher risky outcome
  
  round(p_r_low * r_low + (1-p_r_low) * r_high, digits = 2)
}

# Function for generating choice problems consisting of a risky and a safe option

generate_choice_problems <- function(n, lower, upper) {
  # n: number of choice problems
  # lower: lower boundary of the outcome range
  # upper: upper boundary of the outcome range

  output <- vector("list", n) # each element of the list represents 1 choice problem
  output %>% 
    
    #1 generate choice problems by drawing random outcomes and probabilities
    map(tibble , "names" = c("r_low", "safe", "r_high", "p_r_low") ,
                 "values" = c( sort(round(runif(3, min = lower, max = upper), 2)) , # draw 3 outcomes (sort to omit dominance)
                               round(runif(1, min = .01, max = .99),2) )) %>% # draw probability of r_low
    
    #2 turn list into data frame (each choice problem represents 1 row)
    map(pivot_wider, names_from = "names", values_from = "values") %>%
    bind_rows() %>%
    
    #3 add features
    mutate(p_r_high = round(1-p_r_low, 2) ,
           p_safe = 1 ,
           r_ev = ev(p_r_low, r_low, r_high) ,
           ev_diff = round(r_ev - safe, 2) ,
           ev_ratio = round(r_ev/safe, 2)) %>%
    select(p_r_low, r_low, p_r_high, r_high, r_ev, p_safe, safe, ev_diff, ev_ratio)
}