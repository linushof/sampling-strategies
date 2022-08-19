pacman::p_load(tidyverse)

# Function for generating choice problems between a risky and safe prospect
## n: number of choice problems
## lower: lower boundary of the outcome range
## upper: upper boundary of the outcome range

generate_choice_problems <- function(n, lower, upper) {

  # define function for computing a prospect's expected value (ev)
  ## p_r_low: probability of the smaller risky outcome
  ## r_low: smaller risky outcome
  ## r_high: higher risky outcome

  ev <- function(p_r_low, r_low, r_high) {
    round(p_r_low * r_low + (1-p_r_low) * r_high, digits = 2)
  }

  output <- vector("list", n)

  # for each choice problem, randomly draw outcomes and probabilities
  
  output %>%
    map(tibble, # create tibble for each choice problem
        "names" = c("r_low", "safe", "r_high", "p_r_low"),
        "values" = c(runif(3, min = lower, max = upper) %>% # draw 3 outcomes
                       round(2) %>% 
                        sort(), # sort and assign outcomes to omit dominance, i.e., r_low < safe < r_high
                     runif(1, min = .01, max = .99) %>% # probability of r_low
                       round(2))
        ) %>%

      # each choice problem should be presented in a single row:

      map(pivot_wider, names_from = "names", values_from = "values") %>%
      map_dfr(as.list) %>%

      # add prospect features and compute and compare expected values

      mutate(p_r_high = round(1-p_r_low, 2),
             p_safe = 1,
             r_ev = ev(p_r_low, r_low, r_high),
             ev_diff = round(r_ev - safe, 2),
             ev_ratio = round(r_ev/safe, 2)
      ) %>%
      select(p_r_low, r_low, p_r_high, r_high, r_ev, p_safe, safe, ev_diff, ev_ratio) # sort features
}
