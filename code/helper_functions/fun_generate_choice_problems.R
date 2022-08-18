pacman::p_load(tidyverse)

# Function for generating choice problems between a risky and safe prospect
## n: number of choice problems
## lower: lower boundary of the outcome range
## upper: upper boundary of the outcome range

generate_choice_problems <- function(n, lower, upper) {

  # define function for computing a prospect's expected value (ev)
  ## p_x_low: probability of the smaller risky outcome
  ## x_low: smaller risky outcome
  ## x_high: higher risky outcome

  ev <- function(p_x_low, x_low, x_high) {
    round(p_x_low * x_low + (1-p_x_low) * x_high, digits = 2)
  }

  output <- vector("list", n)

  # for each choice problem, randomly draw outcomes and probabilities
  
  output %>%
    map(tibble, # create tibble for each choice problem
        "names" = c("x_low", "safe", "x_high", "p_x_low"),
        "values" = c(runif(3, min = lower, max = upper) %>% # draw 3 outcomes
                       round(2) %>% 
                        sort(), # sort and assign outcomes to omit dominance, i.e., x_low < safe < x_high
                     runif(1, min = .01, max = .99) %>% # probability of x_low
                       round(2))
        ) %>%

      # each choice problem should be presented in a single row:

      map(pivot_wider, names_from = "names", values_from = "values") %>%
      map_dfr(as.list) %>%

      # add prospect features and compute and compare expected values

      mutate(p_x_high = round(1-p_x_low, 2),
             p_safe = 1,
             x_ev = ev(p_x_low, x_low, x_high),
             ev_diff = round(x_ev - safe, 2),
             ev_ratio = round(x_ev/safe, 2)
      ) %>%
      select(p_x_low, x_low, p_x_high, x_high, x_ev, p_safe, safe, ev_diff, ev_ratio) # sort features
}
