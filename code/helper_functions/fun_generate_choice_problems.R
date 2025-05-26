# Function for generating choice problems consisting of a risky and a safe option

generate_choice_problems <- function(n, lower, upper, pmin=0) {
  
  outcomes <- matrix(runif(n * 3, min = lower, max = upper), ncol = 3) # draw 3 outcomes 
  outcomes <- t(apply(outcomes, 1, sort))  # sort outcomes to avoid dominance
  
  r_low  <- round(outcomes[, 1], 2)
  safe   <- round(outcomes[, 2], 2)
  r_high <- round(outcomes[, 3], 2)
  
  p_r_low <- round(runif(n, min = pmin, max = (1-pmin)), 2) # draw probability of r_low
  p_r_high <- round(1 - p_r_low, 2)
  p_safe <- rep(1, n)
  
  r_ev <- round(p_r_low * r_low + p_r_high * r_high, 2)
  ev_diff <- round(r_ev - safe, 2)
  ev_ratio <- round(r_ev / safe, 2)
  
  data.frame(
    p_r_low, r_low, p_r_high, r_high,
    r_ev, p_safe, safe,
    ev_diff, ev_ratio
    
  )
}
