# Function for generating choice problems consisting of a risky and a safe option
SR_gambles <- function(N, lower, upper, pmin=0) {
  
  # draw outcomes
  
  outcomes <- matrix( round( runif(N * 3, min = lower, max = upper), 2 )  , ncol = 3)  
  outcomes <- t(apply(outcomes, 1, sort))  # sort outcomes to avoid dominance

  # draw probabilities
  
  p1 <- round(runif(N, min = pmin, max = (1-pmin)), 2) # draw probability of r_low

  # store
  
  data.frame(
    o1_1 = outcomes[, 1] , # small
    o1_p1 = p1 , 
    o1_2 = outcomes[, 3] , # large
    o1_p2 = 1-p1 , 
    o2_1 = outcomes[, 2] , # safe
    o2_p1 = rep(1, N) ,  
    o2_2 = rep(0, N) , # placeholder
    o2_p2 = rep(0, N)
    )
}