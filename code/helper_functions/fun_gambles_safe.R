# Function for generating choice problems consisting of a risky and a safe option
N <- 10
lower <- 0
upper <- 100


SR_gambles <- function(N, lower, upper, pmin=0) {
  
  # draw outcomes
  
  outcomes <- matrix( round( runif(N * 3, min = lower, max = upper), 2 )  , ncol = 3)  
  outcomes <- t(apply(outcomes, 1, sort))  # sort outcomes to avoid dominance

  # draw probabilities
  
  p1 <- round(runif(N, min = pmin, max = (1-pmin)), 2) # draw probability of r_low

  
  # # expectation 
  # 
  # r_ev <- round(p_r_low * r_low + p_r_high * r_high, 2)
  # ev_diff <- round(r_ev - safe, 2)
  # ev_ratio <- round(r_ev / safe, 2)
  # 
  
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

problems <- SR_gambles(10, 0, 100)

