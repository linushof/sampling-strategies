library(gtools)

RR_gambles <- function(N, lower, upper, pmin=0){ 
  
  outcomes <- matrix( round( runif( N*4, min=lower, max=upper ), 2 ), nrow=N, ncol=4 ) # draw 4 outcomes 
  
  # sort outcomes to avoid dominance
  
  outcomes <- t(apply(outcomes, 1, sort))  
  
  ## possible assignments: columns indicate options (option 1: c1, c2; option 2: c3, c4); entries are the positions from the sorted outcome vectors)
  position <- permutations(n = length(1:4), r = length(1:4), v = 1:4) 
  
  ## to avoid dominance, remove position combinations where all outcomes from one option are bigger/smaller than from the other option
  
  no_dominance <- position[ !( (position[,1] %in% 1:2 & position[,2] %in% 1:2) | (position[,1] %in% 3:4 & position[,2] %in% 3:4)) , ]
  
  ## obtain a random option assignment for each outcome combination  
  assignment <- no_dominance[sample(1:nrow(no_dominance), nrow(outcomes), replace = TRUE), ]
  
  ## apply option assignment
  new_matrix <- matrix(NA, nrow = N, ncol = 4)
  for (i in 1:N) {
    new_matrix[i, ] <- outcomes[i, assignment[i, ]]
  }
  
  p1 <- matrix(round(runif(N*2, min = pmin, max = (1-pmin)), 2), nrow = N, ncol=2) 
  p2 <- round(1-p1,2)
  
  data.frame(
    o1_1 = new_matrix[,1] ,
    o1_p1 = p1[,1] , 
    o1_2 = new_matrix[,2] ,
    o1_p2 = p2[,1] ,
    o2_1 = new_matrix[,3] ,
    o2_p1 = p1[,2] ,
    o2_2 = new_matrix[,4] ,
    o2_p2 = p2[,2] 
  )  
} 

problems <-  RR_gambles(10, 0, 100)
