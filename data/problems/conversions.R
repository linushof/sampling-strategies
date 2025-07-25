library(readxl)
library(tidyverse)

# original data set

SR_small <- as.data.frame(read_xlsx("data/problems/SR_small.xlsx"))

# create additional data sets ---------------------------------------------


## safe-risky with larger EV differences -----------------------------------

SR_large <- SR_small |> 
  mutate(o1_1 = o1_1*2 , 
         o1_2 = o1_2*2 , 
         o2_1 = o2_1*2 , 
         o2_2 = o2_2*2 ,
         o1_ev = round( o1_1*o1_p1 + o1_2*o1_p2 , 2) ,
         o2_ev = round( o2_1*o2_p1 + o2_2*o2_p2 , 2) ,
         ev_diff = o1_ev-o2_ev , 
         var_o1 = o1_p1 * o1_p2 , 
         var_o2 = o2_p1 * o2_p2 ,
         cvar_o1 = sqrt( round( (o1_1^2) * o1_p1 + (o1_2^2) * o1_p2, 4 ) - round((o1_1 * o1_p1 + o1_2 * o1_p2)^2, 4 ))  ,
         cvar_o2 = sqrt( round( (o2_1^2) * o2_p1 + (o2_2^2) * o2_p2, 4 ) - round((o2_1 * o2_p1 + o2_2 * o2_p2)^2, 4 ))  ,
         higher_risk = if_else(cvar_o1 > cvar_o2, "o1", "o2") , 
         c11 = if_else(o1_1 > o2_1, 1, 0) , 
         c12 = if_else(o1_1 > o2_2, 1, 0) , 
         c21 = if_else(o1_2 > o2_1, 1, 0) ,
         c22 = if_else(o1_2 > o2_2, 1, 0) ,
         cp11 = o1_p1 * o2_p1 ,
         cp12 = o1_p1 * o2_p2 , 
         cp21 = o1_p2 * o2_p1 ,
         cp22 = o1_p2 * o2_p2 ,
         o1_rwp = c11*cp11 + c12*cp12 + c21*cp21 + c22*cp22 , 
         o2_rwp = 1-o1_rwp 
  ) |> 
  select(-c(c11:cp22))


## Risky Risky -------------------------------------------------------------

spread <- 1:5 # outcomes of option 2 diverge {1,2,3,4,5} units from ev
RR <- SR_large |> 
  mutate(o2_ev_old = o2_ev,
         o2_p1 = .5,
         o2_p2 = .5,
         o2_1 = o2_ev_old+spread,
         o2_2 = o2_ev_old-spread,
         o2_ev = o2_p1*o2_1 + o2_p2*o2_2 ,
         c11 = if_else(o1_1 > o2_1, 1, 0) , 
         c12 = if_else(o1_1 > o2_2, 1, 0) , 
         c21 = if_else(o1_2 > o2_1, 1, 0) ,
         c22 = if_else(o1_2 > o2_2, 1, 0) ,
         cp11 = o1_p1 * o2_p1 ,
         cp12 = o1_p1 * o2_p2 , 
         cp21 = o1_p2 * o2_p1 ,
         cp22 = o1_p2 * o2_p2 ,
         o1_rwp = c11*cp11 + c12*cp12 + c21*cp21 + c22*cp22 , 
         o2_rwp = 1-o1_rwp,
         better_rwp = if_else(o1_rwp > o2_rwp, "o1", "o2") ,
         var_o1 = o1_p1 * o1_p2 , 
         var_o2 = o2_p1 * o2_p2 ,
         cvar_o1 = sqrt( round( (o1_1^2) * o1_p1 + (o1_2^2) * o1_p2, 4 ) - round((o1_1 * o1_p1 + o1_2 * o1_p2)^2, 4 ))  ,
         cvar_o2 = sqrt( round( (o2_1^2) * o2_p1 + (o2_2^2) * o2_p2, 4 ) - round((o2_1 * o2_p1 + o2_2 * o2_p2)^2, 4 ))  ,
         higher_risk = if_else(cvar_o1 > cvar_o2, "o1", "o2") 
  ) |> 
  select(id:o2_rare)


# write rds files ---------------------------------------------------------
write_rds(SR_small, "data/problems/SR_small.rds")
write_rds(SR_large, "data/problems/SR_large.rds")
write_rds(RR, "data/problems/RR.rds")
