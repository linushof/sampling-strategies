# preparation -------------------------------------------------------------

# packages
library(pacman)
p_load(tidyverse, magrittr)

# functions
source("code/helper_functions/fun_gambles_safe.R") # call function for generating choice problems
source("code/helper_functions/fun_gambles_risky.R")


# initial problem settings ------------------------------------------------

N <- 1e5
lower <- 0 # lowest possible outcome 
upper <- 100 # highest possible outcome
pmin <- .1 # lowest possible probability 
pmin_com <- .4 # lowest probability for non-rare events
pmax_rare <- .2 # highest probability for rare events
N_trials <- 60 # number of trials per agent
diffs <- seq(.1,.3,.1) # magnitude of EV differences

# generate problems -------------------------------------------------------

## safe-risky --------------------------------------------------------------

set.seed(4564)
SR <- SR_gambles(N, lower, upper, pmin)

SR <- SR %>% 
  mutate(
    ev_o1 = o1_1*o1_p1 + o1_2*o1_p2 ,
    ev_o2 = o2_1*o2_p1 + o2_2*o2_p2 , 
    ev_diff = ev_o1-ev_o2 , 
    ev_diff_sc = abs(ev_diff)/upper , 
    c11 = if_else(o1_1 > o2_1, 1, 0) , 
    c12 = if_else(o1_1 > o2_2, 1, 0) , 
    c21 = if_else(o1_2 > o2_1, 1, 0) ,
    c22 = if_else(o1_2 > o2_2, 1, 0) ,
    cp11 = o1_p1 * o2_p1 ,
    cp12 = o1_p1 * o2_p2 , 
    cp21 = o1_p2 * o2_p1 ,
    cp22 = o1_p2 * o2_p2 ,
    o1_rwp = c11*cp11 + c12*cp12 + c21*cp21 + c22*cp22 , 
    o2_rwp = 1-o1_rwp ,
    o1_psmall = if_else(o1_1 < o1_2, o1_p1, o1_p2) , 
    o1_plarge = 1-o1_psmall , 
    o2_psmall = if_else(o2_1 < o2_2, o2_p1, o2_p2) , 
    o2_plarge = 1-o2_psmall
    ) %>%
  select(-c(c11:cp22))

## risky-risky -------------------------------------------------------------------------

set.seed(1234)
RR <- RR_gambles(N, lower, upper, pmin)

RR %<>% mutate(
  ev_o1 = o1_1*o1_p1 + o1_2*o1_p2 ,
  ev_o2 = o2_1*o2_p1 + o2_2*o2_p2 , 
  ev_diff = ev_o1-ev_o2 , 
  ev_diff_sc = abs(ev_diff)/upper , 
  c11 = if_else(o1_1 > o2_1, 1, 0) , 
  c12 = if_else(o1_1 > o2_2, 1, 0) , 
  c21 = if_else(o1_2 > o2_1, 1, 0) ,
  c22 = if_else(o1_2 > o2_2, 1, 0) ,
  cp11 = o1_p1 * o2_p1 ,
  cp12 = o1_p1 * o2_p2 , 
  cp21 = o1_p2 * o2_p1 ,
  cp22 = o1_p2 * o2_p2 ,
  o1_rwp = c11*cp11 + c12*cp12 + c21*cp21 + c22*cp22 , 
  o2_rwp = 1-o1_rwp ,
  o1_psmall = if_else(o1_1 < o1_2, o1_p1, o1_p2) , 
  o1_plarge = 1-o1_psmall , 
  o2_psmall = if_else(o2_1 < o2_2, o2_p1, o2_p2) , 
  o2_plarge = 1-o2_psmall
  ) %>% 
  select(-c(c11:cp22))


# filter problems -----------------------------------------------------------


## safe-risky --------------------------------------------------------------


### 1) EV direction -------------------------------------------------------------------------

SR_o1 <- SR %>% filter(ev_diff > 0)
SR_o2 <- SR %>% filter(ev_diff < 0)

### 2) ordinal comparisons -------------------------------------------

SR_o1_o1 <- SR_o1 %>% filter(o1_rwp > .5)
SR_o1_o2 <- SR_o1 %>% filter(o2_rwp > .5)
SR_o2_o1 <- SR_o2 %>% filter(o1_rwp > .5)
SR_o2_o2 <- SR_o2 %>% filter(o2_rwp > .5)


### 3) rare events -------------------------------------------------------------

# no rare event
SR_o1_o1_N <- SR_o1_o1 %>% filter(o1_p1 >= pmin_com & o1_p1 <= (1-pmin_com))
SR_o1_o2_N <- SR_o1_o2 %>% filter(o1_p1 >= pmin_com & o1_p1 <= (1-pmin_com))
SR_o2_o1_N <- SR_o2_o1 %>% filter(o1_p1 >= pmin_com & o1_p1 <= (1-pmin_com))
SR_o2_o2_N <- SR_o2_o2 %>% filter(o1_p1 >= pmin_com & o1_p1 <= (1-pmin_com))

# unattractive
SR_o1_o1_U <- SR_o1_o1 %>% filter(o1_psmall <= pmax_rare) 
SR_o2_o1_U <- SR_o2_o1 %>% filter(o1_psmall <= pmax_rare) 

# attractive
SR_o1_o2_A <- SR_o1_o2 %>% filter(o1_plarge <= pmax_rare) 
SR_o2_o2_A <- SR_o2_o2 %>% filter(o1_plarge <= pmax_rare)

### 4) EV magnitude ------------------------------------------------------------

# small (<= 20% outcome range)
SR_o1_o1_N_S <- SR_o1_o1_N %>% filter(ev_diff_sc <= diffs[1])
SR_o1_o2_N_S <- SR_o1_o2_N %>% filter(ev_diff_sc <= diffs[1])
SR_o2_o1_N_S <- SR_o2_o1_N %>% filter(ev_diff_sc <= diffs[1])
SR_o2_o2_N_S <- SR_o2_o2_N %>% filter(ev_diff_sc <= diffs[1])
SR_o1_o1_U_S <- SR_o1_o1_U %>% filter(ev_diff_sc <= diffs[1])
SR_o2_o1_U_S <- SR_o2_o1_U %>% filter(ev_diff_sc <= diffs[1])
SR_o1_o2_A_S <- SR_o1_o2_A %>% filter(ev_diff_sc <= diffs[1])   
SR_o2_o2_A_S <- SR_o2_o2_A %>% filter(ev_diff_sc <= diffs[1])

# medium (> 20% & < 40% outcome range)
SR_o1_o1_N_M <- SR_o1_o1_N %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])
SR_o1_o2_N_M <- SR_o1_o2_N %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])
SR_o2_o1_N_M <- SR_o2_o1_N %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])
SR_o2_o2_N_M <- SR_o2_o2_N %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])
SR_o1_o1_U_M <- SR_o1_o1_U %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])
SR_o2_o1_U_M <- SR_o2_o1_U %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])
SR_o1_o2_A_M <- SR_o1_o2_A %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])   
SR_o2_o2_A_M <- SR_o2_o2_A %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])

# large (> 40% & <= 60% outcome range)
SR_o1_o1_N_L <- SR_o1_o1_N %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])
SR_o1_o2_N_L <- SR_o1_o2_N %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])
SR_o2_o1_N_L <- SR_o2_o1_N %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])
SR_o2_o2_N_L <- SR_o2_o2_N %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])
SR_o1_o1_U_L <- SR_o1_o1_U %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])
SR_o2_o1_U_L <- SR_o2_o1_U %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])
SR_o1_o2_A_L <- SR_o1_o2_A %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])   
SR_o2_o2_A_L <- SR_o2_o2_A %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])

## risky-risky -------------------------------------------------------------


### 1) EV direction -------------------------------------------------------------------------

RR_o1 <- RR %>% filter(ev_diff > 0)
RR_o2 <- RR %>% filter(ev_diff < 0)

### 2) ordinal comparisons -------------------------------------------

RR_o1_o1 <- RR_o1 %>% filter(o1_rwp > .5)
RR_o1_o2 <- RR_o1 %>% filter(o2_rwp > .5)
RR_o2_o1 <- RR_o2 %>% filter(o1_rwp > .5)
RR_o2_o2 <- RR_o2 %>% filter(o2_rwp > .5)

### 3) rare events -------------------------------------------------------------

# no rare event
RR_o1_o1_N <- RR_o1_o1 %>% filter(o1_p1 >= pmin_com & o1_p1 <= (1-pmin_com) & o2_p1 >= pmin_com & o2_p1 <= (1-pmin_com) )
RR_o1_o2_N <- RR_o1_o2 %>% filter(o1_p1 >= pmin_com & o1_p1 <= (1-pmin_com) & o2_p1 >= pmin_com & o2_p1 <= (1-pmin_com) )
RR_o2_o1_N <- RR_o2_o1 %>% filter(o1_p1 >= pmin_com & o1_p1 <= (1-pmin_com) & o2_p1 >= pmin_com & o2_p1 <= (1-pmin_com) )
RR_o2_o2_N <- RR_o2_o2 %>% filter(o1_p1 >= pmin_com & o1_p1 <= (1-pmin_com) & o2_p1 >= pmin_com & o2_p1 <= (1-pmin_com) )

# unattractive
RR_o1_o1_U <- SR_o1_o1 %>% filter( (o1_psmall <= pmax_rare | o2_psmall <= pmax_rare), !(o1_psmall <= pmax_rare & o2_psmall <= pmax_rare) ) 
RR_o2_o1_U <- SR_o2_o1 %>% filter( (o1_psmall <= pmax_rare | o2_psmall <= pmax_rare), !(o1_psmall <= pmax_rare & o2_psmall <= pmax_rare) ) 

# attractive
RR_o1_o2_A <- SR_o1_o2 %>% filter(  (o1_plarge <= pmax_rare | o2_plarge <= pmax_rare), !(o1_plarge <= pmax_rare & o2_plarge <= pmax_rare)  ) 
RR_o2_o2_A <- SR_o2_o2 %>% filter(  (o1_plarge <= pmax_rare | o2_plarge <= pmax_rare), !(o1_plarge <= pmax_rare & o2_plarge <= pmax_rare)  )

### 4) EV magnitude ------------------------------------------------------------

# small (<= 20% outcome range)
RR_o1_o1_N_S <- SR_o1_o1_N %>% filter(ev_diff_sc <= diffs[1])
RR_o1_o2_N_S <- SR_o1_o2_N %>% filter(ev_diff_sc <= diffs[1])
RR_o2_o1_N_S <- SR_o2_o1_N %>% filter(ev_diff_sc <= diffs[1])
RR_o2_o2_N_S <- SR_o2_o2_N %>% filter(ev_diff_sc <= diffs[1])
RR_o1_o1_U_S <- SR_o1_o1_U %>% filter(ev_diff_sc <= diffs[1])
RR_o2_o1_U_S <- SR_o2_o1_U %>% filter(ev_diff_sc <= diffs[1])
RR_o1_o2_A_S <- SR_o1_o2_A %>% filter(ev_diff_sc <= diffs[1])   
RR_o2_o2_A_S <- SR_o2_o2_A %>% filter(ev_diff_sc <= diffs[1])

# medium (> 20% & < 40% outcome range)
RR_o1_o1_N_M <- RR_o1_o1_N %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])
RR_o1_o2_N_M <- RR_o1_o2_N %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])
RR_o2_o1_N_M <- RR_o2_o1_N %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])
RR_o2_o2_N_M <- RR_o2_o2_N %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])
RR_o1_o1_U_M <- RR_o1_o1_U %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])
RR_o2_o1_U_M <- RR_o2_o1_U %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])
RR_o1_o2_A_M <- RR_o1_o2_A %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])   
RR_o2_o2_A_M <- RR_o2_o2_A %>% filter(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2])

# large (> 40% & <= 60% outcome range)
RR_o1_o1_N_L <- RR_o1_o1_N %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])
RR_o1_o2_N_L <- RR_o1_o2_N %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])
RR_o2_o1_N_L <- RR_o2_o1_N %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])
RR_o2_o2_N_L <- RR_o2_o2_N %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])
RR_o1_o1_U_L <- RR_o1_o1_U %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])
RR_o2_o1_U_L <- RR_o2_o1_U %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])
RR_o1_o2_A_L <- RR_o1_o2_A %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])   
RR_o2_o2_A_L <- RR_o2_o2_A %>% filter(ev_diff_sc > diffs[2] & ev_diff_sc <= diffs[3])


# sample problems ---------------------------------------------------------

## safe-risky --------------------------------------------------------------

# small
SR_o1_o1_N_S_S <- SR_o1_o1_N_S[ sample(1:nrow(SR_o1_o1_N_S), N_trials/12, replace=F) , ] 
SR_o1_o2_N_S_S <- SR_o1_o2_N_S[ sample(1:nrow(SR_o1_o2_N_S), N_trials/12, replace=F) , ]  
SR_o2_o1_N_S_S <- SR_o2_o1_N_S[ sample(1:nrow(SR_o2_o1_N_S), N_trials/12, replace=F) , ] 
SR_o2_o2_N_S_S <- SR_o2_o2_N_S[ sample(1:nrow(SR_o2_o2_N_S), N_trials/12, replace=F) , ] 
SR_o1_o1_U_S_S <- SR_o1_o1_U_S[ sample(1:nrow(SR_o1_o1_U_S), N_trials/6, replace=F) , ] 
SR_o2_o1_U_S_S <- SR_o2_o1_U_S[ sample(1:nrow(SR_o2_o1_U_S), N_trials/6, replace=F) , ] 
SR_o1_o2_A_S_S <- SR_o1_o2_A_S[ sample(1:nrow(SR_o1_o2_A_S), N_trials/6, replace=F) , ]    
SR_o2_o2_A_S_S <- SR_o2_o2_A_S[ sample(1:nrow(SR_o2_o2_A_S), N_trials/6, replace=F) , ]

SR_S <- bind_rows(
  SR_o1_o1_N_S_S ,
  SR_o1_o2_N_S_S ,  
  SR_o2_o1_N_S_S , 
  SR_o2_o2_N_S_S , 
  SR_o1_o1_U_S_S , 
  SR_o2_o1_U_S_S , 
  SR_o1_o2_A_S_S ,    
  SR_o2_o2_A_S_S
  ) %>% 
  mutate(id = row_number())


# medium

SR_o1_o1_N_M_S <- SR_o1_o1_N_M[ sample(1:nrow(SR_o1_o1_N_M), N_trials/12, replace=F) , ] 
SR_o1_o2_N_M_S <- SR_o1_o2_N_M[ sample(1:nrow(SR_o1_o2_N_M), N_trials/12, replace=F) , ]  
SR_o2_o1_N_M_S <- SR_o2_o1_N_M[ sample(1:nrow(SR_o2_o1_N_M), N_trials/12, replace=F) , ] 
SR_o2_o2_N_M_S <- SR_o2_o2_N_M[ sample(1:nrow(SR_o2_o2_N_M), N_trials/12, replace=F) , ] 
SR_o1_o1_U_M_S <- SR_o1_o1_U_M[ sample(1:nrow(SR_o1_o1_U_M), N_trials/6, replace=F) , ] 
SR_o2_o1_U_M_S <- SR_o2_o1_U_M[ sample(1:nrow(SR_o2_o1_U_M), N_trials/6, replace=F) , ] 
SR_o1_o2_A_M_S <- SR_o1_o2_A_M[ sample(1:nrow(SR_o1_o2_A_M), N_trials/6, replace=F) , ]    
SR_o2_o2_A_M_S <- SR_o2_o2_A_M[ sample(1:nrow(SR_o2_o2_A_M), N_trials/6, replace=F) , ]

SR_M <- bind_rows(
  SR_o1_o1_N_M_S ,
  SR_o1_o2_N_M_S ,  
  SR_o2_o1_N_M_S , 
  SR_o2_o2_N_M_S , 
  SR_o1_o1_U_M_S , 
  SR_o2_o1_U_M_S , 
  SR_o1_o2_A_M_S ,    
  SR_o2_o2_A_M_S
) %>% 
  mutate(id = row_number())

# large

SR_o1_o1_N_L_S <- SR_o1_o1_N_L[ sample(1:nrow(SR_o1_o1_N_L), N_trials/12, replace=F) , ] 
SR_o1_o2_N_L_S <- SR_o1_o2_N_L[ sample(1:nrow(SR_o1_o2_N_L), N_trials/12, replace=F) , ]  
SR_o2_o1_N_L_S <- SR_o2_o1_N_L[ sample(1:nrow(SR_o2_o1_N_L), N_trials/12, replace=F) , ] 
SR_o2_o2_N_L_S <- SR_o2_o2_N_L[ sample(1:nrow(SR_o2_o2_N_L), N_trials/12, replace=F) , ] 
SR_o1_o1_U_L_S <- SR_o1_o1_U_L[ sample(1:nrow(SR_o1_o1_U_L), N_trials/6, replace=F) , ] 
SR_o2_o1_U_L_S <- SR_o2_o1_U_L[ sample(1:nrow(SR_o2_o1_U_L), N_trials/6, replace=F) , ] 
SR_o1_o2_A_L_S <- SR_o1_o2_A_L[ sample(1:nrow(SR_o1_o2_A_L), N_trials/6, replace=F) , ]    
SR_o2_o2_A_L_S <- SR_o2_o2_A_L[ sample(1:nrow(SR_o2_o2_A_L), N_trials/6, replace=F) , ]

SR_L <- bind_rows(
  SR_o1_o1_N_L_S ,
  SR_o1_o2_N_L_S ,  
  SR_o2_o1_N_L_S , 
  SR_o2_o2_N_L_S , 
  SR_o1_o1_U_L_S , 
  SR_o2_o1_U_L_S , 
  SR_o1_o2_A_L_S ,    
  SR_o2_o2_A_L_S
) %>% 
  mutate(id = row_number())


## risky-risky -------------------------------------------------------------

# small
RR_o1_o1_N_S_S <- RR_o1_o1_N_S[ sample(1:nrow(RR_o1_o1_N_S), N_trials/12, replace=F) , ] 
RR_o1_o2_N_S_S <- RR_o1_o2_N_S[ sample(1:nrow(RR_o1_o2_N_S), N_trials/12, replace=F) , ]  
RR_o2_o1_N_S_S <- RR_o2_o1_N_S[ sample(1:nrow(RR_o2_o1_N_S), N_trials/12, replace=F) , ] 
RR_o2_o2_N_S_S <- RR_o2_o2_N_S[ sample(1:nrow(RR_o2_o2_N_S), N_trials/12, replace=F) , ] 
RR_o1_o1_U_S_S <- RR_o1_o1_U_S[ sample(1:nrow(RR_o1_o1_U_S), N_trials/6, replace=F) , ] 
RR_o2_o1_U_S_S <- RR_o2_o1_U_S[ sample(1:nrow(RR_o2_o1_U_S), N_trials/6, replace=F) , ] 
RR_o1_o2_A_S_S <- RR_o1_o2_A_S[ sample(1:nrow(RR_o1_o2_A_S), N_trials/6, replace=F) , ]    
RR_o2_o2_A_S_S <- RR_o2_o2_A_S[ sample(1:nrow(RR_o2_o2_A_S), N_trials/6, replace=F) , ]

RR_S <- bind_rows(
  RR_o1_o1_N_S_S ,
  RR_o1_o2_N_S_S ,  
  RR_o2_o1_N_S_S , 
  RR_o2_o2_N_S_S , 
  RR_o1_o1_U_S_S , 
  RR_o2_o1_U_S_S , 
  RR_o1_o2_A_S_S ,    
  RR_o2_o2_A_S_S
) %>% 
  mutate(id = row_number())


# medium

RR_o1_o1_N_M_S <- RR_o1_o1_N_M[ sample(1:nrow(RR_o1_o1_N_M), N_trials/12, replace=F) , ] 
RR_o1_o2_N_M_S <- RR_o1_o2_N_M[ sample(1:nrow(RR_o1_o2_N_M), N_trials/12, replace=F) , ]  
RR_o2_o1_N_M_S <- RR_o2_o1_N_M[ sample(1:nrow(RR_o2_o1_N_M), N_trials/12, replace=F) , ] 
RR_o2_o2_N_M_S <- RR_o2_o2_N_M[ sample(1:nrow(RR_o2_o2_N_M), N_trials/12, replace=F) , ] 
RR_o1_o1_U_M_S <- RR_o1_o1_U_M[ sample(1:nrow(RR_o1_o1_U_M), N_trials/6, replace=F) , ] 
RR_o2_o1_U_M_S <- RR_o2_o1_U_M[ sample(1:nrow(RR_o2_o1_U_M), N_trials/6, replace=F) , ] 
RR_o1_o2_A_M_S <- RR_o1_o2_A_M[ sample(1:nrow(RR_o1_o2_A_M), N_trials/6, replace=F) , ]    
RR_o2_o2_A_M_S <- RR_o2_o2_A_M[ sample(1:nrow(RR_o2_o2_A_M), N_trials/6, replace=F) , ]

RR_M <- bind_rows(
  RR_o1_o1_N_M_S ,
  RR_o1_o2_N_M_S ,  
  RR_o2_o1_N_M_S , 
  RR_o2_o2_N_M_S , 
  RR_o1_o1_U_M_S , 
  RR_o2_o1_U_M_S , 
  RR_o1_o2_A_M_S ,    
  RR_o2_o2_A_M_S
) %>% 
  mutate(id = row_number())

# large

RR_o1_o1_N_L_S <- RR_o1_o1_N_L[ sample(1:nrow(RR_o1_o1_N_L), N_trials/12, replace=F) , ] 
RR_o1_o2_N_L_S <- RR_o1_o2_N_L[ sample(1:nrow(RR_o1_o2_N_L), N_trials/12, replace=F) , ]  
RR_o2_o1_N_L_S <- RR_o2_o1_N_L[ sample(1:nrow(RR_o2_o1_N_L), N_trials/12, replace=F) , ] 
RR_o2_o2_N_L_S <- RR_o2_o2_N_L[ sample(1:nrow(RR_o2_o2_N_L), N_trials/12, replace=F) , ] 
RR_o1_o1_U_L_S <- RR_o1_o1_U_L[ sample(1:nrow(RR_o1_o1_U_L), N_trials/6, replace=F) , ] 
RR_o2_o1_U_L_S <- RR_o2_o1_U_L[ sample(1:nrow(RR_o2_o1_U_L), N_trials/6, replace=F) , ] 
RR_o1_o2_A_L_S <- RR_o1_o2_A_L[ sample(1:nrow(RR_o1_o2_A_L), N_trials/6, replace=F) , ]    
RR_o2_o2_A_L_S <- RR_o2_o2_A_L[ sample(1:nrow(RR_o2_o2_A_L), N_trials/6, replace=F) , ]

RR_L <- bind_rows(
  RR_o1_o1_N_L_S ,
  RR_o1_o2_N_L_S ,  
  RR_o2_o1_N_L_S , 
  RR_o2_o2_N_L_S , 
  RR_o1_o1_U_L_S , 
  RR_o2_o1_U_L_S , 
  RR_o1_o2_A_L_S ,    
  RR_o2_o2_A_L_S
) %>% 
  mutate(id = row_number())


# store problems ----------------------------------------------------------
