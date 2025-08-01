library(readxl)
library(tidyverse)

# SR_small: safe-risky (original) -------------------------------------------------------
SR_small <- as.data.frame(read_xlsx("data/problems/SR_small.xlsx"))
write_rds(SR_small, "data/problems/SR_small.rds")

# SR_large: safe-risky with larger absolute EV difference (SR_large) -----------------------------------
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
write_rds(SR_large, "data/problems/SR_large.rds")

# RR: risky-risky build from SR_large (narrow) -------------------------------------------------------------

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
write_rds(RR, "data/problems/RR.rds")



# RR: risky-risky simulated -------------------------------------------------------------
# import functions for generating choice problems
source("code/helper_functions/fun_gambles_risky.R")

# problem/filter settings ------------------------------------------------

N <- 1e7 #1e7
lower <- 0 # lowest possible outcome 
upper <- 100 # highest possible outcome
pmin <- .1 # lowest possible probability 
pmin_com <- .4 # lowest probability for non-rare events
pmax_rare <- .2 # highest probability for rare events
N_trials <- 200 # number of trials per agent
diffs <- c(.05,.10,.15,.20) # magnitude of EV differences

# generate problem pool -------------------------------------------------------

set.seed(667122)
pool <- RR_gambles(N, lower, upper, pmin)

# prepare problems -----------------------------------------------------

RR <- pool |>
  filter(o1_1!=o1_2 & o2_1!=o2_2) |> 
  
  # we start by determining which option is more risky (higher coefficient of variation)
  # we label the more (less) risky option o1 (o2) - similarly to safe-risky problems, where o1 (o2) is the risky (safe) option
  
  mutate( 
    cvar_o1 = sqrt( round( (o1_1^2) * o1_p1 + (o1_2^2) * o1_p2, 4 ) - round((o1_1 * o1_p1 + o1_2 * o1_p2)^2, 4 ))  ,
    cvar_o2 = sqrt( round( (o2_1^2) * o2_p1 + (o2_2^2) * o2_p2, 4 ) - round((o2_1 * o2_p1 + o2_2 * o2_p2)^2, 4 ))  ,
    higher_risk = if_else(cvar_o1 > cvar_o2, "o1", "o2") , 
    o1_1_new = if_else(cvar_o1 > cvar_o2, o1_1, o2_1) ,
    o1_p1_new = if_else(cvar_o1 > cvar_o2, o1_p1, o2_p1) ,
    o1_2_new = if_else(cvar_o1 > cvar_o2, o1_2, o2_2) ,
    o1_p2_new = if_else(cvar_o1 > cvar_o2, o1_p2, o2_p2) ,
    o2_1_new = if_else(cvar_o1 > cvar_o2, o2_1, o1_1) ,
    o2_p1_new = if_else(cvar_o1 > cvar_o2, o2_p1, o1_p1) ,
    o2_2_new = if_else(cvar_o1 > cvar_o2, o2_2, o1_2) ,
    o2_p2_new = if_else(cvar_o1 > cvar_o2, o2_p2, o1_p2) 
  ) |>
  select(o1_1_new:o2_p2_new) |> 
  rename(o1_1 = o1_1_new ,
         o1_p1 = o1_p1_new ,
         o1_2 = o1_2_new ,
         o1_p2 = o1_p2_new ,
         o2_1 = o2_1_new ,
         o2_p1 = o2_p1_new ,
         o2_2 = o2_2_new ,
         o2_p2 = o2_p2_new) |> 
  
  # now we calculate some important properties for both options 
  
  mutate(
    o1_ev = round( o1_1*o1_p1 + o1_2*o1_p2 , 2) ,
    o2_ev = round( o2_1*o2_p1 + o2_2*o2_p2 , 2) ,
    ev_diff = o1_ev-o2_ev , 
    ev_diff_sc = round(abs(ev_diff)/upper , 2) , 
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
    o2_rwp = 1-o1_rwp ,
    o1_psmall = if_else(o1_1 < o1_2, o1_p1, o1_p2) , 
    o1_plarge = 1-o1_psmall , 
    o2_psmall = if_else(o2_1 < o2_2, o2_p1, o2_p2) , 
    o2_plarge = 1-o2_psmall
  ) |> 
  select(-c(c11:cp22))

# apply filters -----------------------------------------------------------
# We stratify the problem pool by applying a sequence of filters. 
# To create the problem set for simulation, we randomly sample from each of the strata/subgroups.
# Stratified sampling prevents a spurious/unbalanced distribution of problem features
# and ensures an interpretable baseline behavior in the aggregate (across all problems).

RR <- RR |> 
  mutate(better_ev = if_else(o1_ev > o2_ev, "o1", "o2") , # Filter 1: option with better EV
         better_rwp = if_else(o1_rwp > o2_rwp, "o1", "o2") , # Filter 2: option wither roundwise winning probability (rwp)
         o1_rare = case_when(
           o1_p1 >= pmin_com & o1_p1 <= (1-pmin_com) ~ "none" ,
           o1_psmall <= pmax_rare ~ "unattractive" ,
           o1_plarge <= pmax_rare  ~ "attractive"
         ) ,
         o2_rare = case_when(
           o2_p1 >= pmin_com & o2_p1 <= (1-pmin_com) ~ "none" ,
           o2_psmall <= pmax_rare ~ "unattractive" ,
           o2_plarge <= pmax_rare  ~ "attractive"
         ),
         ev_mag = case_when(ev_diff_sc > diffs[1] & ev_diff_sc <= diffs[2] ~ "small" , 
                            ev_diff_sc > diffs[3] & ev_diff_sc <= diffs[4] ~ "large") 
  ) 

groups <-  
  RR |>
  drop_na(o1_rare, o2_rare, ev_mag) |> 
  group_by (better_ev, better_rwp, o1_rare, o2_rare, ev_mag, .drop = FALSE) |>
  summarise(group = cur_group_id(),
            n = n()) |> 
  filter(!(better_ev=="o1" & better_rwp=="o1" & o1_rare=="attractive" & o2_rare=="unattractive") , 
         !(better_ev=="o1" & better_rwp=="o2" & o1_rare=="unattractive" & o2_rare=="attractive") , 
         !(better_ev=="o1" & better_rwp=="o2" & o1_rare=="unattractive" & o2_rare=="unattractive") , 
         !(better_ev=="o2" & better_rwp=="o2" & o1_rare=="unattractive" & o2_rare=="attractive") , 
         !(better_ev=="o2" & better_rwp=="o1" & o1_rare=="attractive" & o2_rare=="unattractive"),
         !(better_ev=="o2" & better_rwp=="o1" & o1_rare=="attractive" & o2_rare=="attractive")
         )

# sample simulation set ---------------------------------------------------------

problems <- tibble()
set.seed(71618)
for (i in seq_len(nrow(groups)) ){ 
  
  dat_temp <- RR |> 
    filter(better_ev==groups[[i, "better_ev"]] & 
             better_rwp==groups[[i, "better_rwp"]] & 
             o1_rare==groups[[i, "o1_rare"]] & 
             o2_rare==groups[[i, "o2_rare"]] &
             ev_mag==groups[[i, 'ev_mag']]
    )
  
  smp_tmp <- dat_temp[ sample(1:nrow(dat_temp), 5, replace=F) , ] 
  
  problems <- bind_rows(problems, smp_tmp)
  
}

problems <- 
  problems |> 
  mutate(id=row_number()) |> 
  select(id, everything()) |> 
  select(!(o1_psmall:o2_plarge))

write_rds(problems, 'data/problems/RR2.rds')
