
# preparation -------------------------------------------------------------

pacman::p_load(tidyverse, digest, crayon, readxl)

# load packages 
pacman::p_load(tidyverse,
               scico, # for scientific color palettes
               latex2exp, # for LaTeX expressions in plots
               papaja,
               ggpubr,
               patchwork, 
               readxl,
               viridis,
               viridisLite,
               hrbrthemes)


problems <- as.data.frame(read_rds("data/choice_problems.rds"))

# Fixed N -------------------------------------------------------

round_fixed20 <- round %>% filter(smp <= 20) %>% mutate(model = 'roundwise')
summary_fixed20 <- summary %>% filter(smp <= 20) %>%  mutate(model = 'summary')
fixed20 <- bind_rows(round_fixed20, summary_fixed20)

dat <- fixed20 %>% 
  group_by(model, psi, theta, id, agent) %>% 
  mutate(max_n = max(smp)) %>% 
  ungroup() %>% 
  filter(smp==max_n)

dat2 <- dat %>% 
  mutate(fixed_n_choice = case_when(D > 0 ~ 'r' , 
                                    D < 0 ~ 's' ,
                                    D == 0 ~ sample(c('r','s'), 1, replace = T, prob = c(.5,.5) )
  )   )

dat2 <- left_join(dat2, problems, by=join_by(id))

## expected values
rates_EV <- dat2 %>%
  mutate(norm = case_when(ev_risky/safe > 1 ~ "r", 
                          ev_risky/safe < 1 ~ "s")) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == fixed_n_choice, 1, 0)) %>% 
  group_by(model, psi, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0))

# plot data 

### round-wise
max_EV_roundwise <- rates_EV %>%
  filter(model == "roundwise") %>% 
  ggplot(aes(psi, rate, group = theta, color = as.factor(theta))) +
  scale_color_scico_d(palette = "imola", end = .9, guide = guide_legend(reverse = TRUE)) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(.4, 1), breaks = seq(.5, 1, length.out = 3)) +
  labs(title = "Roundwise Comparison", 
       x = "Switch Rate\n(Search Rule)",
       y = "% EV Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 18) + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))


## expected value

### summary
max_EV_summary <- rates_EV %>%
  filter(model == "summary") %>% 
  mutate(theta = .01 * theta) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(.4, 1), breaks = seq(.5, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switch Rate\n(Search Rule)",
       y = "% EV Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 18) + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

max_EV <- ggarrange(max_EV_summary, max_EV_roundwise, nrow = 1)
ggsave(file='revision/fixed_20_EV_maximization.png', width=14, height=6)



# EV differences ---------------------------------------------------

## Problems -------------------------------------

# larger EV differences for subsample of problems
problems

problems <- problems %>% 
  mutate(safe = safe * 2 , 
         r_1 = r_1 * 2 , 
         r_2 = r_2 *2 , 
         ev_risky = p_r_1*r_1 + p_r_2*r_2 , 
         better_ev = if_else(ev_risky > safe, 'risky', 'safe'))

## Simulation --------------------------------------------------------------


### Summary ------------------------------------------------------

param <- expand.grid(psi = seq(.1, 1, .1) , 
                     theta = seq(100, 300, 50))

n_agents <- 2 # specify number of agents (iterations per strategy and problem)

set.seed(36151) # seed random number generator to make simulations reproducible

# loop over strategies
param_list <- vector("list", nrow(param)) 
for (set in seq_len(nrow(param))) {
  
  # retrieve settings for the search rule and the stopping rule
  psi <- param[[set,"psi"]]
  theta <- param[[set, "theta"]]
  
  # loop over choice problems
  problem_list <- vector("list", nrow(problems))
  for (problem in seq_len(nrow(problems))) {
    
    # retrieve problem features
    
    # option 1 (for SR gambles: risky option)
    o1_1 <- problems[[problem, "o1_1"]] # (smaller) outcome 1 
    o1_2 <- problems[[problem, "o1_2"]] # (larger) outcome 2 
    o1_p1 <- problems[[problem, "o1_p1"]] # probability outcome 1  
    o1_p2 <- problems[[problem, "o1_p2"]] # probability outcome 2 
    
    # option 2 (for SR gambles: safe option)
    o2_1 <- problems[[problem, "o2_1"]] # (smaller/safe) outcome 1
    o2_2 <- problems[[problem, "o2_2"]] # (larger/missing) outcome 2
    o2_p1 <- problems[[problem, "o2_p1"]] # probability outcome 1 (for SR gambles: 1) 
    o2_p2 <- problems[[problem, "o2_p2"]] # probability outcome 2 (for SR gambles: 0)
    
    # loop over agents
    agents_list <- vector("list", n_agents)
    for (agent in seq_along(1:n_agents)){
      
      # specify settings to initiate each choice trial
      
      at <- NULL # attended option
      smp_total <- 0 # total number of sampled outcomes
      smp_total_1 <- 0 # total number of sampled outcomes (option 1)
      smp_total_2 <- 0 # total number of sampled outcomes (option 2)
      out_1 <- NULL # sampled outcome (option 1)
      out_2 <- NULL # sampled outcome (option 2)
      D_1 <- 0 # accumulated evidence (option 1) 
      D_2 <- 0 # accumulated evidence (option 2)
      boundary_reached <- FALSE # choice trial is stopped when TRUE
      
      # specify vectors to store sampling & evidence history
      
      smp <- NULL # total number of sampled outcomes
      D <- NULL # trajectory of decision variable
      choice <- NULL # final choice (risky or safe or no choice)
      
      # START SAMPLING AND ACCUMULATION PROCESS
      
      init <- sample(c("o1", "o2"), size=1) # randomly choose option to initially sample from
      attend <- init
      while(boundary_reached == FALSE) {
        
        # update number of samples
        
        at <- c(at, attend)
        smp_total <- smp_total + 1
        smp <- c(smp, smp_total)
        
        # sample outcome and update evidence for respective option
        
        if(attend == "o1") {# option 1
          
          smp_total_1 <- smp_total_1 + 1
          sampled_outcome <- sample(x = c(o1_1, o1_2), size = 1, prob = c(o1_p1, o1_p2))
          out_1 <- c(out_1, sampled_outcome)
          out_2 <- c(out_2, NA)
          D_1 <- D_1 + sampled_outcome
          p_attend_1 <- 1-psi # probability of remaining on option 1 (for the next sample)
          
        } else {# safe option
          
          smp_total_2 <- smp_total_2 + 1
          sampled_outcome <- sample(x = c(o2_1, o2_2), size = 1, prob = c(o2_p1, o2_p2)) 
          out_2 <- c(out_2, sampled_outcome)
          out_1 <- c(out_1, NA)
          D_2 <- D_2 + sampled_outcome
          p_attend_1 <- psi # probability of switching to option 1 (for the next sample)
          
        }
        
        # update decision variable
        
        if(smp_total_1 > 0 & smp_total_2 > 0){ # choice is only possible after both options were sampled
          diff <- D_1 - D_2
          D <- c(D, diff)
        } else { 
          diff <- 0
          D <- c(D, diff)
        }
        
        # check if the decision threshold is reached: if true, make a choice and stop trial
        
        boundary <- ifelse(diff >= theta, "o1", ifelse(diff <= -1*theta, "o2", NA))
        choice <- c(choice, boundary)
        
        if(is.na(boundary)) { # if threshold isn't reached (no choice), determine from which option to sample next and continue
          
          attend <- sample(c("o1", "o2"), size=1, prob=c(p_attend_1, 1-p_attend_1))
          
        } else {
          
          boundary_reached <- TRUE
          
          # STOP SAMPLING AND ACCUMULATION PROCESS
          
        }
        
      } # close loop choice trial
      agents_list[[agent]] <- data.frame(agent, smp, at, out_1, out_2, D, choice)
    } # close loop agents
    all_agents <- bind_rows(agents_list)
    problem_list[[problem]] <- as.data.frame(expand_grid(id=problems[problem,"id"], all_agents))
  } # close loop choice problems
  all_problems <- bind_rows(problem_list) 
  param_list[[set]] <- as.data.frame(expand_grid(param[set, ], all_problems))
  print(paste("\u2713 Parameter Set No. ", set, " finished!"))
} # close loop strategies
simulation_summary <- bind_rows(param_list)

### Roundwise ----------------------------------------------------


# specify parameters for search rule (psi; switch rate) and stopping rule (theta; threshold)
param <- expand.grid(psi = seq(.1, 1, .1) ,
                     theta = 1:5)

n_agents <- 2 # specify number of agents (iterations per strategy and problem)

set.seed(6535) # seed random number generator to make simulations reproducible

# loop over strategies
param_list <- vector("list", nrow(param))
for (set in seq_len(nrow(param))) {
  
  # retrieve settings for the search rule and the stopping rule
  psi <- param[[set,"psi"]]
  theta <- param[[set, "theta"]]
  
  # loop over choice problems
  problem_list <- vector("list", nrow(problems))
  for (problem in seq_len(nrow(problems))) {
    
    # retrieve problem features
    
    # option 1 (for SR gambles: risky option)
    o1_1 <- problems[[problem, "o1_1"]] # (smaller) outcome 1 
    o1_2 <- problems[[problem, "o1_2"]] # (larger) outcome 2 
    o1_p1 <- problems[[problem, "o1_p1"]] # probability outcome 1  
    o1_p2 <- problems[[problem, "o1_p2"]] # probability outcome 2 
    
    # option 2 (for SR gambles: safe option)
    o2_1 <- problems[[problem, "o2_1"]] # (smaller/safe) outcome 1
    o2_2 <- problems[[problem, "o2_2"]] # (larger/missing) outcome 2
    o2_p1 <- problems[[problem, "o2_p1"]] # probability outcome 1 (for SR gambles: 1) 
    o2_p2 <- problems[[problem, "o2_p2"]] # probability outcome 2 (for SR gambles: 0)
    
    # loop over agents
    agents_list <- vector("list", n_agents)
    for (agent in seq_along(1:n_agents)){
      
      # specify settings to initiate each choice trial
      
      at <- NULL # attended option 
      smp_total <- 0 # total number of sampled outcomes
      out_1 <- NULL # sampled outcome (option 1)
      out_2 <- NULL # sampled outcome (option 2)
      round <- 1 # number of comparison rounds
      round_smp_1_total <- 0 # number of option 1 samples (within a round) 
      round_sum_1_total <- 0 # sum of option 1 samples (within a round)
      round_smp_2_total <- 0 # number of option 2 samples (within a round) 
      round_sum_2_total <- 0 # sum of option 2 samples (within a round)
      diff <- 0 # accumulated evidence
      boundary_reached <- FALSE # choice trial is stopped when TRUE
      
      # specify vectors to store sampling & evidence history
      
      smp <- NULL # total number of sampled outcomes 
      D <- NULL # trajectory of decision variable
      choice <- NULL # final choice (risky or safe or no choice)
      win <- NULL # round win indicator
      all_rounds <- NULL
    
      # START SAMPLING AND ACCUMULATION PROCESS
      
      init <- sample(c("o1", "o2"), size=1) # randomly choose option to initially sample from
      attend <- init
      while(boundary_reached == FALSE) {
        
        # start new comparison round on the initially sampled option
        
        while(attend == init) { 
          
          # update total number of samples
          
          at <- c(at, attend)
          smp_total <- smp_total + 1
          smp <- c(smp, smp_total)
          
          # sample outcome for the initially sampled option and update number and sum of sampled outcomes (round level)
          
          if(attend == "o1") {# risky option
            
            sampled_outcome <- sample(x = c(o1_1, o1_2), size = 1, prob = c(o1_p1, o1_p2))
            out_1 <- c(out_1, sampled_outcome)
            out_2 <- c(out_2, NA)
            round_smp_1_total <- round_smp_1_total + 1
            round_sum_1_total <- round_sum_1_total + sampled_outcome
            p_attend_1 <- 1-psi # probability of remaining on the risky option (for the next sample)
            
          } else {# safe option
            
            sampled_outcome <- sample(x = c(o2_1, o2_2), size = 1, prob = c(o2_p1, o2_p2))
            out_2 <- c(out_2, sampled_outcome)
            out_1 <- c(out_1, NA)
            round_smp_2_total <- round_smp_2_total + 1
            round_sum_2_total <- round_sum_2_total + sampled_outcome
            p_attend_1 <- psi # probability of switching to the safe option (for the next sample)
            
          }
          
          # determine from which option to sample next
          
          attend <- sample(c("o1", "o2"), size = 1, prob = c(p_attend_1, 1-p_attend_1)) # switching according to psi
          
          # add an entry to the following vectors, indicating that round is not completed yet
          win <- c(win, NA) # no round winner is determined 
          D <- c(D, diff) # decision variable does not change
          choice <- c(choice, NA) # no choice determined
          
        } 
        
        # switch to the other option
        
        while(attend != init) {
          
          # update total number of samples
          
          at <- c(at, attend)
          smp_total <- smp_total + 1
          smp <- c(smp, smp_total)
          
          # sample outcome for the other option and update number and sum of sampled outcomes (round level)
          
          if(attend == "o1") {# option 1
            
            sampled_outcome <- sample(x = c(o1_1, o1_2), size = 1, prob = c(o1_p1, o1_p2))
            out_1 <- c(out_1, sampled_outcome)
            out_2 <- c(out_2, NA)
            round_smp_1_total <- round_smp_1_total + 1
            round_sum_1_total <- round_sum_1_total + sampled_outcome
            p_attend_1 <- 1-psi # probability of remaining on the risky option (for the next sample)
            
          } else {# option 2
            
            sampled_outcome <- sample(x = c(o2_1, o2_2), size = 1, prob = c(o2_p1, o2_p2))
            out_2 <- c(out_2, sampled_outcome)
            out_1 <- c(out_1, NA)
            round_smp_2_total <- round_smp_2_total + 1
            round_sum_2_total <- round_sum_2_total + sampled_outcome
            p_attend_1 <- psi # probability of switching to the safe option (for the next sample)
            
          }
          
          # determine from which option to sample next
          
          attend <- sample(c("o1", "o2"), size = 1, prob = c(p_attend_1, 1-p_attend_1)) # switching according to psi
          
          # check for switch back to the initial option
          
          if(attend!=init){ # no switch to initial option (round incomplete)
            
            # add an entry to the following vectors, indicating that round is not completed yet
            win <- c(win, NA)
            D <- c(D, diff)
            choice <- c(choice, NA)
            
          }
        } 
        
        # switch to initial option (round complete): determine round winner and update decision variable
        
        round_mean_1 <- round_sum_1_total/round_smp_1_total # average sampled outcome (option 1, round level)
        round_mean_2 <- round_sum_2_total/round_smp_2_total # average sampled outcome (option 2, round level)
        round_winner <- ifelse(round_mean_1 > round_mean_2, 1, ifelse(round_mean_1 < round_mean_2, -1, 0))
        ###### continue here#####\\\\\\
        win <- c(win, round_winner)
        diff <- diff + round_winner
        D <- c(D, diff)
        
        #round_store <- data.frame(round, round_smp_r, round_sum_r, win)
        round_store <- data.frame(round, win)
        all_rounds <- rbind(all_rounds, round_store)
        
        # check if decision threshold is reached: if true, make a choice and stop trial
        
        boundary <- ifelse(diff >= theta, "o1", ifelse(diff <= -1*theta, "o2", NA))
        choice <- c(choice, boundary)
        
        if(is.na(boundary)) { # if threshold isn't reached (no choice), start new comparison round
          
          round <- round + 1
          round_smp_1_total <- 0
          round_sum_1_total <- 0
          round_smp_2_total <- 0
          round_sum_2_total <- 0
          win <- NULL
          
        } else {
          
          boundary_reached <- TRUE
          
          # STOP SAMPLING AND ACCUMULATION PROCESS
          
        }
        
      } # close loop choice trial
      agents_list[[agent]] <- data.frame(agent, smp, at, out_1, out_2, all_rounds, D, choice)
    } # close loop agents
    all_agents <- bind_rows(agents_list)
    problem_list[[problem]] <- as.data.frame(expand_grid(id=problems[problem,"id"], all_agents))
  } # close loop choice problems
  all_problems <- bind_rows(problem_list)
  param_list[[set]] <- as.data.frame(expand_grid(param[set, ], all_problems))
  print(paste("\u2713 Parameter Set No. ", set, " finished!"))
} # close loop strategies
simulation_roundwise <- bind_rows(param_list)

## Sample size -------------------------------------------------------------


### Summary -----------------------------------------------------------------

sample_size <- simulation_summary %>% 
  group_by(psi, theta, id, agent) %>% 
  summarise(size = max(smp)) %>% 
  ungroup()

sample_size_strategies <- sample_size %>% 
  group_by(psi, theta) %>% 
  summarise(median = median(size) , 
            mean = mean(size)) %>% 
  ungroup()

sample_size_strategies %>% 
  ggplot(aes(x=psi, y=mean, color = theta, group = theta)) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switch Rate\n(Search Rule)",
       y = "Sample Size",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 18) 


### Roundwise ---------------------------------------------------------------

sample_size_roundwise <- simulation_roundwise %>% 
  group_by(psi, theta, id, agent) %>% 
  summarise(size = max(smp)) %>% 
  ungroup()

sample_size_roundwise %>% 
  group_by(psi, theta) %>% 
  summarise(median = median(size) , 
            mean = mean(size)) %>% 
  View()


## Maximization ------------------------------------------------------------


### Summary -----------------------------------------------------------------

choice_data_summary <- simulation_summary %>%
  group_by(psi, theta, id, agent) %>%
  mutate(n_smp = n(), # number of samples
         smp_s = sum(is.na(out_r)), # number of samples safe option
         smp_r = n_smp - smp_s, # number of samples risky option
         sp_r_1 = round(sum(if_else(out_r == r_1, 1, 0), na.rm = TRUE)/smp_r, 2), # sampled probability risky outcome 1
         sp_r_2 = round(1 - sp_r_1, 2), # sampled probability risky outcome 2
         avg_r = round(mean(out_r, na.rm = TRUE), 2)) %>% # sampled average risky option
  ungroup() %>%
  filter(!is.na(choice)) # discard samples without choice

# tidy data 
choice_data_summary <- choice_data_summary %>% 
  mutate(model = "summary") %>%
  select(model, psi, theta, id, agent, n_smp, smp_s, smp_r, sp_r_1, sp_r_2, avg_r, choice) %>% 
  rename(smp = "n_smp")

choice_data_summary <- left_join(choice_data_summary, problems, by=join_by(id))

## expected values

rates_EV <- choice_data_summary %>%
  mutate(norm = case_when(ev_risky/safe > 1 ~ "r", 
                          ev_risky/safe < 1 ~ "s")) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice, 1, 0)) %>% 
  group_by(model, psi, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0))


max_EV_summary <- rates_EV %>%
  filter(model == "summary") %>% 
  mutate(theta = .01 * theta) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(.4, 1), breaks = seq(.5, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switch Rate\n(Search Rule)",
       y = "% EV Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 18) + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

max_EV_summary 

ggsave(file='revision/EV_max_summary_largerDiff.png')



## sampled average
rates_EV_exp <- choice_data_summary  %>%
  mutate(norm = case_when(avg_r/safe > 1 ~ "r", 
                          avg_r/safe < 1 ~ "s")) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice, 1, 0)) %>% 
  group_by(model, psi, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0))


### summary
max_EV_exp_summary <- rates_EV_exp %>%
  filter(model == "summary") %>% 
  mutate(theta = .01 * theta) %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(.4, 1), breaks = seq(.5, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switch Rate\n(Search Rule)",
       y = "% SM Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 18) + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

max_EV_exp_summary 

ggsave(file='revision/SM_max_summary_largerDiff.png')



### Roundwise ---------------------------------------------------------------

choice_data_roundwise <- simulation_roundwise %>% 
  group_by(psi, theta, id, agent) %>% 
  mutate(n_smp = n(), # number of samples
         smp_s = sum(is.na(out_r)), # number of samples safe option
         smp_r = n_smp - smp_s, # number of samples risky option
         sp_r_1 = round(sum(if_else(out_r == r_1, 1, 0), na.rm = TRUE)/smp_r, 2), # sampled probability risky outcome 1
         sp_r_2 = round(1 - sp_r_1, 2), # sampled probability risky outcome 2
         avg_r = round(mean(out_r, na.rm = TRUE), 2)) %>% # sampled average risky option
  ungroup() %>%
  filter(!is.na(choice)) # discard samples without choice

# tidy data
choice_data_roundwise <- choice_data_roundwise %>% 
  mutate(model = "roundwise") %>%
  select(model, psi, theta, id, agent, n_smp, smp_s, smp_r, sp_r_1, sp_r_2, avg_r, choice) %>% 
  rename(smp = "n_smp")

choice_data_roundwise <- left_join(choice_data_roundwise, problems, by=join_by(id))

rates_EV <- choice_data_roundwise %>%
  mutate(norm = case_when(ev_risky/safe > 1 ~ "r", 
                          ev_risky/safe < 1 ~ "s")) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice, 1, 0)) %>% 
  group_by(model, psi, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0))

max_EV_roundwise <- rates_EV %>%
  filter(model == "roundwise") %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  #scale_y_continuous(limits = c(.4, 1), breaks = seq(.5, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switch Rate\n(Search Rule)",
       y = "% EV Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 18) + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

max_EV_roundwise


ggsave(file='revision/EV_max_summary_largerDiff.png')

problems %>% filter(p_r_2 < .5) %>% nrow()

## sampled average
rates_EV_exp <- choice_data_roundwise %>%
  mutate(norm = case_when(avg_r/safe > 1 ~ "r", 
                          avg_r/safe < 1 ~ "s")) %>% # determine option with higher sampled mean 
  filter(!is.na(norm)) %>% # drop options without normative choice 
  mutate(max = ifelse(norm == choice, 1, 0)) %>% 
  group_by(model, psi, theta, max) %>% 
  summarise(n = n()) %>% 
  mutate(rate = round(n/sum(n), 2)) %>% 
  ungroup() %>%
  filter(!(max == 0))


### summary
max_EV_exp_summary <- rates_EV_exp %>%
  filter(model == "roundwise") %>% 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  #scale_y_continuous(limits = c(.4, 1), breaks = seq(.5, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switch Rate\n(Search Rule)",
       y = "% SM Maximization",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_minimal(base_size = 18) + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

max_EV_exp_summary

ggsave(file='revision/SM_max_summary_largerDiff.png')


View(choice_data_roundwise)
choice_data_roundwise %>% group_by(choice) %>% summarise(n=n())
