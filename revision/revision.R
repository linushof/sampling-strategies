
# load packages
pacman::p_load(tidyverse, digest, readxl)

# load data
problems <- as.data.frame(read_rds("data/choice_problems.rds"))
simulation_summary <- read_rds("data/simulation_summary.rds.bz2")
simulation_roundwise <- read_rds("data/simulation_roundwise.rds.bz2")

# summary comparison ------------------------------------------------------

simulation_summary <- left_join(simulation_summary, problems, by=join_by(id)) # add problem features

# compute summaries of choice trials
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


# roundwise comparison ----------------------------------------------------

simulation_roundwise <- left_join(simulation_roundwise, problems, by=join_by(id)) # add problem features

# compute summaries of choice trials
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

# storage -----------------------------------------------------------------

choices <- bind_rows(choice_data_summary, choice_data_roundwise) # merge data sets
checksum_choices <- digest(choices, "sha256")
write_rds(choices, "data/choice_data.rds.bz2", compress = "bz2")






















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


problems <- as.data.frame(read_xlsx("data/choice_problems_general.xlsx"))
View(problems)
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



## Sample size -------------------------------------------------------------


### Summary -----------------------------------------------------------------

sample_size <- simulation_summary %>% 
  group_by(base, rate, theta, id, agent) %>% 
  summarise(size = max(smp)) %>% 
  ungroup()

sample_size_strategies <- sample_size %>% 
  group_by(base, rate, theta) %>% 
  summarise(median = median(size) , 
            mean = mean(size)) %>% 
  ungroup()

sample_size_strategies %>% 
  ggplot(aes(x=base, y=mean, color = theta, group = theta)) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(title = "Summary Comparison", 
       x = "Switch Rate\n(Search Rule)",
       y = "Sample Size",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  facet_wrap(~rate) +
  theme_minimal(base_size = 18) 


### Roundwise ---------------------------------------------------------------

sample_size_M <- simulation_roundwise %>% 
  group_by(base, rate, theta, id, agent) %>% 
  summarise(size = max(smp)) %>% 
  ungroup()

sample_size_strategies_M <- sample_size_M %>% 
  group_by(base, rate, theta) %>% 
  summarise(median = median(size) , 
            mean = mean(size)) %>% 
  ungroup()

View(sample_size_strategies_M)


sample_diff_roundwise <- tibble(sample_size_strategies_S ,
                                S = sample_size_strategies_S$median , 
                                M = sample_size_strategies_M$median  ,
                                Diff = abs(S-M)/S) 

View(sample_diff_roundwise)

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
