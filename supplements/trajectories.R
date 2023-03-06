pacman::p_load(tidyverse, papaja, scico)

# load data
data_round <- read_rds("data/simulation_roundwise.rds.bz2") 

# data wrangling 
data_round_reduced <- data_round %>%
  mutate(psi = 1-(psi+.5)) %>% # recode switching probability
  filter(theta == 5  & psi %in% c((1-.9),1) & threshold == "relative", problem == 43) %>%  
  filter(!is.na(diff))

data_round_reduced %>% 
  ggplot(aes(x = round, y = diff, group_by = agent, color = as.factor(psi))) + 
  facet_wrap(~psi, nrow = 2) + 
  geom_line(position = position_dodge(width = 1), size = .3) + 
  geom_hline(yintercept = c(-5, 5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0,50, 5)) + 
  scale_y_continuous(breaks = seq(-5,5,2)) + 
  scale_color_scico_d(palette = "tokyo", begin = .2, end = .8) + 
  annotate("text", x = 50, y=4.5, label = "Risky") + 
  annotate("text", x = 50, y=- 4.5, label = "Safe") + 
  theme_minimal() + 
  labs(x = "Comparison Round",
       y = expression(paste(Delta, " Round Wins")),
       color = expression(psi))


