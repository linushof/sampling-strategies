# simulation data

simulation_summary <- read_rds("data/compressed/simulation_summary.rds.bz2")

simulation_summary <- simulation_summary %>% 
  rename(psi = s,
         threshold = boundary,
         theta = a, 
         problem = gamble,
         p_x_low = a_p1,
         x_low = a_o1,
         p_x_high = a_p2,
         x_high = a_o2,
         p_safe = b_p1,
         safe = b_o1,
         x_ev = a_ev,
         r = A,
         s = B,
         r_sum = A_sum,
         s_sum = B_sum) %>% 
  mutate(attended = if_else(attended == "a", "r", "s"), 
         choice = if_else(choice == "A", "r", "s")) %>% 
  select(-c(b_p2, b_o2))
                             
## save compressed data
write_rds(simulation_summary, file = "data/compressed/simulation_summary.rds.bz2", compress = "bz2")
