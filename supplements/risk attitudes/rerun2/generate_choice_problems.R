pacman::p_load(tidyverse)
source("code/helper_functions/fun_generate_choice_problems.R") # call function for generating choice problems

# generate 10,000 choice problems

set.seed(16251)
initial_set <- generate_choice_problems(n = 10000, lower = 0, upper = 10)

# sample test set of 60 choice problems

test_set <- tibble()

# stratified sampling of 20 choice problems with no/attractive/unattractive rare outcome
initial_set <- initial_set %>%
  mutate(rare = case_when(p_r_low >= .2 & p_r_low <= .8 ~ "none" ,
                          p_r_low < .2 ~ "unattractive" ,
                          p_r_low > .8 ~ "attractive"))


set.seed(16715)
for(i in unique(initial_set$rare)) {
  type <- initial_set %>% filter(rare == i) # type of rare rare outcome
  smpl <- sample(seq_len(nrow(type)), size = 1000) # random sample of 1000 choice problems
  test_set <- bind_rows(test_set, type[smpl, ])
}

write_rds(test_set, "supplements/risk attitudes/rerun2/problems.rds")
