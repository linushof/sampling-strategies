pacman::p_load(tidyverse)
source("code/helper_functions/fun_generate_choice_problems.R") # call function for generating choice problems

# generate 10,000 safe-risky choice problems

set.seed(349)
initial_set <- generate_choice_problems(n = 10000, lower = 0, upper = 20)
initial_set <- initial_set %>% mutate(rare = case_when(p_r_low >= .2 & p_r_low <= .8 ~ "none",
                                                       p_r_low < .2 ~ "unattractive",
                                                       p_r_low > .8 ~ "attractive"))

# select subset of 60 choice problems
# stratified sampling of 20 choice problems with no/attractive/unattractive rare outcome

test_set <- tibble()

set.seed(735)
for(i in unique(initial_set$rare)) {
  type <- initial_set %>% filter(rare == i) # type of rare rare outcome
  smpl <- sample(seq_len(nrow(type)), size = 20) # random sample of 20 choice problems
  test_set <- bind_rows(test_set, type[smpl, ])
}

write_rds(test_set, "data/choice_problems.rds")