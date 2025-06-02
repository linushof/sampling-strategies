library(tidyverse)
library(scico)

dat <- expand_grid(time = 0:40 , 
                   rate = c(.1,.3,.5) , 
                   base = seq(.1,.9,.2) 
                   )

dat %<>% mutate(decay = base + (1 - base) * exp(-rate * time))

ggplot(dat, aes(x=time,y=decay, color = base, group = base)) +
  geom_line(linewidth = 2) + 
  scale_y_continuous(limits=c(0,1), breaks = seq(0,1,.1)) + 
  facet_wrap(~rate) +
  scale_color_scico(palette="tokyo") + 
  theme_minimal() 
