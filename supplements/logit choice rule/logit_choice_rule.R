# load packages
pacman::p_load(tidyverse , 
               viridis , 
               scico, 
               patchwork, 
               ggpubr, 
               papaja, 
               latex2exp)

# load data
cpt <- read_rds("data/cpt_estimates.rds")
postpred <- read_rds("supplements/posterior predictives/posterior_predictives.rds.bz2")

label_theta <- function(string) {
  TeX(paste("$\\theta=$", string, sep = ""))
}

rho_summary <- cpt %>%
  filter(model == "summary", threshold == "relative", parameter == "rho") %>% 
  ggplot(aes(psi, mean)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0,1,length.out = 3)) + 
  scale_y_continuous(limits = c(-.5, 5.5), breaks = seq(0,5, length.out = 3)) + 
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Consistency  ", varphi))) +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

choice_prob_summary <- postpred %>% 
  filter(model == "summary", threshold == "relative") %>% 
  mutate(alpha.col = ifelse(alpha > 1, TRUE, FALSE)) %>% 
  ggplot(aes(x = psi, y = p_safe_risky, group = psi)) +
  geom_violin(aes(fill = alpha.col)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = "p(safe, risky)", 
       fill = expression(paste(alpha > 1, "  "))) +
  scale_fill_manual(values = c("white", "gray")) + 
  scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0,1,length.out = 3)) + 
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0,1, length.out = 3)) + 
  theme_minimal(base_size = 20)

rho_summary + choice_prob_summary + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A") & theme(legend.position = "top")
ggsave(file = "supplements/logit choice rule/figures/logit_summary.png", width = 14, height = 7)

### combine plots
rho_roundwise <- cpt %>%
  filter(model == "roundwise", threshold == "relative", parameter == "rho") %>% 
  ggplot(aes(psi, mean)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0,1,length.out = 3)) + 
  scale_y_continuous(limits = c(-.5, 5.5), breaks = seq(0,5, length.out = 3)) + 
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = expression(paste("Consistency  ", varphi))) +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 20)

choice_prob_roundwise <- postpred %>% 
  filter(model == "roundwise", threshold == "relative") %>% 
  ggplot(aes(x = psi, y = p_safe_risky, group = psi)) +
  geom_violin() +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), scales = "free") +
  labs(x = expression(paste("Switching Probability  ", psi)), 
       y = "p(safe, risky)") +
  scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0,1,length.out = 3)) + 
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0,1, length.out = 3)) + 
  theme_minimal(base_size = 20)

rho_roundwise + choice_prob_roundwise + plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(file = "supplements/logit choice rule/figures/logit_roundwise.png", width = 14, height = 7)
