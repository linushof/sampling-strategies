# preparation -------------------------------------------------------------
rm(list = ls())

## packages ----------------------------------------------------------------

pacman::p_load(tidyverse,
               scico, # for scientific color palettes
               latex2exp, # for LaTeX expressions in plots
               ggpubr,
               patchwork, 
               viridis,
               gtools)

## helper functions --------------------------------------------------------

source('code/helper_functions/fun_plot_labels.R') # ggplot theme and labels

## data ----------------------------------------------------------------

# choice data
dir_choices <- "data/choices/"
choice_files <- list.files(dir_choices, pattern='choices')
choices <- lapply(paste0(dir_choices, choice_files), read_rds)
names(choices) <- choice_files |> str_remove(".rds.bz2")

# cumulative prospect theory (cpt)
dir_cpt <- "data/cpt/"
cpt_files <- list.files(dir_cpt, pattern='cpt')
cpt_estimates <- lapply(paste0(dir_cpt, cpt_files), read_rds)
names(cpt_estimates) <- cpt_files |> str_remove(".rds")

# additional problem and simulation data for main analyses + appendix
pbs <- read_rds("data/problems/SR_large.rds") # choice problems
summary <- read_rds("data/simulations/simulation_summary_SR_large.rds.bz2") # sampling + choice summary
round <- read_rds("data/simulations/simulation_roundwise_SR_large.rds.bz2") # sampling + choice roundwise

# overview all problem sets --------------------------------------------------------
'Run loops in the following 3 subsections for core analyses in all problem sets.
Analyses include maximization and risk behavior (+ ecological analysis) and cpt
analyses. The results are stored in a respective subfolder' 

## maximization ------------------------------------------------------

for(i in 1:length(choices)){
  sim <- names(choices)[[i]]
  if(str_detect(sim, "decreasing")==FALSE){
    
    dat <- choices[[i]]
    model <- regmatches(sim,  gregexpr("summary|roundwise", sim))[[1]]
    
    rates <- dat |> 
      mutate(better_avg = if_else(o1_avg/o2_avg > 1, "o1", "o2"),  # determine option with higher sampled mean 
             max_avg = if_else(better_avg == choice, 1, 0), 
             max_ev = if_else(better_ev == choice, 1, 0)
      ) |>  
      group_by(model, psi, theta) |>  
      summarise(rate_avg = mean(max_avg, na.rm=T), 
                rate_ev = mean(max_ev, na.rm=T)
      ) |> 
      ungroup() |> 
      pivot_longer(rate_avg:rate_ev, values_to = "rate", names_to = "Benchmark", names_prefix = "rate_") |> 
      mutate(Benchmark = factor(Benchmark, levels = c("ev", "avg"), labels = c("EV", "SM")))
    
    
    rates_eco <- dat |> 
      mutate(max_ev = if_else(better_ev == choice, 1, 0)) |>  
      group_by(model, psi, theta, o1_rare, o2_rare) |>  
      summarise(rate_ev = mean(max_ev, na.rm=T)) |> 
      ungroup()
    
    max_plot <- rates |> 
      ggplot(aes(psi, rate, group = theta, color = theta)) +
      facet_wrap(~Benchmark, nrow=2) +
      scale_color_scico(palette = "imola", end = .9) + 
      scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
      scale_y_continuous(limits = c(.4, 1), breaks = seq(.5, 1, length.out = 3)) +
      labs(title = paste(capwords(model), "Comparison"), 
           x = "Switch Rate\n(Search Rule)",
           y = "Rate",
           color = "Threshold\n(Stopping Rule)") +
      geom_line(linewidth = 1) + 
      geom_point(size = 3) +
      theme_bw() + 
      theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))
    
    ### merge and save plots
    plotfile <- paste0("results/maximization_",sim,".jpg")
    ggsave(file = plotfile, plot=max_plot, units="mm", width = 90, height = 180)
    print(paste("New plot", plotfile, "was generated."))
    
    
    max_plot_eco <- rates_eco |> 
      ggplot(aes(psi, rate_ev, group = theta, color = theta)) +
      facet_grid(factor(o1_rare, levels = c("none", "attractive", "unattractive"))~factor(o2_rare, levels = c("none", "attractive", "unattractive"))) +
      scale_color_scico(palette = "imola", end = .9) + 
      scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
      scale_y_continuous(limits = c(.4, 1), breaks = seq(.5, 1, length.out = 3)) +
      labs(title = paste(capwords(model), "Comparison"), 
           x = "Switch Rate\n(Search Rule)",
           y = "Rate",
           color = "Threshold\n(Stopping Rule)") +
      geom_line(linewidth = 1) + 
      geom_point(size = 3) +
      theme_bw(base_size = 18) + 
      theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))
    
    plotfile <- paste0("results/eco_max_",sim,".png")
    ggsave(file = plotfile, plot=max_plot_eco, width = 10, height = 10)
    print(paste("New plot", plotfile, "was generated."))
    
  }
}

## risk -----------------------------------------------------------

for(i in 1:length(choices)){
  sim <- names(choices)[[i]]
  if(str_detect(sim, "decreasing")==FALSE){
    
    dat <- choices[[i]]
    model <- regmatches(sim,  gregexpr("summary|roundwise", sim))[[1]]
    
    rates <- dat |>  
      mutate(risk = ifelse(choice == higher_risk, 1, 0)) %>% # risk seeking choice
      group_by(model, psi, theta) |> 
      summarise(rate = mean(risk, na.rm=T)) |> 
      ungroup() 
    
    rates_eco <- dat |>  
      mutate(risk = ifelse(choice == higher_risk, 1, 0)) %>% # risk seeking choice
      group_by(model, psi, theta, o1_rare, o2_rare) |> 
      summarise(rate = mean(risk, na.rm=T)) |> 
      ungroup() 
    
    risk_plot <- rates |> 
      ggplot(aes(psi, rate, group = theta, color = theta)) +
      scale_color_scico(palette = "imola", end = .9) + 
      scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, length.out = 3)) +
      labs(title = paste(capwords(model), "Comparison"), 
           x = "Switch Rate\n(Search Rule)",
           y = "Rate",
           color = "Threshold\n(Stopping Rule)") +
      geom_line() + 
      geom_point(size = 1) +
      theme_bw() + 
      theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))
    
    ### merge and save plots
    plotfile <- paste0("results/risk_",sim,".jpg")
    ggsave(file = plotfile, plot=risk_plot, units="mm", width = 90, height = 90)
    print(paste("New plot", plotfile, "was generated."))
    
    
    risk_plot_eco <- rates_eco |> 
      ggplot(aes(psi, rate, group = theta, color = theta)) +
      facet_grid(factor(o1_rare, levels = c("none", "attractive", "unattractive"))~factor(o2_rare, levels = c("none", "attractive", "unattractive"))) +
      scale_color_scico(palette = "imola", end = .9) + 
      #scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, length.out = 3)) +
      labs(title = paste(capwords(model), "Comparison"), 
           x = "Switch Rate\n(Search Rule)",
           y = "Rate",
           color = "Threshold\n(Stopping Rule)") +
      geom_line(linewidth = 1) + 
      geom_point() +
      theme_bw() + 
      theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))
    
    ### merge and save plots
    plotfile <- paste0("results/eco_risk_",sim,".png")
    ggsave(file = plotfile, plot=risk_plot_eco, units = "mm", width = 90, height = 190)
    print(paste("New plot", plotfile, "was generated."))
    
  }
}

## cpt ---------------------------------------------------------------------

for(i in 1:length(cpt_estimates)){
  sim <- names(cpt_estimates)[[i]]
  if(str_detect(sim, "decreasing")==FALSE){
    
    dat <- cpt_estimates[[i]]
    model <- regmatches(sim,  gregexpr("summary|roundwise", sim))[[1]]
    
    # probability weighting function
    
    ## prepare data
    weights <- dat |> 
      select(model, psi, theta, parameter, mean) |> 
      pivot_wider(names_from = parameter, values_from = mean) |>  
      select(model, psi, theta, gamma, delta) |> 
      expand_grid(p = seq(0, 1, .05)) |>  # create vector of sampled relative frequencies
      mutate(w = round(  (delta * p^gamma)/ ((delta * p^gamma)+(1-p)^gamma), 2)) # compute decision weights (see Goldstein & Einhorn, 1987) using the parameter estimates  
    
    ## plot data
    
    #### gamma
    gamma <- dat |> 
      filter(parameter == "gamma") |> 
      ggplot(aes(psi, mean, color = psi)) + 
      scale_color_scico(palette = "tokyo", end = .8) +
      facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
      scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) + 
      scale_y_continuous(limits =c(-.1, 10.1), breaks = seq(0,10, length.out = 3)) +
      labs(x = "Switch Rate (Search Rule)", 
           y = expression(paste("Curvature  ", gamma)),
           color = "Switch\nRate") +
      geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
      geom_point(size = 3) +
      geom_line(linewidth = 1) +
      theme_minimal() + 
      theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))
    
    #### delta
    delta <- dat %>%
      filter(parameter == "delta") %>%
      ggplot(aes(psi, mean, color = psi)) +
      scale_color_scico(palette = "tokyo", end = .8) +
      facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
      scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) +
      scale_y_continuous(limits = c(-0.1, 10.1), breaks = seq(0, 10, length.out = 3)) +
      labs(x = "Switch Rate (Search Rule)", 
           y = expression(paste("Elevation  ", delta)), 
           color = "Switch\nRate") +
      geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) + 
      geom_point(size = 3) +
      geom_line(linewidth = 1) +
      theme_minimal() + 
      theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))
    
    ####  probability weighting
    wf <- weights %>% 
      ggplot(aes(p, w, group = psi, color = psi)) +
      scale_color_scico(palette = "tokyo", end = .8) +
      facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
      scale_x_continuous(breaks = seq(0, 1, length.out = 3)) +
      scale_y_continuous(breaks = seq(0, 1, length.out = 3)) +
      labs(x = TeX("$\\p_{high}$"),
           y = TeX("$w(\\p_{high})$"),
           color = "Switch\nRate") +
      geom_line(linewidth = 1) +
      theme_minimal() + 
      theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))
    
    ### merge and save plots
    weighting_plot <- ggarrange(wf, gamma, delta, nrow = 3, legend = "right", common.legend = T, labels = "AUTO", font.label = list(size = 22))
    plotfile_weighting <- paste0("results/cpt_weighting_",sim,".jpg")
    
    
    ## value function 
    values <- dat |> 
      select(model, psi, theta, parameter, mean) |> 
      pivot_wider(names_from = parameter, values_from = mean) |> 
      select(psi, theta, alpha) |> 
      expand_grid(x = seq(0, 20, 1)) |> # create vector of possible outcomes
      mutate(v = round(x^alpha, 2)) # compute subjective values on the basis of estimated parameters
    
    # plot data 
    
    ### alpha
    alpha <- dat |> 
      filter(parameter == "alpha") |>  
      ggplot(aes(psi, mean, color = psi)) +
      scale_color_scico(palette = "tokyo", end = .8) + 
      facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
      scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0,1, length.out = 3)) + 
      scale_y_continuous(limits = c(-0.1, 2.1), breaks = seq(0,2, length.out = 3)) + 
      labs(x = "Switch Rate (Search Rule)", 
           y = expression(paste("Outcome Sensitivity  ", alpha)),
           color = "Switch\nRate") +
      geom_linerange(aes(ymin=`2.5%`, ymax=`97.5%`), linewidth = 1) +
      geom_point(size = 3) +
      geom_line(linewidth = 1) +
      theme_minimal() + 
      theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))
    
    ### value function 
    vf <- values |>  
      ggplot(aes(x, v, group = psi, color = psi)) +
      scale_color_scico(palette = "tokyo", end = .8) +
      facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
      scale_x_continuous(limits = c(-1, 21), breaks = seq(0, 20, length.out = 3)) +
      scale_y_continuous(limits = c(-1, 20), breaks = seq(0, 20, length.out = 3)) +
      labs(x = "Sampled Outcome",
           y = "Subjective Value",
           color = "Switch\nRate") +
      geom_line(linewidth = 1) +
      theme_minimal() + 
      theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))
    
    # merge
    value_plot <- ggarrange(vf, alpha, nrow = 2, legend = "right", common.legend = T, labels = "AUTO", font.label = list(size = 22) )
    plotfile_value <- paste0("results/cpt_value_",sim,".jpg")
    
    
    ## posterior predictive check
    
    # pre-processing
    cpt_clean <- dat |> 
      select(model:mean) |> 
      filter(parameter %in% c('alpha', 'gamma', 'delta', 'phi')) |>  
      pivot_wider(names_from = parameter, values_from = mean) |>  
      mutate(across(alpha:phi, ~round(., 2)))
    
    choices_clean <- choices[[i]]
    ppset <- choices_clean |> 
      left_join(cpt_clean, by = c("model", "psi", "theta")) %>% 
      filter(!c(o1_smp == 0 | o2_smp == 0)) %>% 
      mutate(choice_obs = if_else(choice == "o1", 1,0) ,
             o1_low = if_else(o1_1 < o1_2, o1_1, o1_2) ,
             o1_sp_low = if_else(o1_1 < o1_2, o1_sp1, o1_sp2) , 
             o1_high = if_else(o1_1 > o1_2, o1_1, o1_2) , 
             o1_sp_high = if_else(o1_1 > o1_2, o1_sp1, o1_sp2) ,
             o2_low = if_else(o2_1 < o2_2 , o2_1, o2_2) ,
             o2_sp_low = if_else(o2_1 < o2_2 , o2_sp1, o2_sp2) , 
             o2_high = if_else(o2_1 > o2_2, o2_1, o2_2) , 
             o2_sp_high = if_else(o2_1 > o2_2, o2_sp1, o2_sp2) 
      )
    
    # make  predictions
    
    set.seed(681271)
    postpred <- ppset %>% mutate(
      o1_w_high = round( (delta * o1_sp_high^gamma) / ( (delta*o1_sp_high^gamma)+(1-o1_sp_high)^gamma ), 2) , 
      o1_w_low = 1 - o1_w_high , 
      o1_v_high = o1_high^alpha , 
      o1_v_low = o1_low^alpha , 
      V_o1 = (o1_w_high * o1_v_high) + (o1_w_low * o1_v_low) ,
      V_o1_scaled = V_o1^(1/alpha) ,
      o2_w_high = round( (delta * o2_sp_high^gamma) / ( (delta*o2_sp_high^gamma)+(1-o2_sp_high)^gamma ), 2) , 
      o2_w_low = 1 - o2_w_high , 
      o2_v_high = o2_high^alpha , 
      o2_v_low = o2_low^alpha , 
      V_o2 = (o2_w_high * o2_v_high) + (o2_w_low * o2_v_low) ,
      V_o2_scaled = V_o2^(1/alpha) ,
      V_diff = V_o1_scaled-V_o2_scaled , 
      p_o1_o2 = round(1 / ( 1 + exp(-phi*V_diff) ) , 2) ,
      choice_pp = rbinom(n=nrow(ppset), size=1, prob=p_o1_o2))
    
    # post-processing
    
    ## compute observed choice rates 
    
    ### risky choice proportions
    
    ### maximization rates
    
    
    max_rates_obs <- choices_clean |>
      mutate(better_avg = if_else(o1_avg/o2_avg > 1, "o1", "o2"),  # determine option with higher sampled mean 
             max_avg = if_else(better_avg == choice, 1, 0) , 
             norm = if_else(better_avg=="o1", "Risky Option", "Safe Option")) |>
      group_by(model, psi, theta, norm) |>  
      summarise(rate_avg = mean(max_avg, na.rm=T)) |>
      ungroup() |> 
      mutate(type = "Sampling Strategy")
    
    
    max_rates_pp <- postpred %>% 
      mutate(better_avg = if_else(o1_avg/o2_avg > 1, 1, 0),  # determine option with higher sampled mean 
             max_avg = if_else(better_avg == choice_pp, 1, 0) , 
             norm = if_else(better_avg==1, "Risky Option", "Safe Option")) |>
      group_by(model, psi, theta, norm) |>  
      summarise(rate_avg = mean(max_avg, na.rm=T)) |>
      ungroup() |> 
      mutate(type = "CPT Posterior Predictive")
    
    max_rates <- bind_rows(max_rates_obs, max_rates_pp)
    
    ## compute predictive accuracy
    
    pp_acc <- postpred %>% 
      select(model, psi, theta, alpha, gamma, delta, phi, id, agent, choice_obs, choice_pp) %>% 
      mutate(match = if_else(choice_obs == choice_pp, 1, 0)) %>% 
      group_by(model, psi, theta, alpha, gamma, delta, phi, match) %>% 
      summarise(count = n()) %>% 
      mutate(perc = round(count/sum(count), 3)) %>% 
      filter(match != 0) %>% 
      ungroup()
    
    # plot data 
    
    max_rates_p <- max_rates %>% 
      ggplot(aes(x=psi, y=rate_avg, group = interaction(norm, type), color = norm)) + 
      facet_wrap(~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), nrow = 1) + 
      geom_point(aes(shape = type), size = 3, alpha = .7) + 
      geom_line() + 
      scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0,1,length.out = 3)) + 
      scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
      scale_shape_manual(values = c(15, 19)) + 
      scale_color_scico_d(palette = "managua", begin = .1, end = .9) + 
      labs(x = "Switch Rate (Search Rule)" ,
           y = "Proportion of\nMaximizing Choices" ,
           color = "Better Average",
           shape = "") + 
      theme_minimal(base_size = 18) + 
      theme(strip.text.y = element_blank(), legend.position = "top") + 
      theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))
    
    pp_acc_p <- pp_acc %>%  
      ggplot(aes(x = psi, y = perc)) + 
      scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
      scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
      facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
      geom_point(size = 3, shape = 18) +
      geom_line() +
      labs(x = "Switch Rate (Search Rule)" , 
           y = "Proportion of\nCorrect Predictions", 
           color = "Choice Consistency") + 
      theme_minimal(base_size = 18) + 
      theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))
    
    ppc_plot <- ggarrange(max_rates_p, pp_acc_p, nrow = 2, common.legend = TRUE, labels = "AUTO", font.label = list(size = 22))
    plotfile_ppc <- paste0("results/cpt_ppc_",sim,".jpg")
    
    
    # save plots
    ggsave(file = plotfile_weighting, plot=weighting_plot, units="mm" , width = 190, height = 190)
    ggsave(file = plotfile_value, plot=value_plot, units="mm" , width = 190, height = 120)
    ggsave(file = plotfile_ppc, plot=ppc_plot, units="mm" , width = 190, height = 190)
    print(paste("New plot", plotfile_weighting, "was generated."))
    print(paste("New plot", plotfile_value, "was generated."))
    print(paste("New plot", plotfile_ppc, "was generated."))
    
  }
}


# cpt_estimates2 <- cpt_estimates[str_detect(names(cpt_estimates), "SR_large")] 
# cpt_estimates2 <- cpt_estimates2 |> bind_rows()
# # # check convergence 
# cpt <- cpt_estimates2  |>  filter(parameter != "lp__") 
# max(cpt$Rhat) # 1.005 --> < 1.01
# mean(cpt$Rhat) # 1.001267 --> 1.001
# min(cpt$n_eff) # 2200
# mean(cpt$n_eff) # 27198








# main analyses -----------------------------------------------------------

'This section entails the source code underlying the analyses reported 
in the main text of the manuscript. 
The analyses focus on safe-risky problems with larger EV differences.'

## data --------------------------------------------------------------------

# get data on relevant problem set (SR_large) 
choices_main <- choices[str_detect(names(choices), "SR_large")] |> 
  bind_rows()

cpt_main <- cpt_estimates[str_detect(names(cpt_estimates), "SR_large")] |> 
  bind_rows()

## Figure 2: trajectories --------------------------------------------------------------
# select and prepare data

## problems 5 and 19 for illustration
problems <- c(5,19)
round_sub <- round |>  filter(id %in% problems) |> mutate(model="Roundwise")
summary_sub <- summary |>  filter(id %in% problems) |>  mutate(model="Summary")
pbs <- pbs |> filter(id %in% problems)

##  medium thresholds for illustration
boundary <- 2

## compute median trajectory for each sampling strategy
summary_median <- summary_sub |>  
  group_by(model, psi, theta, id, smp) |> 
  summarise(count = n(), # number of sampling agents
            median = median(D)) %>%  # median evidence across sampling agents 
  slice(seq_len(min(which(median <= -theta | median >= theta), n()))) # remove samples after median hit the threshold

round_median <- round_sub %>% 
  group_by(model, psi, theta, id, smp) |> 
  summarise(count = n(), # number of sampling agents
            median = median(D)) %>% # median evidence across sampling agents
  slice(seq_len(min(which(median %in% c(-theta, theta)), n()))) # remove samples after median hit the threshold

summary_sub <- summary_sub %>% 
  filter((near(psi, 0.1) | near(psi, 0.7) | near(psi, 1)),
         theta == boundary) %>% 
  mutate(D = D) %>% 
  mutate(D = case_when(D < -boundary ~ -boundary , 
                       D > boundary ~ boundary , 
                       D >= -boundary & D <= boundary ~ D))

round_sub <- round_sub %>%
  filter((near(psi, 0.1) | near(psi, 0.7) | near(psi, 1)),
         theta == boundary)

summary_median_sub <- summary_median %>%  
  filter((near(psi, 0.1) | near(psi, 0.7) | near(psi, 1)),
         theta == boundary) %>%
  mutate(median = case_when(median < -boundary ~ -boundary, 
                            median > boundary ~ boundary, 
                            median >= -boundary & median <= boundary ~ median))

round_median_sub <- round_median %>% 
  filter((near(psi, 0.1) | near(psi, 0.7) | near(psi, 1)),
         theta == boundary)

# plot data 

# create labels
ann_risky <- data.frame(model = c("Summary", "Roundwise"), 
                        psi=c(.1,.1), 
                        smp = c(65,65), 
                        D=c(1.7,1.7), 
                        label=c("Threshold Risky Option", "Threshold Risky Option")
)

ann_safe <- data.frame(model = c("Summary", "Roundwise"), 
                       psi=c(.1,.1), 
                       smp = c(65,65), 
                       D=c(-1.7,-1.7), 
                       label=c("Threshold Safe Option", "Threshold Safe Option")
)

p1_label <- paste0("Risky=[", pbs[1,"o1_1"],",",pbs[1,"o1_p1"],"; ",pbs[1,"o1_1"],"; EV=", pbs[1,"o1_ev"],"]","\n",
                   "Safe=[",pbs[1,"o2_1"],",",pbs[1,"o2_p1"], "; EV=",pbs[1,"o2_1"],"]")
p2_label <- paste0("Risky=[", pbs[2,"o1_1"],",",pbs[2,"o1_p1"],"; ",pbs[2,"o1_1"],"; EV=", pbs[2,"o1_ev"],"]","\n",
                   "Safe=[",pbs[2,"o2_1"],",",pbs[2,"o2_p1"], "; EV=",pbs[2,"o2_1"],"]")
problem_labels <- c(p1_label, p2_label)

stopping_samples_summary <- summary_sub %>%
  group_by(id, psi, agent) %>%
  summarise(stopping_smp = max(smp), .groups = "drop")

median_sub <- bind_rows(summary_median_sub, round_median_sub)
round_sub <- round_sub |> select(-c(round, win))
sub <- bind_rows(summary_sub, round_sub)

# summary comparison
trajectories <- sub |> 
  ggplot(aes(x = smp, y = D)) + 
  facet_grid(factor(model, levels=c("Summary", "Roundwise"))~psi, 
             labeller = labeller(psi = as_labeller(label_psi, default = label_parsed)), 
             scales = "free_x") + 
  scale_y_continuous(limits = c(-boundary, boundary), 
                     breaks = seq(-boundary, boundary, boundary)) +
  labs(title = "", 
       x = "Number of Sampled Outcomes",
       y = "Decision Variable",
       color = "Choice\nProblem",
       alpha = "Agent\nCount", 
       linewidth = "Agent\nCount") +
  geom_hline(yintercept = c(-boundary, 0, boundary), linetype = "dashed") + 
  scale_x_continuous(limits=c(0,100)) +
  geom_line(aes(group = agent), position = position_dodge(width = .1), linewidth = .2, alpha = .1, color="gray") + 
  geom_line(data = median_sub, aes(y = median, alpha = count, linewidth = count, color=as.factor(id))) +
  geom_vline(xintercept=14)+
  geom_text(data = ann_risky, label=ann_risky$label, size.unit = "pt", size = 9, vjust = .6) + 
  geom_text(data = ann_safe, label=ann_safe$label, size.unit = "pt", size = 9) +
  scale_alpha_continuous(labels=function(x) format(x, big.mark = ",", scientific = F), range = c(.3, 1)) +
  scale_linewidth_continuous(range = c(.2, 1)) +
  scale_color_manual(labels = problem_labels, values = c("#DF9D56", "#6EB5E1")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

ggsave(file = "manuscript/figures/accumulation.jpg", plot = trajectories, units="mm", width = 190, height = 190*.75, dpi = 500)

## Figure 3: maximization ------------------------------------------------------------

# filter data sets for the same problem set 
rates <- choices_main |>
  mutate(better_avg = if_else(o1_avg/o2_avg > 1, "o1", "o2"),  # determine option with higher sampled mean 
         max_avg = if_else(better_avg == choice, 1, 0), 
         max_ev = if_else(better_ev == choice, 1, 0)) |>
  group_by(model, psi, theta) |>  
  summarise(rate_avg = mean(max_avg, na.rm=T), 
            rate_ev = mean(max_ev, na.rm=T) ) |>
  ungroup() |> 
  pivot_longer(rate_avg:rate_ev, values_to = "rate", names_to = "Benchmark", names_prefix = "rate_") |> 
  mutate(Benchmark = factor(Benchmark, levels = c("ev", "avg"), labels = c("EV", "SM")))

max_plot <- rates |> 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_grid(Benchmark~factor(model, levels=c("summary", "roundwise"), labels=c("Summary", "Roundwise"))) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_x_continuous(limits = c(-.05, 1.05), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(breaks = seq(.5, 1, length.out = 3)) +
  labs(x = "Switch Rate\n(Search Rule)",
       y = "Maximization Rate",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ggsave("manuscript/figures/maximization.jpg", plot=max_plot, units="mm", width = 190, height = 190*.75)

## Figure 4: risk --------------------------------------------------------------------

rates <- choices_main |>  
  mutate(risk = ifelse(choice == higher_risk, 1, 0)) |>  # risk seeking choice
  group_by(model, psi, theta) |> 
  summarise(rate = mean(risk, na.rm=T)) |> 
  ungroup() 

risk_plot <- rates |> 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_wrap(~factor(model, levels=c("summary", "roundwise"), labels=c("Summary", "Roundwise"))) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_x_continuous(limits = c(-.05, 1.05), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Switch Rate\n(Search Rule)",
       y = "Risky Choice Rate",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth=1, alpha = .5) + 
  geom_point(size = 3) +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ggsave("manuscript/figures/risk.jpg", plot=risk_plot, units="mm", width = 190, height = 190*.50)

## Figures 5-8: cpt ---------------------------------------------------------------------

### probability weighting ---------------------------------------------------

# compute decision weights based on cpt estimates
weights <- cpt_main |> 
  select(model, psi, theta, parameter, mean, `2.5%`, `97.5%`) |> 
  pivot_wider(names_from = parameter, values_from = c(mean, `2.5%`, `97.5%`)) |>  
  select(model, psi, theta, 
         gamma_mean = mean_gamma, delta_mean = mean_delta,
         gamma_low = `2.5%_gamma`, delta_low = `2.5%_delta`,
         gamma_high = `97.5%_gamma`, delta_high = `97.5%_delta`) |> 
  expand_grid(p = seq(0, 1, .001)) |>  
  mutate(
    # mean weights
    w = (delta_mean * p^gamma_mean) / ((delta_mean * p^gamma_mean) + (1 - p)^gamma_mean),
    # lower bound weights
    w_low = (delta_low * p^gamma_low) / ((delta_low * p^gamma_low) + (1 - p)^gamma_low),
    # upper bound weights
    w_high = (delta_high * p^gamma_high) / ((delta_high * p^gamma_high) + (1 - p)^gamma_high)
  )

# summary comparison
weights_summary <- weights |> filter(model=="summary")
cpt_summary <- cpt_main |> filter(model=="summary")

gamma <- cpt_summary |> 
    filter(parameter == "gamma") |> 
    ggplot(aes(psi, mean, color = psi)) + 
    scale_color_scico(palette = "tokyo", end = .8) +
    facet_grid(parameter~theta, labeller = labeller(parameter=as_labeller(label_gamma, default = label_parsed),  theta = as_labeller(label_theta, default = label_parsed))) +
    scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) + 
    scale_y_continuous(limits =c(-.1, 2.1), breaks = seq(0,2, length.out = 3)) +
    labs(x = "Switch Rate (Search Rule)", 
         y = "Estimate",
         color = "Switch\nRate") +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), width=.1) + 
    geom_point() +
    geom_line() +
    theme_bw() + 
    theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))
  
delta <- cpt_summary %>%
    filter(parameter == "delta") %>%
    ggplot(aes(psi, mean, color = psi)) +
    scale_color_scico(palette = "tokyo", end = .8) +
    facet_grid(parameter~theta, labeller = labeller(parameter=as_labeller(label_delta, default = label_parsed),  theta = as_labeller(label_theta, default = label_parsed))) +
    scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) +
    scale_y_continuous(limits = c(-0.1, 5.1), breaks = seq(0, 5, length.out = 3)) +
    labs(x = "Switch Rate (Search Rule)", 
         y = "Estimate", 
         color = "Switch\nRate") +
    geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), width=.1) + 
    geom_point() +
    geom_line() +
    theme_bw() + 
    theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))
  
wf <- weights_summary %>% 
  mutate(parameter="Weighting Function") |> 
  ggplot(aes(p, w, group = psi, color = psi, fill = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  scale_fill_scico(palette = "tokyo", end = .8) +
  facet_grid(parameter~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 3)) +
  labs(x = "p (Sampled Frequency)",
       y = "w(p)",
       color = "Switch\nRate",
       fill = "Switch\nRate") +
  geom_ribbon(aes(ymin = w_low, ymax = w_high), alpha = 0.3, color = NA) + 
  geom_line() +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25),
        panel.grid.major = element_line(linewidth = .25))
  
weighting_plot <- ggarrange(wf, gamma, delta, nrow = 3, legend = "right", common.legend = T)
ggsave(file = "manuscript/figures/cpt_weighting_summary.jpg", plot=weighting_plot, units="mm" , width = 190, height = 190)


# roundwise comparison 
weights_round <- weights |> filter(model=="roundwise", !(near(psi,1) & theta==1)) 
cpt_round <- cpt_main |> filter(model=="roundwise", !(near(psi,1) & theta==1) )

gamma <- cpt_round |> 
  filter(parameter == "gamma") |> 
  ggplot(aes(psi, mean, color = psi)) + 
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_grid(parameter~theta, labeller = labeller(parameter=as_labeller(label_gamma, default = label_parsed),  theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) + 
  scale_y_continuous(limits =c(-.1, 10.1), breaks = seq(0,10, length.out = 3)) +
  labs(x = "Switch Rate (Search Rule)", 
       y = "Estimate",
       color = "Switch\nRate") +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), width=.1) + 
  geom_point() +
  geom_line() +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

delta <- cpt_round %>%
  filter(parameter == "delta") %>%
  ggplot(aes(psi, mean, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  facet_grid(parameter~theta, labeller = labeller(parameter=as_labeller(label_delta, default = label_parsed),  theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-0.1,1.1), breaks = seq(0,1, length.out = 3)) +
  scale_y_continuous(limits = c(-0.1, 5.1), breaks = seq(0, 5, length.out = 3)) +
  labs(x = "Switch Rate (Search Rule)", 
       y = "Estimate", 
       color = "Switch\nRate") +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), width=.1) + 
  geom_point() +
  geom_line() +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

wf <- weights_round %>% 
  mutate(parameter="Weighting Function") |> 
  ggplot(aes(p, w, group = psi, color = psi, fill = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  scale_fill_scico(palette = "tokyo", end = .8) +
  facet_grid(parameter~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 3)) +
  labs(x = "p (Sampled Frequency)",
       y = "w(p)",
       color = "Switch\nRate",
       fill = "Switch\nRate") +
  geom_ribbon(aes(ymin = w_low, ymax = w_high), alpha = 0.3, color = NA) + 
  geom_line() +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25),
        panel.grid.major = element_line(linewidth = .25))

weighting_plot <- ggarrange(wf, gamma, delta, nrow = 3, legend = "right", common.legend = T)
ggsave(file = "manuscript/figures/cpt_weighting_roundwise.jpg", plot=weighting_plot, units="mm" , width = 190, height = 190)


### outcome sensitivity -----------------------------------------------------

## value function
upper_range <- 20
values <- cpt_main |> 
    select(model, psi, theta, parameter, mean, `2.5%`, `97.5%`) |> 
    pivot_wider(names_from = parameter, values_from = c(mean, `2.5%`, `97.5%`)) |> 
    select(model, psi, theta, 
           alpha_mean = mean_alpha, 
           alpha_low = `2.5%_alpha`, 
           alpha_high = `97.5%_alpha`) |> 
    expand_grid(x = seq(0, upper_range, .01)) |> # create vector of possible outcomes
    mutate(v = x^alpha_mean ,
           v_low = x^alpha_low , 
           v_high = x^alpha_high
           ) # compute subjective values on the basis of estimated parameters

# summary comparison
values_summary <- values |> filter(model=="summary")

# alpha
alpha <- cpt_summary |> 
  filter(parameter == "alpha") |>  
  ggplot(aes(psi, mean, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  facet_grid(parameter~theta, labeller = labeller(parameter=as_labeller(label_alpha, default = label_parsed),  theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0,1, length.out = 3)) + 
  scale_y_continuous(limits = c(-0.1, 2.1), breaks = seq(0,2, length.out = 3)) + 
  labs(x = "Switch Rate (Search Rule)" ,
       y = "Estimate",
       color = "Switch\nRate") +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), width = .1) +
  geom_point() +
  geom_line() +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

### value function 
vf <- values_summary |> 
  mutate(parameter="Value Function") |> 
  mutate(v_high = if_else(v_high > upper_range, upper_range, v_high )) |> 
  ggplot(aes(x, v, group = psi, color = psi, fill = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  scale_fill_scico(palette = "tokyo", end = .8) +
  facet_grid(parameter~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-1, 21), breaks = seq(0, 20, length.out = 3)) +
  scale_y_continuous(limits = c(-1, upper_range), breaks = seq(0, upper_range, length.out = 3)) +
  labs(x = "Sampled Outcome",
       y = "Subjective Value",
       color = "Switch\nRate" , 
       fill = "Switch\nRate") +
  geom_line() +
  geom_ribbon(aes(ymin = v_low, ymax = v_high), alpha = 0.3, color = NA) + 
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

value_plot <- ggarrange(vf, alpha, nrow = 2, legend = "right", common.legend = T)
ggsave(file = "manuscript/figures/cpt_value_summary.jpg" , plot=value_plot, units="mm" , width = 190, height = 120)

# roundwise comparison 


# summary comparison
values_round <- values |> filter(model=="roundwise", !(near(psi,1) & theta==1))

# alpha
alpha <- cpt_round |> 
  filter(parameter == "alpha") |>  
  ggplot(aes(psi, mean, color = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) + 
  facet_grid(parameter~theta, labeller = labeller(parameter=as_labeller(label_alpha, default = label_parsed),  theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-0.1, 1.1), breaks = seq(0,1, length.out = 3)) + 
  scale_y_continuous(limits = c(-0.1, 2.1), breaks = seq(0,2, length.out = 3)) + 
  labs(x = "Switch Rate (Search Rule)" ,
       y = "Estimate",
       color = "Switch\nRate") +
  geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`), width = .1) +
  geom_point() +
  geom_line() +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

### value function 
vf <- values_round |> 
  mutate(parameter="Value Function") |> 
  mutate(v_high = if_else(v_high > upper_range, upper_range, v_high )) |> 
  ggplot(aes(x, v, group = psi, color = psi, fill = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  scale_fill_scico(palette = "tokyo", end = .8) +
  facet_grid(parameter~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(limits = c(-1, 21), breaks = seq(0, 20, length.out = 3)) +
  scale_y_continuous(limits = c(-1, upper_range), breaks = seq(0, upper_range, length.out = 3)) +
  labs(x = "Sampled Outcome",
       y = "Subjective Value",
       color = "Switch\nRate" , 
       fill = "Switch\nRate") +
  geom_line() +
  geom_ribbon(aes(ymin = v_low, ymax = v_high), alpha = 0.3, color = NA) + 
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

value_plot <- ggarrange(vf, alpha, nrow = 2, legend = "right", common.legend = T)
ggsave(file = "manuscript/figures/cpt_value_roundwise.jpg" , plot=value_plot, units="mm" , width = 190, height = 120)


## Figure 9: rare events -----------------------------------------------------------------

rates <- choices_main |>  
  mutate(risk = ifelse(choice == higher_risk, 1, 0)) %>% # risk seeking choice
  group_by(model, psi, theta, o1_rare) |> 
  summarise(rate = mean(risk, na.rm=T)) |> 
  ungroup() 

risk_plot <- rates |> 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_grid(factor(o1_rare, levels = c("none", "attractive", "unattractive"))~factor(model, levels=c("summary", "roundwise"), labels=c("Summary", "Roundwise"))) +
  scale_color_scico(palette = "imola", end = .9) + 
  #scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Switch Rate\n(Search Rule)",
       y = "Risky Choice Rate",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size=3) +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ggsave(file = "manuscript/figures/risk_eco.png", plot=risk_plot, units = "mm", width = 190, height = 190*.75)


# appendix --------------------------------------------------------------

'This section entails the source code underlying the analyses reported 
in the appendix of manuscript. 
The analyses focus on safe-risky problems with large EV differences.'


## A: search effort -----------------------------------------------------------

## Maximization as a function of comparison strategies and sample size

effort <- choices_main |>  
  group_by(model, psi, theta) |>  
  summarise(median_n = median(n_smp))

median_n <- effort |>    
  mutate(type="Sample Size") |> 
  ggplot(aes(psi, median_n, group = theta, color = theta, fill=theta)) +
  facet_wrap(~factor(model, levels = c('summary', 'roundwise'), labels = c('Summary', 'Roundwise')), nrow=1) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_fill_scico(palette = "imola", end = .9) +
  scale_shape_manual(values = 21:25, guide = guide_legend(reverse = TRUE)) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  #scale_y_continuous(limits = c(.4, 1), breaks = seq(.5, 1, length.out = 3)) +
  labs(x = "Switch Rate (Search Rule)",
       y = "Median Sample Size",
       fill = "Threshold\n(Stopping Rule)",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth=1) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 14, color="gray", linewidth=1) +
  theme_bw()

ggsave("manuscript/figures/effort_median.jpg", plot=median_n, units="mm" , width = 190, height = 190*.50)

density_n <- choices_main |> 
  ggplot(aes(n_smp, group=theta, color=theta, fill=theta)) +
  facet_grid(psi~factor(model, levels = c('summary', 'roundwise'), labels = c('Summary', 'Roundwise')), 
             scales="free", 
             labeller = labeller(psi = as_labeller(label_psi, default = label_parsed))) +
  geom_density(alpha=.8) +
  #geom_vline(dat=effort_reward, aes(xintercept = median_n), linewidth = 1) 
  geom_vline(xintercept = 14, linetype="dashed", color="gray", linewidth=1 )+
  scale_x_continuous(limits = c(0,150)) +
  scale_fill_scico(palette = "imola", end = .9) +
  scale_color_scico(palette = "imola", end = .9) +
  labs(x = "Sample Size",
       y = "Density",
       fill = "Threshold\n(Stopping Rule)",
       color = "Threshold\n(Stopping Rule)") +
  theme_bw()

ggsave("manuscript/figures/effort_density.jpg", plot=density_n, units="mm" , width = 190, height = 190*.75)

## B: primacy bias ---------------------------------------------------------

summary <- left_join(summary, pbs, by=join_by(id)) # add problem features

choices_summary <- summary |> 
  mutate(out_1 = 100*out_1 , 
         out_2 = 100*out_2) |>
  group_by(psi, theta, id, agent) |> 
  mutate(start = ifelse(smp == 1 & at == "o1", "o1", ifelse(smp == 1 & at == "o2", "o2", NA)), 
         start_o = ifelse(is.na(start), first(start), start),
         n_smp = n(), # number of samples
         smp_o2 = sum(is.na(out_1)), # number of samples safe option
         smp_o1 = n_smp - smp_o2, # number of samples risky option
         sp_o1_1 = round(sum(if_else(out_1 == o1_1, 1, 0), na.rm = TRUE)/smp_o1, 2), # sampled probability risky outcome 1
         sp_o1_2 = round(1 - sp_o1_1, 2), # sampled probability risky outcome 2
         avg_o1 = round(mean(out_1, na.rm = TRUE), 2)) %>% # sampled average risky option
  ungroup() |> 
  filter(!is.na(choice)) |>  
  select(psi, theta, id, agent, start_o, n_smp, smp_o1, smp_o2, sp_o1_1, sp_o1_2, avg_o1, choice) 

# starting bias sample size 

choices_summary <- choices_summary |>  
  mutate(n_starting = ifelse(start_o == "o1", smp_o1, smp_o2), 
         n_second = ifelse(start_o == "o1", smp_o2, smp_o1 ), 
         n_start_sec_diff = n_starting - n_second, 
         start_advantage = if_else(n_start_sec_diff > 0, "First", "Second"))

choices_summary |> 
  ggplot(aes(x=n_start_sec_diff, fill = start_advantage)) + 
  geom_histogram(center = 0, binwidth = 1) + 
  scale_fill_scico_d(palette = "vanimo", begin = .1, end = .9) + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = F)) +
  facet_grid(psi~theta, scales = "free", labeller = labeller(theta = as_labeller(label_theta, default = label_parsed), 
                                                             psi = as_labeller(label_psi, default = label_parsed))) +
  labs(x = TeX("Difference in Sample Size ($\\N_{Initial \\, Option} - \\N_{Second \\,  Option} $)"), 
       y = "Frequency",
       fill = "Sampled Option") +
  theme_bw() +
  #theme(legend.position =  "none") + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ggsave("manuscript/figures/appendix/initial_bias_sample_sizes.jpg", units = "mm", width = 190, height = 190)

# starting bias choice

choices_summary |>  
  mutate(bias_choice = if_else(start_o == choice, TRUE, FALSE)) %>% 
  group_by(psi, theta, bias_choice) |>   
  summarize(count = n()) |>  
  mutate(prop = count/sum(count)) |>  
  filter(bias_choice == TRUE) |> 
  ggplot(aes(x=psi, y=prop, color = theta, group = theta)) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) + 
  scale_color_scico(palette = "imola", end = .9) + 
  scale_y_continuous(limits = c(.5,1), breaks = seq(.5,1,.25)) + 
  scale_x_continuous(breaks = seq(0,1,.5)) + 
  labs(x = "Switch Rate\n(Search Rule)",
       y = "Proportion of\nStarting Option Choices",
       color = "Threshold\n(Stopping Rule)") +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ggsave("manuscript/figures/appendix/initial_bias_choices.jpg", units = "mm", width = 140, height = 140*.75)


# starting bias sampled probabilities 

custom_labels <- c("o1" = "Starting Option: Risky", "o2" = "Starting Option: Safe")

choices_summary |>  
  mutate(extreme = if_else((sp_o1_1 == 1 | sp_o1_1 == 0), TRUE, FALSE)) |>  
  group_by(psi, theta, start_o, extreme) |>   
  summarize(count = n()) |>  
  mutate(prop_extreme = count/sum(count)) |>  
  filter(extreme == TRUE) |>  
  ggplot(aes(x=psi, y=prop_extreme, color = theta, group = theta)) + 
  facet_wrap(~start_o, labeller = as_labeller(custom_labels)) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) + 
  scale_color_scico(palette = "imola", end = .9) + 
  scale_y_continuous(limits = c(-.1,1), breaks = c(0, 1, .5)) + 
  scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0,1,.5)) + 
  labs(x = "Switch Rate\n(Search Rule)",
       y = "Proportion of Choices Without\nSampling Both Risky Outcomes",
       color = "Threshold\n(Stopping Rule)") +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ggsave("manuscript/figures/appendix/initial_bias_sampled_probabilities.jpg", units = "mm", width = 140, height = 140*.50)


# starting option bias risk aversion 

# prepare data 
rates <- choices_summary |>  
  filter(!c(sp_o1_1 == 0 | sp_o1_2 == 0)) |>  # remove choices where the risky option was not experienced as such
  mutate(safe_choice = ifelse(choice == "o2", 1, 0)) |>  # risk averse choice
  group_by(psi, theta, safe_choice) |>  
  summarise(n = n()) |>  
  mutate(rate = round(n/sum(n), 2)) |>  
  ungroup() |> 
  filter(!(safe_choice == 0))

rates |>  
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  scale_color_scico(palette = "imola", end = .9) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Switch Rate\n(Search Rule)",
       y = "% Safe Choices",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ggsave("manuscript/figures/appendix/initial_bias_risk_seeking.jpg", units = "mm", width = 140, height = 140*.75)

# starting bias risky choice proportions

left_join(choices_summary, pbs, by=join_by(id)) |>  
  mutate(choice_o1 = if_else(choice == "o1", 1, 0) , # predict choice of risky option
         o1_low = if_else(o1_1 < o1_2, o1_1, o1_2) ,
         o1_high = if_else(o1_1 > o1_2, o1_1, o1_2) ,         
         sp_o1_low = if_else(o1_low == o1_1, sp_o1_1, sp_o1_2) , 
         sp_o1_high = if_else(o1_high == o1_1, sp_o1_1, sp_o1_2)) |> 
  group_by(psi, theta, sp_o1_high, choice) |>  
  summarize(n = n()) |>  
  mutate(prop = round(n/sum(n),2)) |>  
  filter(choice == "o1") |>  
  mutate(bias = if_else(sp_o1_high == 1, "p=0 or p=1", if_else(sp_o1_high == 0, "p=0 or p=1", "0 < p < 1"))) |>  
  ggplot(aes(x=sp_o1_high, y = prop)) +
  geom_point(aes(color = bias, shape = bias, alpha =bias), size = 2) +
  #geom_line() + 
  scale_color_manual(values = c("gray", "#9c179e")) + 
  scale_shape_manual(values = c(16, 15)) + 
  scale_alpha_manual(values = c(.5, 1)) + 
  facet_grid(psi~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed), 
                                            psi = as_labeller(label_psi, default = label_parsed))) + 
  labs(x = TeX("$\\p_{high}$"), 
       y = "Proportion of Risky Choices", 
       color = "", 
       alpha = "",
       shape = "") +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0,1,.5)) + 
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0,1,.5)) + 
  theme_bw() + 
  theme(legend.position = "top") + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ggsave("manuscript/figures/appendix/initial_bias_risky_choice_proportions.jpg", units = "mm", width = 190, height = 190)


## C: undersampling --------------------------------------------------------

# prepare data 
round <- left_join(round, pbs, by=join_by(id))
names(round)
## compute trial-level and round-level frequencies

### higher risky outcome
freq  <- round |>  
  group_by(psi, theta, id, agent) |> 
  mutate(n_smp = n() , # number of samples
         smp_o2 = sum(is.na(out_1)) , # number of samples safe option
         smp_o1 = n_smp - smp_o2 , # number of samples risky option
         sp_o1_1 = round(sum(if_else(out_1 == o1_1, 1, 0), na.rm = TRUE)/smp_o1, 2) , # sampled probability risky outcome 1
         sp_o1_2 = round(1 - sp_o1_1, 2)) |>  
  ungroup() |> 
  group_by(psi, theta, id, agent, round) |>  
  mutate(n_smp_round = n() , 
         smp_round_o2 = sum(is.na(out_1)) ,
         smp_round_o1 = n_smp_round - smp_round_o2 ,
         round_sp_o1_1 = round(sum(if_else(out_1 == o1_1, 1, 0), na.rm = TRUE)/smp_round_o1, 2),
         round_sp_o1_2 = round(1 - round_sp_o1_1, 2)
  ) 


## compute median round-level frequencies ...

### ... for each sampled frequency on the trial level

freq_trial_1 <- freq |>  
  distinct(psi, theta, id, agent, round, sp_o1_1, round_sp_o1_1) |>  # drop redundant row
  select(psi, theta, id, agent, round, sp_o1_1, round_sp_o1_1) |>  
  rename(p = "sp_o1_1", round_sp = "round_sp_o1_1")

freq_trial_2 <- freq |>  
  distinct(psi, theta, id, agent, round, sp_o1_2, round_sp_o1_2) |>  # drop redundant row
  select(psi, theta, id, agent, round, sp_o1_2, round_sp_o1_2) |>  
  rename(p = "sp_o1_2", round_sp = "round_sp_o1_2")

freq_trial_median <- bind_rows(freq_trial_1, freq_trial_2) |>  
  group_by(psi, theta, p) |> 
  summarise(median_round_sp = median(round_sp, na.rm = TRUE)) |> # compute median round-level frequencies for each parameter combination and trial-level frequency
  mutate(benchmark="Sampled Probability")

### ... for each latent probability

freq_latent_1 <- freq %>% distinct(psi, theta, id, agent, round, o1_p1, round_sp_o1_1) |>  
  select(psi, theta, id, agent, round, o1_p1, round_sp_o1_1) |>  
  rename(p = "o1_p1", round_sp = "round_sp_o1_1")

freq_latent_2 <- freq %>% distinct(psi, theta, id, agent, round, o1_p2, round_sp_o1_2) |>  
  select(psi, theta, id, agent, round, o1_p2, round_sp_o1_2) |>  
  rename(p = "o1_p2", round_sp = "round_sp_o1_2")

freq_latent_median <- bind_rows(freq_latent_1, freq_latent_2) |>  
  group_by(psi, theta, p) |> 
  summarise(median_round_sp = median(round_sp, na.rm = TRUE)) |> 
  mutate(benchmark="Ground-Truth Probability")

### merge data

undersampling <- bind_rows(freq_trial_median, freq_latent_median)

# plot data
## median for trial level frequencies

undersampling_plot <- undersampling |>  
  ggplot(aes(x = p, y = median_round_sp, color = theta)) +
  geom_point(size = 1, alpha = .7) + 
  facet_grid(psi~benchmark, labeller = labeller(psi = as_labeller(label_psi, default = label_parsed))) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, .5)) + 
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, .5)) + 
  labs(x = "Probability",
       y = "Sampled Probability\nWithin Comparison Round", 
       color = "Threshold\n(Stopping Rule)") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  scale_color_scico(palette = "imola", end = .8) + 
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), 
        panel.grid.major = element_line(linewidth = .25))

ggsave(file = "manuscript/figures/appendix/undersampling.jpg",units = "mm", height = 190, width=190)

## D: cpt fit --------------------------------------------------------------

cpt_clean <- cpt_main |> 
  select(model:mean) |> 
  filter(parameter %in% c('alpha', 'gamma', 'delta', 'phi')) |>  
  pivot_wider(names_from = parameter, values_from = mean) |>  
  mutate(across(alpha:phi, ~round(., 2)))

ppset <- choices_main |> 
  left_join(cpt_clean, by = c("model", "psi", "theta")) %>% 
  filter(!c(o1_smp == 0 | o2_smp == 0)) %>% 
  mutate(choice_obs = if_else(choice == "o1", 1,0) ,
         o1_low = if_else(o1_1 < o1_2, o1_1, o1_2) ,
         o1_sp_low = if_else(o1_1 < o1_2, o1_sp1, o1_sp2) , 
         o1_high = if_else(o1_1 > o1_2, o1_1, o1_2) , 
         o1_sp_high = if_else(o1_1 > o1_2, o1_sp1, o1_sp2) ,
         o2_low = if_else(o2_1 < o2_2 , o2_1, o2_2) ,
         o2_sp_low = if_else(o2_1 < o2_2 , o2_sp1, o2_sp2) , 
         o2_high = if_else(o2_1 > o2_2, o2_1, o2_2) , 
         o2_sp_high = if_else(o2_1 > o2_2, o2_sp1, o2_sp2) 
  )

# make  predictions

set.seed(681271)
postpred <- ppset %>% mutate(
  o1_w_high = round( (delta * o1_sp_high^gamma) / ( (delta*o1_sp_high^gamma)+(1-o1_sp_high)^gamma ), 2) , 
  o1_w_low = 1 - o1_w_high , 
  o1_v_high = o1_high^alpha , 
  o1_v_low = o1_low^alpha , 
  V_o1 = (o1_w_high * o1_v_high) + (o1_w_low * o1_v_low) ,
  V_o1_scaled = V_o1^(1/alpha) ,
  o2_w_high = round( (delta * o2_sp_high^gamma) / ( (delta*o2_sp_high^gamma)+(1-o2_sp_high)^gamma ), 2) , 
  o2_w_low = 1 - o2_w_high , 
  o2_v_high = o2_high^alpha , 
  o2_v_low = o2_low^alpha , 
  V_o2 = (o2_w_high * o2_v_high) + (o2_w_low * o2_v_low) ,
  V_o2_scaled = V_o2^(1/alpha) ,
  V_diff = V_o1_scaled-V_o2_scaled , 
  p_o1_o2 = round(1 / ( 1 + exp(-phi*V_diff) ) , 2) ,
  choice_pp = rbinom(n=nrow(ppset), size=1, prob=p_o1_o2))

# post-processing

## compute observed choice rates 

### risky choice proportions

### maximization rates


max_rates_obs <- choices_main |>
  mutate(better_avg = if_else(o1_avg/o2_avg > 1, "o1", "o2"),  # determine option with higher sampled mean 
         max_avg = if_else(better_avg == choice, 1, 0) , 
         norm = if_else(better_avg=="o1", "Risky Option", "Safe Option")) |>
  group_by(model, psi, theta, norm) |>  
  summarise(rate_avg = mean(max_avg, na.rm=T)) |>
  ungroup() |> 
  mutate(type = "Sampling Strategy")


max_rates_pp <- postpred %>% 
  mutate(better_avg = if_else(o1_avg/o2_avg > 1, 1, 0),  # determine option with higher sampled mean 
         max_avg = if_else(better_avg == choice_pp, 1, 0) , 
         norm = if_else(better_avg==1, "Risky Option", "Safe Option")) |>
  group_by(model, psi, theta, norm) |>  
  summarise(rate_avg = mean(max_avg, na.rm=T)) |>
  ungroup() |> 
  mutate(type = "CPT Posterior Predictive")

max_rates <- bind_rows(max_rates_obs, max_rates_pp)

## compute predictive accuracy

pp_acc <- postpred %>% 
  select(model, psi, theta, alpha, gamma, delta, phi, id, agent, choice_obs, choice_pp) %>% 
  mutate(match = if_else(choice_obs == choice_pp, 1, 0)) %>% 
  group_by(model, psi, theta, alpha, gamma, delta, phi, match) %>% 
  summarise(count = n()) %>% 
  mutate(perc = round(count/sum(count), 3)) %>% 
  filter(match != 0) %>% 
  ungroup()


# summary comparison

max_rates_p <- max_rates |> 
  filter(model=="summary") |> 
  ggplot(aes(x=psi, y=rate_avg, group = interaction(norm, type), color = norm)) + 
  facet_wrap(~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), nrow = 1) + 
  geom_point(aes(shape = type), size = 3, alpha = .7) + 
  geom_line() + 
  scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0,1,length.out = 3)) + 
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_shape_manual(values = c(15, 19)) + 
  scale_color_scico_d(palette = "managua", begin = .1, end = .9) + 
  labs(x = "Switch Rate (Search Rule)" ,
       y = "Proportion of\nMaximizing Choices" ,
       color = "Better Average",
       shape = "") + 
  theme_bw() + 
  theme(strip.text.y = element_blank(), legend.position = "top") + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

pp_acc_p <- pp_acc |> 
  filter(model=="summary") |> 
  ggplot(aes(x = psi, y = perc)) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_point(size = 3, shape = 18) +
  geom_line() +
  labs(x = "Switch Rate (Search Rule)" , 
       y = "Proportion of\nCorrect Predictions", 
       color = "Choice Consistency") + 
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ppc_plot <- ggarrange(max_rates_p, pp_acc_p, nrow = 2, 
                      common.legend = TRUE, 
                      labels = "AUTO", 
                      font.label = list(size = 22))
ggsave("manuscript/figures/appendix/cpt_ppc_summary.jpg", plot=ppc_plot, units="mm" , width = 190, height = 190)

# roundwise

max_rates_p <- max_rates |> 
  filter(model=="roundwise") |> 
  ggplot(aes(x=psi, y=rate_avg, group = interaction(norm, type), color = norm)) + 
  facet_wrap(~theta, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed)), nrow = 1) + 
  geom_point(aes(shape = type), size = 3, alpha = .7) + 
  geom_line() + 
  scale_x_continuous(limits = c(-.1,1.1), breaks = seq(0,1,length.out = 3)) + 
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_shape_manual(values = c(15, 19)) + 
  scale_color_scico_d(palette = "managua", begin = .1, end = .9) + 
  labs(x = "Switch Rate (Search Rule)" ,
       y = "Proportion of\nMaximizing Choices" ,
       color = "Better Average",
       shape = "") + 
  theme_bw() + 
  theme(strip.text.y = element_blank(), legend.position = "top") + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

pp_acc_p <- pp_acc |> 
  filter(model=="roundwise") |> 
  ggplot(aes(x = psi, y = perc)) + 
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  facet_wrap(~theta, nrow = 1, labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) + 
  geom_point(size = 3, shape = 18) +
  geom_line() +
  labs(x = "Switch Rate (Search Rule)" , 
       y = "Proportion of\nCorrect Predictions", 
       color = "Choice Consistency") + 
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ppc_plot <- ggarrange(max_rates_p, pp_acc_p, nrow = 2, 
                      common.legend = TRUE, 
                      labels = "AUTO", 
                      font.label = list(size = 22))
ggsave("manuscript/figures/appendix/cpt_ppc_roundwise.jpg", plot=ppc_plot, units="mm" , width = 190, height = 190)


# supplements -------------------------------------------------------------

'This section entails the source code underlying analyses reported 
in the supplementary materials. The analyses include all problem sets.'

## S1: gamble Variations 1 ---------------------------------------------------------

choices_S1 <- choices |> 
  bind_rows(.id="gamble") |>
  mutate(gamble = str_remove(gamble, "choices_(roundwise|summary)_")) |> 
  filter(gamble != "RR2")

cpt_S1 <- cpt_estimates |> 
  bind_rows(.id="gamble") |>
  mutate(gamble = str_remove(gamble, "cpt_choices_(roundwise|summary)_")) |> 
  filter(gamble != "RR2")


### maximization ------------------------------------------------------------

# filter data sets for the same problem set 
rates <- choices_S1 |>
  mutate(max_ev = if_else(better_ev == choice, 1, 0)) |>
  group_by(gamble, model, psi, theta, o1_rare) |>  
  summarise(rate_ev = mean(max_ev, na.rm=T) ) |>
  ungroup() 

max_plot <- rates |> 
  ggplot(aes(psi, rate_ev, group = theta, color = theta)) +
  facet_grid(factor(o1_rare, levels=c("none", "attractive", "unattractive"))~gamble~factor(model, levels=c("summary", "roundwise"), labels=c("Summary", "Roundwise"))) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_x_continuous(limits = c(-.05, 1.05), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(breaks = seq(.5, 1, length.out = 3)) +
  labs(x = "Switch Rate\n(Search Rule)",
       y = "EV Maximization Rate",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ggsave("manuscript/figures/supplements/supp1_maximization.jpg", plot=max_plot, units="mm", width = 190, height = 190)

### risk --------------------------------------------------------------------

rates <- choices_S1 |>  
  mutate(risk = ifelse(choice == higher_risk, 1, 0)) %>% # risk seeking choice
  group_by(gamble, model, psi, theta, o1_rare) |> 
  summarise(rate = mean(risk, na.rm=T)) |> 
  ungroup() 

risk_plot <- rates |> 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_grid(factor(o1_rare, levels=c("none", "attractive", "unattractive"))~gamble~factor(model, levels=c("summary", "roundwise"), labels=c("Summary", "Roundwise"))) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_x_continuous(limits = c(-.05, 1.05), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Switch Rate\n(Search Rule)",
       y = "Risky Choice Rate",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth=1, alpha = .5) + 
  geom_point(size = 3) +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ggsave(file = "manuscript/figures/supplements/supp1_risk.jpg", plot=risk_plot, units="mm", width = 190, height = 190)

### cpt ---------------------------------------------------------------------

cpt_S1 <- cpt_S1 |> filter(!(model=="roundwise" & near(psi,1) & theta==1))

weights <- cpt_S1 |> 
  select(gamble, model, psi, theta, parameter, mean, `2.5%`, `97.5%`) |> 
  pivot_wider(names_from = parameter, values_from = c(mean, `2.5%`, `97.5%`)) |>  
  select(gamble, model, psi, theta, 
         gamma_mean = mean_gamma, delta_mean = mean_delta,
         gamma_low = `2.5%_gamma`, delta_low = `2.5%_delta`,
         gamma_high = `97.5%_gamma`, delta_high = `97.5%_delta`) |> 
  expand_grid(p = seq(0, 1, .001)) |>  
  mutate(
    # mean weights
    w = (delta_mean * p^gamma_mean) / ((delta_mean * p^gamma_mean) + (1 - p)^gamma_mean),
    # lower bound weights
    w_low = (delta_low * p^gamma_low) / ((delta_low * p^gamma_low) + (1 - p)^gamma_low),
    # upper bound weights
    w_high = (delta_high * p^gamma_high) / ((delta_high * p^gamma_high) + (1 - p)^gamma_high)
  )


weights_theta1 <- weights |> filter(theta==1)
weights_theta2 <- weights |> filter(theta==2)
weights_theta3 <- weights |> filter(theta==3)

wf <- weights_theta1 |> 
  ggplot(aes(p, w, group = psi, color = psi, fill = psi, linetype = as.factor(theta))) +
  scale_color_scico(palette = "tokyo", end = .8) +
  scale_fill_scico(palette = "tokyo", end = .8) +
  facet_grid(gamble~factor(model, levels=c("summary", "roundwise"), labels=c("Summary", "Roundwise"))) + 
  scale_x_continuous(breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 3)) +
  labs(x = "p (Sampled Probability)",
       y = "w(p)",
       color = "Switch Rate\n(Search Rule)" , 
       fill = "Switch Rate\n(Search Rule)", 
       linetype = "Threshold\n(Stopping Rule)") +
  geom_ribbon(aes(ymin = w_low, ymax = w_high), alpha = 0.3, color = NA) + 
  geom_ribbon(data=weights_theta2, aes(ymin = w_low, ymax = w_high), alpha = 0.3, color = NA) + 
  geom_ribbon(data=weights_theta3, aes(ymin = w_low, ymax = w_high), alpha = 0.3, color = NA) + 
  geom_line() +
  geom_line(data=weights_theta2) +
  geom_line(data=weights_theta3) +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ggsave(file = "manuscript/figures/supplements/supp1_weighting.jpg", plot=wf, units="mm", width = 190, height = 190)


### search effort -------------------------------------------------------------

# prepare data

## Maximization as a function of comparison strategies and sample size
max_n <- choices_S1 |>  
  mutate(norm = if_else(o1_ev > o2_ev, "o1", "o2") ,
         max = ifelse(norm == choice, 1, 0)) |>  
  group_by(gamble, model, psi, theta) |>  
  summarise(mean_n = mean(n_smp) , 
            median_n = median(n_smp) , 
            max_prop = mean(max),
            max_n = sum(max))

# plot data
base <- tibble(gamble=c("SR_small", "SR_large", "RR"), 
               base=c(14,14,22)) |> 
  expand_grid(model=c("summary", "roundwise"))

effort_plot <- max_n |>
  ggplot(aes(psi, median_n, group = theta, color = theta, fill=theta)) +
  facet_grid(gamble~factor(model, levels=c("summary", "roundwise"), labels=c("Summary", "Roundwise"))) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_fill_scico(palette = "imola", end = .9) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Switch Rate\n(Search Rule)",
       y = "Median Sample Size",
       fill = "Threshold\n(Stopping Rule)",
       color = "Threshold\n(Stopping Rule)",  
       shape = "Switch Rate\n(Search Rule)") +
  geom_line(linewidth=1) + 
  geom_point(size = 3) +
  geom_hline(data=base, aes(yintercept = base), color="gray", linetype="dashed", linewidth=1) +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ggsave(file = "manuscript/figures/supplements/supp1_effort.jpg", plot=effort_plot, units="mm", width = 190, height = 190)

## S2: gamble variations 2 -------------------------------------------------------------

choices_S2 <- choices[str_detect(names(choices), "RR2")] |> 
  bind_rows()

cpt_S2 <- cpt_estimates[str_detect(names(choices), "RR2")] |> 
  bind_rows()

### maximization ------------------------------------------------------------

# filter data sets for the same problem set 
rates <- choices_S2 |>
  mutate(better_avg = if_else(o1_avg/o2_avg > 1, "o1", "o2"),  # determine option with higher sampled mean 
         max_avg = if_else(better_avg == choice, 1, 0), 
         max_ev = if_else(better_ev == choice, 1, 0)) |>
  group_by(model, psi, theta) |>  
  summarise(rate_avg = mean(max_avg, na.rm=T), 
            rate_ev = mean(max_ev, na.rm=T) ) |>
  ungroup() |> 
  pivot_longer(rate_avg:rate_ev, values_to = "rate", names_to = "Benchmark", names_prefix = "rate_") |> 
  mutate(Benchmark = factor(Benchmark, levels = c("ev", "avg"), labels = c("EV", "SM")))

max_plot <- rates |> 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_grid(Benchmark~factor(model, levels=c("summary", "roundwise"), labels=c("Summary", "Roundwise"))) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_x_continuous(limits = c(-.05, 1.05), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(breaks = seq(.5, 1, length.out = 3)) +
  labs(x = "Switch Rate\n(Search Rule)",
       y = "Maximization Rate",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ggsave(file = 'manuscript/figures/supplements/supp2_maximization.png', plot=max_plot, units="mm", width = 190, height = 190*.75)

### risk --------------------------------------------------------------------

rates <- choices_S2 |>  
  mutate(risk = ifelse(choice == higher_risk, 1, 0)) %>% # risk seeking choice
  group_by(model, psi, theta) |> 
  summarise(rate = mean(risk, na.rm=T)) |> 
  ungroup() 

risk_plot <- rates |> 
  ggplot(aes(psi, rate, group = theta, color = theta)) +
  facet_wrap(~factor(model, levels=c("summary", "roundwise"), labels=c("Summary", "Roundwise"))) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_x_continuous(limits = c(-.05, 1.05), breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Switch Rate\n(Search Rule)",
       y = "Risky Choice Rate",
       color = "Threshold\n(Stopping Rule)") +
  geom_line(linewidth=1, alpha = .5) + 
  geom_point(size = 3) +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))


ggsave(file = 'manuscript/figures/supplements/supp2_risk.png', plot=risk_plot, units="mm", width = 190, height = 190*.50)

### cpt ---------------------------------------------------------------------

cpt_S2 <- cpt_S2 |> filter(!(model=="roundwise" & near(psi,1) & theta==1))

# compute decision weights based on cpt estimates
weights <- cpt_S2 |> 
  select(model, psi, theta, parameter, mean, `2.5%`, `97.5%`) |> 
  pivot_wider(names_from = parameter, values_from = c(mean, `2.5%`, `97.5%`)) |>  
  select(model, psi, theta, 
         gamma_mean = mean_gamma, delta_mean = mean_delta,
         gamma_low = `2.5%_gamma`, delta_low = `2.5%_delta`,
         gamma_high = `97.5%_gamma`, delta_high = `97.5%_delta`) |> 
  expand_grid(p = seq(0, 1, .001)) |>  
  mutate(
    # mean weights
    w = (delta_mean * p^gamma_mean) / ((delta_mean * p^gamma_mean) + (1 - p)^gamma_mean),
    # lower bound weights
    w_low = (delta_low * p^gamma_low) / ((delta_low * p^gamma_low) + (1 - p)^gamma_low),
    # upper bound weights
    w_high = (delta_high * p^gamma_high) / ((delta_high * p^gamma_high) + (1 - p)^gamma_high)
  )



wf <- weights %>% 
  mutate(parameter="Weighting Function") |> 
  ggplot(aes(p, w, group = psi, color = psi, fill = psi)) +
  scale_color_scico(palette = "tokyo", end = .8) +
  scale_fill_scico(palette = "tokyo", end = .8) +
  facet_grid(theta~factor(model, levels=c("summary", "roundwise"), labels=c("Summary", "Roundwise")), labeller = labeller(theta = as_labeller(label_theta, default = label_parsed))) +
  scale_x_continuous(breaks = seq(0, 1, length.out = 3)) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 3)) +
  labs(x = "p (Sampled Frequency)",
       y = "w(p)",
       color = "Switch\nRate",
       fill = "Switch\nRate") +
  geom_ribbon(aes(ymin = w_low, ymax = w_high), alpha = 0.3, color = NA) + 
  geom_line() +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25),
        panel.grid.major = element_line(linewidth = .25))

ggsave(file = 'manuscript/figures/supplements/supp2_weighting.png', plot=wf, units="mm", width = 190, height = 190*.75)


### search effort -----------------------------------------------------------

## Maximization as a function of comparison strategies and sample size
max_n <- choices_S2 |>  
  mutate(norm = if_else(o1_ev > o2_ev, "o1", "o2") ,
         max = ifelse(norm == choice, 1, 0)) |>  
  group_by(model, psi, theta) |>  
  summarise(mean_n = mean(n_smp) , 
            median_n = median(n_smp) , 
            max_prop = mean(max),
            max_n = sum(max))

# plot data

effort_plot <- max_n |>
  ggplot(aes(psi, median_n, group = theta, color = theta, fill=theta)) +
  facet_wrap(~factor(model, levels=c("summary", "roundwise"), labels=c("Summary", "Roundwise"))) +
  scale_color_scico(palette = "imola", end = .9) + 
  scale_fill_scico(palette = "imola", end = .9) +
  scale_x_continuous(limits = c(-.1, 1.1), breaks = seq(0, 1, length.out = 3)) +
  labs(x = "Switch Rate\n(Search Rule)",
       y = "Median Sample Size",
       fill = "Threshold\n(Stopping Rule)",
       color = "Threshold\n(Stopping Rule)",  
       shape = "Switch Rate\n(Search Rule)") +
  geom_line(linewidth=1) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 22, color="gray", linetype="dashed", linewidth=1) +
  theme_bw() + 
  theme(panel.grid.minor = element_line(linewidth = .25), panel.grid.major = element_line(linewidth = .25))

ggsave('manuscript/figures/supplements/supp2_effort.png', plot=effort_plot, units="mm", width = 190, height = 190*.5)













