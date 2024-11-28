---
title: "How Sampling Strategies Shape Experience-Based Risky Choice"
author: "Linus Hof, Veronika Zilker, and Thorsten Pachur"
subtitle: "Codebook for source code and data files"
output: 
  html_document: 
    keep_md: true
---

| Variable[^1]                         | Description                                                                   |
|---------------------------------------------------|------------------------------------------------------------------------|
| `id`                                 | choice problem identifier                                                     |
| `safe`                               | safe outcome                                                                  |
| `r_1`, `r_2`                         | risky outcome 1 and 2                                                         |
| `p_r_1`, `p_r_2`                     | ground-truth probability of `r_1` and `r_2`                                   |
| `ev_risky`                           | expected value (EV) risky option                                              |
| `better_ev`                          | option with higher EV (`safe`, `risky`)                                       |
| `rare`                               | `no`/`attractive`/`unattractive` rare event                                   |
| `more probable`                      | more probable risky outcome (`attractive`, `unattractive`)                    |
| `model`                              | comparison rule (`roundwise`, `summary`)                                      |
| `psi`                                | search rule (switching rate)                                                  |
| `theta`                              | stopping rule (decision threshold)                                            |
| `agent`                              | synthetic agent (iteration index)                                             |
| `smp`                                | (cumulative) number of samples in a trial                                     |
| `at`                                 | attended option (`r` = risky, `s` = safe)                                     |
| `out_r`                              | sampled outcome risky option (`NA` if `at==s`)                                |
| `out_s`                              | sampled outcome safe option (`NA` if `at==r`)                                 |
| `round`                              | cumulative number of comparison rounds in a trial                             |
| `win`                                | winner of comparison round (`1` = risky, `-1` = safe)                         |
| `D`                                  | decision variable                                                             |
| `choice`                             | final choice (`r` = risky, `s` = safe)                                        |
| `smp_r`                              | number of samples risky option                                                |
| `smp_s`                              | number of samples safe option                                                 |
| `sp_r_1, sp_r_2`                     | sampled probability of `r_1` and `r_2`                                        |
| `avg_r`                              | sampled average risky option                                                  |
| `parameter`                          | fitted CPT parameter (`alpha`, `gamma`, `delta`, `rho`) and model `deviance`  |
| `mean`                               | mean of posterior distribution                                                |
| `2.5%`, `25%`, `50%`, `75%`, `97.5%` | percentiles of posterior distribution                                         |
| `Rhat`                               | potential scale reduction factor                                              |
| `n.eff`                              | effective sample size                                                         |

[^1]: The codebook for the `exp.txt` dataset of Wulff et al. (2018) can be retrieved from: [https://www.dirkwulff.org/#data)](https://www.dirkwulff.org/#data))
