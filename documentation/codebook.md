### How Sampling Strategies Shape Experience-Based Risky Choice

#### Codebook For Source Code and Data Files

*Linus Hof, Veronika Zilker, and Thorsten Pachur*

| Variable[^codebook-1] | Description |   |   |
|-----------------------------------|-----------------------------------|-----------------------------------|-----------------------------------|
| `id` | choice problem identifier |  |  |
| `o#_1`, `o#_2` | outcome 1, 2 of option \# |  |  |
| `o#_p1`, `o#_p2` | probability of outcome 1, 2 of option \# |  |  |
| `o#_ev` | expected value (EV) of option \# |  |  |
| `ev_diff` | absolute difference in EV between option 1 and 2 |  |  |
| `ev_diff_sc` | relative EV difference (`ev_diff` / outcome range) |  |  |
| `var_o#` | variance option \# |  |  |
| `cvar_o#` | coefficient of variance (CoV) option \# | `o#_rwp` | probability of winning pairwise comparison (roundwise winning probability) |
| `o#_rwp` | probability of winning pairwise comparison (roundwise winning probability, rwp) |  |  |
| `o#_rare` | rare event option \# (`none`, `attractive`, `unattractive`) |  |  |
| `higher_risk` | option with higher CoV (always "o1") |  |  |
| `better_rwp` | option with higher roundwise winning probability (frequent winner) |  |  |
| `better_ev` | option with higher EV |  |  |
| `model` | comparison rule (`roundwise`, `summary`) |  |  |
| `psi` | search rule (switch rate) |  |  |
| `theta` | stopping rule (decision threshold) |  |  |
| `agent` | synthetic agent (iteration index) |  |  |
| `smp` | (cumulative) number of samples in a trial |  |  |
| `at` | attended option |  |  |
| `out_#` | sampled outcome option \# (`NA` if not sampled) |  |  |
| `round` | cumulative number of comparison rounds in a trial (for roundwise comparison rule) |  |  |
| `win` | round winner |  |  |
| `D` | decision variable |  |  |
| `choice` | final choice |  |  |
| `n_smp` | number of samples |  |  |
| `o#_smp_r` | number of samples option \# |  |  |
| `o#_sp1, o#_sp2` | sampled relative frequency of outcome 1, 2 of option \# |  |  |
| `o#_avg` | sampled average/mean option \# |  |  |
| `parameter` | fitted CPT parameter (`alpha`, `gamma`, `delta`, `rho`)(ignore `*.pre`) |  |  |
| `mean` (`se_mean`) | (standard error of) mean of posterior distribution |  |  |
| `2.5%`, `25%`, `50%`, `75%`, `97.5%` | percentiles of posterior distribution |  |  |
| `Rhat` | potential scale reduction factor |  |  |
| `n_eff` | effective sample size |  |  |

[^codebook-1]: The codebook for the `exp.txt` dataset of Wulff et al. (2018) can be retrieved from: [https://www.dirkwulff.org/#data)](https://www.dirkwulff.org/#data))
