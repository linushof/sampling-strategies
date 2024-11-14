### File Descriptions

| File                   | Description                                                                                                                                       |
|------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------|
| `simulate_roundwise.R` | Simulates sampling strategies with a roundwise comparison rule; generates `simulation_roundwise.rds.bz2`                                          |
| `simulate_summary.R`   | Simulates sampling strategies with a summary comparison rule; generates `simulation_summary.rds.bz2`                                              |
| `prepare_analysis.R`   | Summarizes all 6,000,000 simulated trials (from `simulation_roundwise.rds.bz2` and `simulation_summary.rds.bz2)`; generates `choice_data.rds.bz2` |
| `cpt_model.txt`        | `JAGS` model specification of cumulative prospect theory (CPT).                                                                                   |
| `estimate_cpt.R`       | Fits `cpt_model.txt` in `JAGS`; generates `cpt_estimates.rds` and `cpt_posteriors.rds.bz2`                                                        |
| `analysis.R`           | Source code underlying all simulation-based analyses and figures reported in the manuscript                                                       |
| `prepare_reanalysis.R` | Preprocesses data compiled by Wulff et al. (2018) for `reanalysis.R`                                                                              |
| `reanalysis.R`         | Source code underlying all empirical analyses                                                                                                     |
