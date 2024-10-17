
### File Descriptions

| File                          | Description                                                                                                     |
|-------------------------------|-----------------------------------------------------------------------------------------------------------------|
| model_implementation_roundwise.R | Implementation of sampling strategies with roundwise comparisons; simulates sampling and generates `simulation_roundwise.rds.bz2` |
| model_implementation_simulation.R | Implementation of sampling strategies with summary comparisons; simulates sampling and generates `simulation_summary.rds.bz2` |
| prepare_choice_data.R          | Generates summaries of all 6,000,000 simulated sampling and choice processes and generates `choice_data.rds.bz2` |
| CPT_model.txt                  | Specification of the CPT model                                                                                 |
| estimate_cpt.R                 | Fits `CPT_model.txt` in JAGS and generates `cpt_estimates.rds` and `cpt_posteriors.rds.bz2`                     |
| analysis.R                     | Source code underlying all simulation-based analyses and figures reported in the manuscript                     |
| reanalysis.R                   | Source code underlying the predictions and analyses of the summary and the roundwise comparison rule for the data compiled by Wulff et al. (2018); reported in the manuscript |