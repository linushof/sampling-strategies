Download the complete date from the [latest release](https://github.com/linushof/sampling-strategies/releases).

### Data sets


|                                |                                                        |            |         |                                         |
|--------------------------------|--------------------------------------------------------|------------|---------|-----------------------------------------|
| File                           | Description                                            | Repository | Release | External                                |
| `choice_problems.rds`          | Choice problems used for simulation                    | [x]        | [x]     |                                         |
| `simulation_roundwise.rds.bz2` | Full simulation data for the roundwise comparison rule |            | [x]     |                                         |
| `simulation_summary.rds.bz2`   | Full simulation data for the summary comparison rule   |            | [x]     |                                         |
| `choice_data.rds.bz2`          | Précis of all simulated trials                         | [x]        | [x]     |                                         |
| `cpt_estimates.rds`            | Précis of CPT posterior distributions                  | [x]        | [x]     |                                         |
| `cpt_posteriors.rds`           | Full CPT posterior distributions (MCMC samples)        |            | [x]     |                                         |
| `exp.txt`                      | Data compiled by Wulff et al. (2018)                   |            |         | [Link](https://www.dirkwulff.org/#data) |


### Checksums

Use the [checksums](https://github.com/linushof/sampling-strategies/tree/main/documentation/checksums) to check that the downloaded data are up-to-date and uncorrupted, e.g.: 

``` r
# load packages

library(digest)
library(crayon)
library(readr)

#load data

choice_data <- read_rds("data/choice_data.rds.bz2")

# create checksum

checksum_download <- digest(choice_data, algo="sha256")
checksum_original <- '6541c26abce337c99b0618471830c9b32d9edd726d499dacf0760bc304257cc8'

# check data set

if(checksum_download != checksum_original){
  warning("\u2716 Mismatch between downloaded data and original data. Current checksum is: '", checksum_download, "'")
} else{cat(green("\u2713 Data validated. Downloaded data matches the original data."))}
```

### File Dependencies

![](../documentation/file_dependencies.png)


