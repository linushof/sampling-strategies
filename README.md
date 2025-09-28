# How Sampling Strategies Shape Experience-Based Risky Choice

Linus Hof [![ORCID iD](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-2257-2136), Veronika Zilker [![ORCID iD](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-9551-800X), and Thorsten Pachur [![ORCID iD](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0001-6391-4107).

## Abstract

Hallmark phenomena of risky choice, such as risk aversion and deviations from expected value (EV) maximization, are commonly modeled with psychoeconomic curves (e.g., utility function, probability weighting function). 
Yet these functions describe choices rather than the cognitive processes that generate them. 
Here, we examine how aspects of the oft-neglected process of information search---such as switching between options during sampling and stopping based on sampled evidence---can give rise to patterns in experience-based risky choice in the context of the sampling paradigm. 
We develop a computational framework that conceptualizes sampling strategies as consisting of three building blocks: 
a search rule (governing the rate of switching between options during sampling), a comparison rule (roundwise vs. summary evaluation of the options), and a stopping rule (decision threshold). 
In simulation analyses comparing different combinations of these building blocks, we show that frequent switching increases EV-maximizing choices for strategies with a summary comparison rule, but decreases maximization for strategies with a roundwise comparison rule. 
Further, frequent switching promotes an apparent underweighting of rarely experienced events found under the roundwise comparison rule, and curtails overweighting found under the summary comparison rule. 
We also reveal how the sampling strategies give rise to different risk attitudes depending on the existence and attractiveness of rare events. 
Finally, an empirical analysis suggests that people combine switching behavior and comparison rules in a way that fosters EV-maximizing choices. 
Our results underscore the possible contribution of search processes to patterns in risky choice.


## How to reproduce this project?

To ensure full reproducibility, recreate the software setup using the following steps:

-   Install and select `R version 4.3.3` in `RStudio` under `Tools > Global Options > General`
-   Install the [`C++17 toolchain`](https://mc-stan.org/install/index.html) (required for `Stan`)
-   Download the repo via Fork & Clone or ZIP Download (`<> Code` button) and initialize it as R project
-   Run `renv::restore()` to install all required packages/dependencies with the same version

All data sets in the [data folder](https://github.com/linushof/sampling-strategies/tree/main/data) can be reproduced using the code scripts in the [code folder](https://github.com/linushof/sampling-strategies/tree/main/code). You can reproduce all data sets from scratch or just specific data sets. Browse the [documentation](https://github.com/linushof/sampling-strategies/tree/main/documentation) for details about each data set, code script, and relevant file dependencies.
