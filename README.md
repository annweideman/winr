# winr

# Method
Randomization-Based Covariance Adjustment of Win Ratios and Win Odds for Randomized Multi-Visit Studies with Ordinal Outcomes

# Authors
Ann Marie K. Weideman, Elaine K. Kowalewski, Gary G. Koch

# Maintainer
Ann Marie Weideman, anndo1(at)umbc.edu

# Description
An R package that performs randomization-based adjustment of the win ratio and win odds for baseline covariates and strata.

# References
1) Weideman, A.M.K., Kowalewski, E.K., & Koch, G.G. (2024). Randomization-based covariance adjustment of win ratios and win odds for 	randomized multi-visit studies with ordinal outcomes. *Journal of Statistical Research*, 58(1), 33–48. https://doi.org/10.3329/jsr.v58i1.75411.
2) Kowalewski, E.K., Weideman, A.M.K., & Koch, G.G. (2023). SAS macro for randomization-based methods for covariance and stratified adjustment 	of win ratios and win odds for ordinal outcomes. *SESUG 2023 Proceedings*, Paper 139-2023.

# Inputs
* `data`: a dataframe or matrix containing the analysis data. Must be in wide format such that a participant's repeated responses are in a single row, and each response is in a separate column.
* `pid`: a string indicating the name of the variable corresponding to participant ID.
* `baseline`: a string indicating the name of the outcome measured at baseline. If not specified, defaults to NULL, and no baseline adjustment is employed.
* `outcome`: a vector of strings indicating the names of the outcomes measured at each visit. Baseline, if specified, will be concatenated to this vector within the code. The outcomes must have at least an ordinal measurement scale with larger values being better than smaller values. Thus, the outcome can be ordered categories or continuous measurements.
* `covars`: a vector of strings indicating the names of the covariates (measured at baseline) used for adjustment. These covariates must be numeric and can  be measured on a binary, categorical, ordered categorical, or continuous scale. If not specified, defaults to NULL and no covariate adjustment is employed.
* `strata`: a string indicating the name of the variable used for stratification. If not specified, defaults to NULL and no stratification adjustment is utilized.
* `arm`: a string indicating the name of the variable for treatment arm. Treatment arm must be a positive integer such that the test treatment arm is ALWAYS higher in value than the control arm.
* `method`: a string "small" or "large" used to denote the sample size method employed. The small sample size method is recommended unless within-stratum sample size is reasonably large (e.g., >= 50), number of visits is small (e.g., <=6), and number of covariates is small (e.g., <=4). Defaults to "small."
* `sig.level`: significance level (Type I error probability). Defaults to 0.05.
 
**Outputs:** A  dataframe containing
* `logWR`: natural log-transformed win ratio
* `SE_logWR`: standard error of log-transformed win ratio
* `Var_logWR`: sample variance of log-transformed win ratio
* `Chi_Square`: Pearson's Chi-squared test statistic corresponding to `logWR`
* `p_value`: p-value corresponding to the Pearson's Chi-squared test
* `WR`: win ratio
* `LCL_WR`: lower bound of $(1-\alpha)\times 100$% CI for `WR`
* `UCL_WR`: upper bound of $(1-\alpha)\times 100$% CI for `WR`

# Installation
Install the current release from CRAN (not recommended). Published to CRAN on 16-Oct-2024.

```
install.packages("winr")
```

Install the developmental version from GitHub (HIGHLY recommended, as this will allow you to install any bugs that were corrected post-publication to CRAN)

```
if (!require("devtools", character.only = TRUE)) {
   install.packages("devtools", dependencies = TRUE)
}
library("devtools", character.only = TRUE)
devtools::install_github("annweideman/winr")
```




