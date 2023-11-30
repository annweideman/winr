# winr

# Method
Randomization Based Methods for Covariance and Stratified Adjustment of Win Ratios and Win Odds for Ordinal Outcomes

# Author
Ann Marie K. Weideman, Elaine K. Kowalewski, Gary G. Koch

# Maintainer
Ann Marie Weideman, anndo1(at)umbc.edu

# Description
An R package that performs randomization-based adjustment of the win ratio and win odds for covariates and strata.

**Inputs:**
  * `Rs`:   $M \times N$ (mutation by single-cell) matrix of alternative read counts from single-cell RNA sequencing data. We provide details for a pre-processing pipeline for somatic variant calling in the paper; the scripts for this pipeline can be accessed in our Zenodo repository (https://zenodo.org/record/7931384).
  * `Xs`:  $M \times N$ (mutation by single-cell) matrix of total read counts (benign + mutated) from single-cell RNA sequencing data.
  *  `Rb`:  $M \times T$ (mutation by bulk sample) matrix Alternative read counts from bulk DNA whole exome sequencing (WES). We provide details for a pre-processing pipeline for somatic variant calling in the paper; the scripts for this pipeline can be accessed in our Zenodo repository (https://zenodo.org/record/7931384).
  *  `Xb`: Total read counts (benign + mutated) from bulk DNA whole exome sequencing (WES).
  *  `alpha` and `beta`:  $1 \times M$ (number of mutations) vectors of mutation specific gene activation ($\alpha$) and gene deactivation ($\beta$) rates. These can be estimated using either the BPSC methodology in function `get_burstiness_bpsc()` (recommended) or the SCALE methodology with function `get_burstiness_scale()`.
  *  `kappa` and `tau`:   $1 \times 1$ scalars used to compute the sequencing error by $\frac{\kappa}{\kappa + \tau}$. The average error rate of next-generation sequencing is reported to be 0.1\% per nucleotide, so these parameters have been set to default values of $\kappa=1$ and $\tau = 999$ such that $\frac{\kappa}{\kappa + \tau} = 0.001$ (or 0.1\%). 
  *  `Klist`:   A range of possible numbers of subclones to use for DIC calculations. For each $k$ in Klist, the tree with highest mean posterior density across all post-burn-in iterations and all chains is selected, and then the deviance information criterion (DIC) (of type Gelman or Spiegelhalter) is used to select the optimal configuration. We suggest examining the graph of BIC versus number of subclones to determine the optimal number of subclones. 

* `data`: a dataframe or matrix containing the analysis data. Must be in wide format such that a participant's repeated responses are in a single row, and each response is in a separate column.
* `pid`: a string indicating the name of the variable corresponding to participant ID.
* `baseline`: a string indicating the name of the outcome measured at baseline. If not specified, defaults to NULL, and no baseline adjustment is employed.
* `outcome`: a vector of strings indicating the names of the outcomes measured at each visit. Baseline, if specified, will be concatenated to this vector within the code. The outcomes must have at least an ordinal measurement scale with larger values being better than smaller values. Thus, the outcome can be ordered categories or continuous measurements.
* `covars`: a vector of strings indicating the names of the covariates (measured at baseline) used for adjustment. These covariates must be numeric and can  be measured on a binary, categorical, ordered categorical, or continuous scale. If not specified, defaults to NULL and no covariate adjustment is employed.
* `strata`: a string indicating the name of the variable used for stratification. If not specified, defaults to NULL and no stratification is utilized.
* `arm`: a string indicating the name of the variable for treatment arm. Treatment arm must be a positive integer such that the test treatment arm is ALWAYS higher in value than the control arm.
* `method`: a string "small" or "large" used to denote the method employed. The small sample size method is recommended unless within-stratum sample size is reasonably large (e.g., >= 50), number of visits is small (e.g., <=6), and number of covariates is small (e.g., <=4). Defaults to "small."
 
**Outputs:** A  dataframe containing
* `logWR`: natural log-transformed win ratio
* `SE_logWR`: standard error of log-transformed win ratio
* `Var_logWR`: sample variance of log-transformed win ratio
* `Chi_Square`: Pearson's Chi-squared test statistic corresponding to `logWR`
* `p_value`: p-value corresponding to the Pearson's Chi-squared test `WR`
* `LCL_WR`: lower bound of $(1-\alpha)\times 100\%$ CI for `WR`
* `UCL_WR`: upper bound of $(1-\alpha)\times 100\%$ CI for `WR`

# Installation
Install the current release from CRAN (not recommended). Not published on CRAN as of 11/30/23:

```
install.packages("winr")
```

Install the devel version from GitHub (HIGHLY recommended, as this will allow you to install any bugs that were corrected post-publication to CRAN)

```
if (!require("devtools", character.only = TRUE)) {
   install.packages("devtools", dependencies = TRUE)
}
library("devtools", character.only = TRUE)
devtools::install_github("annweideman/winr")
```




