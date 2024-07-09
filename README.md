---
title: "BRBVS Package"
output: github_document
---

# BRBVS Package

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/BRBVS)](https://cran.r-project.org/package=BRBVS)

## Overview

The **BRBVS** package is available on CRAN and provides tools for Variable Ranking and Variable Selection in the context of bivariate survival copulas affected by general censoring schemes. This package is particularly useful for statistical analysis and research in survival analysis and copula modeling.

## Installation

You can install the package directly from CRAN:

```r
install.packages("BRBVS")
library(BRBVS)
```
## Usage

```r
# Assuming you want to use the BRBVS algorithm with the Clayton copula 
# and proportional odds in both margins and assuming a sample size of n = 1000, 
# under the package BRBVS in R, we have:


Bivrbvs <- BRBVS(y = Y, x = X, kmax = 5, copula = 'C0', 
                 margins = c('PO', 'PO'), m = 1000 / 2, tau = 0.5,
                 n.rep = 50, metric = 'FIM')


# The argument y = Y specifies the response variables, in this case t11, t12, t21, t22, 
# and the censoring indicators cens1, cens2, while x = X specifies the predictor variables.
# The kmax = 5 parameter sets the maximum number of variables to be selected k_max.
# The copula = 'C0' parameter specifies the copula to be used, in this case a Clayton copula.
# The margins parameter is defined as c('PO', 'PO'), indicating that both margins follow 
# the proportional odds model.
# The m = 1000 / 2 parameter sets the sub-sample size to be used in the estimation phase 
# after the bootstrap, effectively setting it to 500.
# The tau = 0.5 parameter specifies the threshold for variable inclusion, introduced in Equation \eqref{eq:hat_s_v}.
# The n.rep = 50 parameter sets the number of bootstrap replications B.
# Finally, the parameter metric = 'FIM' specifies the measure \omega, in this case 
# the Fisher information defined in Table \ref{tab:measures} to be used for ranking the covariates.

# Summary of the BRBVS object

summary(Bivrbvs)

# Plot of the BRBVS results

PlotBRBVS(Bivrbvs)

```
## Documentation 

The full documentation cal be found on http://www.r-pkg.org/badges/version/BRBVS)](https://cran.r-project.org/package=BRBVS


## Contributing 

We welcome contributions to the BRBVS package. Please fork the repository and submit a pull request with your changes. For major changes, please open an issue first to discuss what you would like to change.


