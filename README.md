
<!-- markdownlint-disable-file -->
<!-- README.md needs to be generated from README.Rmd. Please edit that file -->

# mmrm <img src="man/figures/logo.svg" align="right" width="175" />

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)  

Mixed models for repeated measures (MMRM) are a popular choice for
analyzing longitudinal continuous outcomes in randomized clinical trials
and beyond; see [Cnaan, Laird and Slasor
(1997)](https://doi.org/10.1002/(SICI)1097-0258(19971030)16:20%3C2349::AID-SIM667%3E3.0.CO;2-E)
for a tutorial and [Mallinckrodt, Lane and Schnell
(2008)](https://doi.org/10.1177/009286150804200402) for a review. This
package implements MMRM based on the marginal linear model without
random effects using Template Model Builder (`TMB`) which enables fast
and robust model fitting. Users can specify a variety of covariance
matrices, weight observations, fit models with restricted or standard
maximum likelihood inference, perform hypothesis testing with
Satterthwaite adjusted degrees of freedom, and extract least square
means estimates by using `emmeans`.

## Main Features

- Responses are assumed normally distributed.
- Covariances:
  - Structures: unstructured, Toeplitz, AR1, compound symmetry, and
    ante-dependence.
  - Groups: shared covariance structure for all subjects, or group
    specific covariance structures.
  - Variances: homogeneous or heterogeneous across time points.
- Hypothesis testing:
  - Least square means: `emmeans` package can be used with model outputs
    to obtain least square means.
  - Degrees of freedom adjustment: Satterthwaite-adjusted one- and
    multi-dimensional contrasts.
- Model inference:
  - Supports REML and ML.
  - Supports weights.
  - Automatic changing of optimizer in the case of non-convergence.
  - Manual control of optimization routine.

## Installation

### CRAN

You can install the current stable version from CRAN with:

``` r
install.packages("mmrm")
```

### GitHub

You can install the current development version from GitHub with:

``` r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("openpharma/mmrm")
```

## Getting Started

You can get started by trying out the example:

``` r
library(mmrm)
fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)
```

This specifies an MMRM with the given covariates and an unstructured
covariance matrix for the timepoints (also called visits in the clinical
trial context, here given by `AVISIT`) within the subjects (here
`USUBJID`). While by default this uses restricted maximum likelihood
(REML), it is also possible to use ML, see `?mmrm`.

You can look at the results high-level:

``` r
fit
#> mmrm fit
#> 
#> Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#> Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Covariance:  unstructured (10 variance parameters)
#> Method:      REML
#> Deviance:    3386.45
#> 
#> Coefficients: 
#>                   (Intercept) RACEBlack or African American 
#>                   30.77747548                    1.53049977 
#>                     RACEWhite                     SEXFemale 
#>                    5.64356535                    0.32606192 
#>                      ARMCDTRT                    AVISITVIS2 
#>                    3.77423004                    4.83958845 
#>                    AVISITVIS3                    AVISITVIS4 
#>                   10.34211288                   15.05389826 
#>           ARMCDTRT:AVISITVIS2           ARMCDTRT:AVISITVIS3 
#>                   -0.04192625                   -0.69368537 
#>           ARMCDTRT:AVISITVIS4 
#>                    0.62422703 
#> 
#> Model Inference Optimization:
#> Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch
```

The `summary()` method then provides the coefficients table with
Satterthwaite degrees of freedom as well as the covariance matrix
estimate:

``` r
summary(fit)
#> mmrm fit
#> 
#> Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#> Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Covariance:  unstructured (10 variance parameters)
#> Method:      REML
#> 
#> Model selection criteria:
#>      AIC      BIC   logLik deviance 
#>   3406.4   3439.3  -1693.2   3386.4 
#> 
#> Coefficients: 
#>                                Estimate Std. Error        df t value Pr(>|t|)
#> (Intercept)                    30.77748    0.88656 218.80000  34.715  < 2e-16
#> RACEBlack or African American   1.53050    0.62448 168.67000   2.451 0.015272
#> RACEWhite                       5.64357    0.66561 157.14000   8.479 1.56e-14
#> SEXFemale                       0.32606    0.53195 166.13000   0.613 0.540744
#> ARMCDTRT                        3.77423    1.07415 145.55000   3.514 0.000589
#> AVISITVIS2                      4.83959    0.80172 143.88000   6.037 1.27e-08
#> AVISITVIS3                     10.34211    0.82269 155.56000  12.571  < 2e-16
#> AVISITVIS4                     15.05390    1.31281 138.47000  11.467  < 2e-16
#> ARMCDTRT:AVISITVIS2            -0.04193    1.12932 138.56000  -0.037 0.970439
#> ARMCDTRT:AVISITVIS3            -0.69369    1.18765 158.17000  -0.584 0.559996
#> ARMCDTRT:AVISITVIS4             0.62423    1.85085 129.72000   0.337 0.736463
#>                                  
#> (Intercept)                   ***
#> RACEBlack or African American *  
#> RACEWhite                     ***
#> SEXFemale                        
#> ARMCDTRT                      ***
#> AVISITVIS2                    ***
#> AVISITVIS3                    ***
#> AVISITVIS4                    ***
#> ARMCDTRT:AVISITVIS2              
#> ARMCDTRT:AVISITVIS3              
#> ARMCDTRT:AVISITVIS4              
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Covariance estimate:
#>         VIS1    VIS2    VIS3    VIS4
#> VIS1 40.5537 14.3960  4.9747 13.3867
#> VIS2 14.3960 26.5715  2.7855  7.4745
#> VIS3  4.9747  2.7855 14.8979  0.9082
#> VIS4 13.3867  7.4745  0.9082 95.5568
```

## Details

For a more detailed introduction to all of the features of this package,
look at the introduction vignette:

``` r
vignette("introduction")
```

For the available covariance structures, look at the covariance
vignette:

``` r
vignette("covariance")
```

In order to understand how `mmrm` is fitting the models, you can read
the details at:

``` r
vignette("algorithm")
```
