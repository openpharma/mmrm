# mmrm

Mixed models for repeated measures (MMRM) are a popular choice for
analyzing longitudinal continuous outcomes in randomized clinical trials
and beyond; see [Cnaan, Laird and Slasor
(1997)](https://doi.org/10.1002%2f%28SICI%291097-0258%2819971030%2916%3a20%3c2349%3a%3aAID-SIM667%3e3.0.CO%3b2-E)
for a tutorial and [Mallinckrodt, Lane and Schnell
(2008)](https://doi.org/10.1177/009286150804200402) for a review. This
package implements MMRM based on the marginal linear model without
random effects using Template Model Builder (`TMB`) which enables fast
and robust model fitting. Users can specify a variety of covariance
matrices, weight observations, fit models with restricted or standard
maximum likelihood inference, perform hypothesis testing with
Satterthwaite or Kenward-Roger adjustment, and extract least square
means estimates by using `emmeans`.

**Scope:**

- Continuous responses with normal (but potentially heteroscedastic)
  residuals.
- Marginal linear models (no individual-level random effects).

**Main Features:**

- Flexible covariance specification:
  - [Structures](https://openpharma.github.io/mmrm/main/articles/covariance.html):
    unstructured, Toeplitz, AR1, compound symmetry, ante-dependence, and
    spatial exponential.
  - Groups: shared covariance structure for all subjects or
    group-specific covariance estimates.
  - Variances: homogeneous or heterogeneous across time points.
- Inference:
  - Supports REML and ML.
  - Supports weights.
- Hypothesis testing:
  - [Least square
    means](https://openpharma.github.io/mmrm/main/reference/emmeans_support.html):
    can be obtained with the `emmeans` package
  - One- and multi-dimensional linear contrasts of model parameters can
    be tested.
  - [Satterthwaite](https://openpharma.github.io/mmrm/main/articles/satterthwaite.html)
    adjusted degrees of freedom.
  - [Kenward-Roger](https://openpharma.github.io/mmrm/main/articles/kenward.html)
    adjusted degrees of freedom and coefficients covariance matrix.
  - [Coefficient
    Covariance](https://openpharma.github.io/mmrm/main/articles/coef_vcov.html)
- `C++` backend:
  - Fast implementation using `C++` and automatic differentiation to
    obtain precise gradient information for model fitting.
  - Model fitting algorithm
    [details](https://openpharma.github.io/mmrm/main/articles/algorithm.html)
    used in `mmrm`.
- Package ecosystems integration:
  - Integration with [tidymodels](https://www.tidymodels.org/) package
    ecosystem
    - Dedicated [parsnip](https://parsnip.tidymodels.org/) engine for
      linear regression
    - Integration with [recipes](https://recipes.tidymodels.org/)
  - Integration with [tern](https://insightsengineering.github.io/tern/)
    package ecosystems
    - The [tern.mmrm](https://insightsengineering.github.io/tern.mmrm/)
      package can be used to run the `mmrm` fit and generate tabulation
      and plots of least square means per visit and treatment arm,
      tabulation of model diagnostics, diagnostic graphs, and other
      standard model outputs.

## Installation

### Release

You can install the current release version from *CRAN* with:

``` r
install.packages("mmrm")
```

### Development

You can install the current development version from *R-Universe* with:

``` r
install.packages(
  "mmrm",
  repos = c("https://openpharma.r-universe.dev", "https://cloud.r-project.org")
)
```

This is preferred, because for Windows and MacOS systems you can install
pre-compiled binary versions of the packages, avoiding the need for
compilation.

Alternatively, you can install the current development version from
*GitHub* with:

``` r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("openpharma/mmrm")
```

Note that this installation from source can take a substantial amount of
time, because compilation of the `C++` sources is required.

## Getting Started

See also the [introductory
vignette](https://openpharma.github.io/mmrm/main/articles/introduction.html)
or get started by trying out the example:

``` r
library(mmrm)
fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)
```

The code specifies an MMRM with the given covariates and an unstructured
covariance matrix for the timepoints (also called visits in the clinical
trial context, here given by `AVISIT`) within the subjects (here
`USUBJID`). While by default this uses restricted maximum likelihood
(REML), it is also possible to use ML, see
[`?mmrm`](https://openpharma.github.io/mmrm/reference/mmrm.md).

Printing the object will show you output which should be familiar to
anyone who has used any popular modeling functions such as
[`stats::lm()`](https://rdrr.io/r/stats/lm.html),
[`stats::glm()`](https://rdrr.io/r/stats/glm.html),
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html),
and [`lme4::nlmer()`](https://rdrr.io/pkg/lme4/man/nlmer.html). From
this print out we see the function call, the data used, the covariance
structure with number of variance parameters, as well as the likelihood
method, and model deviance achieved. Additionally the user is provided a
printout of the estimated coefficients and the model convergence
information:

``` r
fit
#> mmrm fit
#> 
#> Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#> Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Covariance:  unstructured (10 variance parameters)
#> Inference:   REML
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

The [`summary()`](https://rdrr.io/r/base/summary.html) method then
provides the coefficients table with Satterthwaite degrees of freedom as
well as the covariance matrix estimate:

``` r
summary(fit)
#> mmrm fit
#> 
#> Formula:     FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#> Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Covariance:  unstructured (10 variance parameters)
#> Method:      Satterthwaite
#> Vcov Method: Asymptotic
#> Inference:   REML
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

## Citing `mmrm`

To cite `mmrm` please see
[here](https://openpharma.github.io/mmrm/main/authors.html#citation).
