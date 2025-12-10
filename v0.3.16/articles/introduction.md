# Package Introduction

## Common usage

A minimal call of
[`mmrm()`](https://openpharma.github.io/mmrm/reference/mmrm.md),
consisting of only formula and data arguments will produce an object of
class `mmrm`, `mmrm_fit`, and `mmrm_tmb`.

Here we fit a mmrm model with `us` (unstructured) covariance structure
specified, as well as the defaults of `reml = TRUE` and
`control = mmrm_control()`.

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

## Common customizations

From the high-level
[`mmrm()`](https://openpharma.github.io/mmrm/main/reference/mmrm.html)
interface, common changes to the default function call can be specified.

### Control Function

For fine control,
[`mmrm_control()`](https://openpharma.github.io/mmrm/main/reference/mmrm_control.html)
is provided. This function allows the user to choose the adjustment
method for the degrees of freedom and the coefficients covariance
matrix, specify optimization routines, number of cores to be used on
Unix systems for trying several optimizers in parallel, provide a vector
of starting parameter values, decide the action to be taken when the
defined design matrix is singular, not drop unobserved visit levels. For
example:

``` r
mmrm_control(
  method = "Kenward-Roger",
  optimizer = c("L-BFGS-B", "BFGS"),
  n_cores = 2,
  start = c(0, 1, 1, 0, 1, 0),
  accept_singular = FALSE,
  drop_visit_levels = FALSE
)
```

Note that this control list can either be passed via the `control`
argument to `mmrm`, or selected controls can be directly specified in
the `mmrm` call. We will see this below.

#### Starting Values

The starting values will affect the optimization result. A better
starting value usually can make the optimization more efficient. In
`mmrm` we provide two starting value functions, one is `std_start` and
the other is `emp_start`. `std_start` will try to use the identity
matrix as the covariance, however there are convergence problems for
`ar1` and `ar1h` if the identity matrix is provided, thus for these two
covariance structures we use \\\rho=0.5\\ instead. `emp_start` will try
to use the empirical covariance matrix of the residuals of the ordinary
least squares model as the starting value for unstructured covariance
structure. If some timepoints are missing from data, identity matrix
will be used for that submatrix. The correlation between existing and
non-existing timepoints are set to 0.

As the starting values will affect the result, please be cautious on
choosing the starting values.

##### Example of Default Starting Value Fails

Here we provide an example where the `std_start` fails. In the following
code chunk, we will create a dummy dataset for mmrm analysis.

``` r
gen_data <- function(
    n = 100,
    mu = -100 / 52,
    delta = 50 / 52,
    mua = 2000,
    sigmaa = 300,
    sigmab = 60,
    corab = 0.2,
    sigma = 10,
    times = c(0, 2, 6, 12, 24, 36, 52, 70, 88, 104)) {
  nt <- length(times)
  out <- data.frame(
    pts = rep(seq_len(n * 2), each = nt),
    trt = rep(c("Treatment", "Placebo"), rep(n * nt, 2)),
    time = rep(times, n * 2)
  )

  covab <- corab * sigmaa * sigmab # cov between a and b
  cov <- matrix(c(sigmaa^2, covab, covab, sigmab^2), ncol = 2) # Cov matrix for the slope and intercept
  si <- rbind(
    MASS::mvrnorm(n, mu = c(mua, mu + delta), Sigma = cov),
    MASS::mvrnorm(n, mu = c(mua, mu + delta), Sigma = cov)
  )
  idx <- rep(seq_len(n * 2), each = nt)
  out$fev1 <- si[idx, 1] + si[idx, 2] * times + rnorm(n * nt * 2, sd = sigma)
  out$trt <- factor(out$trt)
  out$time <- factor(out$time)
  out$pts <- factor(out$pts)
  return(out)
}
set.seed(123)
out <- gen_data()
```

In the generated data, the variance is not in the same scale across
visits.

``` r
vapply(split(out$fev1, out$time), sd, FUN.VALUE = 1)
#>         0         2         6        12        24        36        52        70 
#>  278.6079  319.0589  482.4172  799.9107 1491.1440 2194.5776 3140.0768 4204.9355 
#>        88       104 
#> 5272.6041 6221.2195
```

Using `emp_start` as the starting value, `mmrm` will converge fast.

``` r
mmrm(fev1 ~ trt * time + us(time | pts), data = out, start = emp_start)
#> mmrm fit
#> 
#> Formula:     fev1 ~ trt * time + us(time | pts)
#> Data:        out (used 2000 observations from 200 subjects with maximum 10 
#> timepoints)
#> Covariance:  unstructured (55 variance parameters)
#> Inference:   REML
#> Deviance:    19154.63
#> 
#> Coefficients: 
#>          (Intercept)         trtTreatment                time2 
#>         1962.6980059           11.3831958            0.2130684 
#>                time6               time12               time24 
#>           -1.6901336           -1.1984277           -7.3954489 
#>               time36               time52               time70 
#>          -11.4078895          -15.8040920          -22.5556524 
#>               time88              time104   trtTreatment:time2 
#>          -28.2068895          -33.6608067            6.0436315 
#>   trtTreatment:time6  trtTreatment:time12  trtTreatment:time24 
#>           27.3686373           49.4246567          107.3638488 
#>  trtTreatment:time36  trtTreatment:time52  trtTreatment:time70 
#>          161.4310444          233.6438342          316.7387101 
#>  trtTreatment:time88 trtTreatment:time104 
#>          397.9895967          471.8871913 
#> 
#> Model Inference Optimization:
#> Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch
```

However, if we use `std_start`, there will be convergence problems. We
can also force a specific optimization algorithm and add control
details, here e.g. choosing `nlminb` with increased maximum number of
function evaluations and iterations.

``` r
mmrm(
  fev1 ~ trt * time + us(time | pts),
  data = out,
  start = std_start,
  optimizer = "nlminb",
  optimizer_control = list(eval.max = 1000, iter.max = 1000)
)
```

### REML or ML

Users can specify if REML should be used (default) or if ML should be
used in optimization.

``` r
fit_ml <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  reml = FALSE
)
fit_ml
#> mmrm fit
#> 
#> Formula:     FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID)
#> Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Covariance:  unstructured (10 variance parameters)
#> Inference:   ML
#> Deviance:    3397.934
#> 
#> Coefficients: 
#>                   (Intercept) RACEBlack or African American 
#>                    30.9663423                     1.5086851 
#>                     RACEWhite                      ARMCDTRT 
#>                     5.6133151                     3.7761037 
#>                    AVISITVIS2                    AVISITVIS3 
#>                     4.8270155                    10.3353319 
#>                    AVISITVIS4           ARMCDTRT:AVISITVIS2 
#>                    15.0487715                    -0.0156154 
#>           ARMCDTRT:AVISITVIS3           ARMCDTRT:AVISITVIS4 
#>                    -0.6663598                     0.6317222 
#> 
#> Model Inference Optimization:
#> Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch
```

### Optimizer

Users can specify which optimizer should be used, changing from the
default of four optimizers, which starts with `L-BFGS-B` and proceeds
through the other choices if optimization fails to converge. Other
choices are `BFGS`, `CG`, `nlminb` and other user-defined custom
optimizers.

`L-BFGS-B`, `BFGS` and `CG` are all implemented with
[`stats::optim()`](https://rdrr.io/r/stats/optim.html) and the Hessian
is not used, while `nlminb` is using
[`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html) which in turn
uses both the gradient and the Hessian (by default but can be switch
off) for the optimization.

``` r
fit_opt <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  optimizer = "BFGS"
)
fit_opt
#> mmrm fit
#> 
#> Formula:     FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID)
#> Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Covariance:  unstructured (10 variance parameters)
#> Inference:   REML
#> Deviance:    3387.373
#> 
#> Coefficients: 
#>                   (Intercept) RACEBlack or African American 
#>                   30.96768936                    1.50467465 
#>                     RACEWhite                      ARMCDTRT 
#>                    5.61310613                    3.77554452 
#>                    AVISITVIS2                    AVISITVIS3 
#>                    4.82858600                   10.33317622 
#>                    AVISITVIS4           ARMCDTRT:AVISITVIS2 
#>                   15.05257117                   -0.01735504 
#>           ARMCDTRT:AVISITVIS3           ARMCDTRT:AVISITVIS4 
#>                   -0.66752133                    0.63095590 
#> 
#> Model Inference Optimization:
#> Converged with code 0 and message: No message provided.
```

### Covariance Structure

Covariance structures supported by the `mmrm` are being continuously
developed. For a complete list and description please visit the
[covariance
vignette](https://openpharma.github.io/mmrm/articles/covariance.md).
Below we see the function call for homogeneous compound symmetry (`cs`).

``` r
fit_cs <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + cs(AVISIT | USUBJID),
  data = fev_data,
  reml = FALSE
)
fit_cs
#> mmrm fit
#> 
#> Formula:     FEV1 ~ RACE + ARMCD * AVISIT + cs(AVISIT | USUBJID)
#> Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Covariance:  compound symmetry (2 variance parameters)
#> Inference:   ML
#> Deviance:    3536.989
#> 
#> Coefficients: 
#>                   (Intercept) RACEBlack or African American 
#>                    31.4207078                     0.5357236 
#>                     RACEWhite                      ARMCDTRT 
#>                     5.4546329                     3.4305211 
#>                    AVISITVIS2                    AVISITVIS3 
#>                     4.8326353                    10.2395076 
#>                    AVISITVIS4           ARMCDTRT:AVISITVIS2 
#>                    15.0672680                     0.2801641 
#>           ARMCDTRT:AVISITVIS3           ARMCDTRT:AVISITVIS4 
#>                    -0.5894964                     0.7939750 
#> 
#> Model Inference Optimization:
#> Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch
```

The time points have to be unique for each subject. That is, there
cannot be time points with multiple observations for any subject. The
rationale is that these observations would need to be correlated, but it
is not possible within the currently implemented covariance structure
framework to do that correctly. Moreover, for non-spatial covariance
structures, the time variable must be coded as a factor.

### Weighting

Users can perform weighted MMRM by specifying a numeric vector `weights`
with positive values.

``` r
fit_wt <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  weights = fev_data$WEIGHT
)
fit_wt
#> mmrm fit
#> 
#> Formula:     FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID)
#> Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Weights:     fev_data$WEIGHT
#> Covariance:  unstructured (10 variance parameters)
#> Inference:   REML
#> Deviance:    3476.526
#> 
#> Coefficients: 
#>                   (Intercept) RACEBlack or African American 
#>                   31.20065229                    1.18452837 
#>                     RACEWhite                      ARMCDTRT 
#>                    5.36525917                    3.39695951 
#>                    AVISITVIS2                    AVISITVIS3 
#>                    4.85890820                   10.03942420 
#>                    AVISITVIS4           ARMCDTRT:AVISITVIS2 
#>                   14.79354054                    0.03418184 
#>           ARMCDTRT:AVISITVIS3           ARMCDTRT:AVISITVIS4 
#>                    0.01308088                    0.86701567 
#> 
#> Model Inference Optimization:
#> Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch
```

### Grouped Covariance Structure

Grouped covariance structures are supported by the`mmrm` package.
Covariance matrices for each group are identically structured
(unstructured, compound symmetry, etc) but the estimates are allowed to
vary across groups. We use the form `cs(time | group / subject)` to
specify the group variable.

Here is an example of how we use `ARMCD` as group variable.

``` r
fit_cs <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + cs(AVISIT | ARMCD / USUBJID),
  data = fev_data,
  reml = FALSE
)
VarCorr(fit_cs)
#> $PBO
#>           VIS1      VIS2      VIS3      VIS4
#> VIS1 37.822313  3.600996  3.600996  3.600996
#> VIS2  3.600996 37.822313  3.600996  3.600996
#> VIS3  3.600996  3.600996 37.822313  3.600996
#> VIS4  3.600996  3.600996  3.600996 37.822313
#> 
#> $TRT
#>          VIS1     VIS2     VIS3     VIS4
#> VIS1 49.58031 10.98097 10.98097 10.98097
#> VIS2 10.98097 49.58031 10.98097 10.98097
#> VIS3 10.98097 10.98097 49.58031 10.98097
#> VIS4 10.98097 10.98097 10.98097 49.58031
```

We can see that the estimated covariance matrices are different in
different `ARMCD` groups.

### Adjustment Method

In additional to the residual and Between-Within degrees of freedom,
both Satterthwaite and Kenward-Roger adjustment methods are available.
The default is Satterthwaite adjustment of the degrees of freedom. To
use e.g. the Kenward-Roger adjustment of the degrees of freedom as well
as the coefficients covariance matrix, use the `method` argument:

A list of all allowed `method` is

1.  “Kenward-Roger”
2.  “Satterthwaite”
3.  “Residual”
4.  “Between-Within”

``` r
fit_kr <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  method = "Kenward-Roger"
)
```

Note that this requires `reml = TRUE`, i.e. Kenward-Roger adjustment is
not possible when using maximum likelihood inference. While this
adjustment choice is not visible in the
[`print()`](https://rdrr.io/r/base/print.html) result of the fitted
model (because the initial model fit is not affected by the choice of
the adjustment method), looking at the `summary` we see the method and
the correspondingly adjusted standard errors and degrees of freedom:

``` r
summary(fit_kr)
#> mmrm fit
#> 
#> Formula:     FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID)
#> Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Covariance:  unstructured (10 variance parameters)
#> Method:      Kenward-Roger
#> Vcov Method: Kenward-Roger
#> Inference:   REML
#> 
#> Model selection criteria:
#>      AIC      BIC   logLik deviance 
#>   3407.4   3440.2  -1693.7   3387.4 
#> 
#> Coefficients: 
#>                                Estimate Std. Error        df t value Pr(>|t|)
#> (Intercept)                    30.96770    0.83335 187.91000  37.160  < 2e-16
#> RACEBlack or African American   1.50465    0.62901 169.95000   2.392  0.01784
#> RACEWhite                       5.61310    0.67139 158.87000   8.360 2.98e-14
#> ARMCDTRT                        3.77556    1.07910 146.27000   3.499  0.00062
#> AVISITVIS2                      4.82859    0.80408 143.66000   6.005 1.49e-08
#> AVISITVIS3                     10.33317    0.82303 155.66000  12.555  < 2e-16
#> AVISITVIS4                     15.05256    1.30180 138.39000  11.563  < 2e-16
#> ARMCDTRT:AVISITVIS2            -0.01737    1.13154 138.39000  -0.015  0.98777
#> ARMCDTRT:AVISITVIS3            -0.66753    1.18714 158.21000  -0.562  0.57470
#> ARMCDTRT:AVISITVIS4             0.63094    1.83319 129.64000   0.344  0.73127
#>                                  
#> (Intercept)                   ***
#> RACEBlack or African American *  
#> RACEWhite                     ***
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
#> VIS1 40.7335 14.2740  5.1411 13.5288
#> VIS2 14.2740 26.2243  2.6391  7.3219
#> VIS3  5.1411  2.6391 14.9497  1.0341
#> VIS4 13.5288  7.3219  1.0341 95.6006
```

For one-dimensional contrasts as in the coefficients table above, the
degrees of freedom are the same for Kenward-Roger and Satterthwaite.
However, Kenward-Roger uses adjusted standard errors, hence the p-values
are different.

Note that if you would like to match SAS results for an unstructured
covariance model, you can use the linear Kenward-Roger approximation:

``` r
fit_kr_lin <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  method = "Kenward-Roger",
  vcov = "Kenward-Roger-Linear"
)
```

This is due to the different parametrization of the unstructured
covariance matrix, see the [Kenward-Roger
vignette](https://openpharma.github.io/mmrm/articles/kenward.html#parameterization-methods-and-kenward-roger)
for details.

### Variance-covariance for Coefficients

There are multiple variance-covariance estimator available for the
coefficients, including:

1.  “Asymptotic”
2.  “Empirical” (Cluster Robust Sandwich)
3.  “Empirical-Jackknife”
4.  “Empirical-Bias-Reduced”
5.  “Kenward-Roger”
6.  “Kenward-Roger-Linear”

Please note that, not all combinations of variance-covariance for
coefficients and method of degrees of freedom are possible,
e.g. “Kenward-Roger” and “Kenward-Roger-Linear” are available only when
the degrees of freedom method is “Kenward-Roger”.

Details can be found in [Coefficients Covariance Matrix Adjustment
vignette](https://openpharma.github.io/mmrm/articles/coef_vcov.md) and
[Weighted Least Square Empirical
Covariance](https://openpharma.github.io/mmrm/articles/empirical_wls.md).

An example of using other variance-covariance is:

``` r
fit_emp <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  method = "Satterthwaite",
  vcov = "Empirical"
)
```

### Keeping Unobserved Visits

Sometimes not all possible time points are observed in a given data set.
When using a structured covariance matrix, e.g. with auto-regressive
structure, then it can be relevant to keep the correct distance between
the observed time points.

Consider the following example where we have deliberately removed the
`VIS3` observations from our initial example data set `fev_data` to
obtain `sparse_data`. We first fit the model where we do not drop the
visit level explicitly, using the `drop_visit_levels = FALSE` choice.
Second we fit the model by default without this option.

``` r
sparse_data <- fev_data[fev_data$AVISIT != "VIS3", ]
sparse_result <- mmrm(
  FEV1 ~ RACE + ar1(AVISIT | USUBJID),
  data = sparse_data,
  drop_visit_levels = FALSE
)

dropped_result <- mmrm(
  FEV1 ~ RACE + ar1(AVISIT | USUBJID),
  data = sparse_data
)
#> In AVISIT there are dropped visits: VIS3.
#>  Additional attributes including contrasts are lost.
#> To avoid this behavior, make sure use `drop_visit_levels = FALSE`.
```

We see that we get a message about the dropped visit level by default.
Now we can compare the estimated correlation matrices:

``` r
cov2cor(VarCorr(sparse_result))
#>            VIS1      VIS2      VIS3       VIS4
#> VIS1 1.00000000 0.4051834 0.1641736 0.06652042
#> VIS2 0.40518341 1.0000000 0.4051834 0.16417360
#> VIS3 0.16417360 0.4051834 1.0000000 0.40518341
#> VIS4 0.06652042 0.1641736 0.4051834 1.00000000
cov2cor(VarCorr(dropped_result))
#>            VIS1      VIS2       VIS4
#> VIS1 1.00000000 0.1468464 0.02156386
#> VIS2 0.14684640 1.0000000 0.14684640
#> VIS4 0.02156386 0.1468464 1.00000000
```

We see that when using the default, second result, we just drop the
`VIS3` from the covariance matrix. As a consequence, we model the
correlation between `VIS2` and `VIS4` the same as the correlation
between `VIS1` and `VIS2`. Hence we get a smaller correlation estimate
here compared to the first result, which includes `VIS3` explicitly.

## Extraction of model features

Similar to model objects created in other packages, components of `mmrm`
and `mmrm_tmb` objects can be accessed with standard methods.
Additionally,
[`component()`](https://openpharma.github.io/mmrm/main/reference/component.html)
is provided to allow deeper and more precise access for those interested
in digging through model output. Complete documentation of standard
model output methods supported for `mmrm_tmb` objects [can be found at
the package
website.](https://openpharma.github.io/mmrm/main/reference/mmrm_tmb_methods.html)

### Summary

The `summary` method for `mmrm` objects provides easy access to
frequently needed model components.

``` r
fit <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)
fit_summary <- summary(fit)
```

From this summary object, you can easily retrieve the coefficients
table.

``` r
fit_summary$coefficients
#>                                  Estimate Std. Error       df     t value
#> (Intercept)                   30.96769899  0.8293349 187.9132 37.34040185
#> RACEBlack or African American  1.50464863  0.6206596 169.9454  2.42427360
#> RACEWhite                      5.61309565  0.6630909 158.8700  8.46504747
#> ARMCDTRT                       3.77555734  1.0762774 146.2690  3.50797778
#> AVISITVIS2                     4.82858803  0.8017144 143.6593  6.02282805
#> AVISITVIS3                    10.33317002  0.8224414 155.6572 12.56401918
#> AVISITVIS4                    15.05255715  1.3128602 138.3916 11.46546844
#> ARMCDTRT:AVISITVIS2           -0.01737409  1.1291645 138.3926 -0.01538668
#> ARMCDTRT:AVISITVIS3           -0.66753189  1.1865359 158.2106 -0.56258887
#> ARMCDTRT:AVISITVIS4            0.63094392  1.8507884 129.6377  0.34090549
#>                                   Pr(>|t|)
#> (Intercept)                   7.122411e-89
#> RACEBlack or African American 1.638725e-02
#> RACEWhite                     1.605553e-14
#> ARMCDTRT                      6.001485e-04
#> AVISITVIS2                    1.366921e-08
#> AVISITVIS3                    1.927523e-25
#> AVISITVIS4                    8.242709e-22
#> ARMCDTRT:AVISITVIS2           9.877459e-01
#> ARMCDTRT:AVISITVIS3           5.745112e-01
#> ARMCDTRT:AVISITVIS4           7.337266e-01
```

Other model parameters and metadata available in the summary object is
as follows:

``` r
str(fit_summary)
#> List of 15
#>  $ cov_type        : chr "us"
#>  $ reml            : logi TRUE
#>  $ n_groups        : int 1
#>  $ n_theta         : int 10
#>  $ n_subjects      : int 197
#>  $ n_timepoints    : int 4
#>  $ n_obs           : int 537
#>  $ beta_vcov       : num [1:10, 1:10] 0.688 -0.207 -0.163 -0.569 -0.422 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:10] "(Intercept)" "RACEBlack or African American" "RACEWhite" "ARMCDTRT" ...
#>   .. ..$ : chr [1:10] "(Intercept)" "RACEBlack or African American" "RACEWhite" "ARMCDTRT" ...
#>  $ varcor          : num [1:4, 1:4] 40.73 14.27 5.14 13.53 14.27 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:4] "VIS1" "VIS2" "VIS3" "VIS4"
#>   .. ..$ : chr [1:4] "VIS1" "VIS2" "VIS3" "VIS4"
#>  $ method          : chr "Satterthwaite"
#>  $ vcov            : chr "Asymptotic"
#>  $ coefficients    : num [1:10, 1:5] 30.97 1.5 5.61 3.78 4.83 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:10] "(Intercept)" "RACEBlack or African American" "RACEWhite" "ARMCDTRT" ...
#>   .. ..$ : chr [1:5] "Estimate" "Std. Error" "df" "t value" ...
#>  $ n_singular_coefs: int 0
#>  $ aic_list        :List of 4
#>   ..$ AIC     : num 3407
#>   ..$ BIC     : num 3440
#>   ..$ logLik  :Class 'logLik' : -1694 (df=10)
#>   ..$ deviance: num 3387
#>  $ call            : language mmrm(formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID), data = fev_data)
#>  - attr(*, "class")= chr "summary.mmrm"
```

### Residuals

The `residuals` method for `mmrm` objects can be used to provide three
different types of residuals:

1.  Response or raw residuals - the difference between the observed and
    fitted or predicted value. MMRMs can allow for heteroscedasticity,
    so these residuals should be interpreted with caution.

``` r
fit <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)
residuals_resp <- residuals(fit, type = "response")
```

2.  Pearson residuals - the raw residuals scaled by the estimated
    standard deviation of the response. This residual type is better
    suited to identifying outlying observations and the appropriateness
    of the covariance structure, compared to the raw residuals.

``` r
residuals_pearson <- residuals(fit, type = "pearson")
```

3.  Normalized or scaled residuals - the raw residuals are
    ‘de-correlated’ based on the Cholesky decomposition of the
    variance-covariance matrix. These residuals should approximately
    follow the standard normal distribution, and therefore can be used
    to check for normality (@galecki2013linear).

``` r
residuals_norm <- residuals(fit, type = "normalized")
```

### `broom` extensions

`mmrm` also contains S3 methods methods for `tidy`, `glance` and
`augment` which were introduced by
[`broom`](https://broom.tidymodels.org/). Note that these methods will
work also without loading the `broom` package. Please see
[`?mmrm_tidiers`](https://openpharma.github.io/mmrm/reference/mmrm_tidiers.md)
for the detailed documentation.

For example, we can apply the `tidy` method to return a summary table of
coefficient estimates:

``` r
fit |>
  tidy()
#> # A tibble: 10 × 6
#>    term                          estimate std.error    df statistic  p.value
#>    <chr>                            <dbl>     <dbl> <dbl>     <dbl>    <dbl>
#>  1 (Intercept)                    31.0        0.829  188.   37.3    7.12e-89
#>  2 RACEBlack or African American   1.50       0.621  170.    2.42   1.64e- 2
#>  3 RACEWhite                       5.61       0.663  159.    8.47   1.61e-14
#>  4 ARMCDTRT                        3.78       1.08   146.    3.51   6.00e- 4
#>  5 AVISITVIS2                      4.83       0.802  144.    6.02   1.37e- 8
#>  6 AVISITVIS3                     10.3        0.822  156.   12.6    1.93e-25
#>  7 AVISITVIS4                     15.1        1.31   138.   11.5    8.24e-22
#>  8 ARMCDTRT:AVISITVIS2            -0.0174     1.13   138.   -0.0154 9.88e- 1
#>  9 ARMCDTRT:AVISITVIS3            -0.668      1.19   158.   -0.563  5.75e- 1
#> 10 ARMCDTRT:AVISITVIS4             0.631      1.85   130.    0.341  7.34e- 1
```

We can also specify some details to request confidence intervals of
specific confidence level:

``` r
fit |>
  tidy(conf.int = TRUE, conf.level = 0.9)
#> # A tibble: 10 × 8
#>    term           estimate std.error    df statistic  p.value conf.low conf.high
#>    <chr>             <dbl>     <dbl> <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#>  1 (Intercept)     31.0        0.829  188.   37.3    7.12e-89   29.6       32.3 
#>  2 ARMCDTRT         3.78       1.08   146.    3.51   6.00e- 4    1.99       5.56
#>  3 ARMCDTRT:AVIS…  -0.0174     1.13   138.   -0.0154 9.88e- 1   -1.89       1.85
#>  4 ARMCDTRT:AVIS…  -0.668      1.19   158.   -0.563  5.75e- 1   -2.63       1.30
#>  5 ARMCDTRT:AVIS…   0.631      1.85   130.    0.341  7.34e- 1   -2.44       3.70
#>  6 AVISITVIS2       4.83       0.802  144.    6.02   1.37e- 8    3.50       6.16
#>  7 AVISITVIS3      10.3        0.822  156.   12.6    1.93e-25    8.97      11.7 
#>  8 AVISITVIS4      15.1        1.31   138.   11.5    8.24e-22   12.9       17.2 
#>  9 RACEBlack or …   1.50       0.621  170.    2.42   1.64e- 2    0.478      2.53
#> 10 RACEWhite        5.61       0.663  159.    8.47   1.61e-14    4.52       6.71
```

Or we can apply the `glance` method to return a summary table of
goodness of fit statistics:

``` r
fit |>
  glance()
#> # A tibble: 1 × 4
#>     AIC   BIC logLik    deviance
#>   <dbl> <dbl> <logLik>     <dbl>
#> 1 3407. 3440. -1693.687    3387.
```

Finally, we can use the `augment` method to return a merged `tibble` of
the data, fitted values and residuals:

``` r
fit |>
  augment()
#> # A tibble: 537 × 8
#>    .rownames  FEV1 RACE                     ARMCD AVISIT USUBJID .fitted  .resid
#>        <dbl> <dbl> <fct>                    <fct> <fct>  <fct>     <dbl>   <dbl>
#>  1         2  40.0 Black or African Americ… TRT   VIS2   PT1        41.1  -1.09 
#>  2         4  20.5 Black or African Americ… TRT   VIS4   PT1        51.9 -31.4  
#>  3         6  31.5 Asian                    PBO   VIS2   PT2        35.8  -4.34 
#>  4         7  36.9 Asian                    PBO   VIS3   PT2        41.3  -4.42 
#>  5         8  48.8 Asian                    PBO   VIS4   PT2        46.0   2.79 
#>  6        10  36.0 Black or African Americ… PBO   VIS2   PT3        37.3  -1.31 
#>  7        12  37.2 Black or African Americ… PBO   VIS4   PT3        47.5 -10.4  
#>  8        13  33.9 Asian                    TRT   VIS1   PT4        34.7  -0.851
#>  9        14  33.7 Asian                    TRT   VIS2   PT4        39.6  -5.81 
#> 10        16  54.5 Asian                    TRT   VIS4   PT4        50.4   4.02 
#> # ℹ 527 more rows
```

Also here we can specify details for the prediction intervals and type
of residuals via the arguments:

``` r
fit |>
  augment(interval = "confidence", type.residuals = "normalized")
#> # A tibble: 537 × 11
#>    .rownames  FEV1 RACE       ARMCD AVISIT USUBJID .fitted .lower .upper .se.fit
#>        <dbl> <dbl> <fct>      <fct> <fct>  <fct>     <dbl>  <dbl>  <dbl>   <dbl>
#>  1         2  40.0 Black or … TRT   VIS2   PT1        41.1   41.0   41.1   0.515
#>  2         4  20.5 Black or … TRT   VIS4   PT1        51.9   51.8   52.0   1.55 
#>  3         6  31.5 Asian      PBO   VIS2   PT2        35.8   35.8   35.8   0.486
#>  4         7  36.9 Asian      PBO   VIS3   PT2        41.3   41.3   41.3   0.325
#>  5         8  48.8 Asian      PBO   VIS4   PT2        46.0   45.9   46.1   1.53 
#>  6        10  36.0 Black or … PBO   VIS2   PT3        37.3   37.3   37.3   0.462
#>  7        12  37.2 Black or … PBO   VIS4   PT3        47.5   47.4   47.6   1.51 
#>  8        13  33.9 Asian      TRT   VIS1   PT4        34.7   34.7   34.8   0.709
#>  9        14  33.7 Asian      TRT   VIS2   PT4        39.6   39.5   39.6   0.468
#> 10        16  54.5 Asian      TRT   VIS4   PT4        50.4   50.3   50.5   1.55 
#> # ℹ 527 more rows
#> # ℹ 1 more variable: .resid <dbl>
```

### Other components

Specific model quantities not supported by methods can be retrieved with
the
[`component()`](https://openpharma.github.io/mmrm/main/reference/component.html)
function. The default will output all supported components.

For example, a user may want information about convergence:

``` r
component(fit, name = c("convergence", "evaluations", "conv_message"))
#> $convergence
#> [1] 0
#> 
#> $evaluations
#> function gradient 
#>       17       17 
#> 
#> $conv_message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
```

or the original low-level call:

``` r
component(fit, name = "call")
#> mmrm(formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID), 
#>     data = fev_data)
```

the user could also ask for all provided components by not specifying
the `name` argument.

``` r
component(fit)
```

## Lower level functions

### Low-level mmrm

The lower level function which is called by
[`mmrm()`](https://openpharma.github.io/mmrm/main/reference/mmrm.html)
is
[`fit_mmrm()`](https://openpharma.github.io/mmrm/main/reference/fit_mmrm.html).
This function is exported and can be used directly. It is similar to
[`mmrm()`](https://openpharma.github.io/mmrm/main/reference/mmrm.html)
but lacks some post-processing and support for Satterthwaite and
Kenward-Roger calculations. It may be useful for other packages that
want to fit the model without the adjustment calculations.

``` r
fit_mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  weights = rep(1, nrow(fev_data)),
  reml = TRUE,
  control = mmrm_control()
)
#> mmrm fit
#> 
#> Formula:     FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID)
#> Data:        fev_data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Weights:     rep(1, nrow(fev_data))
#> Covariance:  unstructured (10 variance parameters)
#> Inference:   REML
#> Deviance:    3387.373
#> 
#> Coefficients: 
#>                   (Intercept) RACEBlack or African American 
#>                   30.96769899                    1.50464863 
#>                     RACEWhite                      ARMCDTRT 
#>                    5.61309565                    3.77555734 
#>                    AVISITVIS2                    AVISITVIS3 
#>                    4.82858803                   10.33317002 
#>                    AVISITVIS4           ARMCDTRT:AVISITVIS2 
#>                   15.05255715                   -0.01737409 
#>           ARMCDTRT:AVISITVIS3           ARMCDTRT:AVISITVIS4 
#>                   -0.66753189                    0.63094392 
#> 
#> Model Inference Optimization:
#> Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch
```

## Hypothesis testing

This package supports estimation of one- and multi-dimensional contrasts
(t-test and F-test calculation) with the
[`df_1d()`](https://openpharma.github.io/mmrm/main/reference/df_1d.html)
and
[`df_md()`](https://openpharma.github.io/mmrm/main/reference/df_md.html)
functions. Both functions utilize the chosen adjustment method from the
initial `mmrm` call for the calculation of degrees of freedom and (for
Kenward-Roger methods) the variance estimates for the test-statistics.

### One-dimensional contrasts

Compute the test of a one-dimensional (vector) contrast for a `mmrm`
object with Satterthwaite degrees of freedom.

``` r
fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)

contrast <- numeric(length(component(fit, "beta_est")))
contrast[3] <- 1

df_1d(fit, contrast)
#> $est
#> [1] 5.643565
#> 
#> $se
#> [1] 0.6656093
#> 
#> $df
#> [1] 157.1382
#> 
#> $t_stat
#> [1] 8.478795
#> 
#> $p_val
#> [1] 1.564869e-14
```

This works similarly when choosing a Kenward-Roger adjustment:

``` r
fit_kr <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  method = "Kenward-Roger"
)

df_1d(fit_kr, contrast)
#> $est
#> [1] 5.643565
#> 
#> $se
#> [1] 0.6740941
#> 
#> $df
#> [1] 157.1382
#> 
#> $t_stat
#> [1] 8.372073
#> 
#> $p_val
#> [1] 2.931654e-14
```

We see that because this is a one-dimensional contrast, the degrees of
freedoms are identical for Satterthwaite and Kenward-Roger. However, the
standard errors are different and therefore the p-values are different.

Additional options for the degrees of freedom `method` are Residual and
Between-Within.

### Multi-dimensional contrasts

Compute the test of a multi-dimensional (matrix) contrast for the above
defined `mmrm` object with Satterthwaite degrees of freedom:

``` r
contrast <- matrix(data = 0, nrow = 2, ncol = length(component(fit, "beta_est")))
contrast[1, 2] <- contrast[2, 3] <- 1

df_md(fit, contrast)
#> $num_df
#> [1] 2
#> 
#> $denom_df
#> [1] 165.5553
#> 
#> $f_stat
#> [1] 36.91143
#> 
#> $p_val
#> [1] 5.544575e-14
```

And for the Kenward-Roger adjustment:

``` r
df_md(fit_kr, contrast)
#> $num_df
#> [1] 2
#> 
#> $denom_df
#> [1] 165.5728
#> 
#> $f_stat
#> [1] 35.99422
#> 
#> $p_val
#> [1] 1.04762e-13
```

We see that for the multi-dimensional contrast we get slightly different
denominator degrees of freedom for the two adjustment methods.

Also the simpler Residual and Between-Within `method` choices can be
used of course together with multidimensional contrasts.

### Support for emmeans

This package includes methods that allow `mmrm` objects to be used with
the `emmeans` package. `emmeans` computes estimated marginal means (also
called least-square means) for the coefficients of the MMRM. For
example, in order to see the least-square means by visit and by
treatment arm:

``` r
library(emmeans)
#> mmrm() registered as emmeans extension
#> Welcome to emmeans.
#> Caution: You lose important information if you filter this package's results.
#> See '? untidy'
lsmeans_by_visit <- emmeans(fit, ~ ARMCD | AVISIT)
lsmeans_by_visit
#> AVISIT = VIS1:
#>  ARMCD emmean    SE  df lower.CL upper.CL
#>  PBO     33.3 0.755 148     31.8     34.8
#>  TRT     37.1 0.763 143     35.6     38.6
#> 
#> AVISIT = VIS2:
#>  ARMCD emmean    SE  df lower.CL upper.CL
#>  PBO     38.2 0.612 147     37.0     39.4
#>  TRT     41.9 0.602 143     40.7     43.1
#> 
#> AVISIT = VIS3:
#>  ARMCD emmean    SE  df lower.CL upper.CL
#>  PBO     43.7 0.462 130     42.8     44.6
#>  TRT     46.8 0.509 130     45.7     47.8
#> 
#> AVISIT = VIS4:
#>  ARMCD emmean    SE  df lower.CL upper.CL
#>  PBO     48.4 1.190 134     46.0     50.7
#>  TRT     52.8 1.190 133     50.4     55.1
#> 
#> Results are averaged over the levels of: RACE, SEX 
#> Confidence level used: 0.95
```

Note that the degrees of freedom choice is inherited here from the
initial `mmrm` fit. Furthermore, we can also obtain the differences
between the treatment arms for each visit by applying
[`pairs()`](https://rdrr.io/r/graphics/pairs.html) on the object
returned by
[`emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
earlier:

``` r
pairs(lsmeans_by_visit, reverse = TRUE)
#> AVISIT = VIS1:
#>  contrast  estimate    SE  df t.ratio p.value
#>  TRT - PBO     3.77 1.070 146   3.514  0.0006
#> 
#> AVISIT = VIS2:
#>  contrast  estimate    SE  df t.ratio p.value
#>  TRT - PBO     3.73 0.859 145   4.346  <.0001
#> 
#> AVISIT = VIS3:
#>  contrast  estimate    SE  df t.ratio p.value
#>  TRT - PBO     3.08 0.690 131   4.467  <.0001
#> 
#> AVISIT = VIS4:
#>  contrast  estimate    SE  df t.ratio p.value
#>  TRT - PBO     4.40 1.680 133   2.617  0.0099
#> 
#> Results are averaged over the levels of: RACE, SEX
```

(This is similar like the `pdiff` option in SAS `PROC MIXED`.) Note that
we use here the `reverse` argument to obtain treatment minus placebo
results, instead of placebo minus treatment results.

To further obtain the confidence interval of the least square mean
differences, we can apply
[`confint()`](https://rdrr.io/r/stats/confint.html) on the result
returned by [`pairs()`](https://rdrr.io/r/graphics/pairs.html) .

This is similar to the `LSMEANS` in SAS, with `CL` and `DIFF` options.

``` r
confint(pairs(lsmeans_by_visit, reverse = TRUE))
#> AVISIT = VIS1:
#>  contrast  estimate    SE  df lower.CL upper.CL
#>  TRT - PBO     3.77 1.070 146     1.65     5.90
#> 
#> AVISIT = VIS2:
#>  contrast  estimate    SE  df lower.CL upper.CL
#>  TRT - PBO     3.73 0.859 145     2.03     5.43
#> 
#> AVISIT = VIS3:
#>  contrast  estimate    SE  df lower.CL upper.CL
#>  TRT - PBO     3.08 0.690 131     1.72     4.44
#> 
#> AVISIT = VIS4:
#>  contrast  estimate    SE  df lower.CL upper.CL
#>  TRT - PBO     4.40 1.680 133     1.07     7.72
#> 
#> Results are averaged over the levels of: RACE, SEX 
#> Confidence level used: 0.95
```

### Support for car

This package includes methods that allow `mmrm` objects to be used with
[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html). This function
conducts type II/III hypothesis testing for the effects in `mmrm`
models, using either F-tests (default) or Chi-squared tests.

#### Mathematical basis

For each effect, the Chi-squared test statistic is calculated using: \\
\chi^2=\[\mathbf{L}\cdot b\]^T\cdot
\[\mathbf{L}\cdot\mathbf{V}\cdot\mathbf{L}^T\]^{-1}\cdot\mathbf{L}\cdot
b \\ where \\\mathbf{L}\\ is the desired contrast matrix, \\b\\ is a
one-column matrix containing the coefficients of the non-aliased terms
in the model, and \\\mathbf{V}\\ is the variance-covariance matrix of
the coefficients.

The F-test statistic is calculated by dividing the Chi-squared test
statistic by the numerator degrees of freedom \\\text{df}\_\text{num}\\,
which is equal to the number of parameters associated with the effect
and therefore the number of rows in the contrast matrix \\\mathbf{L}\\.
It is also multiplied by a scaling factor \\\lambda\\ (e.g., when
utilizing the adjusted Kenward-Roger degrees of freedom), which can be
set to 1 if not needed:

\\ F = \lambda\frac{\chi^2}{\text{df}\_\text{num}} \\

#### Examples

In order to see if the used covariates are related to the response using
a type II test:

``` r
library(car)
#> Loading required package: carData
#> mmrm() registered as car::Anova extension
Anova(fit, type = "II")
#> Analysis of Fixed Effect Table (Type II F tests)
#>              Df Res.Df        F    Pr(>F)    
#> RACE          2 165.56  36.9114 5.545e-14 ***
#> SEX           1 166.13   0.3757    0.5407    
#> ARMCD         1 169.12  31.4209 8.296e-08 ***
#> AVISIT        3 148.65 142.8221 < 2.2e-16 ***
#> ARMCD:AVISIT  3 147.91   0.2581    0.8555    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Note that for the F-test, the denominator degrees of freedom choice is
inherited here from the initial `mmrm` fit. In addition, please note
that if you see results that are slightly different from SAS, it could
be because the reference level is set differently for categorical
covariates.

We can also use type III hypothesis testing, this time choosing a
Chi-squared test statistic:

``` r
Anova(fit, type = "III", test.statistic = "Chisq")
#> Analysis of Fixed Effect Table (Type III Chisq tests)
#>              Df    Chisq Pr(>Chisq)    
#> RACE          2  73.8229  < 2.2e-16 ***
#> SEX           1   0.3757     0.5399    
#> ARMCD         1  31.6631  1.834e-08 ***
#> AVISIT        3 426.3367  < 2.2e-16 ***
#> ARMCD:AVISIT  3   0.7742     0.8556    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Tidymodels

### Tidymodels

`mmrm` is compatible to work in a `tidymodels` workflow. The following
is an example of how such a workflow would be constructed.

``` r
library(tidymodels)
```

#### Direct fit

First we define the direct method to fit an `mmrm` model using the
`parsnip` package functions `linear_reg()` and `set_engine()`.

- `linear_reg()` defines a model that can predict numeric values from
  predictors using a linear function
- `set_engine()` is used to specify which package or system will be used
  to fit the model, along with any arguments specific to that software.
  We can set the method to adjust degrees of freedom directly in the
  call.

``` r
model <- linear_reg() |>
  set_engine("mmrm", method = "Satterthwaite") |>
  fit(FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID), fev_data)
model
#> parsnip model object
#> 
#> mmrm fit
#> 
#> Formula:     FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID)
#> Data:        data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Weights:     weights
#> Covariance:  unstructured (10 variance parameters)
#> Inference:   REML
#> Deviance:    3387.373
#> 
#> Coefficients: 
#>                   (Intercept) RACEBlack or African American 
#>                   30.96769899                    1.50464863 
#>                     RACEWhite                      ARMCDTRT 
#>                    5.61309565                    3.77555734 
#>                    AVISITVIS2                    AVISITVIS3 
#>                    4.82858803                   10.33317002 
#>                    AVISITVIS4           ARMCDTRT:AVISITVIS2 
#>                   15.05255715                   -0.01737409 
#>           ARMCDTRT:AVISITVIS3           ARMCDTRT:AVISITVIS4 
#>                   -0.66753189                    0.63094392 
#> 
#> Model Inference Optimization:
#> Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch
```

We can also pass in the full `mmrm_control` object into the
`set_engine()` call:

``` r
model_with_control <- linear_reg() |>
  set_engine("mmrm", control = mmrm_control(method = "Satterthwaite")) |>
  fit(FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID), fev_data)
```

#### Predictions

Lastly, we can also obtain predictions with the
[`predict()`](https://rdrr.io/r/stats/predict.html) method:

``` r
predict(model, new_data = fev_data)
#> # A tibble: 800 × 1
#>    .pred
#>    <dbl>
#>  1  36.2
#>  2  41.1
#>  3  45.9
#>  4  51.9
#>  5  31.0
#>  6  35.8
#>  7  41.3
#>  8  46.0
#>  9  32.5
#> 10  37.3
#> # ℹ 790 more rows
```

Note that we need to explicitly pass `new_data` because the method
definition does not allow to default it to the data set we used for the
model fitting automatically.

By using the `type = "numeric"` default of
[`predict()`](https://rdrr.io/r/stats/predict.html) as above we cannot
further customize the calculations. We obtain predicted values without
confidence intervals or standard errors.

On the other hand, when using `type = "raw"` we can customize the
calculations via the `opts` list:

``` r
predict(
  model,
  new_data = fev_data,
  type = "raw",
  opts = list(se.fit = TRUE, interval = "prediction", nsim = 10L)
)
#>          fit        se      lwr      upr
#> 1   32.47877  5.912741 20.89001 44.06753
#> 2   39.97105  0.000000 39.97105 39.97105
#> 3   45.70508  4.035906 37.79485 53.61531
#> 4   20.48379  0.000000 20.48379 20.48379
#> 5   28.01243  5.596962 17.04258 38.98227
#> 6   31.45522  0.000000 31.45522 31.45522
#> 7   36.87889  0.000000 36.87889 36.87889
#> 8   48.80809  0.000000 48.80809 48.80809
#> 9   30.73774  5.707393 19.55145 41.92402
#> 10  35.98699  0.000000 35.98699 35.98699
#> 11  42.64153  3.880319 35.03624 50.24681
#> 12  37.16444  0.000000 37.16444 37.16444
#> 13  33.89229  0.000000 33.89229 33.89229
#> 14  33.74637  0.000000 33.74637 33.74637
#> 15  44.04155  3.809048 36.57595 51.50715
#> 16  54.45055  0.000000 54.45055 54.45055
#> 17  32.31386  0.000000 32.31386 32.31386
#> 18  37.31982  4.727282 28.05451 46.58512
#> 19  46.79361  0.000000 46.79361 46.79361
#> 20  41.71154  0.000000 41.71154 41.71154
#> 21  31.17198  6.125535 19.16615 43.17781
#> 22  36.63341  5.156900 26.52607 46.74075
#> 23  39.02423  0.000000 39.02423 39.02423
#> 24  47.26333  9.838596 27.98004 66.54663
#> 25  31.93050  0.000000 31.93050 31.93050
#> 26  32.90947  0.000000 32.90947 32.90947
#> 27  41.27523  3.770645 33.88490 48.66556
#> 28  48.28031  0.000000 48.28031 48.28031
#> 29  32.23021  0.000000 32.23021 32.23021
#> 30  35.91080  0.000000 35.91080 35.91080
#> 31  45.54898  0.000000 45.54898 45.54898
#> 32  53.02877  0.000000 53.02877 53.02877
#> 33  47.16898  0.000000 47.16898 47.16898
#> 34  46.64287  0.000000 46.64287 46.64287
#> 35  50.84665  3.791595 43.41526 58.27804
#> 36  58.09713  0.000000 58.09713 58.09713
#> 37  33.21881  6.101077 21.26092 45.17670
#> 38  37.68412  5.136786 27.61621 47.75204
#> 39  44.97613  0.000000 44.97613 44.97613
#> 40  47.67506  9.829325 28.40993 66.94018
#> 41  44.32755  0.000000 44.32755 44.32755
#> 42  38.97813  0.000000 38.97813 38.97813
#> 43  43.72862  0.000000 43.72862 43.72862
#> 44  46.43393  0.000000 46.43393 46.43393
#> 45  40.34576  0.000000 40.34576 40.34576
#> 46  42.76568  0.000000 42.76568 42.76568
#> 47  40.11155  0.000000 40.11155 40.11155
#> 48  49.71974  9.688168 30.73129 68.70820
#> 49  41.46341  5.999508 29.70459 53.22223
#> 50  45.73510  5.097137 35.74490 55.72531
#> 51  53.31791  0.000000 53.31791 53.31791
#> 52  56.07641  0.000000 56.07641 56.07641
#> 53  32.16382  6.091635 20.22444 44.10321
#> 54  37.14256  5.127849 27.09216 47.19296
#> 55  41.90837  0.000000 41.90837 41.90837
#> 56  47.46284  9.811555 28.23255 66.69314
#> 57  27.78883  6.120827 15.79223 39.78543
#> 58  34.13887  5.181902 23.98253 44.29521
#> 59  34.65663  0.000000 34.65663 34.65663
#> 60  39.07791  0.000000 39.07791 39.07791
#> 61  31.18775  5.686639 20.04214 42.33336
#> 62  35.89612  0.000000 35.89612 35.89612
#> 63  41.31608  3.862475 33.74577 48.88639
#> 64  47.67264  0.000000 47.67264 47.67264
#> 65  22.65440  0.000000 22.65440 22.65440
#> 66  36.35488  4.868379 26.81303 45.89672
#> 67  45.20175  3.892415 37.57275 52.83074
#> 68  40.85376  0.000000 40.85376 40.85376
#> 69  32.60048  0.000000 32.60048 32.60048
#> 70  33.64329  0.000000 33.64329 33.64329
#> 71  44.00451  3.809385 36.53825 51.47076
#> 72  40.92278  0.000000 40.92278 40.92278
#> 73  32.14831  0.000000 32.14831 32.14831
#> 74  46.43604  0.000000 46.43604 46.43604
#> 75  41.34973  0.000000 41.34973 41.34973
#> 76  66.30382  0.000000 66.30382 66.30382
#> 77  42.79902  5.658634 31.70830 53.88974
#> 78  47.95358  0.000000 47.95358 47.95358
#> 79  53.97364  0.000000 53.97364 53.97364
#> 80  56.89204  9.742736 37.79663 75.98746
#> 81  46.35384  5.685601 35.21027 57.49741
#> 82  56.64544  0.000000 56.64544 56.64544
#> 83  49.70872  0.000000 49.70872 49.70872
#> 84  60.40497  0.000000 60.40497 60.40497
#> 85  45.98525  0.000000 45.98525 45.98525
#> 86  51.90911  0.000000 51.90911 51.90911
#> 87  41.50787  0.000000 41.50787 41.50787
#> 88  53.42727  0.000000 53.42727 53.42727
#> 89  23.86859  0.000000 23.86859 23.86859
#> 90  35.98563  0.000000 35.98563 35.98563
#> 91  43.60626  0.000000 43.60626 43.60626
#> 92  44.77520  9.654905 25.85193 63.69846
#> 93  29.59773  0.000000 29.59773 29.59773
#> 94  35.50688  0.000000 35.50688 35.50688
#> 95  55.42944  0.000000 55.42944 55.42944
#> 96  52.10530  0.000000 52.10530 52.10530
#> 97  31.69644  0.000000 31.69644 31.69644
#> 98  32.16159  0.000000 32.16159 32.16159
#> 99  51.04735  0.000000 51.04735 51.04735
#> 100 55.85987  0.000000 55.85987 55.85987
#> 101 49.11706  0.000000 49.11706 49.11706
#> 102 49.25544  0.000000 49.25544 49.25544
#> 103 51.72211  0.000000 51.72211 51.72211
#> 104 69.99128  0.000000 69.99128 69.99128
#> 105 22.07169  0.000000 22.07169 22.07169
#> 106 36.35845  4.913836 26.72751 45.98939
#> 107 46.08393  0.000000 46.08393 46.08393
#> 108 52.42288  0.000000 52.42288 52.42288
#> 109 37.69466  0.000000 37.69466 37.69466
#> 110 44.59400  0.000000 44.59400 44.59400
#> 111 52.08897  0.000000 52.08897 52.08897
#> 112 58.22961  0.000000 58.22961 58.22961
#> 113 37.22824  0.000000 37.22824 37.22824
#> 114 34.39863  0.000000 34.39863 34.39863
#> 115 45.88949  3.859316 38.32537 53.45361
#> 116 36.34012  0.000000 36.34012 36.34012
#> 117 45.44182  0.000000 45.44182 45.44182
#> 118 41.54847  0.000000 41.54847 41.54847
#> 119 43.92172  0.000000 43.92172 43.92172
#> 120 61.83243  0.000000 61.83243 61.83243
#> 121 27.25656  0.000000 27.25656 27.25656
#> 122 34.77803  4.791670 25.38653 44.16953
#> 123 45.65133  0.000000 45.65133 45.65133
#> 124 44.56078  9.719823 25.51027 63.61128
#> 125 33.19334  0.000000 33.19334 33.19334
#> 126 39.72671  4.749336 30.41818 49.03524
#> 127 45.59637  3.822090 38.10521 53.08753
#> 128 41.66826  0.000000 41.66826 41.66826
#> 129 27.12753  0.000000 27.12753 27.12753
#> 130 31.74858  0.000000 31.74858 31.74858
#> 131 44.57711  3.899603 36.93403 52.22019
#> 132 41.60000  0.000000 41.60000 41.60000
#> 133 39.45250  0.000000 39.45250 39.45250
#> 134 32.61823  0.000000 32.61823 32.61823
#> 135 34.62445  0.000000 34.62445 34.62445
#> 136 45.90515  0.000000 45.90515 45.90515
#> 137 36.17780  0.000000 36.17780 36.17780
#> 138 39.79796  0.000000 39.79796 39.79796
#> 139 45.87019  3.774249 38.47280 53.26759
#> 140 50.08272  0.000000 50.08272 50.08272
#> 141 36.27753  5.769857 24.96882 47.58624
#> 142 44.64316  0.000000 44.64316 44.64316
#> 143 44.88252  3.916974 37.20539 52.55964
#> 144 39.73529  0.000000 39.73529 39.73529
#> 145 34.06164  0.000000 34.06164 34.06164
#> 146 40.18592  0.000000 40.18592 40.18592
#> 147 41.17584  0.000000 41.17584 41.17584
#> 148 57.76669  0.000000 57.76669 57.76669
#> 149 38.18460  0.000000 38.18460 38.18460
#> 150 38.61735  4.784591 29.23973 47.99498
#> 151 47.19893  0.000000 47.19893 47.19893
#> 152 48.18237  9.724165 29.12335 67.24138
#> 153 37.32785  0.000000 37.32785 37.32785
#> 154 37.89476  4.733260 28.61774 47.17178
#> 155 43.16048  0.000000 43.16048 43.16048
#> 156 41.40349  0.000000 41.40349 41.40349
#> 157 30.15733  0.000000 30.15733 30.15733
#> 158 35.84353  0.000000 35.84353 35.84353
#> 159 40.95250  0.000000 40.95250 40.95250
#> 160 46.76086  9.566529 28.01080 65.51091
#> 161 36.09804  5.654062 25.01628 47.17980
#> 162 41.37928  0.000000 41.37928 41.37928
#> 163 50.17316  0.000000 50.17316 50.17316
#> 164 45.35226  0.000000 45.35226 45.35226
#> 165 39.06491  0.000000 39.06491 39.06491
#> 166 39.39597  4.772705 30.04164 48.75030
#> 167 43.69372  3.822999 36.20078 51.18667
#> 168 42.11960  0.000000 42.11960 42.11960
#> 169 29.81042  0.000000 29.81042 29.81042
#> 170 42.57055  0.000000 42.57055 42.57055
#> 171 47.81652  0.000000 47.81652 47.81652
#> 172 68.06024  0.000000 68.06024 68.06024
#> 173 35.62071  0.000000 35.62071 35.62071
#> 174 40.83933  4.755961 31.51782 50.16085
#> 175 45.83438  3.820987 38.34538 53.32338
#> 176 51.72310  9.660640 32.78859 70.65760
#> 177 33.89134  0.000000 33.89134 33.89134
#> 178 36.42808  0.000000 36.42808 36.42808
#> 179 37.57519  0.000000 37.57519 37.57519
#> 180 58.46873  0.000000 58.46873 58.46873
#> 181 19.54516  0.000000 19.54516 19.54516
#> 182 31.13541  0.000000 31.13541 31.13541
#> 183 40.89955  0.000000 40.89955 40.89955
#> 184 42.08852  9.685702 23.10490 61.07215
#> 185 22.18809  0.000000 22.18809 22.18809
#> 186 41.05857  0.000000 41.05857 41.05857
#> 187 37.32452  0.000000 37.32452 37.32452
#> 188 47.28633  9.744787 28.18689 66.38576
#> 189 35.28933  5.649814 24.21590 46.36277
#> 190 43.12432  0.000000 43.12432 43.12432
#> 191 41.99349  0.000000 41.99349 41.99349
#> 192 49.12250  9.742215 30.02811 68.21689
#> 193 44.03080  0.000000 44.03080 44.03080
#> 194 38.66417  0.000000 38.66417 38.66417
#> 195 53.45993  0.000000 53.45993 53.45993
#> 196 53.14026  9.701180 34.12630 72.15423
#> 197 29.81948  0.000000 29.81948 29.81948
#> 198 30.43859  0.000000 30.43859 30.43859
#> 199 40.18095  0.000000 40.18095 40.18095
#> 200 48.07272  9.683770 29.09288 67.05256
#> 201 26.78578  0.000000 26.78578 26.78578
#> 202 34.55115  0.000000 34.55115 34.55115
#> 203 44.66449  3.862911 37.09332 52.23566
#> 204 40.06421  0.000000 40.06421 40.06421
#> 205 36.66313  5.713540 25.46480 47.86147
#> 206 43.09329  0.000000 43.09329 43.09329
#> 207 46.09670  3.885440 38.48138 53.71202
#> 208 45.71567  0.000000 45.71567 45.71567
#> 209 40.74992  0.000000 40.74992 40.74992
#> 210 44.74635  0.000000 44.74635 44.74635
#> 211 47.51420  3.806246 40.05410 54.97431
#> 212 53.24620  9.634106 34.36369 72.12870
#> 213 40.35635  6.336839 27.93638 52.77633
#> 214 45.16757  5.233093 34.91089 55.42424
#> 215 50.02199  3.964035 42.25262 57.79136
#> 216 56.03985  9.851431 36.73140 75.34830
#> 217 40.14674  0.000000 40.14674 40.14674
#> 218 48.75859  0.000000 48.75859 48.75859
#> 219 46.43462  0.000000 46.43462 46.43462
#> 220 53.05319  9.681879 34.07705 72.02932
#> 221 29.33990  0.000000 29.33990 29.33990
#> 222 36.24424  4.736448 26.96098 45.52751
#> 223 42.39946  3.797689 34.95613 49.84280
#> 224 47.93165  0.000000 47.93165 47.93165
#> 225 36.59443  5.536196 25.74369 47.44518
#> 226 41.11632  0.000000 41.11632 41.11632
#> 227 47.05889  0.000000 47.05889 47.05889
#> 228 52.24599  0.000000 52.24599 52.24599
#> 229 45.12054  5.722896 33.90387 56.33721
#> 230 54.14236  0.000000 54.14236 54.14236
#> 231 50.44618  0.000000 50.44618 50.44618
#> 232 58.53593  9.812513 39.30376 77.76811
#> 233 37.53657  0.000000 37.53657 37.53657
#> 234 44.28282  4.705849 35.05952 53.50611
#> 235 49.45840  0.000000 49.45840 49.45840
#> 236 59.12866  0.000000 59.12866 59.12866
#> 237 40.31268  0.000000 40.31268 40.31268
#> 238 39.66049  0.000000 39.66049 39.66049
#> 239 50.89726  0.000000 50.89726 50.89726
#> 240 56.13116  0.000000 56.13116 56.13116
#> 241 32.82981  0.000000 32.82981 32.82981
#> 242 46.53837  0.000000 46.53837 46.53837
#> 243 44.46016  3.813273 36.98629 51.93404
#> 244 51.81265  0.000000 51.81265 51.81265
#> 245 27.76886  5.798215 16.40457 39.13316
#> 246 29.91939  0.000000 29.91939 29.91939
#> 247 40.70944  3.919446 33.02746 48.39141
#> 248 44.37942  9.772369 25.22592 63.53291
#> 249 43.47695  5.657111 32.38922 54.56469
#> 250 51.05656  0.000000 51.05656 51.05656
#> 251 50.50059  0.000000 50.50059 50.50059
#> 252 64.11388  0.000000 64.11388 64.11388
#> 253 32.21843  0.000000 32.21843 32.21843
#> 254 29.64732  0.000000 29.64732 29.64732
#> 255 42.48707  3.825593 34.98904 49.98509
#> 256 45.09919  0.000000 45.09919 45.09919
#> 257 39.75659  0.000000 39.75659 39.75659
#> 258 37.28894  0.000000 37.28894 37.28894
#> 259 44.80145  0.000000 44.80145 44.80145
#> 260 65.95920  0.000000 65.95920 65.95920
#> 261 33.43439  0.000000 33.43439 33.43439
#> 262 33.57042  0.000000 33.57042 33.57042
#> 263 39.91543  0.000000 39.91543 39.91543
#> 264 49.57098  0.000000 49.57098 49.57098
#> 265 38.91634  0.000000 38.91634 38.91634
#> 266 36.69011  0.000000 36.69011 36.69011
#> 267 45.66665  0.000000 45.66665 45.66665
#> 268 52.07431  0.000000 52.07431 52.07431
#> 269 42.21411  0.000000 42.21411 42.21411
#> 270 45.02901  0.000000 45.02901 45.02901
#> 271 50.22530  3.787549 42.80184 57.64877
#> 272 56.56084  9.613563 37.71861 75.40308
#> 273 30.98338  0.000000 30.98338 30.98338
#> 274 44.72932  0.000000 44.72932 44.72932
#> 275 40.68711  0.000000 40.68711 40.68711
#> 276 34.71530  0.000000 34.71530 34.71530
#> 277 27.30752  0.000000 27.30752 27.30752
#> 278 37.31585  0.000000 37.31585 37.31585
#> 279 42.23592  3.774888 34.83727 49.63456
#> 280 44.83000  0.000000 44.83000 44.83000
#> 281 32.93042  0.000000 32.93042 32.93042
#> 282 44.91911  0.000000 44.91911 44.91911
#> 283 45.68636  0.000000 45.68636 45.68636
#> 284 65.98800  0.000000 65.98800 65.98800
#> 285 46.60130  0.000000 46.60130 46.60130
#> 286 40.89786  0.000000 40.89786 40.89786
#> 287 46.66708  0.000000 46.66708 46.66708
#> 288 53.97575  9.695349 34.97322 72.97829
#> 289 34.51394  6.017235 22.72038 46.30751
#> 290 40.31956  5.104486 30.31495 50.32417
#> 291 43.83270  0.000000 43.83270 43.83270
#> 292 44.11604  0.000000 44.11604 44.11604
#> 293 38.29612  0.000000 38.29612 38.29612
#> 294 42.38074  4.737909 33.09461 51.66687
#> 295 51.38570  0.000000 51.38570 51.38570
#> 296 56.20979  0.000000 56.20979 56.20979
#> 297 35.93809  5.662820 24.83917 47.03701
#> 298 43.45819  0.000000 43.45819 43.45819
#> 299 38.38741  0.000000 38.38741 38.38741
#> 300 56.42818  0.000000 56.42818 56.42818
#> 301 39.05050  0.000000 39.05050 39.05050
#> 302 44.66707  4.734130 35.38834 53.94579
#> 303 49.86836  3.798310 42.42381 57.31291
#> 304 54.09200  0.000000 54.09200 54.09200
#> 305 31.40521  0.000000 31.40521 31.40521
#> 306 46.13330  0.000000 46.13330 46.13330
#> 307 45.29845  0.000000 45.29845 45.29845
#> 308 28.06936  0.000000 28.06936 28.06936
#> 309 39.05960  5.575819 28.13119 49.98800
#> 310 42.50283  0.000000 42.50283 42.50283
#> 311 46.45368  0.000000 46.45368 46.45368
#> 312 64.97366  0.000000 64.97366 64.97366
#> 313 30.67876  6.209504 18.50835 42.84916
#> 314 35.63991  5.168247 25.51033 45.76949
#> 315 41.27878  3.939781 33.55695 49.00061
#> 316 43.97847  0.000000 43.97847 43.97847
#> 317 35.33466  0.000000 35.33466 35.33466
#> 318 39.34378  0.000000 39.34378 39.34378
#> 319 41.27633  0.000000 41.27633 41.27633
#> 320 50.74572  9.593589 31.94263 69.54881
#> 321 34.18429  5.567728 23.27174 45.09684
#> 322 39.83058  0.000000 39.83058 39.83058
#> 323 43.49673  0.000000 43.49673 43.49673
#> 324 44.06114  0.000000 44.06114 44.06114
#> 325 41.43742  0.000000 41.43742 41.43742
#> 326 40.68384  4.752301 31.36950 49.99818
#> 327 46.16954  0.000000 46.16954 46.16954
#> 328 54.24024  0.000000 54.24024 54.24024
#> 329 36.61831  0.000000 36.61831 36.61831
#> 330 42.09272  0.000000 42.09272 42.09272
#> 331 50.69556  0.000000 50.69556 50.69556
#> 332 51.72563  0.000000 51.72563 51.72563
#> 333 32.08271  6.224939 19.88206 44.28337
#> 334 36.39974  5.176989 26.25303 46.54645
#> 335 41.38610  3.949088 33.64603 49.12617
#> 336 53.89947  0.000000 53.89947 53.89947
#> 337 31.94884  5.992019 20.20470 43.69298
#> 338 36.34138  5.082315 26.38023 46.30253
#> 339 39.94420  0.000000 39.94420 39.94420
#> 340 56.42482  0.000000 56.42482 56.42482
#> 341 41.86385  0.000000 41.86385 41.86385
#> 342 34.56420  0.000000 34.56420 34.56420
#> 343 38.68927  0.000000 38.68927 38.68927
#> 344 62.88743  0.000000 62.88743 62.88743
#> 345 28.85343  0.000000 28.85343 28.85343
#> 346 38.89808  4.816314 29.45828 48.33789
#> 347 49.29495  0.000000 49.29495 49.29495
#> 348 48.90884  9.717830 29.86224 67.95543
#> 349 28.74029  0.000000 28.74029 28.74029
#> 350 36.38836  4.731296 27.11519 45.66153
#> 351 43.59994  0.000000 43.59994 43.59994
#> 352 57.38616  0.000000 57.38616 57.38616
#> 353 35.36824  0.000000 35.36824 35.36824
#> 354 43.06110  0.000000 43.06110 43.06110
#> 355 31.27551  0.000000 31.27551 31.27551
#> 356 54.13245  0.000000 54.13245 54.13245
#> 357 25.97050  0.000000 25.97050 25.97050
#> 358 34.04514  4.771834 24.69252 43.39777
#> 359 40.67015  3.824350 33.17456 48.16574
#> 360 44.36054  9.679443 25.38918 63.33190
#> 361 39.69391  5.986137 27.96130 51.42653
#> 362 44.79720  5.080982 34.83866 54.75574
#> 363 51.17493  0.000000 51.17493 51.17493
#> 364 48.44043  0.000000 48.44043 48.44043
#> 365 43.33128  0.000000 43.33128 43.33128
#> 366 46.45918  4.756622 37.13638 55.78199
#> 367 55.93546  0.000000 55.93546 55.93546
#> 368 54.15312  0.000000 54.15312 54.15312
#> 369 33.79699  5.568759 22.88242 44.71155
#> 370 40.60252  0.000000 40.60252 40.60252
#> 371 44.44715  0.000000 44.44715 44.44715
#> 372 40.54161  0.000000 40.54161 40.54161
#> 373 33.95563  0.000000 33.95563 33.95563
#> 374 36.84083  4.708234 27.61286 46.06879
#> 375 43.67802  0.000000 43.67802 43.67802
#> 376 42.76023  0.000000 42.76023 42.76023
#> 377 34.18486  5.670318 23.07124 45.29848
#> 378 42.82678  0.000000 42.82678 42.82678
#> 379 39.59218  0.000000 39.59218 39.59218
#> 380 47.93427  9.763892 28.79739 67.07114
#> 381 33.49216  0.000000 33.49216 33.49216
#> 382 35.39266  0.000000 35.39266 35.39266
#> 383 42.88943  3.767668 35.50494 50.27393
#> 384 42.36266  0.000000 42.36266 42.36266
#> 385 48.54368  0.000000 48.54368 48.54368
#> 386 43.94366  0.000000 43.94366 43.94366
#> 387 50.98218  3.843404 43.44925 58.51512
#> 388 47.91204  0.000000 47.91204 47.91204
#> 389 20.72928  0.000000 20.72928 20.72928
#> 390 28.00599  0.000000 28.00599 28.00599
#> 391 40.19255  0.000000 40.19255 40.19255
#> 392 37.79360  0.000000 37.79360 37.79360
#> 393 32.17343  5.756572 20.89076 43.45611
#> 394 36.75177  0.000000 36.75177 36.75177
#> 395 42.75025  3.876654 35.15215 50.34835
#> 396 47.37158  9.724823 28.31127 66.43188
#> 397 34.59822  0.000000 34.59822 34.59822
#> 398 39.32034  0.000000 39.32034 39.32034
#> 399 40.65702  0.000000 40.65702 40.65702
#> 400 48.52002  9.581534 29.74056 67.29948
#> 401 40.41786  5.654539 29.33516 51.50055
#> 402 43.03255  0.000000 43.03255 43.03255
#> 403 54.65715  0.000000 54.65715 54.65715
#> 404 55.54195  9.746507 36.43914 74.64475
#> 405 35.55742  0.000000 35.55742 35.55742
#> 406 43.70215  0.000000 43.70215 43.70215
#> 407 42.52157  0.000000 42.52157 42.52157
#> 408 54.89337  0.000000 54.89337 54.89337
#> 409 32.03460  0.000000 32.03460 32.03460
#> 410 29.45107  0.000000 29.45107 29.45107
#> 411 45.35138  0.000000 45.35138 45.35138
#> 412 45.33026  9.655106 26.40660 64.25392
#> 413 38.73784  0.000000 38.73784 38.73784
#> 414 39.30063  4.744771 30.00104 48.60021
#> 415 41.42283  0.000000 41.42283 41.42283
#> 416 47.32385  0.000000 47.32385 47.32385
#> 417 40.41284  5.693716 29.25336 51.57232
#> 418 47.55310  0.000000 47.55310 47.55310
#> 419 49.06509  0.000000 49.06509 49.06509
#> 420 53.79511  9.787574 34.61182 72.97840
#> 421 29.22591  0.000000 29.22591 29.22591
#> 422 40.08175  0.000000 40.08175 40.08175
#> 423 45.68142  0.000000 45.68142 45.68142
#> 424 41.47403  0.000000 41.47403 41.47403
#> 425 37.55612  6.054508 25.68950 49.42274
#> 426 41.98123  5.126912 31.93266 52.02979
#> 427 42.51970  0.000000 42.51970 42.51970
#> 428 69.36099  0.000000 69.36099 69.36099
#> 429 42.39760  0.000000 42.39760 42.39760
#> 430 43.72376  0.000000 43.72376 43.72376
#> 431 49.47601  0.000000 49.47601 49.47601
#> 432 51.94188  0.000000 51.94188 51.94188
#> 433 31.77722  5.649803 20.70381 42.85063
#> 434 40.59100  0.000000 40.59100 40.59100
#> 435 39.97833  0.000000 39.97833 39.97833
#> 436 31.69049  0.000000 31.69049 31.69049
#> 437 33.99809  5.621072 22.98099 45.01519
#> 438 37.20517  0.000000 37.20517 37.20517
#> 439 46.28740  0.000000 46.28740 46.28740
#> 440 49.81365  9.707624 30.78706 68.84024
#> 441 35.15913  6.237532 22.93379 47.38447
#> 442 40.63997  5.195156 30.45765 50.82229
#> 443 46.80529  3.974044 39.01631 54.59428
#> 444 41.58720  0.000000 41.58720 41.58720
#> 445 32.17365  0.000000 32.17365 32.17365
#> 446 37.07479  4.723605 27.81670 46.33289
#> 447 40.69375  0.000000 40.69375 40.69375
#> 448 47.52336  9.619916 28.66867 66.37805
#> 449 32.28771  0.000000 32.28771 32.28771
#> 450 41.76205  0.000000 41.76205 41.76205
#> 451 40.06768  0.000000 40.06768 40.06768
#> 452 47.21582  9.604447 28.39145 66.04019
#> 453 29.14213  0.000000 29.14213 29.14213
#> 454 39.50989  0.000000 39.50989 39.50989
#> 455 43.32349  0.000000 43.32349 43.32349
#> 456 47.16756  0.000000 47.16756 47.16756
#> 457 40.93020  0.000000 40.93020 40.93020
#> 458 42.19406  0.000000 42.19406 42.19406
#> 459 41.21057  0.000000 41.21057 41.21057
#> 460 49.76205  9.678268 30.79300 68.73111
#> 461 38.54330  0.000000 38.54330 38.54330
#> 462 41.44104  4.721838 32.18640 50.69567
#> 463 43.96324  0.000000 43.96324 43.96324
#> 464 42.67652  0.000000 42.67652 42.67652
#> 465 22.79584  0.000000 22.79584 22.79584
#> 466 33.91004  4.819802 24.46340 43.35668
#> 467 41.58421  3.868905 34.00129 49.16712
#> 468 44.31106  9.734993 25.23082 63.39130
#> 469 31.43559  0.000000 31.43559 31.43559
#> 470 38.85064  0.000000 38.85064 38.85064
#> 471 48.24288  0.000000 48.24288 48.24288
#> 472 46.15940  9.733764 27.08157 65.23723
#> 473 44.71302  0.000000 44.71302 44.71302
#> 474 51.85370  0.000000 51.85370 51.85370
#> 475 50.77548  3.841975 43.24535 58.30562
#> 476 58.11432  9.678110 39.14557 77.08307
#> 477 30.56757  0.000000 30.56757 30.56757
#> 478 35.65607  4.753774 26.33885 44.97330
#> 479 41.25037  3.807173 33.78844 48.71229
#> 480 45.88736  9.660673 26.95279 64.82193
#> 481 37.37624  6.222732 25.17990 49.57257
#> 482 41.66978  5.182030 31.51319 51.82637
#> 483 45.99979  3.963307 38.23185 53.76773
#> 484 59.90473  0.000000 59.90473 59.90473
#> 485 33.87728  6.266356 21.59544 46.15911
#> 486 37.28988  5.289179 26.92328 47.65648
#> 487 49.76150  0.000000 49.76150 49.76150
#> 488 46.60552 10.011086 26.98415 66.22689
#> 489 47.21985  0.000000 47.21985 47.21985
#> 490 40.34525  0.000000 40.34525 40.34525
#> 491 48.29793  0.000000 48.29793 48.29793
#> 492 54.57153  9.690632 35.57824 73.56482
#> 493 36.13680  5.594379 25.17202 47.10158
#> 494 44.39634  0.000000 44.39634 44.39634
#> 495 41.71421  0.000000 41.71421 41.71421
#> 496 47.37535  0.000000 47.37535 47.37535
#> 497 42.03797  0.000000 42.03797 42.03797
#> 498 37.56100  0.000000 37.56100 37.56100
#> 499 45.11793  0.000000 45.11793 45.11793
#> 500 52.86788  9.663191 33.92837 71.80739
#> 501 34.62530  0.000000 34.62530 34.62530
#> 502 45.28206  0.000000 45.28206 45.28206
#> 503 44.51505  3.813649 37.04043 51.98966
#> 504 63.57761  0.000000 63.57761 63.57761
#> 505 35.80878  0.000000 35.80878 35.80878
#> 506 40.93038  4.731397 31.65702 50.20375
#> 507 45.85156  3.801097 38.40155 53.30157
#> 508 52.67314  0.000000 52.67314 52.67314
#> 509 35.88734  0.000000 35.88734 35.88734
#> 510 38.73222  0.000000 38.73222 38.73222
#> 511 46.70361  0.000000 46.70361 46.70361
#> 512 53.65398  0.000000 53.65398 53.65398
#> 513 36.71543  0.000000 36.71543 36.71543
#> 514 43.89170  4.768765 34.54509 53.23830
#> 515 49.56246  3.822896 42.06972 57.05519
#> 516 54.83060  9.666709 35.88420 73.77700
#> 517 37.85241  5.651515 26.77564 48.92917
#> 518 41.54317  0.000000 41.54317 41.54317
#> 519 51.67909  0.000000 51.67909 51.67909
#> 520 51.76691  9.753753 32.64990 70.88391
#> 521 27.40130  0.000000 27.40130 27.40130
#> 522 30.33517  0.000000 30.33517 30.33517
#> 523 37.73092  0.000000 37.73092 37.73092
#> 524 29.11668  0.000000 29.11668 29.11668
#> 525 30.03596  5.578964 19.10139 40.97053
#> 526 32.08830  0.000000 32.08830 32.08830
#> 527 41.66067  0.000000 41.66067 41.66067
#> 528 53.90815  0.000000 53.90815 53.90815
#> 529 34.02622  5.600234 23.04996 45.00247
#> 530 35.06937  0.000000 35.06937 35.06937
#> 531 47.17615  0.000000 47.17615 47.17615
#> 532 56.49347  0.000000 56.49347 56.49347
#> 533 34.02880  5.579114 23.09394 44.96366
#> 534 38.88006  0.000000 38.88006 38.88006
#> 535 47.54070  0.000000 47.54070 47.54070
#> 536 43.53705  0.000000 43.53705 43.53705
#> 537 31.82054  0.000000 31.82054 31.82054
#> 538 39.62816  0.000000 39.62816 39.62816
#> 539 44.95543  0.000000 44.95543 44.95543
#> 540 21.11543  0.000000 21.11543 21.11543
#> 541 34.74671  0.000000 34.74671 34.74671
#> 542 43.27308  4.761738 33.94024 52.60591
#> 543 49.29538  3.814731 41.81865 56.77212
#> 544 56.69249  0.000000 56.69249 56.69249
#> 545 22.73126  0.000000 22.73126 22.73126
#> 546 32.50075  0.000000 32.50075 32.50075
#> 547 42.37206  0.000000 42.37206 42.37206
#> 548 42.89847  0.000000 42.89847 42.89847
#> 549 55.62582  0.000000 55.62582 55.62582
#> 550 45.38998  0.000000 45.38998 45.38998
#> 551 52.66743  0.000000 52.66743 52.66743
#> 552 56.87348  9.971143 37.33040 76.41657
#> 553 30.66032  6.271714 18.36798 42.95265
#> 554 37.44228  5.313096 27.02880 47.85576
#> 555 34.18931  0.000000 34.18931 34.18931
#> 556 45.59740  0.000000 45.59740 45.59740
#> 557 28.89198  0.000000 28.89198 28.89198
#> 558 38.46147  0.000000 38.46147 38.46147
#> 559 42.42099  3.772239 35.02753 49.81444
#> 560 49.90357  0.000000 49.90357 49.90357
#> 561 39.74586  5.687601 28.59837 50.89335
#> 562 44.14167  0.000000 44.14167 44.14167
#> 563 49.91712  3.872352 42.32745 57.50679
#> 564 55.24278  0.000000 55.24278 55.24278
#> 565 36.24790  6.337597 23.82644 48.66937
#> 566 41.05912  5.232697 30.80322 51.31502
#> 567 45.91354  3.968932 38.13458 53.69251
#> 568 51.93141  9.851064 32.62368 71.23914
#> 569 27.38001  0.000000 27.38001 27.38001
#> 570 33.63251  0.000000 33.63251 33.63251
#> 571 44.70168  3.870596 37.11545 52.28791
#> 572 39.34410  0.000000 39.34410 39.34410
#> 573 26.98575  0.000000 26.98575 26.98575
#> 574 24.04175  0.000000 24.04175 24.04175
#> 575 42.16648  0.000000 42.16648 42.16648
#> 576 44.75380  0.000000 44.75380 44.75380
#> 577 31.55469  0.000000 31.55469 31.55469
#> 578 44.42696  0.000000 44.42696 44.42696
#> 579 44.10343  0.000000 44.10343 44.10343
#> 580 48.06505  9.653418 29.14470 66.98541
#> 581 34.87547  5.563398 23.97141 45.77953
#> 582 37.87445  0.000000 37.87445 37.87445
#> 583 48.31828  0.000000 48.31828 48.31828
#> 584 50.21520  0.000000 50.21520 50.21520
#> 585 41.94615  0.000000 41.94615 41.94615
#> 586 39.62690  0.000000 39.62690 39.62690
#> 587 46.69763  0.000000 46.69763 46.69763
#> 588 49.44653  9.697597 30.43959 68.45347
#> 589 38.01775  5.630528 26.98211 49.05338
#> 590 43.75255  0.000000 43.75255 43.75255
#> 591 47.38873  0.000000 47.38873 47.38873
#> 592 52.70780  9.715725 33.66532 71.75027
#> 593 32.43412  0.000000 32.43412 32.43412
#> 594 43.07163  0.000000 43.07163 43.07163
#> 595 42.99551  0.000000 42.99551 42.99551
#> 596 53.82759  0.000000 53.82759 53.82759
#> 597 39.45747  6.082469 27.53605 51.37889
#> 598 42.93167  5.154112 32.82979 53.03354
#> 599 50.64802  0.000000 50.64802 50.64802
#> 600 63.44051  0.000000 63.44051 63.44051
#> 601 34.48949  0.000000 34.48949 34.48949
#> 602 40.08056  0.000000 40.08056 40.08056
#> 603 41.86656  3.776035 34.46566 49.26745
#> 604 47.46553  0.000000 47.46553 47.46553
#> 605 32.03992  5.735557 20.79844 43.28141
#> 606 37.11697  0.000000 37.11697 37.11697
#> 607 44.12071  3.907079 36.46297 51.77844
#> 608 36.25120  0.000000 36.25120 36.25120
#> 609 29.20171  0.000000 29.20171 29.20171
#> 610 31.53773  0.000000 31.53773 31.53773
#> 611 42.35683  0.000000 42.35683 42.35683
#> 612 64.78352  0.000000 64.78352 64.78352
#> 613 32.72757  0.000000 32.72757 32.72757
#> 614 37.50022  0.000000 37.50022 37.50022
#> 615 42.76167  3.778132 35.35667 50.16667
#> 616 57.03861  0.000000 57.03861 57.03861
#> 617 36.32475  0.000000 36.32475 36.32475
#> 618 40.15241  4.717823 30.90564 49.39917
#> 619 41.46725  0.000000 41.46725 41.46725
#> 620 59.01411  0.000000 59.01411 59.01411
#> 621 30.14970  0.000000 30.14970 30.14970
#> 622 34.91740  0.000000 34.91740 34.91740
#> 623 52.13900  0.000000 52.13900 52.13900
#> 624 58.73839  0.000000 58.73839 58.73839
#> 625 35.83185  0.000000 35.83185 35.83185
#> 626 41.04423  4.735212 31.76338 50.32507
#> 627 45.82688  3.805665 38.36791 53.28584
#> 628 56.41409  0.000000 56.41409 56.41409
#> 629 37.80184  5.546148 26.93159 48.67209
#> 630 43.55593  0.000000 43.55593 43.55593
#> 631 44.26320  0.000000 44.26320 44.26320
#> 632 59.25579  0.000000 59.25579 59.25579
#> 633 28.47314  0.000000 28.47314 28.47314
#> 634 47.47581  0.000000 47.47581 47.47581
#> 635 44.01685  3.876057 36.41992 51.61378
#> 636 49.57489  9.772639 30.42087 68.72891
#> 637 39.38085  5.710671 28.18814 50.57356
#> 638 46.47483  0.000000 46.47483 46.47483
#> 639 51.22677  0.000000 51.22677 51.22677
#> 640 45.82777  0.000000 45.82777 45.82777
#> 641 33.43408  5.760149 22.14439 44.72376
#> 642 39.06783  0.000000 39.06783 39.06783
#> 643 42.98333  3.875913 35.38668 50.57998
#> 644 48.01822  9.731482 28.94487 67.09158
#> 645 29.99542  0.000000 29.99542 29.99542
#> 646 35.69583  4.740593 26.40444 44.98722
#> 647 41.11547  3.806696 33.65449 48.57646
#> 648 54.17796  0.000000 54.17796 54.17796
#> 649 39.32289  5.719220 28.11342 50.53235
#> 650 44.55743  0.000000 44.55743 44.55743
#> 651 47.26282  3.895636 39.62751 54.89812
#> 652 62.59579  0.000000 62.59579 62.59579
#> 653 31.80300  5.537108 20.95047 42.65554
#> 654 35.48396  0.000000 35.48396 35.48396
#> 655 44.07768  0.000000 44.07768 44.07768
#> 656 46.57837  0.000000 46.57837 46.57837
#> 657 47.67979  0.000000 47.67979 47.67979
#> 658 47.73388  4.786444 38.35262 57.11514
#> 659 50.94631  3.846320 43.40766 58.48496
#> 660 58.47218  9.717386 39.42645 77.51790
#> 661 22.15439  0.000000 22.15439 22.15439
#> 662 35.14301  4.881795 25.57487 44.71116
#> 663 42.82000  3.907268 35.16190 50.47811
#> 664 46.24563  9.775317 27.08637 65.40490
#> 665 34.27765  0.000000 34.27765 34.27765
#> 666 36.90059  0.000000 36.90059 36.90059
#> 667 43.05627  3.769928 35.66735 50.44519
#> 668 40.54285  0.000000 40.54285 40.54285
#> 669 29.09494  0.000000 29.09494 29.09494
#> 670 37.21768  0.000000 37.21768 37.21768
#> 671 43.08491  0.000000 43.08491 43.08491
#> 672 46.50100  9.577673 27.72911 65.27290
#> 673 27.12174  0.000000 27.12174 27.12174
#> 674 34.11916  0.000000 34.11916 34.11916
#> 675 45.56320  3.880031 37.95848 53.16793
#> 676 48.00823  9.713551 28.97002 67.04644
#> 677 35.93048  5.605558 24.94379 46.91717
#> 678 40.80230  0.000000 40.80230 40.80230
#> 679 45.89269  0.000000 45.89269 45.89269
#> 680 43.69153  0.000000 43.69153 43.69153
#> 681 28.56569  5.770737 17.25525 39.87613
#> 682 29.22869  0.000000 29.22869 29.22869
#> 683 40.67646  3.939045 32.95608 48.39685
#> 684 55.68362  0.000000 55.68362 55.68362
#> 685 31.90698  0.000000 31.90698 31.90698
#> 686 37.31061  0.000000 37.31061 37.31061
#> 687 40.75546  0.000000 40.75546 40.75546
#> 688 49.50911  9.593235 30.70671 68.31150
#> 689 42.19474  0.000000 42.19474 42.19474
#> 690 44.87228  0.000000 44.87228 44.87228
#> 691 47.55198  0.000000 47.55198 47.55198
#> 692 56.68097  9.596940 37.87131 75.49063
#> 693 50.62894  0.000000 50.62894 50.62894
#> 694 45.47551  0.000000 45.47551 45.47551
#> 695 48.62168  0.000000 48.62168 48.62168
#> 696 56.58212  9.749187 37.47406 75.69018
#> 697 29.66493  0.000000 29.66493 29.66493
#> 698 34.57406  0.000000 34.57406 34.57406
#> 699 42.45295  3.779255 35.04575 49.86015
#> 700 38.11676  0.000000 38.11676 38.11676
#> 701 33.77204  0.000000 33.77204 33.77204
#> 702 34.26148  0.000000 34.26148 34.26148
#> 703 45.29511  3.853063 37.74324 52.84697
#> 704 58.81037  0.000000 58.81037 58.81037
#> 705 31.46668  6.111158 19.48904 43.44433
#> 706 36.78469  5.144500 26.70166 46.86773
#> 707 39.88119  0.000000 39.88119 39.88119
#> 708 47.32261  9.826144 28.06373 66.58150
#> 709 31.62708  0.000000 31.62708 31.62708
#> 710 37.03239  4.729226 27.76328 46.30150
#> 711 42.69162  3.788915 35.26548 50.11775
#> 712 48.22049  0.000000 48.22049 48.22049
#> 713 42.58829  0.000000 42.58829 42.58829
#> 714 45.80410  4.698963 36.59430 55.01390
#> 715 49.33262  0.000000 49.33262 49.33262
#> 716 53.74331  0.000000 53.74331 53.74331
#> 717 29.71857  0.000000 29.71857 29.71857
#> 718 30.45651  0.000000 30.45651 30.45651
#> 719 38.29800  0.000000 38.29800 38.29800
#> 720 45.15328  9.615571 26.30711 63.99946
#> 721 36.81040  0.000000 36.81040 36.81040
#> 722 37.61606  4.733690 28.33820 46.89393
#> 723 42.35045  0.000000 42.35045 42.35045
#> 724 39.39860  0.000000 39.39860 39.39860
#> 725 36.09876  6.016152 24.30732 47.89020
#> 726 40.94066  5.098526 30.94773 50.93358
#> 727 49.73629  0.000000 49.73629 49.73629
#> 728 41.58082  0.000000 41.58082 41.58082
#> 729 43.58901  0.000000 43.58901 43.58901
#> 730 40.16762  0.000000 40.16762 40.16762
#> 731 46.70338  3.815795 39.22456 54.18221
#> 732 53.94830  9.667795 34.99977 72.89683
#> 733 39.60913  5.741144 28.35669 50.86156
#> 734 41.08206  0.000000 41.08206 41.08206
#> 735 49.65683  3.914188 41.98517 57.32850
#> 736 69.37409  0.000000 69.37409 69.37409
#> 737 34.12096  5.582665 23.17914 45.06279
#> 738 41.27625  0.000000 41.27625 41.27625
#> 739 44.76138  0.000000 44.76138 44.76138
#> 740 39.69815  0.000000 39.69815 39.69815
#> 741 38.44296  0.000000 38.44296 38.44296
#> 742 48.20586  0.000000 48.20586 48.20586
#> 743 47.54082  3.905595 39.88600 55.19564
#> 744 35.50735  0.000000 35.50735 35.50735
#> 745 32.08153  0.000000 32.08153 32.08153
#> 746 37.16398  4.752728 27.84881 46.47916
#> 747 42.75619  3.807270 35.29408 50.21830
#> 748 47.39510  9.660100 28.46165 66.32855
#> 749 44.69256  0.000000 44.69256 44.69256
#> 750 41.45664  4.864206 31.92298 50.99031
#> 751 42.18689  0.000000 42.18689 42.18689
#> 752 51.68534  9.765987 32.54436 70.82632
#> 753 37.01741  0.000000 37.01741 37.01741
#> 754 38.26920  0.000000 38.26920 38.26920
#> 755 49.28806  0.000000 49.28806 49.28806
#> 756 50.67485  9.622862 31.81439 69.53532
#> 757 40.45953  0.000000 40.45953 40.45953
#> 758 45.10337  0.000000 45.10337 45.10337
#> 759 45.58250  0.000000 45.58250 45.58250
#> 760 62.96989  0.000000 62.96989 62.96989
#> 761 30.78252  0.000000 30.78252 30.78252
#> 762 41.58139  4.811277 32.15146 51.01132
#> 763 48.87398  3.849305 41.32948 56.41848
#> 764 44.69667  0.000000 44.69667 44.69667
#> 765 32.72491  0.000000 32.72491 32.72491
#> 766 45.78702  0.000000 45.78702 45.78702
#> 767 48.74886  0.000000 48.74886 48.74886
#> 768 84.08449  0.000000 84.08449 84.08449
#> 769 28.60809  5.692304 17.45138 39.76480
#> 770 30.19495  0.000000 30.19495 30.19495
#> 771 36.78573  0.000000 36.78573 36.78573
#> 772 61.03588  0.000000 61.03588 61.03588
#> 773 20.36749  0.000000 20.36749 20.36749
#> 774 35.22480  0.000000 35.22480 35.22480
#> 775 37.42847  0.000000 37.42847 37.42847
#> 776 30.20501  0.000000 30.20501 30.20501
#> 777 41.72819  5.646700 30.66087 52.79552
#> 778 49.12862  0.000000 49.12862 49.12862
#> 779 47.31234  0.000000 47.31234 47.31234
#> 780 57.08286  9.739411 37.99396 76.17175
#> 781 19.28388  0.000000 19.28388 19.28388
#> 782 30.00682  0.000000 30.00682 30.00682
#> 783 39.69711  3.895230 32.06260 47.33162
#> 784 49.21768  0.000000 49.21768 49.21768
#> 785 31.42637  6.221006 19.23342 43.61932
#> 786 36.73485  5.172897 26.59615 46.87354
#> 787 42.72556  3.946954 34.98967 50.46145
#> 788 40.13353  0.000000 40.13353 40.13353
#> 789 42.34534  0.000000 42.34534 42.34534
#> 790 52.32575  0.000000 52.32575 52.32575
#> 791 46.92223  3.895248 39.28769 54.55678
#> 792 69.26254  0.000000 69.26254 69.26254
#> 793 40.35635  6.336839 27.93638 52.77633
#> 794 45.16757  5.233093 34.91089 55.42424
#> 795 50.02199  3.964035 42.25262 57.79136
#> 796 56.03985  9.851431 36.73140 75.34830
#> 797 35.70341  0.000000 35.70341 35.70341
#> 798 41.64454  0.000000 41.64454 41.64454
#> 799 43.29513  3.774074 35.89808 50.69218
#> 800 54.25081  0.000000 54.25081 54.25081
```

The result is now a matrix, because that is what the
[`predict()`](https://rdrr.io/r/stats/predict.html) method returns for
`mmrm` objects. Note that this cannot be changed to return a `tibble` at
the moment.

Similarly, we can also use the
[`augment()`](https://generics.r-lib.org/reference/augment.html) method
to add predicted values to a new data set:

``` r
augment(model, new_data = fev_data) |>
  select(USUBJID, AVISIT, .resid, .pred)
#> # A tibble: 800 × 4
#>    USUBJID AVISIT .resid .pred
#>    <fct>   <fct>   <dbl> <dbl>
#>  1 PT1     VIS1    NA     36.2
#>  2 PT1     VIS2    -1.09  41.1
#>  3 PT1     VIS3    NA     45.9
#>  4 PT1     VIS4   -31.4   51.9
#>  5 PT2     VIS1    NA     31.0
#>  6 PT2     VIS2    -4.34  35.8
#>  7 PT2     VIS3    -4.42  41.3
#>  8 PT2     VIS4     2.79  46.0
#>  9 PT3     VIS1    NA     32.5
#> 10 PT3     VIS2    -1.31  37.3
#> # ℹ 790 more rows
```

Note that here we cannot customize the `predict` options as this is
currently not supported by the
[`augment()`](https://generics.r-lib.org/reference/augment.html) method
in `parsnip`.

#### Using mmrm in workflows

We can leverage the `workflows` package in order to fit the same model.

- First we define the specification for linear regression with the mmrm
  engine.
- Second we define the workflow, by defining the outcome and predictors
  that will be used in the formula. We then add the model using the
  formula.
- Lastly, we fit the model

``` r
mmrm_spec <- linear_reg() |>
  set_engine("mmrm", method = "Satterthwaite")

mmrm_wflow <- workflow() |>
  add_variables(outcomes = FEV1, predictors = c(RACE, ARMCD, AVISIT, USUBJID)) |>
  add_model(mmrm_spec, formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID))

mmrm_wflow |>
  fit(data = fev_data)
#> ══ Workflow [trained] ══════════════════════════════════════════════════════════
#> Preprocessor: Variables
#> Model: linear_reg()
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> Outcomes: FEV1
#> Predictors: c(RACE, ARMCD, AVISIT, USUBJID)
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> mmrm fit
#> 
#> Formula:     FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID)
#> Data:        data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Weights:     weights
#> Covariance:  unstructured (10 variance parameters)
#> Inference:   REML
#> Deviance:    3387.373
#> 
#> Coefficients: 
#>                   (Intercept) RACEBlack or African American 
#>                   30.96769899                    1.50464863 
#>                     RACEWhite                      ARMCDTRT 
#>                    5.61309565                    3.77555734 
#>                    AVISITVIS2                    AVISITVIS3 
#>                    4.82858803                   10.33317002 
#>                    AVISITVIS4           ARMCDTRT:AVISITVIS2 
#>                   15.05255715                   -0.01737409 
#>           ARMCDTRT:AVISITVIS3           ARMCDTRT:AVISITVIS4 
#>                   -0.66753189                    0.63094392 
#> 
#> Model Inference Optimization:
#> Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch
```

We can separate out the data preparation step from the modeling step
using the `recipes` package. Here we are converting the `ARMCD` variable
into a dummy variable and creating an interaction term with the new
dummy variable and each visit.

``` r
mmrm_recipe <- recipe(FEV1 ~ ., data = fev_data) |>
  step_dummy(ARMCD) |>
  step_interact(terms = ~ starts_with("ARMCD"):AVISIT)
```

Using `prep()` and `juice()` we can see what the transformed data that
will be used in the model fit looks like.

``` r
mmrm_recipe |>
  prep() |>
  juice()
#> Warning: Categorical variables used in `step_interact()` should probably be avoided;
#> This can lead to differences in dummy variable values that are produced by
#> ?step_dummy (`?recipes::step_dummy()`). Please convert all involved variables
#> to dummy variables first.
#> # A tibble: 800 × 13
#>    USUBJID AVISIT RACE       SEX   FEV1_BL WEIGHT VISITN VISITN2  FEV1 ARMCD_TRT
#>    <fct>   <fct>  <fct>      <fct>   <dbl>  <dbl>  <int>   <dbl> <dbl>     <dbl>
#>  1 PT1     VIS1   Black or … Fema…    25.3  0.677      1  -0.626  NA           1
#>  2 PT1     VIS2   Black or … Fema…    25.3  0.801      2   0.184  40.0         1
#>  3 PT1     VIS3   Black or … Fema…    25.3  0.709      3  -0.836  NA           1
#>  4 PT1     VIS4   Black or … Fema…    25.3  0.809      4   1.60   20.5         1
#>  5 PT2     VIS1   Asian      Male     45.0  0.465      1   0.330  NA           0
#>  6 PT2     VIS2   Asian      Male     45.0  0.233      2  -0.820  31.5         0
#>  7 PT2     VIS3   Asian      Male     45.0  0.360      3   0.487  36.9         0
#>  8 PT2     VIS4   Asian      Male     45.0  0.507      4   0.738  48.8         0
#>  9 PT3     VIS1   Black or … Fema…    43.5  0.682      1   0.576  NA           0
#> 10 PT3     VIS2   Black or … Fema…    43.5  0.892      2  -0.305  36.0         0
#> # ℹ 790 more rows
#> # ℹ 3 more variables: ARMCD_TRT_x_AVISITVIS2 <dbl>,
#> #   ARMCD_TRT_x_AVISITVIS3 <dbl>, ARMCD_TRT_x_AVISITVIS4 <dbl>
```

We can pass the covariance structure as well in the `set_engine()`
definition. This allows for more flexibility on presetting different
covariance structures in the pipeline while keeping the data preparation
step independent.

``` r
mmrm_spec_with_cov <- linear_reg() |>
  set_engine(
    "mmrm",
    method = "Satterthwaite",
    covariance = as.cov_struct(~ us(AVISIT | USUBJID))
  )
```

We combine these steps into a workflow:

``` r
(mmrm_wflow_nocov <- workflow() |>
  add_model(mmrm_spec_with_cov, formula = FEV1 ~ SEX) |>
  add_recipe(mmrm_recipe))
#> ══ Workflow ════════════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 2 Recipe Steps
#> 
#> • step_dummy()
#> • step_interact()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> Linear Regression Model Specification (regression)
#> 
#> Engine-Specific Arguments:
#>   method = Satterthwaite
#>   covariance = as.cov_struct(~us(AVISIT | USUBJID))
#> 
#> Computational engine: mmrm
```

Last step is to fit the data with the workflow object

``` r
(fit_tidy <- fit(mmrm_wflow_nocov, data = fev_data))
#> Warning: Categorical variables used in `step_interact()` should probably be avoided;
#> This can lead to differences in dummy variable values that are produced by
#> ?step_dummy (`?recipes::step_dummy()`). Please convert all involved variables
#> to dummy variables first.
#> ══ Workflow [trained] ══════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 2 Recipe Steps
#> 
#> • step_dummy()
#> • step_interact()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> mmrm fit
#> 
#> Formula:     FEV1 ~ SEX
#> Data:        data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Weights:     weights
#> Covariance:  unstructured (10 variance parameters)
#> Inference:   REML
#> Deviance:    3699.803
#> 
#> Coefficients: 
#> (Intercept)   SEXFemale 
#> 42.80540973  0.04513432 
#> 
#> Model Inference Optimization:
#> Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch
```

To retrieve the fit object from within the workflow object run the
following

``` r
fit_tidy |>
  hardhat::extract_fit_engine()
#> mmrm fit
#> 
#> Formula:     FEV1 ~ SEX
#> Data:        data (used 537 observations from 197 subjects with maximum 4 
#> timepoints)
#> Weights:     weights
#> Covariance:  unstructured (10 variance parameters)
#> Inference:   REML
#> Deviance:    3699.803
#> 
#> Coefficients: 
#> (Intercept)   SEXFemale 
#> 42.80540973  0.04513432 
#> 
#> Model Inference Optimization:
#> Converged with code 0 and message: convergence: rel_reduction_of_f <= factr*epsmch
```

## Acknowledgments

The `mmrm` package is based on previous work internal in Roche, namely
the `tern` and `tern.mmrm` packages which were based on `lme4`. The work
done in the `rbmi` package has been important since it used `glmmTMB`
for fitting MMRMs.

We would like to thank Ben Bolker from the `glmmTMB` team for multiple
discussions when we tried to get the Satterthwaite degrees of freedom
implemented with `glmmTMB` (see
<https://github.com/glmmTMB/glmmTMB/blob/satterthwaite_df/glmmTMB/vignettes/satterthwaite_unstructured_example2.Rmd>).
Also Ben helped us significantly with an example showing how to use
`TMB` for a random effect vector
(<https://github.com/bbolker/tmb-case-studies/tree/master/vectorMixed>).

## References
