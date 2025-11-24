# Component Access for `mmrm_tmb` Objects

**\[stable\]**

## Usage

``` r
component(
  object,
  name = c("cov_type", "subject_var", "n_theta", "n_subjects", "n_timepoints", "n_obs",
    "beta_vcov", "beta_vcov_complete", "varcor", "score_per_subject", "formula",
    "dataset", "n_groups", "reml", "convergence", "evaluations", "method", "optimizer",
    "conv_message", "call", "theta_est", "beta_est", "beta_est_complete", "beta_aliased",
    "x_matrix", "y_vector", "neg_log_lik", "jac_list", "theta_vcov", "full_frame",
    "xlev", "contrasts")
)
```

## Arguments

- object:

  (`mmrm_tmb`)  
  the fitted MMRM.

- name:

  (`character`)  
  the component(s) to be retrieved.

## Value

The corresponding component of the object, see details.

## Details

Available `component()` names are as follows:

- `call`: low-level function call which generated the model.

- `formula`: model formula.

- `dataset`: data set name.

- `cov_type`: covariance structure type.

- `n_theta`: number of parameters.

- `n_subjects`: number of subjects.

- `n_timepoints`: number of modeled time points.

- `n_obs`: total number of observations.

- `reml`: was REML used (ML was used if `FALSE`).

- `neg_log_lik`: negative log likelihood.

- `convergence`: convergence code from optimizer.

- `conv_message`: message accompanying the convergence code.

- `evaluations`: number of function evaluations for optimization.

- `method`: Adjustment method which was used (for `mmrm` objects),
  otherwise `NULL` (for `mmrm_tmb` objects).

- `beta_vcov`: estimated variance-covariance matrix of coefficients
  (excluding aliased coefficients). When Kenward-Roger/Empirical
  adjusted coefficients covariance matrix is used, the adjusted
  covariance matrix is returned (to still obtain the original asymptotic
  covariance matrix use `object$beta_vcov`).

- `beta_vcov_complete`: estimated variance-covariance matrix including
  aliased coefficients with entries set to `NA`.

- `varcor`: estimated covariance matrix for residuals. If there are
  multiple groups, a named list of estimated covariance matrices for
  residuals will be returned. The names are the group levels.

- `score_per_subject`: score per subject in empirical covariance. See
  the vignette
  [`vignette("coef_vcov", package = "mmrm")`](https://openpharma.github.io/mmrm/articles/coef_vcov.md).

- `theta_est`: estimated variance parameters.

- `beta_est`: estimated coefficients (excluding aliased coefficients).

- `beta_est_complete`: estimated coefficients including aliased
  coefficients set to `NA`.

- `beta_aliased`: whether each coefficient was aliased (i.e. cannot be
  estimated) or not.

- `theta_vcov`: estimated variance-covariance matrix of variance
  parameters.

- `x_matrix`: design matrix used (excluding aliased columns).

- `xlev`: a named list of character vectors giving the full set of
  levels to be assumed for each factor.

- `contrasts`: a list of contrasts used for each factor.

- `y_vector`: response vector used.

- `jac_list`: Jacobian, see
  [`h_jac_list()`](https://openpharma.github.io/mmrm/reference/h_jac_list.md)
  for details.

- `full_frame`: `data.frame` with `n` rows containing all variables
  needed in the model.

## See also

In the `lme4` package there is a similar function `getME()`.

## Examples

``` r
fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)
# Get all available components.
component(fit)
#> $cov_type
#> [1] "us"
#> 
#> $subject_var
#> [1] "USUBJID"
#> 
#> $n_theta
#> [1] 10
#> 
#> $n_subjects
#> [1] 197
#> 
#> $n_timepoints
#> [1] 4
#> 
#> $n_obs
#> [1] 537
#> 
#> $beta_vcov
#>                               (Intercept) RACEBlack or African American
#> (Intercept)                     0.7859971                  -0.226212328
#> RACEBlack or African American  -0.2262123                   0.389969478
#> RACEWhite                      -0.1771113                   0.181466304
#> SEXFemale                      -0.1684152                   0.031537926
#> ARMCDTRT                       -0.5674809                   0.028374129
#> AVISITVIS2                     -0.4227565                   0.002972514
#> AVISITVIS3                     -0.5231223                   0.010825469
#> AVISITVIS4                     -0.4406442                   0.002205681
#> ARMCDTRT:AVISITVIS2             0.4225282                   0.005382569
#> ARMCDTRT:AVISITVIS3             0.5218971                   0.011420575
#> ARMCDTRT:AVISITVIS4             0.4489247                  -0.012589283
#>                                  RACEWhite    SEXFemale     ARMCDTRT
#> (Intercept)                   -0.177111308 -0.168415217 -0.567480906
#> RACEBlack or African American  0.181466304  0.031537926  0.028374129
#> RACEWhite                      0.443035801  0.023364777 -0.042968995
#> SEXFemale                      0.023364777  0.282971189  0.001814594
#> ARMCDTRT                      -0.042968995  0.001814594  1.153791725
#> AVISITVIS2                    -0.003149280  0.006471853  0.419528600
#> AVISITVIS3                    -0.002952986  0.006771404  0.517277529
#> AVISITVIS4                    -0.008230720  0.004088901  0.440653554
#> ARMCDTRT:AVISITVIS2            0.013485683 -0.016801299 -0.845758354
#> ARMCDTRT:AVISITVIS3            0.006720617 -0.024696304 -1.044355829
#> ARMCDTRT:AVISITVIS4            0.002967665 -0.009038640 -0.877606881
#>                                 AVISITVIS2   AVISITVIS3   AVISITVIS4
#> (Intercept)                   -0.422756455 -0.523122299 -0.440644229
#> RACEBlack or African American  0.002972514  0.010825469  0.002205681
#> RACEWhite                     -0.003149280 -0.002952986 -0.008230720
#> SEXFemale                      0.006471853  0.006771404  0.004088901
#> ARMCDTRT                       0.419528600  0.517277529  0.440653554
#> AVISITVIS2                     0.642749706  0.399048940  0.368340113
#> AVISITVIS3                     0.399048940  0.676823960  0.401800094
#> AVISITVIS4                     0.368340113  0.401800094  1.723478787
#> ARMCDTRT:AVISITVIS2           -0.643020114 -0.399203255 -0.368624024
#> ARMCDTRT:AVISITVIS3           -0.399238901 -0.676484876 -0.401792995
#> ARMCDTRT:AVISITVIS4           -0.368506585 -0.402167824 -1.723586879
#>                               ARMCDTRT:AVISITVIS2 ARMCDTRT:AVISITVIS3
#> (Intercept)                           0.422528163         0.521897062
#> RACEBlack or African American         0.005382569         0.011420575
#> RACEWhite                             0.013485683         0.006720617
#> SEXFemale                            -0.016801299        -0.024696304
#> ARMCDTRT                             -0.845758354        -1.044355829
#> AVISITVIS2                           -0.643020114        -0.399238901
#> AVISITVIS3                           -0.399203255        -0.676484876
#> AVISITVIS4                           -0.368624024        -0.401792995
#> ARMCDTRT:AVISITVIS2                   1.275359305         0.805849821
#> ARMCDTRT:AVISITVIS3                   0.805849821         1.410501907
#> ARMCDTRT:AVISITVIS4                   0.728711516         0.796418986
#>                               ARMCDTRT:AVISITVIS4
#> (Intercept)                           0.448924745
#> RACEBlack or African American        -0.012589283
#> RACEWhite                             0.002967665
#> SEXFemale                            -0.009038640
#> ARMCDTRT                             -0.877606881
#> AVISITVIS2                           -0.368506585
#> AVISITVIS3                           -0.402167824
#> AVISITVIS4                           -1.723586879
#> ARMCDTRT:AVISITVIS2                   0.728711516
#> ARMCDTRT:AVISITVIS3                   0.796418986
#> ARMCDTRT:AVISITVIS4                   3.425654435
#> 
#> $beta_vcov_complete
#>                               (Intercept) RACEBlack or African American
#> (Intercept)                     0.7859971                  -0.226212328
#> RACEBlack or African American  -0.2262123                   0.389969478
#> RACEWhite                      -0.1771113                   0.181466304
#> SEXFemale                      -0.1684152                   0.031537926
#> ARMCDTRT                       -0.5674809                   0.028374129
#> AVISITVIS2                     -0.4227565                   0.002972514
#> AVISITVIS3                     -0.5231223                   0.010825469
#> AVISITVIS4                     -0.4406442                   0.002205681
#> ARMCDTRT:AVISITVIS2             0.4225282                   0.005382569
#> ARMCDTRT:AVISITVIS3             0.5218971                   0.011420575
#> ARMCDTRT:AVISITVIS4             0.4489247                  -0.012589283
#>                                  RACEWhite    SEXFemale     ARMCDTRT
#> (Intercept)                   -0.177111308 -0.168415217 -0.567480906
#> RACEBlack or African American  0.181466304  0.031537926  0.028374129
#> RACEWhite                      0.443035801  0.023364777 -0.042968995
#> SEXFemale                      0.023364777  0.282971189  0.001814594
#> ARMCDTRT                      -0.042968995  0.001814594  1.153791725
#> AVISITVIS2                    -0.003149280  0.006471853  0.419528600
#> AVISITVIS3                    -0.002952986  0.006771404  0.517277529
#> AVISITVIS4                    -0.008230720  0.004088901  0.440653554
#> ARMCDTRT:AVISITVIS2            0.013485683 -0.016801299 -0.845758354
#> ARMCDTRT:AVISITVIS3            0.006720617 -0.024696304 -1.044355829
#> ARMCDTRT:AVISITVIS4            0.002967665 -0.009038640 -0.877606881
#>                                 AVISITVIS2   AVISITVIS3   AVISITVIS4
#> (Intercept)                   -0.422756455 -0.523122299 -0.440644229
#> RACEBlack or African American  0.002972514  0.010825469  0.002205681
#> RACEWhite                     -0.003149280 -0.002952986 -0.008230720
#> SEXFemale                      0.006471853  0.006771404  0.004088901
#> ARMCDTRT                       0.419528600  0.517277529  0.440653554
#> AVISITVIS2                     0.642749706  0.399048940  0.368340113
#> AVISITVIS3                     0.399048940  0.676823960  0.401800094
#> AVISITVIS4                     0.368340113  0.401800094  1.723478787
#> ARMCDTRT:AVISITVIS2           -0.643020114 -0.399203255 -0.368624024
#> ARMCDTRT:AVISITVIS3           -0.399238901 -0.676484876 -0.401792995
#> ARMCDTRT:AVISITVIS4           -0.368506585 -0.402167824 -1.723586879
#>                               ARMCDTRT:AVISITVIS2 ARMCDTRT:AVISITVIS3
#> (Intercept)                           0.422528163         0.521897062
#> RACEBlack or African American         0.005382569         0.011420575
#> RACEWhite                             0.013485683         0.006720617
#> SEXFemale                            -0.016801299        -0.024696304
#> ARMCDTRT                             -0.845758354        -1.044355829
#> AVISITVIS2                           -0.643020114        -0.399238901
#> AVISITVIS3                           -0.399203255        -0.676484876
#> AVISITVIS4                           -0.368624024        -0.401792995
#> ARMCDTRT:AVISITVIS2                   1.275359305         0.805849821
#> ARMCDTRT:AVISITVIS3                   0.805849821         1.410501907
#> ARMCDTRT:AVISITVIS4                   0.728711516         0.796418986
#>                               ARMCDTRT:AVISITVIS4
#> (Intercept)                           0.448924745
#> RACEBlack or African American        -0.012589283
#> RACEWhite                             0.002967665
#> SEXFemale                            -0.009038640
#> ARMCDTRT                             -0.877606881
#> AVISITVIS2                           -0.368506585
#> AVISITVIS3                           -0.402167824
#> AVISITVIS4                           -1.723586879
#> ARMCDTRT:AVISITVIS2                   0.728711516
#> ARMCDTRT:AVISITVIS3                   0.796418986
#> ARMCDTRT:AVISITVIS4                   3.425654435
#> 
#> $varcor
#>           VIS1      VIS2       VIS3       VIS4
#> VIS1 40.553664 14.396045  4.9747288 13.3866534
#> VIS2 14.396045 26.571483  2.7854661  7.4744790
#> VIS3  4.974729  2.785466 14.8978517  0.9082111
#> VIS4 13.386653  7.474479  0.9082111 95.5568420
#> 
#> $score_per_subject
#> NULL
#> 
#> $formula
#> [1] "FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)"
#> 
#> $dataset
#> fev_data
#> 
#> $n_groups
#> [1] 1
#> 
#> $reml
#> [1] TRUE
#> 
#> $convergence
#> [1] 0
#> 
#> $evaluations
#> function gradient 
#>       15       15 
#> 
#> $method
#> [1] "Satterthwaite"
#> 
#> $optimizer
#> [1] "L-BFGS-B"
#> 
#> $conv_message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
#> 
#> $call
#> mmrm(formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | 
#>     USUBJID), data = fev_data)
#> 
#> $theta_est
#>  [1]  1.85131306  1.53312022  1.32799829  2.25400062  0.48798054  0.20701987
#>  [7]  0.05832020  0.22067693  0.06169127 -0.02401589
#> 
#> $beta_est
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
#> $beta_est_complete
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
#> $beta_aliased
#>                   (Intercept) RACEBlack or African American 
#>                         FALSE                         FALSE 
#>                     RACEWhite                     SEXFemale 
#>                         FALSE                         FALSE 
#>                      ARMCDTRT                    AVISITVIS2 
#>                         FALSE                         FALSE 
#>                    AVISITVIS3                    AVISITVIS4 
#>                         FALSE                         FALSE 
#>           ARMCDTRT:AVISITVIS2           ARMCDTRT:AVISITVIS3 
#>                         FALSE                         FALSE 
#>           ARMCDTRT:AVISITVIS4 
#>                         FALSE 
#> 
#> $x_matrix
#>     (Intercept) RACEBlack or African American RACEWhite SEXFemale ARMCDTRT
#> 2             1                             1         0         1        1
#> 4             1                             1         0         1        1
#> 6             1                             0         0         0        0
#> 7             1                             0         0         0        0
#> 8             1                             0         0         0        0
#> 10            1                             1         0         1        0
#> 12            1                             1         0         1        0
#> 13            1                             0         0         1        1
#> 14            1                             0         0         1        1
#> 16            1                             0         0         1        1
#> 17            1                             1         0         0        0
#> 19            1                             1         0         0        0
#> 20            1                             1         0         0        0
#> 23            1                             1         0         0        0
#> 25            1                             0         0         1        0
#> 26            1                             0         0         1        0
#> 28            1                             0         0         1        0
#> 29            1                             1         0         0        0
#> 30            1                             1         0         0        0
#> 31            1                             1         0         0        0
#> 32            1                             1         0         0        0
#> 33            1                             0         1         0        1
#> 34            1                             0         1         0        1
#> 36            1                             0         1         0        1
#> 39            1                             1         0         1        0
#> 41            1                             0         0         1        1
#> 42            1                             0         0         1        1
#> 43            1                             0         0         1        1
#> 44            1                             0         0         1        1
#> 45            1                             0         0         0        0
#> 46            1                             0         0         0        0
#> 47            1                             0         0         0        0
#> 51            1                             0         1         0        1
#> 52            1                             0         1         0        1
#> 55            1                             1         0         0        0
#> 59            1                             0         0         0        0
#> 60            1                             0         0         0        0
#> 62            1                             0         0         1        0
#> 64            1                             0         0         1        0
#> 65            1                             0         1         1        0
#> 68            1                             0         1         1        0
#> 69            1                             0         0         0        1
#> 70            1                             0         0         0        1
#> 72            1                             0         0         0        1
#> 73            1                             0         0         0        1
#> 74            1                             0         0         0        1
#> 75            1                             0         0         0        1
#> 76            1                             0         0         0        1
#> 78            1                             0         1         1        1
#> 79            1                             0         1         1        1
#> 82            1                             0         1         0        1
#> 83            1                             0         1         0        1
#> 84            1                             0         1         0        1
#> 85            1                             0         1         0        1
#> 86            1                             0         1         0        1
#> 87            1                             0         1         0        1
#> 88            1                             0         1         0        1
#> 89            1                             1         0         1        0
#> 90            1                             1         0         1        0
#> 91            1                             1         0         1        0
#> 93            1                             0         1         1        1
#> 94            1                             0         1         1        1
#> 95            1                             0         1         1        1
#> 96            1                             0         1         1        1
#> 97            1                             0         1         1        1
#> 98            1                             0         1         1        1
#> 99            1                             0         1         1        1
#> 100           1                             0         1         1        1
#> 101           1                             0         1         1        1
#> 102           1                             0         1         1        1
#> 103           1                             0         1         1        1
#> 104           1                             0         1         1        1
#> 105           1                             1         0         1        1
#> 107           1                             1         0         1        1
#> 108           1                             1         0         1        1
#> 109           1                             1         0         0        1
#> 110           1                             1         0         0        1
#> 111           1                             1         0         0        1
#> 112           1                             1         0         0        1
#> 113           1                             1         0         0        1
#> 114           1                             1         0         0        1
#> 116           1                             1         0         0        1
#> 117           1                             0         0         1        1
#> 118           1                             0         0         1        1
#> 119           1                             0         0         1        1
#> 120           1                             0         0         1        1
#> 121           1                             0         0         1        0
#> 123           1                             0         0         1        0
#> 125           1                             1         0         0        1
#> 128           1                             1         0         0        1
#> 129           1                             1         0         0        1
#> 130           1                             1         0         0        1
#> 132           1                             1         0         0        1
#> 133           1                             0         0         1        0
#> 134           1                             0         0         1        0
#> 135           1                             0         0         1        0
#> 136           1                             0         0         1        0
#> 137           1                             1         0         1        1
#> 138           1                             1         0         1        1
#> 140           1                             1         0         1        1
#> 142           1                             0         0         1        1
#> 144           1                             0         0         1        1
#> 145           1                             0         1         1        0
#> 146           1                             0         1         1        0
#> 147           1                             0         1         1        0
#> 148           1                             0         1         1        0
#> 149           1                             0         0         1        0
#> 151           1                             0         0         1        0
#> 153           1                             0         0         1        0
#> 155           1                             0         0         1        0
#> 156           1                             0         0         1        0
#> 157           1                             1         0         0        0
#> 158           1                             1         0         0        0
#> 159           1                             1         0         0        0
#> 162           1                             1         0         0        0
#> 163           1                             1         0         0        0
#> 164           1                             1         0         0        0
#> 165           1                             1         0         0        0
#> 168           1                             1         0         0        0
#> 169           1                             1         0         1        1
#> 170           1                             1         0         1        1
#> 171           1                             1         0         1        1
#> 172           1                             1         0         1        1
#> 173           1                             1         0         0        1
#> 177           1                             0         0         1        0
#> 178           1                             0         0         1        0
#> 179           1                             0         0         1        0
#> 180           1                             0         0         1        0
#> 181           1                             0         0         0        0
#> 182           1                             0         0         0        0
#> 183           1                             0         0         0        0
#> 185           1                             0         0         0        1
#> 186           1                             0         0         0        1
#> 187           1                             0         0         0        1
#> 190           1                             1         0         0        0
#> 191           1                             1         0         0        0
#> 193           1                             0         1         1        0
#> 194           1                             0         1         1        0
#> 195           1                             0         1         1        0
#> 197           1                             0         0         1        1
#> 198           1                             0         0         1        1
#> 199           1                             0         0         1        1
#> 201           1                             1         0         1        1
#> 202           1                             1         0         1        1
#> 204           1                             1         0         1        1
#> 206           1                             1         0         1        1
#> 208           1                             1         0         1        1
#> 209           1                             0         1         0        0
#> 210           1                             0         1         0        0
#> 217           1                             0         0         0        1
#> 218           1                             0         0         0        1
#> 219           1                             0         0         0        1
#> 221           1                             1         0         0        0
#> 224           1                             1         0         0        0
#> 226           1                             1         0         0        1
#> 227           1                             1         0         0        1
#> 228           1                             1         0         0        1
#> 230           1                             0         1         1        1
#> 231           1                             0         1         1        1
#> 233           1                             0         1         1        1
#> 235           1                             0         1         1        1
#> 236           1                             0         1         1        1
#> 237           1                             1         0         1        1
#> 238           1                             1         0         1        1
#> 239           1                             1         0         1        1
#> 240           1                             1         0         1        1
#> 241           1                             0         0         0        1
#> 242           1                             0         0         0        1
#> 244           1                             0         0         0        1
#> 246           1                             0         0         1        0
#> 250           1                             0         1         1        0
#> 251           1                             0         1         1        0
#> 252           1                             0         1         1        0
#> 253           1                             1         0         1        0
#> 254           1                             1         0         1        0
#> 256           1                             1         0         1        0
#> 257           1                             0         0         1        1
#> 258           1                             0         0         1        1
#> 259           1                             0         0         1        1
#> 260           1                             0         0         1        1
#> 261           1                             0         0         0        1
#> 262           1                             0         0         0        1
#> 263           1                             0         0         0        1
#> 264           1                             0         0         0        1
#> 265           1                             0         1         1        1
#> 266           1                             0         1         1        1
#> 267           1                             0         1         1        1
#> 268           1                             0         1         1        1
#> 269           1                             0         1         1        1
#> 270           1                             0         1         1        1
#> 273           1                             1         0         0        0
#> 274           1                             1         0         0        0
#> 275           1                             1         0         0        0
#> 276           1                             1         0         0        0
#> 277           1                             1         0         0        0
#> 278           1                             1         0         0        0
#> 280           1                             1         0         0        0
#> 281           1                             0         0         0        1
#> 282           1                             0         0         0        1
#> 283           1                             0         0         0        1
#> 284           1                             0         0         0        1
#> 285           1                             0         0         1        1
#> 286           1                             0         0         1        1
#> 287           1                             0         0         1        1
#> 291           1                             0         1         0        0
#> 292           1                             0         1         0        0
#> 293           1                             0         1         1        0
#> 295           1                             0         1         1        0
#> 296           1                             0         1         1        0
#> 298           1                             0         1         0        0
#> 299           1                             0         1         0        0
#> 300           1                             0         1         0        0
#> 301           1                             0         1         0        1
#> 304           1                             0         1         0        1
#> 305           1                             0         0         0        1
#> 306           1                             0         0         0        1
#> 307           1                             0         0         0        1
#> 308           1                             0         0         0        1
#> 310           1                             0         1         1        1
#> 311           1                             0         1         1        1
#> 312           1                             0         1         1        1
#> 316           1                             0         0         1        0
#> 317           1                             0         0         0        1
#> 318           1                             0         0         0        1
#> 319           1                             0         0         0        1
#> 322           1                             0         1         0        0
#> 323           1                             0         1         0        0
#> 324           1                             0         1         0        0
#> 325           1                             1         0         1        0
#> 327           1                             1         0         1        0
#> 328           1                             1         0         1        0
#> 329           1                             0         1         0        1
#> 330           1                             0         1         0        1
#> 331           1                             0         1         0        1
#> 332           1                             0         1         0        1
#> 336           1                             0         0         1        0
#> 339           1                             0         0         1        0
#> 340           1                             0         0         1        0
#> 341           1                             1         0         1        1
#> 342           1                             1         0         1        1
#> 343           1                             1         0         1        1
#> 344           1                             1         0         1        1
#> 345           1                             0         1         0        0
#> 347           1                             0         1         0        0
#> 349           1                             1         0         1        0
#> 351           1                             1         0         1        0
#> 352           1                             1         0         1        0
#> 353           1                             1         0         0        0
#> 354           1                             1         0         0        0
#> 355           1                             1         0         0        0
#> 356           1                             1         0         0        0
#> 357           1                             0         0         0        0
#> 363           1                             0         1         1        1
#> 364           1                             0         1         1        1
#> 365           1                             0         1         1        1
#> 367           1                             0         1         1        1
#> 368           1                             0         1         1        1
#> 370           1                             1         0         0        0
#> 371           1                             1         0         0        0
#> 372           1                             1         0         0        0
#> 373           1                             0         0         1        0
#> 375           1                             0         0         1        0
#> 376           1                             0         0         1        0
#> 378           1                             0         0         1        0
#> 379           1                             0         0         1        0
#> 381           1                             1         0         1        0
#> 382           1                             1         0         1        0
#> 384           1                             1         0         1        0
#> 385           1                             0         1         0        1
#> 386           1                             0         1         0        1
#> 388           1                             0         1         0        1
#> 389           1                             0         0         0        0
#> 390           1                             0         0         0        0
#> 391           1                             0         0         0        0
#> 392           1                             0         0         0        0
#> 394           1                             1         0         0        0
#> 397           1                             1         0         1        0
#> 398           1                             1         0         1        0
#> 399           1                             1         0         1        0
#> 402           1                             0         1         0        1
#> 403           1                             0         1         0        1
#> 405           1                             0         0         1        0
#> 406           1                             0         0         1        0
#> 407           1                             0         0         1        0
#> 408           1                             0         0         1        0
#> 409           1                             0         0         1        0
#> 410           1                             0         0         1        0
#> 411           1                             0         0         1        0
#> 413           1                             1         0         1        0
#> 415           1                             1         0         1        0
#> 416           1                             1         0         1        0
#> 418           1                             1         0         1        1
#> 419           1                             1         0         1        1
#> 421           1                             0         0         1        1
#> 422           1                             0         0         1        1
#> 423           1                             0         0         1        1
#> 424           1                             0         0         1        1
#> 427           1                             0         1         1        0
#> 428           1                             0         1         1        0
#> 429           1                             0         1         0        1
#> 430           1                             0         1         0        1
#> 431           1                             0         1         0        1
#> 432           1                             0         1         0        1
#> 434           1                             1         0         1        0
#> 435           1                             1         0         1        0
#> 436           1                             1         0         1        0
#> 438           1                             0         0         0        1
#> 439           1                             0         0         0        1
#> 444           1                             0         1         1        0
#> 445           1                             1         0         1        0
#> 447           1                             1         0         1        0
#> 449           1                             0         0         0        0
#> 450           1                             0         0         0        0
#> 451           1                             0         0         0        0
#> 453           1                             1         0         0        0
#> 454           1                             1         0         0        0
#> 455           1                             1         0         0        0
#> 456           1                             1         0         0        0
#> 457           1                             0         0         1        0
#> 458           1                             0         0         1        0
#> 459           1                             0         0         1        0
#> 461           1                             1         0         0        1
#> 463           1                             1         0         0        1
#> 464           1                             1         0         0        1
#> 465           1                             1         0         0        0
#> 469           1                             0         0         1        0
#> 470           1                             0         0         1        0
#> 471           1                             0         0         1        0
#> 473           1                             0         1         0        1
#> 474           1                             0         1         0        1
#> 477           1                             0         0         1        0
#> 484           1                             1         0         0        1
#> 487           1                             0         0         1        0
#> 489           1                             0         1         1        0
#> 490           1                             0         1         1        0
#> 491           1                             0         1         1        0
#> 494           1                             0         0         1        1
#> 495           1                             0         0         1        1
#> 496           1                             0         0         1        1
#> 497           1                             0         1         0        0
#> 498           1                             0         1         0        0
#> 499           1                             0         1         0        0
#> 501           1                             0         0         0        1
#> 502           1                             0         0         0        1
#> 504           1                             0         0         0        1
#> 505           1                             1         0         1        1
#> 508           1                             1         0         1        1
#> 509           1                             0         0         1        1
#> 510           1                             0         0         1        1
#> 511           1                             0         0         1        1
#> 512           1                             0         0         1        1
#> 513           1                             0         1         0        1
#> 518           1                             0         1         0        0
#> 519           1                             0         1         0        0
#> 521           1                             0         0         1        0
#> 522           1                             0         0         1        0
#> 523           1                             0         0         1        0
#> 524           1                             0         0         1        0
#> 526           1                             0         0         0        0
#> 527           1                             0         0         0        0
#> 528           1                             0         0         0        0
#> 530           1                             0         1         0        0
#> 531           1                             0         1         0        0
#> 532           1                             0         1         0        0
#> 534           1                             1         0         0        0
#> 535           1                             1         0         0        0
#> 536           1                             1         0         0        0
#> 537           1                             1         0         0        0
#> 538           1                             1         0         0        0
#> 539           1                             1         0         0        0
#> 540           1                             1         0         0        0
#> 541           1                             0         1         1        1
#> 544           1                             0         1         1        1
#> 545           1                             0         0         1        1
#> 546           1                             0         0         1        1
#> 547           1                             0         0         1        1
#> 548           1                             0         0         1        1
#> 549           1                             0         0         0        1
#> 550           1                             0         0         0        1
#> 551           1                             0         0         0        1
#> 555           1                             0         0         1        1
#> 556           1                             0         0         1        1
#> 557           1                             1         0         1        0
#> 558           1                             1         0         1        0
#> 560           1                             1         0         1        0
#> 562           1                             0         1         0        1
#> 564           1                             0         1         0        1
#> 569           1                             1         0         1        1
#> 570           1                             1         0         1        1
#> 572           1                             1         0         1        1
#> 573           1                             0         0         1        0
#> 574           1                             0         0         1        0
#> 575           1                             0         0         1        0
#> 576           1                             0         0         1        0
#> 577           1                             1         0         0        0
#> 578           1                             1         0         0        0
#> 579           1                             1         0         0        0
#> 582           1                             0         0         1        1
#> 583           1                             0         0         1        1
#> 584           1                             0         0         1        1
#> 585           1                             0         0         1        0
#> 586           1                             0         0         1        0
#> 587           1                             0         0         1        0
#> 590           1                             1         0         0        1
#> 591           1                             1         0         0        1
#> 593           1                             0         0         1        0
#> 594           1                             0         0         1        0
#> 595           1                             0         0         1        0
#> 596           1                             0         0         1        0
#> 599           1                             0         1         0        0
#> 600           1                             0         1         0        0
#> 601           1                             0         0         1        0
#> 602           1                             0         0         1        0
#> 604           1                             0         0         1        0
#> 606           1                             0         0         1        1
#> 608           1                             0         0         1        1
#> 609           1                             1         0         0        0
#> 610           1                             1         0         0        0
#> 611           1                             1         0         0        0
#> 612           1                             1         0         0        0
#> 613           1                             1         0         1        0
#> 614           1                             1         0         1        0
#> 616           1                             1         0         1        0
#> 617           1                             0         0         0        1
#> 619           1                             0         0         0        1
#> 620           1                             0         0         0        1
#> 621           1                             0         1         0        0
#> 622           1                             0         1         0        0
#> 623           1                             0         1         0        0
#> 624           1                             0         1         0        0
#> 625           1                             1         0         0        1
#> 628           1                             1         0         0        1
#> 630           1                             1         0         0        1
#> 631           1                             1         0         0        1
#> 632           1                             1         0         0        1
#> 633           1                             0         0         1        1
#> 634           1                             0         0         1        1
#> 638           1                             0         0         0        1
#> 639           1                             0         0         0        1
#> 640           1                             0         0         0        1
#> 642           1                             1         0         1        0
#> 645           1                             0         0         0        0
#> 648           1                             0         0         0        0
#> 650           1                             0         1         0        0
#> 652           1                             0         1         0        0
#> 654           1                             1         0         1        0
#> 655           1                             1         0         1        0
#> 656           1                             1         0         1        0
#> 657           1                             0         1         1        1
#> 661           1                             0         0         0        1
#> 665           1                             1         0         0        0
#> 666           1                             1         0         0        0
#> 668           1                             1         0         0        0
#> 669           1                             1         0         1        0
#> 670           1                             1         0         1        0
#> 671           1                             1         0         1        0
#> 673           1                             0         1         1        0
#> 674           1                             0         1         1        0
#> 678           1                             0         1         1        1
#> 679           1                             0         1         1        1
#> 680           1                             0         1         1        1
#> 682           1                             0         0         1        0
#> 684           1                             0         0         1        0
#> 685           1                             0         0         1        1
#> 686           1                             0         0         1        1
#> 687           1                             0         0         1        1
#> 689           1                             0         1         1        1
#> 690           1                             0         1         1        1
#> 691           1                             0         1         1        1
#> 693           1                             1         0         1        1
#> 694           1                             1         0         1        1
#> 695           1                             1         0         1        1
#> 697           1                             1         0         1        0
#> 698           1                             1         0         1        0
#> 700           1                             1         0         1        0
#> 701           1                             1         0         0        1
#> 702           1                             1         0         0        1
#> 704           1                             1         0         0        1
#> 707           1                             1         0         0        0
#> 709           1                             1         0         0        0
#> 712           1                             1         0         0        0
#> 713           1                             0         1         0        1
#> 715           1                             0         1         0        1
#> 716           1                             0         1         0        1
#> 717           1                             0         0         0        0
#> 718           1                             0         0         0        0
#> 719           1                             0         0         0        0
#> 721           1                             0         0         1        0
#> 723           1                             0         0         1        0
#> 724           1                             0         0         1        0
#> 727           1                             1         0         1        1
#> 728           1                             1         0         1        1
#> 729           1                             1         0         1        1
#> 730           1                             1         0         1        1
#> 734           1                             0         1         1        1
#> 736           1                             0         1         1        1
#> 738           1                             1         0         1        0
#> 739           1                             1         0         1        0
#> 740           1                             1         0         1        0
#> 741           1                             0         1         0        0
#> 742           1                             0         1         0        0
#> 744           1                             0         1         0        0
#> 745           1                             1         0         1        0
#> 749           1                             1         0         1        0
#> 751           1                             1         0         1        0
#> 753           1                             0         0         1        1
#> 754           1                             0         0         1        1
#> 755           1                             0         0         1        1
#> 757           1                             1         0         1        1
#> 758           1                             1         0         1        1
#> 759           1                             1         0         1        1
#> 760           1                             1         0         1        1
#> 761           1                             0         1         0        1
#> 764           1                             0         1         0        1
#> 765           1                             0         1         1        1
#> 766           1                             0         1         1        1
#> 767           1                             0         1         1        1
#> 768           1                             0         1         1        1
#> 770           1                             0         0         0        0
#> 771           1                             0         0         0        0
#> 772           1                             0         0         0        0
#> 773           1                             1         0         0        0
#> 774           1                             1         0         0        0
#> 775           1                             1         0         0        0
#> 776           1                             1         0         0        0
#> 778           1                             0         1         1        1
#> 779           1                             0         1         1        1
#> 781           1                             0         0         0        0
#> 782           1                             0         0         0        0
#> 784           1                             0         0         0        0
#> 788           1                             1         0         0        0
#> 789           1                             1         0         0        1
#> 790           1                             1         0         0        1
#> 792           1                             1         0         0        1
#> 797           1                             1         0         0        0
#> 798           1                             1         0         0        0
#> 800           1                             1         0         0        0
#>     AVISITVIS2 AVISITVIS3 AVISITVIS4 ARMCDTRT:AVISITVIS2 ARMCDTRT:AVISITVIS3
#> 2            1          0          0                   1                   0
#> 4            0          0          1                   0                   0
#> 6            1          0          0                   0                   0
#> 7            0          1          0                   0                   0
#> 8            0          0          1                   0                   0
#> 10           1          0          0                   0                   0
#> 12           0          0          1                   0                   0
#> 13           0          0          0                   0                   0
#> 14           1          0          0                   1                   0
#> 16           0          0          1                   0                   0
#> 17           0          0          0                   0                   0
#> 19           0          1          0                   0                   0
#> 20           0          0          1                   0                   0
#> 23           0          1          0                   0                   0
#> 25           0          0          0                   0                   0
#> 26           1          0          0                   0                   0
#> 28           0          0          1                   0                   0
#> 29           0          0          0                   0                   0
#> 30           1          0          0                   0                   0
#> 31           0          1          0                   0                   0
#> 32           0          0          1                   0                   0
#> 33           0          0          0                   0                   0
#> 34           1          0          0                   1                   0
#> 36           0          0          1                   0                   0
#> 39           0          1          0                   0                   0
#> 41           0          0          0                   0                   0
#> 42           1          0          0                   1                   0
#> 43           0          1          0                   0                   1
#> 44           0          0          1                   0                   0
#> 45           0          0          0                   0                   0
#> 46           1          0          0                   0                   0
#> 47           0          1          0                   0                   0
#> 51           0          1          0                   0                   1
#> 52           0          0          1                   0                   0
#> 55           0          1          0                   0                   0
#> 59           0          1          0                   0                   0
#> 60           0          0          1                   0                   0
#> 62           1          0          0                   0                   0
#> 64           0          0          1                   0                   0
#> 65           0          0          0                   0                   0
#> 68           0          0          1                   0                   0
#> 69           0          0          0                   0                   0
#> 70           1          0          0                   1                   0
#> 72           0          0          1                   0                   0
#> 73           0          0          0                   0                   0
#> 74           1          0          0                   1                   0
#> 75           0          1          0                   0                   1
#> 76           0          0          1                   0                   0
#> 78           1          0          0                   1                   0
#> 79           0          1          0                   0                   1
#> 82           1          0          0                   1                   0
#> 83           0          1          0                   0                   1
#> 84           0          0          1                   0                   0
#> 85           0          0          0                   0                   0
#> 86           1          0          0                   1                   0
#> 87           0          1          0                   0                   1
#> 88           0          0          1                   0                   0
#> 89           0          0          0                   0                   0
#> 90           1          0          0                   0                   0
#> 91           0          1          0                   0                   0
#> 93           0          0          0                   0                   0
#> 94           1          0          0                   1                   0
#> 95           0          1          0                   0                   1
#> 96           0          0          1                   0                   0
#> 97           0          0          0                   0                   0
#> 98           1          0          0                   1                   0
#> 99           0          1          0                   0                   1
#> 100          0          0          1                   0                   0
#> 101          0          0          0                   0                   0
#> 102          1          0          0                   1                   0
#> 103          0          1          0                   0                   1
#> 104          0          0          1                   0                   0
#> 105          0          0          0                   0                   0
#> 107          0          1          0                   0                   1
#> 108          0          0          1                   0                   0
#> 109          0          0          0                   0                   0
#> 110          1          0          0                   1                   0
#> 111          0          1          0                   0                   1
#> 112          0          0          1                   0                   0
#> 113          0          0          0                   0                   0
#> 114          1          0          0                   1                   0
#> 116          0          0          1                   0                   0
#> 117          0          0          0                   0                   0
#> 118          1          0          0                   1                   0
#> 119          0          1          0                   0                   1
#> 120          0          0          1                   0                   0
#> 121          0          0          0                   0                   0
#> 123          0          1          0                   0                   0
#> 125          0          0          0                   0                   0
#> 128          0          0          1                   0                   0
#> 129          0          0          0                   0                   0
#> 130          1          0          0                   1                   0
#> 132          0          0          1                   0                   0
#> 133          0          0          0                   0                   0
#> 134          1          0          0                   0                   0
#> 135          0          1          0                   0                   0
#> 136          0          0          1                   0                   0
#> 137          0          0          0                   0                   0
#> 138          1          0          0                   1                   0
#> 140          0          0          1                   0                   0
#> 142          1          0          0                   1                   0
#> 144          0          0          1                   0                   0
#> 145          0          0          0                   0                   0
#> 146          1          0          0                   0                   0
#> 147          0          1          0                   0                   0
#> 148          0          0          1                   0                   0
#> 149          0          0          0                   0                   0
#> 151          0          1          0                   0                   0
#> 153          0          0          0                   0                   0
#> 155          0          1          0                   0                   0
#> 156          0          0          1                   0                   0
#> 157          0          0          0                   0                   0
#> 158          1          0          0                   0                   0
#> 159          0          1          0                   0                   0
#> 162          1          0          0                   0                   0
#> 163          0          1          0                   0                   0
#> 164          0          0          1                   0                   0
#> 165          0          0          0                   0                   0
#> 168          0          0          1                   0                   0
#> 169          0          0          0                   0                   0
#> 170          1          0          0                   1                   0
#> 171          0          1          0                   0                   1
#> 172          0          0          1                   0                   0
#> 173          0          0          0                   0                   0
#> 177          0          0          0                   0                   0
#> 178          1          0          0                   0                   0
#> 179          0          1          0                   0                   0
#> 180          0          0          1                   0                   0
#> 181          0          0          0                   0                   0
#> 182          1          0          0                   0                   0
#> 183          0          1          0                   0                   0
#> 185          0          0          0                   0                   0
#> 186          1          0          0                   1                   0
#> 187          0          1          0                   0                   1
#> 190          1          0          0                   0                   0
#> 191          0          1          0                   0                   0
#> 193          0          0          0                   0                   0
#> 194          1          0          0                   0                   0
#> 195          0          1          0                   0                   0
#> 197          0          0          0                   0                   0
#> 198          1          0          0                   1                   0
#> 199          0          1          0                   0                   1
#> 201          0          0          0                   0                   0
#> 202          1          0          0                   1                   0
#> 204          0          0          1                   0                   0
#> 206          1          0          0                   1                   0
#> 208          0          0          1                   0                   0
#> 209          0          0          0                   0                   0
#> 210          1          0          0                   0                   0
#> 217          0          0          0                   0                   0
#> 218          1          0          0                   1                   0
#> 219          0          1          0                   0                   1
#> 221          0          0          0                   0                   0
#> 224          0          0          1                   0                   0
#> 226          1          0          0                   1                   0
#> 227          0          1          0                   0                   1
#> 228          0          0          1                   0                   0
#> 230          1          0          0                   1                   0
#> 231          0          1          0                   0                   1
#> 233          0          0          0                   0                   0
#> 235          0          1          0                   0                   1
#> 236          0          0          1                   0                   0
#> 237          0          0          0                   0                   0
#> 238          1          0          0                   1                   0
#> 239          0          1          0                   0                   1
#> 240          0          0          1                   0                   0
#> 241          0          0          0                   0                   0
#> 242          1          0          0                   1                   0
#> 244          0          0          1                   0                   0
#> 246          1          0          0                   0                   0
#> 250          1          0          0                   0                   0
#> 251          0          1          0                   0                   0
#> 252          0          0          1                   0                   0
#> 253          0          0          0                   0                   0
#> 254          1          0          0                   0                   0
#> 256          0          0          1                   0                   0
#> 257          0          0          0                   0                   0
#> 258          1          0          0                   1                   0
#> 259          0          1          0                   0                   1
#> 260          0          0          1                   0                   0
#> 261          0          0          0                   0                   0
#> 262          1          0          0                   1                   0
#> 263          0          1          0                   0                   1
#> 264          0          0          1                   0                   0
#> 265          0          0          0                   0                   0
#> 266          1          0          0                   1                   0
#> 267          0          1          0                   0                   1
#> 268          0          0          1                   0                   0
#> 269          0          0          0                   0                   0
#> 270          1          0          0                   1                   0
#> 273          0          0          0                   0                   0
#> 274          1          0          0                   0                   0
#> 275          0          1          0                   0                   0
#> 276          0          0          1                   0                   0
#> 277          0          0          0                   0                   0
#> 278          1          0          0                   0                   0
#> 280          0          0          1                   0                   0
#> 281          0          0          0                   0                   0
#> 282          1          0          0                   1                   0
#> 283          0          1          0                   0                   1
#> 284          0          0          1                   0                   0
#> 285          0          0          0                   0                   0
#> 286          1          0          0                   1                   0
#> 287          0          1          0                   0                   1
#> 291          0          1          0                   0                   0
#> 292          0          0          1                   0                   0
#> 293          0          0          0                   0                   0
#> 295          0          1          0                   0                   0
#> 296          0          0          1                   0                   0
#> 298          1          0          0                   0                   0
#> 299          0          1          0                   0                   0
#> 300          0          0          1                   0                   0
#> 301          0          0          0                   0                   0
#> 304          0          0          1                   0                   0
#> 305          0          0          0                   0                   0
#> 306          1          0          0                   1                   0
#> 307          0          1          0                   0                   1
#> 308          0          0          1                   0                   0
#> 310          1          0          0                   1                   0
#> 311          0          1          0                   0                   1
#> 312          0          0          1                   0                   0
#> 316          0          0          1                   0                   0
#> 317          0          0          0                   0                   0
#> 318          1          0          0                   1                   0
#> 319          0          1          0                   0                   1
#> 322          1          0          0                   0                   0
#> 323          0          1          0                   0                   0
#> 324          0          0          1                   0                   0
#> 325          0          0          0                   0                   0
#> 327          0          1          0                   0                   0
#> 328          0          0          1                   0                   0
#> 329          0          0          0                   0                   0
#> 330          1          0          0                   1                   0
#> 331          0          1          0                   0                   1
#> 332          0          0          1                   0                   0
#> 336          0          0          1                   0                   0
#> 339          0          1          0                   0                   0
#> 340          0          0          1                   0                   0
#> 341          0          0          0                   0                   0
#> 342          1          0          0                   1                   0
#> 343          0          1          0                   0                   1
#> 344          0          0          1                   0                   0
#> 345          0          0          0                   0                   0
#> 347          0          1          0                   0                   0
#> 349          0          0          0                   0                   0
#> 351          0          1          0                   0                   0
#> 352          0          0          1                   0                   0
#> 353          0          0          0                   0                   0
#> 354          1          0          0                   0                   0
#> 355          0          1          0                   0                   0
#> 356          0          0          1                   0                   0
#> 357          0          0          0                   0                   0
#> 363          0          1          0                   0                   1
#> 364          0          0          1                   0                   0
#> 365          0          0          0                   0                   0
#> 367          0          1          0                   0                   1
#> 368          0          0          1                   0                   0
#> 370          1          0          0                   0                   0
#> 371          0          1          0                   0                   0
#> 372          0          0          1                   0                   0
#> 373          0          0          0                   0                   0
#> 375          0          1          0                   0                   0
#> 376          0          0          1                   0                   0
#> 378          1          0          0                   0                   0
#> 379          0          1          0                   0                   0
#> 381          0          0          0                   0                   0
#> 382          1          0          0                   0                   0
#> 384          0          0          1                   0                   0
#> 385          0          0          0                   0                   0
#> 386          1          0          0                   1                   0
#> 388          0          0          1                   0                   0
#> 389          0          0          0                   0                   0
#> 390          1          0          0                   0                   0
#> 391          0          1          0                   0                   0
#> 392          0          0          1                   0                   0
#> 394          1          0          0                   0                   0
#> 397          0          0          0                   0                   0
#> 398          1          0          0                   0                   0
#> 399          0          1          0                   0                   0
#> 402          1          0          0                   1                   0
#> 403          0          1          0                   0                   1
#> 405          0          0          0                   0                   0
#> 406          1          0          0                   0                   0
#> 407          0          1          0                   0                   0
#> 408          0          0          1                   0                   0
#> 409          0          0          0                   0                   0
#> 410          1          0          0                   0                   0
#> 411          0          1          0                   0                   0
#> 413          0          0          0                   0                   0
#> 415          0          1          0                   0                   0
#> 416          0          0          1                   0                   0
#> 418          1          0          0                   1                   0
#> 419          0          1          0                   0                   1
#> 421          0          0          0                   0                   0
#> 422          1          0          0                   1                   0
#> 423          0          1          0                   0                   1
#> 424          0          0          1                   0                   0
#> 427          0          1          0                   0                   0
#> 428          0          0          1                   0                   0
#> 429          0          0          0                   0                   0
#> 430          1          0          0                   1                   0
#> 431          0          1          0                   0                   1
#> 432          0          0          1                   0                   0
#> 434          1          0          0                   0                   0
#> 435          0          1          0                   0                   0
#> 436          0          0          1                   0                   0
#> 438          1          0          0                   1                   0
#> 439          0          1          0                   0                   1
#> 444          0          0          1                   0                   0
#> 445          0          0          0                   0                   0
#> 447          0          1          0                   0                   0
#> 449          0          0          0                   0                   0
#> 450          1          0          0                   0                   0
#> 451          0          1          0                   0                   0
#> 453          0          0          0                   0                   0
#> 454          1          0          0                   0                   0
#> 455          0          1          0                   0                   0
#> 456          0          0          1                   0                   0
#> 457          0          0          0                   0                   0
#> 458          1          0          0                   0                   0
#> 459          0          1          0                   0                   0
#> 461          0          0          0                   0                   0
#> 463          0          1          0                   0                   1
#> 464          0          0          1                   0                   0
#> 465          0          0          0                   0                   0
#> 469          0          0          0                   0                   0
#> 470          1          0          0                   0                   0
#> 471          0          1          0                   0                   0
#> 473          0          0          0                   0                   0
#> 474          1          0          0                   1                   0
#> 477          0          0          0                   0                   0
#> 484          0          0          1                   0                   0
#> 487          0          1          0                   0                   0
#> 489          0          0          0                   0                   0
#> 490          1          0          0                   0                   0
#> 491          0          1          0                   0                   0
#> 494          1          0          0                   1                   0
#> 495          0          1          0                   0                   1
#> 496          0          0          1                   0                   0
#> 497          0          0          0                   0                   0
#> 498          1          0          0                   0                   0
#> 499          0          1          0                   0                   0
#> 501          0          0          0                   0                   0
#> 502          1          0          0                   1                   0
#> 504          0          0          1                   0                   0
#> 505          0          0          0                   0                   0
#> 508          0          0          1                   0                   0
#> 509          0          0          0                   0                   0
#> 510          1          0          0                   1                   0
#> 511          0          1          0                   0                   1
#> 512          0          0          1                   0                   0
#> 513          0          0          0                   0                   0
#> 518          1          0          0                   0                   0
#> 519          0          1          0                   0                   0
#> 521          0          0          0                   0                   0
#> 522          1          0          0                   0                   0
#> 523          0          1          0                   0                   0
#> 524          0          0          1                   0                   0
#> 526          1          0          0                   0                   0
#> 527          0          1          0                   0                   0
#> 528          0          0          1                   0                   0
#> 530          1          0          0                   0                   0
#> 531          0          1          0                   0                   0
#> 532          0          0          1                   0                   0
#> 534          1          0          0                   0                   0
#> 535          0          1          0                   0                   0
#> 536          0          0          1                   0                   0
#> 537          0          0          0                   0                   0
#> 538          1          0          0                   0                   0
#> 539          0          1          0                   0                   0
#> 540          0          0          1                   0                   0
#> 541          0          0          0                   0                   0
#> 544          0          0          1                   0                   0
#> 545          0          0          0                   0                   0
#> 546          1          0          0                   1                   0
#> 547          0          1          0                   0                   1
#> 548          0          0          1                   0                   0
#> 549          0          0          0                   0                   0
#> 550          1          0          0                   1                   0
#> 551          0          1          0                   0                   1
#> 555          0          1          0                   0                   1
#> 556          0          0          1                   0                   0
#> 557          0          0          0                   0                   0
#> 558          1          0          0                   0                   0
#> 560          0          0          1                   0                   0
#> 562          1          0          0                   1                   0
#> 564          0          0          1                   0                   0
#> 569          0          0          0                   0                   0
#> 570          1          0          0                   1                   0
#> 572          0          0          1                   0                   0
#> 573          0          0          0                   0                   0
#> 574          1          0          0                   0                   0
#> 575          0          1          0                   0                   0
#> 576          0          0          1                   0                   0
#> 577          0          0          0                   0                   0
#> 578          1          0          0                   0                   0
#> 579          0          1          0                   0                   0
#> 582          1          0          0                   1                   0
#> 583          0          1          0                   0                   1
#> 584          0          0          1                   0                   0
#> 585          0          0          0                   0                   0
#> 586          1          0          0                   0                   0
#> 587          0          1          0                   0                   0
#> 590          1          0          0                   1                   0
#> 591          0          1          0                   0                   1
#> 593          0          0          0                   0                   0
#> 594          1          0          0                   0                   0
#> 595          0          1          0                   0                   0
#> 596          0          0          1                   0                   0
#> 599          0          1          0                   0                   0
#> 600          0          0          1                   0                   0
#> 601          0          0          0                   0                   0
#> 602          1          0          0                   0                   0
#> 604          0          0          1                   0                   0
#> 606          1          0          0                   1                   0
#> 608          0          0          1                   0                   0
#> 609          0          0          0                   0                   0
#> 610          1          0          0                   0                   0
#> 611          0          1          0                   0                   0
#> 612          0          0          1                   0                   0
#> 613          0          0          0                   0                   0
#> 614          1          0          0                   0                   0
#> 616          0          0          1                   0                   0
#> 617          0          0          0                   0                   0
#> 619          0          1          0                   0                   1
#> 620          0          0          1                   0                   0
#> 621          0          0          0                   0                   0
#> 622          1          0          0                   0                   0
#> 623          0          1          0                   0                   0
#> 624          0          0          1                   0                   0
#> 625          0          0          0                   0                   0
#> 628          0          0          1                   0                   0
#> 630          1          0          0                   1                   0
#> 631          0          1          0                   0                   1
#> 632          0          0          1                   0                   0
#> 633          0          0          0                   0                   0
#> 634          1          0          0                   1                   0
#> 638          1          0          0                   1                   0
#> 639          0          1          0                   0                   1
#> 640          0          0          1                   0                   0
#> 642          1          0          0                   0                   0
#> 645          0          0          0                   0                   0
#> 648          0          0          1                   0                   0
#> 650          1          0          0                   0                   0
#> 652          0          0          1                   0                   0
#> 654          1          0          0                   0                   0
#> 655          0          1          0                   0                   0
#> 656          0          0          1                   0                   0
#> 657          0          0          0                   0                   0
#> 661          0          0          0                   0                   0
#> 665          0          0          0                   0                   0
#> 666          1          0          0                   0                   0
#> 668          0          0          1                   0                   0
#> 669          0          0          0                   0                   0
#> 670          1          0          0                   0                   0
#> 671          0          1          0                   0                   0
#> 673          0          0          0                   0                   0
#> 674          1          0          0                   0                   0
#> 678          1          0          0                   1                   0
#> 679          0          1          0                   0                   1
#> 680          0          0          1                   0                   0
#> 682          1          0          0                   0                   0
#> 684          0          0          1                   0                   0
#> 685          0          0          0                   0                   0
#> 686          1          0          0                   1                   0
#> 687          0          1          0                   0                   1
#> 689          0          0          0                   0                   0
#> 690          1          0          0                   1                   0
#> 691          0          1          0                   0                   1
#> 693          0          0          0                   0                   0
#> 694          1          0          0                   1                   0
#> 695          0          1          0                   0                   1
#> 697          0          0          0                   0                   0
#> 698          1          0          0                   0                   0
#> 700          0          0          1                   0                   0
#> 701          0          0          0                   0                   0
#> 702          1          0          0                   1                   0
#> 704          0          0          1                   0                   0
#> 707          0          1          0                   0                   0
#> 709          0          0          0                   0                   0
#> 712          0          0          1                   0                   0
#> 713          0          0          0                   0                   0
#> 715          0          1          0                   0                   1
#> 716          0          0          1                   0                   0
#> 717          0          0          0                   0                   0
#> 718          1          0          0                   0                   0
#> 719          0          1          0                   0                   0
#> 721          0          0          0                   0                   0
#> 723          0          1          0                   0                   0
#> 724          0          0          1                   0                   0
#> 727          0          1          0                   0                   1
#> 728          0          0          1                   0                   0
#> 729          0          0          0                   0                   0
#> 730          1          0          0                   1                   0
#> 734          1          0          0                   1                   0
#> 736          0          0          1                   0                   0
#> 738          1          0          0                   0                   0
#> 739          0          1          0                   0                   0
#> 740          0          0          1                   0                   0
#> 741          0          0          0                   0                   0
#> 742          1          0          0                   0                   0
#> 744          0          0          1                   0                   0
#> 745          0          0          0                   0                   0
#> 749          0          0          0                   0                   0
#> 751          0          1          0                   0                   0
#> 753          0          0          0                   0                   0
#> 754          1          0          0                   1                   0
#> 755          0          1          0                   0                   1
#> 757          0          0          0                   0                   0
#> 758          1          0          0                   1                   0
#> 759          0          1          0                   0                   1
#> 760          0          0          1                   0                   0
#> 761          0          0          0                   0                   0
#> 764          0          0          1                   0                   0
#> 765          0          0          0                   0                   0
#> 766          1          0          0                   1                   0
#> 767          0          1          0                   0                   1
#> 768          0          0          1                   0                   0
#> 770          1          0          0                   0                   0
#> 771          0          1          0                   0                   0
#> 772          0          0          1                   0                   0
#> 773          0          0          0                   0                   0
#> 774          1          0          0                   0                   0
#> 775          0          1          0                   0                   0
#> 776          0          0          1                   0                   0
#> 778          1          0          0                   1                   0
#> 779          0          1          0                   0                   1
#> 781          0          0          0                   0                   0
#> 782          1          0          0                   0                   0
#> 784          0          0          1                   0                   0
#> 788          0          0          1                   0                   0
#> 789          0          0          0                   0                   0
#> 790          1          0          0                   1                   0
#> 792          0          0          1                   0                   0
#> 797          0          0          0                   0                   0
#> 798          1          0          0                   0                   0
#> 800          0          0          1                   0                   0
#>     ARMCDTRT:AVISITVIS4
#> 2                     0
#> 4                     1
#> 6                     0
#> 7                     0
#> 8                     0
#> 10                    0
#> 12                    0
#> 13                    0
#> 14                    0
#> 16                    1
#> 17                    0
#> 19                    0
#> 20                    0
#> 23                    0
#> 25                    0
#> 26                    0
#> 28                    0
#> 29                    0
#> 30                    0
#> 31                    0
#> 32                    0
#> 33                    0
#> 34                    0
#> 36                    1
#> 39                    0
#> 41                    0
#> 42                    0
#> 43                    0
#> 44                    1
#> 45                    0
#> 46                    0
#> 47                    0
#> 51                    0
#> 52                    1
#> 55                    0
#> 59                    0
#> 60                    0
#> 62                    0
#> 64                    0
#> 65                    0
#> 68                    0
#> 69                    0
#> 70                    0
#> 72                    1
#> 73                    0
#> 74                    0
#> 75                    0
#> 76                    1
#> 78                    0
#> 79                    0
#> 82                    0
#> 83                    0
#> 84                    1
#> 85                    0
#> 86                    0
#> 87                    0
#> 88                    1
#> 89                    0
#> 90                    0
#> 91                    0
#> 93                    0
#> 94                    0
#> 95                    0
#> 96                    1
#> 97                    0
#> 98                    0
#> 99                    0
#> 100                   1
#> 101                   0
#> 102                   0
#> 103                   0
#> 104                   1
#> 105                   0
#> 107                   0
#> 108                   1
#> 109                   0
#> 110                   0
#> 111                   0
#> 112                   1
#> 113                   0
#> 114                   0
#> 116                   1
#> 117                   0
#> 118                   0
#> 119                   0
#> 120                   1
#> 121                   0
#> 123                   0
#> 125                   0
#> 128                   1
#> 129                   0
#> 130                   0
#> 132                   1
#> 133                   0
#> 134                   0
#> 135                   0
#> 136                   0
#> 137                   0
#> 138                   0
#> 140                   1
#> 142                   0
#> 144                   1
#> 145                   0
#> 146                   0
#> 147                   0
#> 148                   0
#> 149                   0
#> 151                   0
#> 153                   0
#> 155                   0
#> 156                   0
#> 157                   0
#> 158                   0
#> 159                   0
#> 162                   0
#> 163                   0
#> 164                   0
#> 165                   0
#> 168                   0
#> 169                   0
#> 170                   0
#> 171                   0
#> 172                   1
#> 173                   0
#> 177                   0
#> 178                   0
#> 179                   0
#> 180                   0
#> 181                   0
#> 182                   0
#> 183                   0
#> 185                   0
#> 186                   0
#> 187                   0
#> 190                   0
#> 191                   0
#> 193                   0
#> 194                   0
#> 195                   0
#> 197                   0
#> 198                   0
#> 199                   0
#> 201                   0
#> 202                   0
#> 204                   1
#> 206                   0
#> 208                   1
#> 209                   0
#> 210                   0
#> 217                   0
#> 218                   0
#> 219                   0
#> 221                   0
#> 224                   0
#> 226                   0
#> 227                   0
#> 228                   1
#> 230                   0
#> 231                   0
#> 233                   0
#> 235                   0
#> 236                   1
#> 237                   0
#> 238                   0
#> 239                   0
#> 240                   1
#> 241                   0
#> 242                   0
#> 244                   1
#> 246                   0
#> 250                   0
#> 251                   0
#> 252                   0
#> 253                   0
#> 254                   0
#> 256                   0
#> 257                   0
#> 258                   0
#> 259                   0
#> 260                   1
#> 261                   0
#> 262                   0
#> 263                   0
#> 264                   1
#> 265                   0
#> 266                   0
#> 267                   0
#> 268                   1
#> 269                   0
#> 270                   0
#> 273                   0
#> 274                   0
#> 275                   0
#> 276                   0
#> 277                   0
#> 278                   0
#> 280                   0
#> 281                   0
#> 282                   0
#> 283                   0
#> 284                   1
#> 285                   0
#> 286                   0
#> 287                   0
#> 291                   0
#> 292                   0
#> 293                   0
#> 295                   0
#> 296                   0
#> 298                   0
#> 299                   0
#> 300                   0
#> 301                   0
#> 304                   1
#> 305                   0
#> 306                   0
#> 307                   0
#> 308                   1
#> 310                   0
#> 311                   0
#> 312                   1
#> 316                   0
#> 317                   0
#> 318                   0
#> 319                   0
#> 322                   0
#> 323                   0
#> 324                   0
#> 325                   0
#> 327                   0
#> 328                   0
#> 329                   0
#> 330                   0
#> 331                   0
#> 332                   1
#> 336                   0
#> 339                   0
#> 340                   0
#> 341                   0
#> 342                   0
#> 343                   0
#> 344                   1
#> 345                   0
#> 347                   0
#> 349                   0
#> 351                   0
#> 352                   0
#> 353                   0
#> 354                   0
#> 355                   0
#> 356                   0
#> 357                   0
#> 363                   0
#> 364                   1
#> 365                   0
#> 367                   0
#> 368                   1
#> 370                   0
#> 371                   0
#> 372                   0
#> 373                   0
#> 375                   0
#> 376                   0
#> 378                   0
#> 379                   0
#> 381                   0
#> 382                   0
#> 384                   0
#> 385                   0
#> 386                   0
#> 388                   1
#> 389                   0
#> 390                   0
#> 391                   0
#> 392                   0
#> 394                   0
#> 397                   0
#> 398                   0
#> 399                   0
#> 402                   0
#> 403                   0
#> 405                   0
#> 406                   0
#> 407                   0
#> 408                   0
#> 409                   0
#> 410                   0
#> 411                   0
#> 413                   0
#> 415                   0
#> 416                   0
#> 418                   0
#> 419                   0
#> 421                   0
#> 422                   0
#> 423                   0
#> 424                   1
#> 427                   0
#> 428                   0
#> 429                   0
#> 430                   0
#> 431                   0
#> 432                   1
#> 434                   0
#> 435                   0
#> 436                   0
#> 438                   0
#> 439                   0
#> 444                   0
#> 445                   0
#> 447                   0
#> 449                   0
#> 450                   0
#> 451                   0
#> 453                   0
#> 454                   0
#> 455                   0
#> 456                   0
#> 457                   0
#> 458                   0
#> 459                   0
#> 461                   0
#> 463                   0
#> 464                   1
#> 465                   0
#> 469                   0
#> 470                   0
#> 471                   0
#> 473                   0
#> 474                   0
#> 477                   0
#> 484                   1
#> 487                   0
#> 489                   0
#> 490                   0
#> 491                   0
#> 494                   0
#> 495                   0
#> 496                   1
#> 497                   0
#> 498                   0
#> 499                   0
#> 501                   0
#> 502                   0
#> 504                   1
#> 505                   0
#> 508                   1
#> 509                   0
#> 510                   0
#> 511                   0
#> 512                   1
#> 513                   0
#> 518                   0
#> 519                   0
#> 521                   0
#> 522                   0
#> 523                   0
#> 524                   0
#> 526                   0
#> 527                   0
#> 528                   0
#> 530                   0
#> 531                   0
#> 532                   0
#> 534                   0
#> 535                   0
#> 536                   0
#> 537                   0
#> 538                   0
#> 539                   0
#> 540                   0
#> 541                   0
#> 544                   1
#> 545                   0
#> 546                   0
#> 547                   0
#> 548                   1
#> 549                   0
#> 550                   0
#> 551                   0
#> 555                   0
#> 556                   1
#> 557                   0
#> 558                   0
#> 560                   0
#> 562                   0
#> 564                   1
#> 569                   0
#> 570                   0
#> 572                   1
#> 573                   0
#> 574                   0
#> 575                   0
#> 576                   0
#> 577                   0
#> 578                   0
#> 579                   0
#> 582                   0
#> 583                   0
#> 584                   1
#> 585                   0
#> 586                   0
#> 587                   0
#> 590                   0
#> 591                   0
#> 593                   0
#> 594                   0
#> 595                   0
#> 596                   0
#> 599                   0
#> 600                   0
#> 601                   0
#> 602                   0
#> 604                   0
#> 606                   0
#> 608                   1
#> 609                   0
#> 610                   0
#> 611                   0
#> 612                   0
#> 613                   0
#> 614                   0
#> 616                   0
#> 617                   0
#> 619                   0
#> 620                   1
#> 621                   0
#> 622                   0
#> 623                   0
#> 624                   0
#> 625                   0
#> 628                   1
#> 630                   0
#> 631                   0
#> 632                   1
#> 633                   0
#> 634                   0
#> 638                   0
#> 639                   0
#> 640                   1
#> 642                   0
#> 645                   0
#> 648                   0
#> 650                   0
#> 652                   0
#> 654                   0
#> 655                   0
#> 656                   0
#> 657                   0
#> 661                   0
#> 665                   0
#> 666                   0
#> 668                   0
#> 669                   0
#> 670                   0
#> 671                   0
#> 673                   0
#> 674                   0
#> 678                   0
#> 679                   0
#> 680                   1
#> 682                   0
#> 684                   0
#> 685                   0
#> 686                   0
#> 687                   0
#> 689                   0
#> 690                   0
#> 691                   0
#> 693                   0
#> 694                   0
#> 695                   0
#> 697                   0
#> 698                   0
#> 700                   0
#> 701                   0
#> 702                   0
#> 704                   1
#> 707                   0
#> 709                   0
#> 712                   0
#> 713                   0
#> 715                   0
#> 716                   1
#> 717                   0
#> 718                   0
#> 719                   0
#> 721                   0
#> 723                   0
#> 724                   0
#> 727                   0
#> 728                   1
#> 729                   0
#> 730                   0
#> 734                   0
#> 736                   1
#> 738                   0
#> 739                   0
#> 740                   0
#> 741                   0
#> 742                   0
#> 744                   0
#> 745                   0
#> 749                   0
#> 751                   0
#> 753                   0
#> 754                   0
#> 755                   0
#> 757                   0
#> 758                   0
#> 759                   0
#> 760                   1
#> 761                   0
#> 764                   1
#> 765                   0
#> 766                   0
#> 767                   0
#> 768                   1
#> 770                   0
#> 771                   0
#> 772                   0
#> 773                   0
#> 774                   0
#> 775                   0
#> 776                   0
#> 778                   0
#> 779                   0
#> 781                   0
#> 782                   0
#> 784                   0
#> 788                   0
#> 789                   0
#> 790                   0
#> 792                   1
#> 797                   0
#> 798                   0
#> 800                   0
#> attr(,"assign")
#>  [1] 0 1 1 2 3 4 4 4 5 5 5
#> attr(,"contrasts")
#> attr(,"contrasts")$RACE
#>                           Black or African American White
#> Asian                                             0     0
#> Black or African American                         1     0
#> White                                             0     1
#> 
#> attr(,"contrasts")$SEX
#>        Female
#> Male        0
#> Female      1
#> 
#> attr(,"contrasts")$ARMCD
#>     TRT
#> PBO   0
#> TRT   1
#> 
#> attr(,"contrasts")$AVISIT
#>      VIS2 VIS3 VIS4
#> VIS1    0    0    0
#> VIS2    1    0    0
#> VIS3    0    1    0
#> VIS4    0    0    1
#> 
#> 
#> $y_vector
#>   [1] 39.97105 20.48379 31.45522 36.87889 48.80809 35.98699 37.16444 33.89229
#>   [9] 33.74637 54.45055 32.31386 46.79361 41.71154 39.02423 31.93050 32.90947
#>  [17] 48.28031 32.23021 35.91080 45.54898 53.02877 47.16898 46.64287 58.09713
#>  [25] 44.97613 44.32755 38.97813 43.72862 46.43393 40.34576 42.76568 40.11155
#>  [33] 53.31791 56.07641 41.90837 34.65663 39.07791 35.89612 47.67264 22.65440
#>  [41] 40.85376 32.60048 33.64329 40.92278 32.14831 46.43604 41.34973 66.30382
#>  [49] 47.95358 53.97364 56.64544 49.70872 60.40497 45.98525 51.90911 41.50787
#>  [57] 53.42727 23.86859 35.98563 43.60626 29.59773 35.50688 55.42944 52.10530
#>  [65] 31.69644 32.16159 51.04735 55.85987 49.11706 49.25544 51.72211 69.99128
#>  [73] 22.07169 46.08393 52.42288 37.69466 44.59400 52.08897 58.22961 37.22824
#>  [81] 34.39863 36.34012 45.44182 41.54847 43.92172 61.83243 27.25656 45.65133
#>  [89] 33.19334 41.66826 27.12753 31.74858 41.60000 39.45250 32.61823 34.62445
#>  [97] 45.90515 36.17780 39.79796 50.08272 44.64316 39.73529 34.06164 40.18592
#> [105] 41.17584 57.76669 38.18460 47.19893 37.32785 43.16048 41.40349 30.15733
#> [113] 35.84353 40.95250 41.37928 50.17316 45.35226 39.06491 42.11960 29.81042
#> [121] 42.57055 47.81652 68.06024 35.62071 33.89134 36.42808 37.57519 58.46873
#> [129] 19.54516 31.13541 40.89955 22.18809 41.05857 37.32452 43.12432 41.99349
#> [137] 44.03080 38.66417 53.45993 29.81948 30.43859 40.18095 26.78578 34.55115
#> [145] 40.06421 43.09329 45.71567 40.74992 44.74635 40.14674 48.75859 46.43462
#> [153] 29.33990 47.93165 41.11632 47.05889 52.24599 54.14236 50.44618 37.53657
#> [161] 49.45840 59.12866 40.31268 39.66049 50.89726 56.13116 32.82981 46.53837
#> [169] 51.81265 29.91939 51.05656 50.50059 64.11388 32.21843 29.64732 45.09919
#> [177] 39.75659 37.28894 44.80145 65.95920 33.43439 33.57042 39.91543 49.57098
#> [185] 38.91634 36.69011 45.66665 52.07431 42.21411 45.02901 30.98338 44.72932
#> [193] 40.68711 34.71530 27.30752 37.31585 44.83000 32.93042 44.91911 45.68636
#> [201] 65.98800 46.60130 40.89786 46.66708 43.83270 44.11604 38.29612 51.38570
#> [209] 56.20979 43.45819 38.38741 56.42818 39.05050 54.09200 31.40521 46.13330
#> [217] 45.29845 28.06936 42.50283 46.45368 64.97366 43.97847 35.33466 39.34378
#> [225] 41.27633 39.83058 43.49673 44.06114 41.43742 46.16954 54.24024 36.61831
#> [233] 42.09272 50.69556 51.72563 53.89947 39.94420 56.42482 41.86385 34.56420
#> [241] 38.68927 62.88743 28.85343 49.29495 28.74029 43.59994 57.38616 35.36824
#> [249] 43.06110 31.27551 54.13245 25.97050 51.17493 48.44043 43.33128 55.93546
#> [257] 54.15312 40.60252 44.44715 40.54161 33.95563 43.67802 42.76023 42.82678
#> [265] 39.59218 33.49216 35.39266 42.36266 48.54368 43.94366 47.91204 20.72928
#> [273] 28.00599 40.19255 37.79360 36.75177 34.59822 39.32034 40.65702 43.03255
#> [281] 54.65715 35.55742 43.70215 42.52157 54.89337 32.03460 29.45107 45.35138
#> [289] 38.73784 41.42283 47.32385 47.55310 49.06509 29.22591 40.08175 45.68142
#> [297] 41.47403 42.51970 69.36099 42.39760 43.72376 49.47601 51.94188 40.59100
#> [305] 39.97833 31.69049 37.20517 46.28740 41.58720 32.17365 40.69375 32.28771
#> [313] 41.76205 40.06768 29.14213 39.50989 43.32349 47.16756 40.93020 42.19406
#> [321] 41.21057 38.54330 43.96324 42.67652 22.79584 31.43559 38.85064 48.24288
#> [329] 44.71302 51.85370 30.56757 59.90473 49.76150 47.21985 40.34525 48.29793
#> [337] 44.39634 41.71421 47.37535 42.03797 37.56100 45.11793 34.62530 45.28206
#> [345] 63.57761 35.80878 52.67314 35.88734 38.73222 46.70361 53.65398 36.71543
#> [353] 41.54317 51.67909 27.40130 30.33517 37.73092 29.11668 32.08830 41.66067
#> [361] 53.90815 35.06937 47.17615 56.49347 38.88006 47.54070 43.53705 31.82054
#> [369] 39.62816 44.95543 21.11543 34.74671 56.69249 22.73126 32.50075 42.37206
#> [377] 42.89847 55.62582 45.38998 52.66743 34.18931 45.59740 28.89198 38.46147
#> [385] 49.90357 44.14167 55.24278 27.38001 33.63251 39.34410 26.98575 24.04175
#> [393] 42.16648 44.75380 31.55469 44.42696 44.10343 37.87445 48.31828 50.21520
#> [401] 41.94615 39.62690 46.69763 43.75255 47.38873 32.43412 43.07163 42.99551
#> [409] 53.82759 50.64802 63.44051 34.48949 40.08056 47.46553 37.11697 36.25120
#> [417] 29.20171 31.53773 42.35683 64.78352 32.72757 37.50022 57.03861 36.32475
#> [425] 41.46725 59.01411 30.14970 34.91740 52.13900 58.73839 35.83185 56.41409
#> [433] 43.55593 44.26320 59.25579 28.47314 47.47581 46.47483 51.22677 45.82777
#> [441] 39.06783 29.99542 54.17796 44.55743 62.59579 35.48396 44.07768 46.57837
#> [449] 47.67979 22.15439 34.27765 36.90059 40.54285 29.09494 37.21768 43.08491
#> [457] 27.12174 34.11916 40.80230 45.89269 43.69153 29.22869 55.68362 31.90698
#> [465] 37.31061 40.75546 42.19474 44.87228 47.55198 50.62894 45.47551 48.62168
#> [473] 29.66493 34.57406 38.11676 33.77204 34.26148 58.81037 39.88119 31.62708
#> [481] 48.22049 42.58829 49.33262 53.74331 29.71857 30.45651 38.29800 36.81040
#> [489] 42.35045 39.39860 49.73629 41.58082 43.58901 40.16762 41.08206 69.37409
#> [497] 41.27625 44.76138 39.69815 38.44296 48.20586 35.50735 32.08153 44.69256
#> [505] 42.18689 37.01741 38.26920 49.28806 40.45953 45.10337 45.58250 62.96989
#> [513] 30.78252 44.69667 32.72491 45.78702 48.74886 84.08449 30.19495 36.78573
#> [521] 61.03588 20.36749 35.22480 37.42847 30.20501 49.12862 47.31234 19.28388
#> [529] 30.00682 49.21768 40.13353 42.34534 52.32575 69.26254 35.70341 41.64454
#> [537] 54.25081
#> 
#> $neg_log_lik
#> [1] 1693.225
#> 
#> $jac_list
#> $jac_list[[1]]
#>              [,1]         [,2]          [,3]         [,4]        [,5]
#>  [1,]  1.16951547 -0.051411669 -0.0450053577 -0.029651633 -1.11563537
#>  [2,] -0.05141167  0.087578417  0.0345764451  0.007196977  0.00260112
#>  [3,] -0.04500536  0.034576445  0.0850344790  0.007128258 -0.01178718
#>  [4,] -0.02965163  0.007196977  0.0071282583  0.061521074 -0.01065994
#>  [5,] -1.11563537  0.002601120 -0.0117871834 -0.010659937  2.28786967
#>  [6,] -0.98171081  0.004118345  0.0089518223 -0.003655354  0.97932241
#>  [7,] -1.07753684  0.005790797  0.0100023530 -0.006895078  1.07595394
#>  [8,] -0.99821296 -0.001933165  0.0086761892 -0.005754318  0.99903467
#>  [9,]  0.97217159  0.008520375  0.0132545932  0.002102199 -1.98758938
#> [10,]  1.06909044  0.008539116  0.0053378784  0.004325848 -2.18658687
#> [11,]  0.99545698  0.001249297  0.0000785895  0.008328731 -2.01758658
#>               [,6]         [,7]         [,8]         [,9]        [,10]
#>  [1,] -0.981710811 -1.077536844 -0.998212960  0.972171586  1.069090436
#>  [2,]  0.004118345  0.005790797 -0.001933165  0.008520375  0.008539116
#>  [3,]  0.008951822  0.010002353  0.008676189  0.013254593  0.005337878
#>  [4,] -0.003655354 -0.006895078 -0.005754318  0.002102199  0.004325848
#>  [5,]  0.979322409  1.075953942  0.999034668 -1.987589379 -2.186586867
#>  [6,]  0.836813950  0.933379638  0.857077689 -0.836557168 -0.933038454
#>  [7,]  0.933379638  1.029968833  0.953510481 -0.932874939 -1.029216841
#>  [8,]  0.857077689  0.953510481  0.877307817 -0.857056128 -0.953603898
#>  [9,] -0.836557168 -0.932874939 -0.857056128  1.689099664  1.888995410
#> [10,] -0.933038454 -1.029216841 -0.953603898  1.888995410  2.089042080
#> [11,] -0.857161705 -0.953646838 -0.877357597  1.718938218  1.919022103
#>               [,11]
#>  [1,]  0.9954569814
#>  [2,]  0.0012492968
#>  [3,]  0.0000785895
#>  [4,]  0.0083287309
#>  [5,] -2.0175865751
#>  [6,] -0.8571617054
#>  [7,] -0.9536468379
#>  [8,] -0.8773575973
#>  [9,]  1.7189382180
#> [10,]  1.9190221026
#> [11,]  1.7497813115
#> 
#> $jac_list[[2]]
#>                [,1]         [,2]          [,3]         [,4]          [,5]
#>  [1,]  0.1275306312 -0.129971302 -0.0910717692 -0.098349416 -0.0096292730
#>  [2,] -0.1299713021  0.223312665  0.1008449781  0.022808956  0.0192625868
#>  [3,] -0.0910717692  0.100844978  0.2411274630  0.008554981 -0.0182023655
#>  [4,] -0.0983494163  0.022808956  0.0085549805  0.158588894  0.0074601050
#>  [5,] -0.0096292730  0.019262587 -0.0182023655  0.007460105  0.0052924415
#>  [6,]  0.1435399949 -0.001146971 -0.0062318230  0.002417212 -0.1423316326
#>  [7,] -0.0054058828  0.005303578 -0.0057570500  0.007567400  0.0017862783
#>  [8,] -0.0009468737 -0.001369925 -0.0089479596  0.005898220  0.0012090248
#>  [9,] -0.1357904146 -0.003851407 -0.0054496317 -0.008289146  0.2986740317
#> [10,]  0.0090279320  0.004055441 -0.0009261889 -0.019591078  0.0003322176
#> [11,]  0.0105498524 -0.009420670 -0.0019295670 -0.011019805 -0.0012298550
#>               [,6]          [,7]          [,8]         [,9]         [,10]
#>  [1,]  0.143539995 -0.0054058828 -0.0009468737 -0.135790415  0.0090279320
#>  [2,] -0.001146971  0.0053035777 -0.0013699252 -0.003851407  0.0040554409
#>  [3,] -0.006231823 -0.0057570500 -0.0089479596 -0.005449632 -0.0009261889
#>  [4,]  0.002417212  0.0075673997  0.0058982201 -0.008289146 -0.0195910778
#>  [5,] -0.142331633  0.0017862783  0.0012090248  0.298674032  0.0003322176
#>  [6,]  0.448003739 -0.1162181844 -0.0706352008 -0.448120908  0.1161018057
#>  [7,] -0.116218184  0.0006208091  0.0004239333  0.115940070 -0.0006363702
#>  [8,] -0.070635201  0.0004239333  0.0004185595  0.070507365 -0.0006302980
#>  [9,] -0.448120908  0.1159400700  0.0705073652  0.858765561 -0.2400454621
#> [10,]  0.116101806 -0.0006363702 -0.0006302980 -0.240045462  0.0023653114
#> [11,]  0.070632348 -0.0007028853 -0.0003653047 -0.146687106  0.0008087297
#>               [,11]
#>  [1,]  0.0105498524
#>  [2,] -0.0094206701
#>  [3,] -0.0019295670
#>  [4,] -0.0110198053
#>  [5,] -0.0012298550
#>  [6,]  0.0706323476
#>  [7,] -0.0007028853
#>  [8,] -0.0003653047
#>  [9,] -0.1466871056
#> [10,]  0.0008087297
#> [11,]  0.0008254937
#> 
#> $jac_list[[3]]
#>               [,1]         [,2]         [,3]         [,4]         [,5]
#>  [1,]  0.245560091 -0.241005288 -0.199099088 -0.186929231 -0.007083200
#>  [2,] -0.241005288  0.415633400  0.207029802  0.031893969  0.028836257
#>  [3,] -0.199099088  0.207029802  0.500748613  0.030611285 -0.048411146
#>  [4,] -0.186929231  0.031893969  0.030611285  0.306670056  0.005230923
#>  [5,] -0.007083200  0.028836257 -0.048411146  0.005230923  0.011939767
#>  [6,] -0.006054804  0.001585372 -0.007248864  0.012399309  0.001516357
#>  [7,]  0.038517950  0.007606844 -0.008011985  0.011376159 -0.044066774
#>  [8,] -0.002604366  0.001058044 -0.013637332  0.008572980  0.002399258
#>  [9,]  0.007160701  0.005781258  0.015632421 -0.023181005 -0.001960056
#> [10,] -0.036322393  0.009928402  0.006711191 -0.029277060  0.097958291
#> [11,]  0.012868838 -0.013894309  0.004513448 -0.013783909 -0.002935595
#>                [,6]         [,7]          [,8]         [,9]        [,10]
#>  [1,] -0.0060548037  0.038517950 -0.0026043657  0.007160701 -0.036322393
#>  [2,]  0.0015853720  0.007606844  0.0010580439  0.005781258  0.009928402
#>  [3,] -0.0072488639 -0.008011985 -0.0136373324  0.015632421  0.006711191
#>  [4,]  0.0123993094  0.011376159  0.0085729799 -0.023181005 -0.029277060
#>  [5,]  0.0015163570 -0.044066774  0.0023992582 -0.001960056  0.097958291
#>  [6,]  0.0005531212 -0.019244874  0.0005443284 -0.001131917  0.018650673
#>  [7,] -0.0192448740  0.322767842 -0.0369215897  0.018771762 -0.322953100
#>  [8,]  0.0005443284 -0.036921590  0.0006655006 -0.001070581  0.036691455
#>  [9,] -0.0011319169  0.018771762 -0.0010705809  0.002312297 -0.037787975
#> [10,]  0.0186506731 -0.322953100  0.0366914553 -0.037787975  0.729050666
#> [11,] -0.0007378620  0.036578230 -0.0007538174  0.001139903 -0.078301890
#>               [,11]
#>  [1,]  0.0128688383
#>  [2,] -0.0138943092
#>  [3,]  0.0045134481
#>  [4,] -0.0137839090
#>  [5,] -0.0029355953
#>  [6,] -0.0007378620
#>  [7,]  0.0365782297
#>  [8,] -0.0007538174
#>  [9,]  0.0011399033
#> [10,] -0.0783018897
#> [11,]  0.0012934309
#> 
#> $jac_list[[4]]
#>               [,1]          [,2]          [,3]          [,4]          [,5]
#>  [1,]  0.029388061 -0.0300363972 -0.0190464016 -0.0219001535 -0.0026139703
#>  [2,] -0.030036397  0.0534144734  0.0204813833  0.0011759493  0.0060482952
#>  [3,] -0.019046402  0.0204813833  0.0591610472  0.0004350294 -0.0075372942
#>  [4,] -0.021900153  0.0011759493  0.0004350294  0.0391623541  0.0015980966
#>  [5,] -0.002613970  0.0060482952 -0.0075372942  0.0015980966  0.0024815727
#>  [6,] -0.001287289  0.0013882825 -0.0017696952  0.0017825394  0.0005500661
#>  [7,] -0.001819822  0.0029497206 -0.0021392898  0.0014943282  0.0008816109
#>  [8,]  0.120475742  0.0066564090 -0.0025523369 -0.0005390796 -0.1213358437
#>  [9,]  0.001514454  0.0003149104  0.0035339826 -0.0042346457 -0.0006413058
#> [10,]  0.001998149  0.0003181908  0.0023183532 -0.0048503189 -0.0004152991
#> [11,] -0.121026181 -0.0031128838  0.0032728586 -0.0016022973  0.2665382626
#>                [,6]          [,7]          [,8]          [,9]         [,10]
#>  [1,] -0.0012872891 -0.0018198219  0.1204757422  0.0015144537  0.0019981493
#>  [2,]  0.0013882825  0.0029497206  0.0066564090  0.0003149104  0.0003181908
#>  [3,] -0.0017696952 -0.0021392898 -0.0025523369  0.0035339826  0.0023183532
#>  [4,]  0.0017825394  0.0014943282 -0.0005390796 -0.0042346457 -0.0048503189
#>  [5,]  0.0005500661  0.0008816109 -0.1213358437 -0.0006413058 -0.0004152991
#>  [6,]  0.0001286007  0.0001813006 -0.0503065912 -0.0002302344 -0.0001918273
#>  [7,]  0.0001813006  0.0002904376 -0.1134126363 -0.0002434026 -0.0001634397
#>  [8,] -0.0503065912 -0.1134126363  2.5685656977  0.0503712957  0.1139567512
#>  [9,] -0.0002302344 -0.0002434026  0.0503712957  0.0005410877  0.0005376701
#> [10,] -0.0001918273 -0.0001634397  0.1139567512  0.0005376701  0.0005457575
#> [11,]  0.0502540493  0.1134358458 -2.5686970395 -0.1159679830 -0.2486909707
#>              [,11]
#>  [1,] -0.121026181
#>  [2,] -0.003112884
#>  [3,]  0.003272859
#>  [4,] -0.001602297
#>  [5,]  0.266538263
#>  [6,]  0.050254049
#>  [7,]  0.113435846
#>  [8,] -2.568697039
#>  [9,] -0.115967983
#> [10,] -0.248690971
#> [11,]  5.099408635
#> 
#> $jac_list[[5]]
#>               [,1]          [,2]          [,3]          [,4]         [,5]
#>  [1,]  0.004551316 -0.0879251088 -0.0675549695 -0.0619695010  0.082243669
#>  [2,] -0.087925109  0.1485375633  0.0683640555  0.0155723082  0.009620691
#>  [3,] -0.067554970  0.0683640555  0.1594567206  0.0077525334 -0.017394272
#>  [4,] -0.061969501  0.0155723082  0.0077525334  0.1048824324 -0.005511091
#>  [5,]  0.082243669  0.0096206906 -0.0173942723 -0.0055110912 -0.156537882
#>  [6,]  0.385905592 -0.0011341107  0.0027606977 -0.0059892149 -0.383415856
#>  [7,]  0.077428636  0.0046614852  0.0022487030  0.0005457593 -0.079946648
#>  [8,]  0.084542958 -0.0004665567  0.0005575601 -0.0003711182 -0.084356478
#>  [9,] -0.386649427 -0.0035180887 -0.0074083951  0.0127403370  0.789358432
#> [10,] -0.082343916  0.0054837328  0.0048099473 -0.0025669755  0.163052708
#> [11,] -0.085004957 -0.0033454411  0.0014294625  0.0037285530  0.162124303
#>               [,6]          [,7]          [,8]         [,9]        [,10]
#>  [1,]  0.385905592  0.0774286360  0.0845429582 -0.386649427 -0.082343916
#>  [2,] -0.001134111  0.0046614852 -0.0004665567 -0.003518089  0.005483733
#>  [3,]  0.002760698  0.0022487030  0.0005575601 -0.007408395  0.004809947
#>  [4,] -0.005989215  0.0005457593 -0.0003711182  0.012740337 -0.002566976
#>  [5,] -0.383415856 -0.0799466479 -0.0843564778  0.789358432  0.163052708
#>  [6,] -0.453414690 -0.3488427143 -0.2936422302  0.453945774  0.349184039
#>  [7,] -0.348842714 -0.0807840816 -0.0848246967  0.349077736  0.081076804
#>  [8,] -0.293642230 -0.0848246967 -0.0924408763  0.293908832  0.084761720
#>  [9,]  0.453945774  0.3490777358  0.2939088316 -0.944430402 -0.718327934
#> [10,]  0.349184039  0.0810768042  0.0847617197 -0.718327934 -0.165357544
#> [11,]  0.293825856  0.0847300389  0.0924354630 -0.593948987 -0.164508835
#>              [,11]
#>  [1,] -0.085004957
#>  [2,] -0.003345441
#>  [3,]  0.001429462
#>  [4,]  0.003728553
#>  [5,]  0.162124303
#>  [6,]  0.293825856
#>  [7,]  0.084730039
#>  [8,]  0.092435463
#>  [9,] -0.593948987
#> [10,] -0.164508835
#> [11,] -0.172559426
#> 
#> $jac_list[[6]]
#>              [,1]         [,2]         [,3]         [,4]        [,5]
#>  [1,]  0.08542158 -0.137826562 -0.128270224 -0.078379266  0.05003098
#>  [2,] -0.13782656  0.220960935  0.114251875  0.018491525  0.01153614
#>  [3,] -0.12827022  0.114251875  0.261843779  0.019186952 -0.02405239
#>  [4,] -0.07837927  0.018491525  0.019186952  0.160356325 -0.01893061
#>  [5,]  0.05003098  0.011536140 -0.024052393 -0.018930605 -0.06335921
#>  [6,]  0.03659983  0.008950647  0.015700461 -0.014175944 -0.03717414
#>  [7,]  0.26882833  0.006132241  0.015058304 -0.015518470 -0.26769477
#>  [8,]  0.04288401  0.005444285  0.015721157 -0.017201177 -0.04062223
#>  [9,] -0.04557652  0.006113619  0.007583599  0.007848601  0.06486010
#> [10,] -0.27641022  0.002373388  0.000309449  0.014331909  0.54658686
#> [11,] -0.04574165 -0.002160890 -0.003210129  0.015234860  0.06878771
#>               [,6]         [,7]         [,8]         [,9]        [,10]
#>  [1,]  0.036599825  0.268828329  0.042884008 -0.045576525 -0.276410217
#>  [2,]  0.008950647  0.006132241  0.005444285  0.006113619  0.002373388
#>  [3,]  0.015700461  0.015058304  0.015721157  0.007583599  0.000309449
#>  [4,] -0.014175944 -0.015518470 -0.017201177  0.007848601  0.014331909
#>  [5,] -0.037174140 -0.267694771 -0.040622229  0.064860104  0.546586858
#>  [6,] -0.036537526 -0.182235351 -0.036883214  0.037420938  0.183580391
#>  [7,] -0.182235351 -0.430330046 -0.191856061  0.183200040  0.431352140
#>  [8,] -0.036883214 -0.191856061 -0.040133148  0.037660138  0.192906564
#>  [9,]  0.037420938  0.183200040  0.037660138 -0.066023554 -0.369293971
#> [10,]  0.183580391  0.431352140  0.192906564 -0.369293971 -0.890527917
#> [11,]  0.036772746  0.191909626  0.039985136 -0.065412138 -0.388112965
#>              [,11]
#>  [1,] -0.045741650
#>  [2,] -0.002160890
#>  [3,] -0.003210129
#>  [4,]  0.015234860
#>  [5,]  0.068787709
#>  [6,]  0.036772746
#>  [7,]  0.191909626
#>  [8,]  0.039985136
#>  [9,] -0.065412138
#> [10,] -0.388112965
#> [11,] -0.069601482
#> 
#> $jac_list[[7]]
#>               [,1]         [,2]         [,3]         [,4]         [,5]
#>  [1,]  0.114086175 -0.097427416 -0.083769145 -0.084335895 -0.015177898
#>  [2,] -0.097427416  0.171565410  0.091327221  0.014994572  0.012589378
#>  [3,] -0.083769145  0.091327221  0.214271278  0.021099078 -0.018328293
#>  [4,] -0.084335895  0.014994572  0.021099078  0.128252725  0.003057877
#>  [5,] -0.015177898  0.012589378 -0.018328293  0.003057877  0.026047603
#>  [6,] -0.004551890 -0.009562539 -0.014683447 -0.005348838  0.015231555
#>  [7,] -0.007876719 -0.005961612 -0.011655743  0.002717432  0.012237857
#>  [8,] -0.010527501 -0.005408481 -0.012494864  0.007646904  0.012257413
#>  [9,]  0.015278321 -0.001787168 -0.003423079  0.002034905 -0.026811775
#> [10,]  0.016026962 -0.001839695  0.001582605 -0.006787331 -0.021212859
#> [11,]  0.017090675 -0.004524007  0.001369912 -0.007745888 -0.022343973
#>               [,6]         [,7]         [,8]         [,9]        [,10]
#>  [1,] -0.004551890 -0.007876719 -0.010527501  0.015278321  0.016026962
#>  [2,] -0.009562539 -0.005961612 -0.005408481 -0.001787168 -0.001839695
#>  [3,] -0.014683447 -0.011655743 -0.012494864 -0.003423079  0.001582605
#>  [4,] -0.005348838  0.002717432  0.007646904  0.002034905 -0.006787331
#>  [5,]  0.015231555  0.012237857  0.012257413 -0.026811775 -0.021212859
#>  [6,]  0.004657887  0.186432601  0.014340668 -0.004729216 -0.186448882
#>  [7,]  0.186432601  0.028282920  0.031912916 -0.186868015 -0.028980205
#>  [8,]  0.014340668  0.031912916  0.011976149 -0.014607480 -0.032546102
#>  [9,] -0.004729216 -0.186868015 -0.014607480  0.011985702  0.401133296
#> [10,] -0.186448882 -0.028980205 -0.032546102  0.401133296  0.054465603
#> [11,] -0.013914903 -0.031751354 -0.011780294  0.026473177  0.067484739
#>              [,11]
#>  [1,]  0.017090675
#>  [2,] -0.004524007
#>  [3,]  0.001369912
#>  [4,] -0.007745888
#>  [5,] -0.022343973
#>  [6,] -0.013914903
#>  [7,] -0.031751354
#>  [8,] -0.011780294
#>  [9,]  0.026473177
#> [10,]  0.067484739
#> [11,]  0.021879293
#> 
#> $jac_list[[8]]
#>               [,1]          [,2]          [,3]          [,4]          [,5]
#>  [1,] -0.006323446 -0.0330247366 -0.0306335901 -2.713874e-02  0.0429007221
#>  [2,] -0.033024737  0.0689271629  0.0264214269  3.098127e-03  0.0007687077
#>  [3,] -0.030633590  0.0264214269  0.0727011520  2.052435e-03 -0.0042893481
#>  [4,] -0.027138741  0.0030981272  0.0020524350  5.004350e-02 -0.0033130635
#>  [5,]  0.042900722  0.0007687077 -0.0042893481 -3.313064e-03 -0.0739170777
#>  [6,]  0.037776600 -0.0029030175  0.0031236253 -8.298186e-05 -0.0379760317
#>  [7,]  0.040175431 -0.0026049030  0.0036099238 -3.983308e-04 -0.0404707348
#>  [8,]  0.596038716  0.0125429026  0.0064458746 -7.951491e-03 -0.5979172824
#>  [9,] -0.039473964  0.0048333133 -0.0003088884  5.586363e-04  0.0703908752
#> [10,] -0.041733451  0.0069117337 -0.0021797920 -2.391215e-04  0.0750270535
#> [11,] -0.606706604  0.0066710180  0.0026709546  8.298983e-03  1.2937347672
#>                [,6]          [,7]         [,8]          [,9]         [,10]
#>  [1,]  3.777660e-02  0.0401754313  0.596038716 -0.0394739639 -0.0417334514
#>  [2,] -2.903018e-03 -0.0026049030  0.012542903  0.0048333133  0.0069117337
#>  [3,]  3.123625e-03  0.0036099238  0.006445875 -0.0003088884 -0.0021797920
#>  [4,] -8.298186e-05 -0.0003983308 -0.007951491  0.0005586363 -0.0002391215
#>  [5,] -3.797603e-02 -0.0404707348 -0.597917282  0.0703908752  0.0750270535
#>  [6,] -3.799992e-02 -0.0371638541 -0.388140282  0.0380613384  0.0369626454
#>  [7,] -3.716385e-02 -0.0395400762 -0.528270621  0.0372504433  0.0393651494
#>  [8,] -3.881403e-01 -0.5282706210 -0.719929653  0.3887917031  0.5296190334
#>  [9,]  3.806134e-02  0.0372504433  0.388791703 -0.0710154527 -0.0696899316
#> [10,]  3.696265e-02  0.0393651494  0.529619033 -0.0696899316 -0.0737155779
#> [11,]  3.883582e-01  0.5289079300  0.719834715 -0.8523505456 -1.1452150083
#>              [,11]
#>  [1,] -0.606706604
#>  [2,]  0.006671018
#>  [3,]  0.002670955
#>  [4,]  0.008298983
#>  [5,]  1.293734767
#>  [6,]  0.388358232
#>  [7,]  0.528907930
#>  [8,]  0.719834715
#>  [9,] -0.852350546
#> [10,] -1.145215008
#> [11,] -1.607721760
#> 
#> $jac_list[[9]]
#>               [,1]          [,2]          [,3]          [,4]         [,5]
#>  [1,]  0.041473955 -0.0314606247 -0.0168549181 -2.315242e-02 -0.015155127
#>  [2,] -0.031460625  0.0538809583  0.0221715378  3.655728e-03  0.005427256
#>  [3,] -0.016854918  0.0221715378  0.0558983063 -9.962610e-04 -0.007335694
#>  [4,] -0.023152416  0.0036557279 -0.0009962610  3.814286e-02  0.004271949
#>  [5,] -0.015155127  0.0054272559 -0.0073356944  4.271949e-03  0.024739070
#>  [6,] -0.014696964  0.0039311657 -0.0056691003 -1.123568e-05  0.015484056
#>  [7,] -0.013186167  0.0024647462 -0.0036293159  2.608542e-03  0.012322919
#>  [8,] -0.006261574  0.0137271535 -0.0049091078 -3.313129e-03  0.005509252
#>  [9,]  0.018453255 -0.0070166484  0.0002455213 -2.350538e-03 -0.027859077
#> [10,]  0.014008275  0.0005465727  0.0030723304 -6.571520e-03 -0.023077379
#> [11,]  0.004507730 -0.0020504007  0.0049401852 -3.219137e-03 -0.020306630
#>                [,6]         [,7]         [,8]          [,9]         [,10]
#>  [1,] -1.469696e-02 -0.013186167 -0.006261574  0.0184532547  0.0140082750
#>  [2,]  3.931166e-03  0.002464746  0.013727153 -0.0070166484  0.0005465727
#>  [3,] -5.669100e-03 -0.003629316 -0.004909108  0.0002455213  0.0030723304
#>  [4,] -1.123568e-05  0.002608542 -0.003313129 -0.0023505380 -0.0065715199
#>  [5,]  1.548406e-02  0.012322919  0.005509252 -0.0278590774 -0.0230773794
#>  [6,]  3.916072e-03  0.014184771  0.435631005 -0.0039403723 -0.0138334905
#>  [7,]  1.418477e-02  0.011665630  0.022251231 -0.0143350660 -0.0116250112
#>  [8,]  4.356310e-01  0.022251231  0.108575188 -0.4352779847 -0.0209640014
#>  [9,] -3.940372e-03 -0.014335066 -0.435277985  0.0083177316  0.0267726480
#> [10,] -1.383349e-02 -0.011625011 -0.020964001  0.0267726480  0.0233268429
#> [11,] -4.357096e-01 -0.022010417 -0.108751446  0.9189699565  0.0633575572
#>              [,11]
#>  [1,]  0.004507730
#>  [2,] -0.002050401
#>  [3,]  0.004940185
#>  [4,] -0.003219137
#>  [5,] -0.020306630
#>  [6,] -0.435709562
#>  [7,] -0.022010417
#>  [8,] -0.108751446
#>  [9,]  0.918969957
#> [10,]  0.063357557
#> [11,]  0.246113612
#> 
#> $jac_list[[10]]
#>                [,1]          [,2]         [,3]         [,4]          [,5]
#>  [1,]  0.0475094828 -0.0400177827 -0.028260626 -0.034012882 -0.0080787157
#>  [2,] -0.0400177827  0.0684544431  0.031820681  0.003857394  0.0065758994
#>  [3,] -0.0282606263  0.0318206813  0.091939662  0.002503657 -0.0134695684
#>  [4,] -0.0340128825  0.0038573936  0.002503657  0.053295287  0.0039965441
#>  [5,] -0.0080787157  0.0065758994 -0.013469568  0.003996544  0.0125472134
#>  [6,] -0.0070638919  0.0004634724 -0.004672035  0.005868902  0.0054358657
#>  [7,] -0.0054458176  0.0014959299 -0.004698971  0.003207218  0.0049143464
#>  [8,] -0.0008086081  0.0032326095  0.008125465 -0.006370958  0.0003637877
#>  [9,]  0.0068928167  0.0013272395  0.007033868 -0.007504714 -0.0092578196
#> [10,]  0.0062173719 -0.0029139665  0.005195188 -0.003467928 -0.0090043526
#> [11,] -0.0110162084  0.0114643244  0.011169857  0.009527844 -0.0081867865
#>                [,6]         [,7]          [,8]         [,9]        [,10]
#>  [1,] -0.0070638919 -0.005445818 -0.0008086081  0.006892817  0.006217372
#>  [2,]  0.0004634724  0.001495930  0.0032326095  0.001327239 -0.002913966
#>  [3,] -0.0046720350 -0.004698971  0.0081254648  0.007033868  0.005195188
#>  [4,]  0.0058689018  0.003207218 -0.0063709584 -0.007504714 -0.003467928
#>  [5,]  0.0054358657  0.004914346  0.0003637877 -0.009257820 -0.009004353
#>  [6,]  0.0055361284  0.003344703 -0.0009680439 -0.005842292 -0.003635238
#>  [7,]  0.0033447028  0.004126786  0.3172248172 -0.003525064 -0.004295150
#>  [8,] -0.0009680439  0.317224817 -0.0604796319  0.001431173 -0.316729583
#>  [9,] -0.0058422920 -0.003525064  0.0014311730  0.009950109  0.007200077
#> [10,] -0.0036352381 -0.004295150 -0.3167295835  0.007200077  0.009148932
#> [11,]  0.0008961357 -0.317074451  0.0602901594  0.008112522  0.703015362
#>               [,11]
#>  [1,] -0.0110162084
#>  [2,]  0.0114643244
#>  [3,]  0.0111698566
#>  [4,]  0.0095278440
#>  [5,] -0.0081867865
#>  [6,]  0.0008961357
#>  [7,] -0.3170744512
#>  [8,]  0.0602901594
#>  [9,]  0.0081125222
#> [10,]  0.7030153618
#> [11,] -0.1100765249
#> 
#> 
#> $theta_vcov
#>                [,1]          [,2]          [,3]          [,4]          [,5]
#>  [1,]  3.702524e-03 -1.215947e-04 -4.731168e-05  1.450739e-05  1.497878e-03
#>  [2,] -1.215947e-04  4.166297e-03  5.181830e-06 -8.501705e-05 -2.892842e-03
#>  [3,] -4.731168e-05  5.181830e-06  4.125814e-03 -1.641174e-05 -9.040069e-05
#>  [4,]  1.450739e-05 -8.501705e-05 -1.641174e-05  3.954303e-03  1.872784e-04
#>  [5,]  1.497878e-03 -2.892842e-03 -9.040069e-05  1.872784e-04  1.162924e-02
#>  [6,]  9.528883e-04 -3.051437e-04 -1.464540e-03  1.037445e-04  1.171667e-03
#>  [7,] -2.602792e-04  7.154405e-04 -1.610053e-04 -1.022748e-04 -8.342308e-04
#>  [8,]  4.907597e-04  3.579757e-04  1.323707e-04 -1.731809e-03 -4.115907e-04
#>  [9,]  2.835132e-05  1.522769e-04 -5.031469e-05 -5.113057e-06 -9.715561e-05
#> [10,] -7.267721e-05 -1.851224e-05  8.198068e-05  1.959919e-04 -1.454858e-04
#>                [,6]          [,7]          [,8]          [,9]         [,10]
#>  [1,]  0.0009528883 -0.0002602792  0.0004907597  2.835132e-05 -7.267721e-05
#>  [2,] -0.0003051437  0.0007154405  0.0003579757  1.522769e-04 -1.851224e-05
#>  [3,] -0.0014645400 -0.0001610053  0.0001323707 -5.031469e-05  8.198068e-05
#>  [4,]  0.0001037445 -0.0001022748 -0.0017318089 -5.113057e-06  1.959919e-04
#>  [5,]  0.0011716674 -0.0008342308 -0.0004115907 -9.715561e-05 -1.454858e-04
#>  [6,]  0.0113483186 -0.0011633727 -0.0004920010  1.126993e-04 -1.244717e-04
#>  [7,] -0.0011633727  0.0117530828  0.0003182121 -2.510208e-04  6.492154e-04
#>  [8,] -0.0004920010  0.0003182121  0.0152793330 -2.440027e-03  1.085145e-03
#>  [9,]  0.0001126993 -0.0002510208 -0.0024400268  9.713650e-03 -5.203255e-04
#> [10,] -0.0001244717  0.0006492154  0.0010851454 -5.203255e-04  1.123606e-02
#> 
#> $full_frame
#>         FEV1                      RACE    SEX ARMCD AVISIT USUBJID (weights)
#> 2   39.97105 Black or African American Female   TRT   VIS2     PT1         1
#> 4   20.48379 Black or African American Female   TRT   VIS4     PT1         1
#> 6   31.45522                     Asian   Male   PBO   VIS2     PT2         1
#> 7   36.87889                     Asian   Male   PBO   VIS3     PT2         1
#> 8   48.80809                     Asian   Male   PBO   VIS4     PT2         1
#> 10  35.98699 Black or African American Female   PBO   VIS2     PT3         1
#> 12  37.16444 Black or African American Female   PBO   VIS4     PT3         1
#> 13  33.89229                     Asian Female   TRT   VIS1     PT4         1
#> 14  33.74637                     Asian Female   TRT   VIS2     PT4         1
#> 16  54.45055                     Asian Female   TRT   VIS4     PT4         1
#> 17  32.31386 Black or African American   Male   PBO   VIS1     PT5         1
#> 19  46.79361 Black or African American   Male   PBO   VIS3     PT5         1
#> 20  41.71154 Black or African American   Male   PBO   VIS4     PT5         1
#> 23  39.02423 Black or African American   Male   PBO   VIS3     PT6         1
#> 25  31.93050                     Asian Female   PBO   VIS1     PT7         1
#> 26  32.90947                     Asian Female   PBO   VIS2     PT7         1
#> 28  48.28031                     Asian Female   PBO   VIS4     PT7         1
#> 29  32.23021 Black or African American   Male   PBO   VIS1     PT8         1
#> 30  35.91080 Black or African American   Male   PBO   VIS2     PT8         1
#> 31  45.54898 Black or African American   Male   PBO   VIS3     PT8         1
#> 32  53.02877 Black or African American   Male   PBO   VIS4     PT8         1
#> 33  47.16898                     White   Male   TRT   VIS1     PT9         1
#> 34  46.64287                     White   Male   TRT   VIS2     PT9         1
#> 36  58.09713                     White   Male   TRT   VIS4     PT9         1
#> 39  44.97613 Black or African American Female   PBO   VIS3    PT10         1
#> 41  44.32755                     Asian Female   TRT   VIS1    PT11         1
#> 42  38.97813                     Asian Female   TRT   VIS2    PT11         1
#> 43  43.72862                     Asian Female   TRT   VIS3    PT11         1
#> 44  46.43393                     Asian Female   TRT   VIS4    PT11         1
#> 45  40.34576                     Asian   Male   PBO   VIS1    PT12         1
#> 46  42.76568                     Asian   Male   PBO   VIS2    PT12         1
#> 47  40.11155                     Asian   Male   PBO   VIS3    PT12         1
#> 51  53.31791                     White   Male   TRT   VIS3    PT13         1
#> 52  56.07641                     White   Male   TRT   VIS4    PT13         1
#> 55  41.90837 Black or African American   Male   PBO   VIS3    PT14         1
#> 59  34.65663                     Asian   Male   PBO   VIS3    PT15         1
#> 60  39.07791                     Asian   Male   PBO   VIS4    PT15         1
#> 62  35.89612                     Asian Female   PBO   VIS2    PT16         1
#> 64  47.67264                     Asian Female   PBO   VIS4    PT16         1
#> 65  22.65440                     White Female   PBO   VIS1    PT17         1
#> 68  40.85376                     White Female   PBO   VIS4    PT17         1
#> 69  32.60048                     Asian   Male   TRT   VIS1    PT18         1
#> 70  33.64329                     Asian   Male   TRT   VIS2    PT18         1
#> 72  40.92278                     Asian   Male   TRT   VIS4    PT18         1
#> 73  32.14831                     Asian   Male   TRT   VIS1    PT19         1
#> 74  46.43604                     Asian   Male   TRT   VIS2    PT19         1
#> 75  41.34973                     Asian   Male   TRT   VIS3    PT19         1
#> 76  66.30382                     Asian   Male   TRT   VIS4    PT19         1
#> 78  47.95358                     White Female   TRT   VIS2    PT20         1
#> 79  53.97364                     White Female   TRT   VIS3    PT20         1
#> 82  56.64544                     White   Male   TRT   VIS2    PT21         1
#> 83  49.70872                     White   Male   TRT   VIS3    PT21         1
#> 84  60.40497                     White   Male   TRT   VIS4    PT21         1
#> 85  45.98525                     White   Male   TRT   VIS1    PT22         1
#> 86  51.90911                     White   Male   TRT   VIS2    PT22         1
#> 87  41.50787                     White   Male   TRT   VIS3    PT22         1
#> 88  53.42727                     White   Male   TRT   VIS4    PT22         1
#> 89  23.86859 Black or African American Female   PBO   VIS1    PT23         1
#> 90  35.98563 Black or African American Female   PBO   VIS2    PT23         1
#> 91  43.60626 Black or African American Female   PBO   VIS3    PT23         1
#> 93  29.59773                     White Female   TRT   VIS1    PT24         1
#> 94  35.50688                     White Female   TRT   VIS2    PT24         1
#> 95  55.42944                     White Female   TRT   VIS3    PT24         1
#> 96  52.10530                     White Female   TRT   VIS4    PT24         1
#> 97  31.69644                     White Female   TRT   VIS1    PT25         1
#> 98  32.16159                     White Female   TRT   VIS2    PT25         1
#> 99  51.04735                     White Female   TRT   VIS3    PT25         1
#> 100 55.85987                     White Female   TRT   VIS4    PT25         1
#> 101 49.11706                     White Female   TRT   VIS1    PT26         1
#> 102 49.25544                     White Female   TRT   VIS2    PT26         1
#> 103 51.72211                     White Female   TRT   VIS3    PT26         1
#> 104 69.99128                     White Female   TRT   VIS4    PT26         1
#> 105 22.07169 Black or African American Female   TRT   VIS1    PT27         1
#> 107 46.08393 Black or African American Female   TRT   VIS3    PT27         1
#> 108 52.42288 Black or African American Female   TRT   VIS4    PT27         1
#> 109 37.69466 Black or African American   Male   TRT   VIS1    PT28         1
#> 110 44.59400 Black or African American   Male   TRT   VIS2    PT28         1
#> 111 52.08897 Black or African American   Male   TRT   VIS3    PT28         1
#> 112 58.22961 Black or African American   Male   TRT   VIS4    PT28         1
#> 113 37.22824 Black or African American   Male   TRT   VIS1    PT29         1
#> 114 34.39863 Black or African American   Male   TRT   VIS2    PT29         1
#> 116 36.34012 Black or African American   Male   TRT   VIS4    PT29         1
#> 117 45.44182                     Asian Female   TRT   VIS1    PT30         1
#> 118 41.54847                     Asian Female   TRT   VIS2    PT30         1
#> 119 43.92172                     Asian Female   TRT   VIS3    PT30         1
#> 120 61.83243                     Asian Female   TRT   VIS4    PT30         1
#> 121 27.25656                     Asian Female   PBO   VIS1    PT31         1
#> 123 45.65133                     Asian Female   PBO   VIS3    PT31         1
#> 125 33.19334 Black or African American   Male   TRT   VIS1    PT32         1
#> 128 41.66826 Black or African American   Male   TRT   VIS4    PT32         1
#> 129 27.12753 Black or African American   Male   TRT   VIS1    PT33         1
#> 130 31.74858 Black or African American   Male   TRT   VIS2    PT33         1
#> 132 41.60000 Black or African American   Male   TRT   VIS4    PT33         1
#> 133 39.45250                     Asian Female   PBO   VIS1    PT34         1
#> 134 32.61823                     Asian Female   PBO   VIS2    PT34         1
#> 135 34.62445                     Asian Female   PBO   VIS3    PT34         1
#> 136 45.90515                     Asian Female   PBO   VIS4    PT34         1
#> 137 36.17780 Black or African American Female   TRT   VIS1    PT35         1
#> 138 39.79796 Black or African American Female   TRT   VIS2    PT35         1
#> 140 50.08272 Black or African American Female   TRT   VIS4    PT35         1
#> 142 44.64316                     Asian Female   TRT   VIS2    PT36         1
#> 144 39.73529                     Asian Female   TRT   VIS4    PT36         1
#> 145 34.06164                     White Female   PBO   VIS1    PT37         1
#> 146 40.18592                     White Female   PBO   VIS2    PT37         1
#> 147 41.17584                     White Female   PBO   VIS3    PT37         1
#> 148 57.76669                     White Female   PBO   VIS4    PT37         1
#> 149 38.18460                     Asian Female   PBO   VIS1    PT38         1
#> 151 47.19893                     Asian Female   PBO   VIS3    PT38         1
#> 153 37.32785                     Asian Female   PBO   VIS1    PT39         1
#> 155 43.16048                     Asian Female   PBO   VIS3    PT39         1
#> 156 41.40349                     Asian Female   PBO   VIS4    PT39         1
#> 157 30.15733 Black or African American   Male   PBO   VIS1    PT40         1
#> 158 35.84353 Black or African American   Male   PBO   VIS2    PT40         1
#> 159 40.95250 Black or African American   Male   PBO   VIS3    PT40         1
#> 162 41.37928 Black or African American   Male   PBO   VIS2    PT41         1
#> 163 50.17316 Black or African American   Male   PBO   VIS3    PT41         1
#> 164 45.35226 Black or African American   Male   PBO   VIS4    PT41         1
#> 165 39.06491 Black or African American   Male   PBO   VIS1    PT42         1
#> 168 42.11960 Black or African American   Male   PBO   VIS4    PT42         1
#> 169 29.81042 Black or African American Female   TRT   VIS1    PT43         1
#> 170 42.57055 Black or African American Female   TRT   VIS2    PT43         1
#> 171 47.81652 Black or African American Female   TRT   VIS3    PT43         1
#> 172 68.06024 Black or African American Female   TRT   VIS4    PT43         1
#> 173 35.62071 Black or African American   Male   TRT   VIS1    PT44         1
#> 177 33.89134                     Asian Female   PBO   VIS1    PT45         1
#> 178 36.42808                     Asian Female   PBO   VIS2    PT45         1
#> 179 37.57519                     Asian Female   PBO   VIS3    PT45         1
#> 180 58.46873                     Asian Female   PBO   VIS4    PT45         1
#> 181 19.54516                     Asian   Male   PBO   VIS1    PT46         1
#> 182 31.13541                     Asian   Male   PBO   VIS2    PT46         1
#> 183 40.89955                     Asian   Male   PBO   VIS3    PT46         1
#> 185 22.18809                     Asian   Male   TRT   VIS1    PT47         1
#> 186 41.05857                     Asian   Male   TRT   VIS2    PT47         1
#> 187 37.32452                     Asian   Male   TRT   VIS3    PT47         1
#> 190 43.12432 Black or African American   Male   PBO   VIS2    PT48         1
#> 191 41.99349 Black or African American   Male   PBO   VIS3    PT48         1
#> 193 44.03080                     White Female   PBO   VIS1    PT49         1
#> 194 38.66417                     White Female   PBO   VIS2    PT49         1
#> 195 53.45993                     White Female   PBO   VIS3    PT49         1
#> 197 29.81948                     Asian Female   TRT   VIS1    PT50         1
#> 198 30.43859                     Asian Female   TRT   VIS2    PT50         1
#> 199 40.18095                     Asian Female   TRT   VIS3    PT50         1
#> 201 26.78578 Black or African American Female   TRT   VIS1    PT51         1
#> 202 34.55115 Black or African American Female   TRT   VIS2    PT51         1
#> 204 40.06421 Black or African American Female   TRT   VIS4    PT51         1
#> 206 43.09329 Black or African American Female   TRT   VIS2    PT52         1
#> 208 45.71567 Black or African American Female   TRT   VIS4    PT52         1
#> 209 40.74992                     White   Male   PBO   VIS1    PT53         1
#> 210 44.74635                     White   Male   PBO   VIS2    PT53         1
#> 217 40.14674                     Asian   Male   TRT   VIS1    PT55         1
#> 218 48.75859                     Asian   Male   TRT   VIS2    PT55         1
#> 219 46.43462                     Asian   Male   TRT   VIS3    PT55         1
#> 221 29.33990 Black or African American   Male   PBO   VIS1    PT56         1
#> 224 47.93165 Black or African American   Male   PBO   VIS4    PT56         1
#> 226 41.11632 Black or African American   Male   TRT   VIS2    PT57         1
#> 227 47.05889 Black or African American   Male   TRT   VIS3    PT57         1
#> 228 52.24599 Black or African American   Male   TRT   VIS4    PT57         1
#> 230 54.14236                     White Female   TRT   VIS2    PT58         1
#> 231 50.44618                     White Female   TRT   VIS3    PT58         1
#> 233 37.53657                     White Female   TRT   VIS1    PT59         1
#> 235 49.45840                     White Female   TRT   VIS3    PT59         1
#> 236 59.12866                     White Female   TRT   VIS4    PT59         1
#> 237 40.31268 Black or African American Female   TRT   VIS1    PT60         1
#> 238 39.66049 Black or African American Female   TRT   VIS2    PT60         1
#> 239 50.89726 Black or African American Female   TRT   VIS3    PT60         1
#> 240 56.13116 Black or African American Female   TRT   VIS4    PT60         1
#> 241 32.82981                     Asian   Male   TRT   VIS1    PT61         1
#> 242 46.53837                     Asian   Male   TRT   VIS2    PT61         1
#> 244 51.81265                     Asian   Male   TRT   VIS4    PT61         1
#> 246 29.91939                     Asian Female   PBO   VIS2    PT62         1
#> 250 51.05656                     White Female   PBO   VIS2    PT63         1
#> 251 50.50059                     White Female   PBO   VIS3    PT63         1
#> 252 64.11388                     White Female   PBO   VIS4    PT63         1
#> 253 32.21843 Black or African American Female   PBO   VIS1    PT64         1
#> 254 29.64732 Black or African American Female   PBO   VIS2    PT64         1
#> 256 45.09919 Black or African American Female   PBO   VIS4    PT64         1
#> 257 39.75659                     Asian Female   TRT   VIS1    PT65         1
#> 258 37.28894                     Asian Female   TRT   VIS2    PT65         1
#> 259 44.80145                     Asian Female   TRT   VIS3    PT65         1
#> 260 65.95920                     Asian Female   TRT   VIS4    PT65         1
#> 261 33.43439                     Asian   Male   TRT   VIS1    PT66         1
#> 262 33.57042                     Asian   Male   TRT   VIS2    PT66         1
#> 263 39.91543                     Asian   Male   TRT   VIS3    PT66         1
#> 264 49.57098                     Asian   Male   TRT   VIS4    PT66         1
#> 265 38.91634                     White Female   TRT   VIS1    PT67         1
#> 266 36.69011                     White Female   TRT   VIS2    PT67         1
#> 267 45.66665                     White Female   TRT   VIS3    PT67         1
#> 268 52.07431                     White Female   TRT   VIS4    PT67         1
#> 269 42.21411                     White Female   TRT   VIS1    PT68         1
#> 270 45.02901                     White Female   TRT   VIS2    PT68         1
#> 273 30.98338 Black or African American   Male   PBO   VIS1    PT69         1
#> 274 44.72932 Black or African American   Male   PBO   VIS2    PT69         1
#> 275 40.68711 Black or African American   Male   PBO   VIS3    PT69         1
#> 276 34.71530 Black or African American   Male   PBO   VIS4    PT69         1
#> 277 27.30752 Black or African American   Male   PBO   VIS1    PT70         1
#> 278 37.31585 Black or African American   Male   PBO   VIS2    PT70         1
#> 280 44.83000 Black or African American   Male   PBO   VIS4    PT70         1
#> 281 32.93042                     Asian   Male   TRT   VIS1    PT71         1
#> 282 44.91911                     Asian   Male   TRT   VIS2    PT71         1
#> 283 45.68636                     Asian   Male   TRT   VIS3    PT71         1
#> 284 65.98800                     Asian   Male   TRT   VIS4    PT71         1
#> 285 46.60130                     Asian Female   TRT   VIS1    PT72         1
#> 286 40.89786                     Asian Female   TRT   VIS2    PT72         1
#> 287 46.66708                     Asian Female   TRT   VIS3    PT72         1
#> 291 43.83270                     White   Male   PBO   VIS3    PT73         1
#> 292 44.11604                     White   Male   PBO   VIS4    PT73         1
#> 293 38.29612                     White Female   PBO   VIS1    PT74         1
#> 295 51.38570                     White Female   PBO   VIS3    PT74         1
#> 296 56.20979                     White Female   PBO   VIS4    PT74         1
#> 298 43.45819                     White   Male   PBO   VIS2    PT75         1
#> 299 38.38741                     White   Male   PBO   VIS3    PT75         1
#> 300 56.42818                     White   Male   PBO   VIS4    PT75         1
#> 301 39.05050                     White   Male   TRT   VIS1    PT76         1
#> 304 54.09200                     White   Male   TRT   VIS4    PT76         1
#> 305 31.40521                     Asian   Male   TRT   VIS1    PT77         1
#> 306 46.13330                     Asian   Male   TRT   VIS2    PT77         1
#> 307 45.29845                     Asian   Male   TRT   VIS3    PT77         1
#> 308 28.06936                     Asian   Male   TRT   VIS4    PT77         1
#> 310 42.50283                     White Female   TRT   VIS2    PT78         1
#> 311 46.45368                     White Female   TRT   VIS3    PT78         1
#> 312 64.97366                     White Female   TRT   VIS4    PT78         1
#> 316 43.97847                     Asian Female   PBO   VIS4    PT79         1
#> 317 35.33466                     Asian   Male   TRT   VIS1    PT80         1
#> 318 39.34378                     Asian   Male   TRT   VIS2    PT80         1
#> 319 41.27633                     Asian   Male   TRT   VIS3    PT80         1
#> 322 39.83058                     White   Male   PBO   VIS2    PT81         1
#> 323 43.49673                     White   Male   PBO   VIS3    PT81         1
#> 324 44.06114                     White   Male   PBO   VIS4    PT81         1
#> 325 41.43742 Black or African American Female   PBO   VIS1    PT82         1
#> 327 46.16954 Black or African American Female   PBO   VIS3    PT82         1
#> 328 54.24024 Black or African American Female   PBO   VIS4    PT82         1
#> 329 36.61831                     White   Male   TRT   VIS1    PT83         1
#> 330 42.09272                     White   Male   TRT   VIS2    PT83         1
#> 331 50.69556                     White   Male   TRT   VIS3    PT83         1
#> 332 51.72563                     White   Male   TRT   VIS4    PT83         1
#> 336 53.89947                     Asian Female   PBO   VIS4    PT84         1
#> 339 39.94420                     Asian Female   PBO   VIS3    PT85         1
#> 340 56.42482                     Asian Female   PBO   VIS4    PT85         1
#> 341 41.86385 Black or African American Female   TRT   VIS1    PT86         1
#> 342 34.56420 Black or African American Female   TRT   VIS2    PT86         1
#> 343 38.68927 Black or African American Female   TRT   VIS3    PT86         1
#> 344 62.88743 Black or African American Female   TRT   VIS4    PT86         1
#> 345 28.85343                     White   Male   PBO   VIS1    PT87         1
#> 347 49.29495                     White   Male   PBO   VIS3    PT87         1
#> 349 28.74029 Black or African American Female   PBO   VIS1    PT88         1
#> 351 43.59994 Black or African American Female   PBO   VIS3    PT88         1
#> 352 57.38616 Black or African American Female   PBO   VIS4    PT88         1
#> 353 35.36824 Black or African American   Male   PBO   VIS1    PT89         1
#> 354 43.06110 Black or African American   Male   PBO   VIS2    PT89         1
#> 355 31.27551 Black or African American   Male   PBO   VIS3    PT89         1
#> 356 54.13245 Black or African American   Male   PBO   VIS4    PT89         1
#> 357 25.97050                     Asian   Male   PBO   VIS1    PT90         1
#> 363 51.17493                     White Female   TRT   VIS3    PT91         1
#> 364 48.44043                     White Female   TRT   VIS4    PT91         1
#> 365 43.33128                     White Female   TRT   VIS1    PT92         1
#> 367 55.93546                     White Female   TRT   VIS3    PT92         1
#> 368 54.15312                     White Female   TRT   VIS4    PT92         1
#> 370 40.60252 Black or African American   Male   PBO   VIS2    PT93         1
#> 371 44.44715 Black or African American   Male   PBO   VIS3    PT93         1
#> 372 40.54161 Black or African American   Male   PBO   VIS4    PT93         1
#> 373 33.95563                     Asian Female   PBO   VIS1    PT94         1
#> 375 43.67802                     Asian Female   PBO   VIS3    PT94         1
#> 376 42.76023                     Asian Female   PBO   VIS4    PT94         1
#> 378 42.82678                     Asian Female   PBO   VIS2    PT95         1
#> 379 39.59218                     Asian Female   PBO   VIS3    PT95         1
#> 381 33.49216 Black or African American Female   PBO   VIS1    PT96         1
#> 382 35.39266 Black or African American Female   PBO   VIS2    PT96         1
#> 384 42.36266 Black or African American Female   PBO   VIS4    PT96         1
#> 385 48.54368                     White   Male   TRT   VIS1    PT97         1
#> 386 43.94366                     White   Male   TRT   VIS2    PT97         1
#> 388 47.91204                     White   Male   TRT   VIS4    PT97         1
#> 389 20.72928                     Asian   Male   PBO   VIS1    PT98         1
#> 390 28.00599                     Asian   Male   PBO   VIS2    PT98         1
#> 391 40.19255                     Asian   Male   PBO   VIS3    PT98         1
#> 392 37.79360                     Asian   Male   PBO   VIS4    PT98         1
#> 394 36.75177 Black or African American   Male   PBO   VIS2    PT99         1
#> 397 34.59822 Black or African American Female   PBO   VIS1   PT100         1
#> 398 39.32034 Black or African American Female   PBO   VIS2   PT100         1
#> 399 40.65702 Black or African American Female   PBO   VIS3   PT100         1
#> 402 43.03255                     White   Male   TRT   VIS2   PT101         1
#> 403 54.65715                     White   Male   TRT   VIS3   PT101         1
#> 405 35.55742                     Asian Female   PBO   VIS1   PT102         1
#> 406 43.70215                     Asian Female   PBO   VIS2   PT102         1
#> 407 42.52157                     Asian Female   PBO   VIS3   PT102         1
#> 408 54.89337                     Asian Female   PBO   VIS4   PT102         1
#> 409 32.03460                     Asian Female   PBO   VIS1   PT103         1
#> 410 29.45107                     Asian Female   PBO   VIS2   PT103         1
#> 411 45.35138                     Asian Female   PBO   VIS3   PT103         1
#> 413 38.73784 Black or African American Female   PBO   VIS1   PT104         1
#> 415 41.42283 Black or African American Female   PBO   VIS3   PT104         1
#> 416 47.32385 Black or African American Female   PBO   VIS4   PT104         1
#> 418 47.55310 Black or African American Female   TRT   VIS2   PT105         1
#> 419 49.06509 Black or African American Female   TRT   VIS3   PT105         1
#> 421 29.22591                     Asian Female   TRT   VIS1   PT106         1
#> 422 40.08175                     Asian Female   TRT   VIS2   PT106         1
#> 423 45.68142                     Asian Female   TRT   VIS3   PT106         1
#> 424 41.47403                     Asian Female   TRT   VIS4   PT106         1
#> 427 42.51970                     White Female   PBO   VIS3   PT107         1
#> 428 69.36099                     White Female   PBO   VIS4   PT107         1
#> 429 42.39760                     White   Male   TRT   VIS1   PT108         1
#> 430 43.72376                     White   Male   TRT   VIS2   PT108         1
#> 431 49.47601                     White   Male   TRT   VIS3   PT108         1
#> 432 51.94188                     White   Male   TRT   VIS4   PT108         1
#> 434 40.59100 Black or African American Female   PBO   VIS2   PT109         1
#> 435 39.97833 Black or African American Female   PBO   VIS3   PT109         1
#> 436 31.69049 Black or African American Female   PBO   VIS4   PT109         1
#> 438 37.20517                     Asian   Male   TRT   VIS2   PT110         1
#> 439 46.28740                     Asian   Male   TRT   VIS3   PT110         1
#> 444 41.58720                     White Female   PBO   VIS4   PT111         1
#> 445 32.17365 Black or African American Female   PBO   VIS1   PT112         1
#> 447 40.69375 Black or African American Female   PBO   VIS3   PT112         1
#> 449 32.28771                     Asian   Male   PBO   VIS1   PT113         1
#> 450 41.76205                     Asian   Male   PBO   VIS2   PT113         1
#> 451 40.06768                     Asian   Male   PBO   VIS3   PT113         1
#> 453 29.14213 Black or African American   Male   PBO   VIS1   PT114         1
#> 454 39.50989 Black or African American   Male   PBO   VIS2   PT114         1
#> 455 43.32349 Black or African American   Male   PBO   VIS3   PT114         1
#> 456 47.16756 Black or African American   Male   PBO   VIS4   PT114         1
#> 457 40.93020                     Asian Female   PBO   VIS1   PT115         1
#> 458 42.19406                     Asian Female   PBO   VIS2   PT115         1
#> 459 41.21057                     Asian Female   PBO   VIS3   PT115         1
#> 461 38.54330 Black or African American   Male   TRT   VIS1   PT116         1
#> 463 43.96324 Black or African American   Male   TRT   VIS3   PT116         1
#> 464 42.67652 Black or African American   Male   TRT   VIS4   PT116         1
#> 465 22.79584 Black or African American   Male   PBO   VIS1   PT117         1
#> 469 31.43559                     Asian Female   PBO   VIS1   PT118         1
#> 470 38.85064                     Asian Female   PBO   VIS2   PT118         1
#> 471 48.24288                     Asian Female   PBO   VIS3   PT118         1
#> 473 44.71302                     White   Male   TRT   VIS1   PT119         1
#> 474 51.85370                     White   Male   TRT   VIS2   PT119         1
#> 477 30.56757                     Asian Female   PBO   VIS1   PT120         1
#> 484 59.90473 Black or African American   Male   TRT   VIS4   PT121         1
#> 487 49.76150                     Asian Female   PBO   VIS3   PT122         1
#> 489 47.21985                     White Female   PBO   VIS1   PT123         1
#> 490 40.34525                     White Female   PBO   VIS2   PT123         1
#> 491 48.29793                     White Female   PBO   VIS3   PT123         1
#> 494 44.39634                     Asian Female   TRT   VIS2   PT124         1
#> 495 41.71421                     Asian Female   TRT   VIS3   PT124         1
#> 496 47.37535                     Asian Female   TRT   VIS4   PT124         1
#> 497 42.03797                     White   Male   PBO   VIS1   PT125         1
#> 498 37.56100                     White   Male   PBO   VIS2   PT125         1
#> 499 45.11793                     White   Male   PBO   VIS3   PT125         1
#> 501 34.62530                     Asian   Male   TRT   VIS1   PT126         1
#> 502 45.28206                     Asian   Male   TRT   VIS2   PT126         1
#> 504 63.57761                     Asian   Male   TRT   VIS4   PT126         1
#> 505 35.80878 Black or African American Female   TRT   VIS1   PT127         1
#> 508 52.67314 Black or African American Female   TRT   VIS4   PT127         1
#> 509 35.88734                     Asian Female   TRT   VIS1   PT128         1
#> 510 38.73222                     Asian Female   TRT   VIS2   PT128         1
#> 511 46.70361                     Asian Female   TRT   VIS3   PT128         1
#> 512 53.65398                     Asian Female   TRT   VIS4   PT128         1
#> 513 36.71543                     White   Male   TRT   VIS1   PT129         1
#> 518 41.54317                     White   Male   PBO   VIS2   PT130         1
#> 519 51.67909                     White   Male   PBO   VIS3   PT130         1
#> 521 27.40130                     Asian Female   PBO   VIS1   PT131         1
#> 522 30.33517                     Asian Female   PBO   VIS2   PT131         1
#> 523 37.73092                     Asian Female   PBO   VIS3   PT131         1
#> 524 29.11668                     Asian Female   PBO   VIS4   PT131         1
#> 526 32.08830                     Asian   Male   PBO   VIS2   PT132         1
#> 527 41.66067                     Asian   Male   PBO   VIS3   PT132         1
#> 528 53.90815                     Asian   Male   PBO   VIS4   PT132         1
#> 530 35.06937                     White   Male   PBO   VIS2   PT133         1
#> 531 47.17615                     White   Male   PBO   VIS3   PT133         1
#> 532 56.49347                     White   Male   PBO   VIS4   PT133         1
#> 534 38.88006 Black or African American   Male   PBO   VIS2   PT134         1
#> 535 47.54070 Black or African American   Male   PBO   VIS3   PT134         1
#> 536 43.53705 Black or African American   Male   PBO   VIS4   PT134         1
#> 537 31.82054 Black or African American   Male   PBO   VIS1   PT135         1
#> 538 39.62816 Black or African American   Male   PBO   VIS2   PT135         1
#> 539 44.95543 Black or African American   Male   PBO   VIS3   PT135         1
#> 540 21.11543 Black or African American   Male   PBO   VIS4   PT135         1
#> 541 34.74671                     White Female   TRT   VIS1   PT136         1
#> 544 56.69249                     White Female   TRT   VIS4   PT136         1
#> 545 22.73126                     Asian Female   TRT   VIS1   PT137         1
#> 546 32.50075                     Asian Female   TRT   VIS2   PT137         1
#> 547 42.37206                     Asian Female   TRT   VIS3   PT137         1
#> 548 42.89847                     Asian Female   TRT   VIS4   PT137         1
#> 549 55.62582                     Asian   Male   TRT   VIS1   PT138         1
#> 550 45.38998                     Asian   Male   TRT   VIS2   PT138         1
#> 551 52.66743                     Asian   Male   TRT   VIS3   PT138         1
#> 555 34.18931                     Asian Female   TRT   VIS3   PT139         1
#> 556 45.59740                     Asian Female   TRT   VIS4   PT139         1
#> 557 28.89198 Black or African American Female   PBO   VIS1   PT140         1
#> 558 38.46147 Black or African American Female   PBO   VIS2   PT140         1
#> 560 49.90357 Black or African American Female   PBO   VIS4   PT140         1
#> 562 44.14167                     White   Male   TRT   VIS2   PT141         1
#> 564 55.24278                     White   Male   TRT   VIS4   PT141         1
#> 569 27.38001 Black or African American Female   TRT   VIS1   PT143         1
#> 570 33.63251 Black or African American Female   TRT   VIS2   PT143         1
#> 572 39.34410 Black or African American Female   TRT   VIS4   PT143         1
#> 573 26.98575                     Asian Female   PBO   VIS1   PT144         1
#> 574 24.04175                     Asian Female   PBO   VIS2   PT144         1
#> 575 42.16648                     Asian Female   PBO   VIS3   PT144         1
#> 576 44.75380                     Asian Female   PBO   VIS4   PT144         1
#> 577 31.55469 Black or African American   Male   PBO   VIS1   PT145         1
#> 578 44.42696 Black or African American   Male   PBO   VIS2   PT145         1
#> 579 44.10343 Black or African American   Male   PBO   VIS3   PT145         1
#> 582 37.87445                     Asian Female   TRT   VIS2   PT146         1
#> 583 48.31828                     Asian Female   TRT   VIS3   PT146         1
#> 584 50.21520                     Asian Female   TRT   VIS4   PT146         1
#> 585 41.94615                     Asian Female   PBO   VIS1   PT147         1
#> 586 39.62690                     Asian Female   PBO   VIS2   PT147         1
#> 587 46.69763                     Asian Female   PBO   VIS3   PT147         1
#> 590 43.75255 Black or African American   Male   TRT   VIS2   PT148         1
#> 591 47.38873 Black or African American   Male   TRT   VIS3   PT148         1
#> 593 32.43412                     Asian Female   PBO   VIS1   PT149         1
#> 594 43.07163                     Asian Female   PBO   VIS2   PT149         1
#> 595 42.99551                     Asian Female   PBO   VIS3   PT149         1
#> 596 53.82759                     Asian Female   PBO   VIS4   PT149         1
#> 599 50.64802                     White   Male   PBO   VIS3   PT150         1
#> 600 63.44051                     White   Male   PBO   VIS4   PT150         1
#> 601 34.48949                     Asian Female   PBO   VIS1   PT151         1
#> 602 40.08056                     Asian Female   PBO   VIS2   PT151         1
#> 604 47.46553                     Asian Female   PBO   VIS4   PT151         1
#> 606 37.11697                     Asian Female   TRT   VIS2   PT152         1
#> 608 36.25120                     Asian Female   TRT   VIS4   PT152         1
#> 609 29.20171 Black or African American   Male   PBO   VIS1   PT153         1
#> 610 31.53773 Black or African American   Male   PBO   VIS2   PT153         1
#> 611 42.35683 Black or African American   Male   PBO   VIS3   PT153         1
#> 612 64.78352 Black or African American   Male   PBO   VIS4   PT153         1
#> 613 32.72757 Black or African American Female   PBO   VIS1   PT154         1
#> 614 37.50022 Black or African American Female   PBO   VIS2   PT154         1
#> 616 57.03861 Black or African American Female   PBO   VIS4   PT154         1
#> 617 36.32475                     Asian   Male   TRT   VIS1   PT155         1
#> 619 41.46725                     Asian   Male   TRT   VIS3   PT155         1
#> 620 59.01411                     Asian   Male   TRT   VIS4   PT155         1
#> 621 30.14970                     White   Male   PBO   VIS1   PT156         1
#> 622 34.91740                     White   Male   PBO   VIS2   PT156         1
#> 623 52.13900                     White   Male   PBO   VIS3   PT156         1
#> 624 58.73839                     White   Male   PBO   VIS4   PT156         1
#> 625 35.83185 Black or African American   Male   TRT   VIS1   PT157         1
#> 628 56.41409 Black or African American   Male   TRT   VIS4   PT157         1
#> 630 43.55593 Black or African American   Male   TRT   VIS2   PT158         1
#> 631 44.26320 Black or African American   Male   TRT   VIS3   PT158         1
#> 632 59.25579 Black or African American   Male   TRT   VIS4   PT158         1
#> 633 28.47314                     Asian Female   TRT   VIS1   PT159         1
#> 634 47.47581                     Asian Female   TRT   VIS2   PT159         1
#> 638 46.47483                     Asian   Male   TRT   VIS2   PT160         1
#> 639 51.22677                     Asian   Male   TRT   VIS3   PT160         1
#> 640 45.82777                     Asian   Male   TRT   VIS4   PT160         1
#> 642 39.06783 Black or African American Female   PBO   VIS2   PT161         1
#> 645 29.99542                     Asian   Male   PBO   VIS1   PT162         1
#> 648 54.17796                     Asian   Male   PBO   VIS4   PT162         1
#> 650 44.55743                     White   Male   PBO   VIS2   PT163         1
#> 652 62.59579                     White   Male   PBO   VIS4   PT163         1
#> 654 35.48396 Black or African American Female   PBO   VIS2   PT164         1
#> 655 44.07768 Black or African American Female   PBO   VIS3   PT164         1
#> 656 46.57837 Black or African American Female   PBO   VIS4   PT164         1
#> 657 47.67979                     White Female   TRT   VIS1   PT165         1
#> 661 22.15439                     Asian   Male   TRT   VIS1   PT166         1
#> 665 34.27765 Black or African American   Male   PBO   VIS1   PT167         1
#> 666 36.90059 Black or African American   Male   PBO   VIS2   PT167         1
#> 668 40.54285 Black or African American   Male   PBO   VIS4   PT167         1
#> 669 29.09494 Black or African American Female   PBO   VIS1   PT168         1
#> 670 37.21768 Black or African American Female   PBO   VIS2   PT168         1
#> 671 43.08491 Black or African American Female   PBO   VIS3   PT168         1
#> 673 27.12174                     White Female   PBO   VIS1   PT169         1
#> 674 34.11916                     White Female   PBO   VIS2   PT169         1
#> 678 40.80230                     White Female   TRT   VIS2   PT170         1
#> 679 45.89269                     White Female   TRT   VIS3   PT170         1
#> 680 43.69153                     White Female   TRT   VIS4   PT170         1
#> 682 29.22869                     Asian Female   PBO   VIS2   PT171         1
#> 684 55.68362                     Asian Female   PBO   VIS4   PT171         1
#> 685 31.90698                     Asian Female   TRT   VIS1   PT172         1
#> 686 37.31061                     Asian Female   TRT   VIS2   PT172         1
#> 687 40.75546                     Asian Female   TRT   VIS3   PT172         1
#> 689 42.19474                     White Female   TRT   VIS1   PT173         1
#> 690 44.87228                     White Female   TRT   VIS2   PT173         1
#> 691 47.55198                     White Female   TRT   VIS3   PT173         1
#> 693 50.62894 Black or African American Female   TRT   VIS1   PT174         1
#> 694 45.47551 Black or African American Female   TRT   VIS2   PT174         1
#> 695 48.62168 Black or African American Female   TRT   VIS3   PT174         1
#> 697 29.66493 Black or African American Female   PBO   VIS1   PT175         1
#> 698 34.57406 Black or African American Female   PBO   VIS2   PT175         1
#> 700 38.11676 Black or African American Female   PBO   VIS4   PT175         1
#> 701 33.77204 Black or African American   Male   TRT   VIS1   PT176         1
#> 702 34.26148 Black or African American   Male   TRT   VIS2   PT176         1
#> 704 58.81037 Black or African American   Male   TRT   VIS4   PT176         1
#> 707 39.88119 Black or African American   Male   PBO   VIS3   PT177         1
#> 709 31.62708 Black or African American   Male   PBO   VIS1   PT178         1
#> 712 48.22049 Black or African American   Male   PBO   VIS4   PT178         1
#> 713 42.58829                     White   Male   TRT   VIS1   PT179         1
#> 715 49.33262                     White   Male   TRT   VIS3   PT179         1
#> 716 53.74331                     White   Male   TRT   VIS4   PT179         1
#> 717 29.71857                     Asian   Male   PBO   VIS1   PT180         1
#> 718 30.45651                     Asian   Male   PBO   VIS2   PT180         1
#> 719 38.29800                     Asian   Male   PBO   VIS3   PT180         1
#> 721 36.81040                     Asian Female   PBO   VIS1   PT181         1
#> 723 42.35045                     Asian Female   PBO   VIS3   PT181         1
#> 724 39.39860                     Asian Female   PBO   VIS4   PT181         1
#> 727 49.73629 Black or African American Female   TRT   VIS3   PT182         1
#> 728 41.58082 Black or African American Female   TRT   VIS4   PT182         1
#> 729 43.58901 Black or African American Female   TRT   VIS1   PT183         1
#> 730 40.16762 Black or African American Female   TRT   VIS2   PT183         1
#> 734 41.08206                     White Female   TRT   VIS2   PT184         1
#> 736 69.37409                     White Female   TRT   VIS4   PT184         1
#> 738 41.27625 Black or African American Female   PBO   VIS2   PT185         1
#> 739 44.76138 Black or African American Female   PBO   VIS3   PT185         1
#> 740 39.69815 Black or African American Female   PBO   VIS4   PT185         1
#> 741 38.44296                     White   Male   PBO   VIS1   PT186         1
#> 742 48.20586                     White   Male   PBO   VIS2   PT186         1
#> 744 35.50735                     White   Male   PBO   VIS4   PT186         1
#> 745 32.08153 Black or African American Female   PBO   VIS1   PT187         1
#> 749 44.69256 Black or African American Female   PBO   VIS1   PT188         1
#> 751 42.18689 Black or African American Female   PBO   VIS3   PT188         1
#> 753 37.01741                     Asian Female   TRT   VIS1   PT189         1
#> 754 38.26920                     Asian Female   TRT   VIS2   PT189         1
#> 755 49.28806                     Asian Female   TRT   VIS3   PT189         1
#> 757 40.45953 Black or African American Female   TRT   VIS1   PT190         1
#> 758 45.10337 Black or African American Female   TRT   VIS2   PT190         1
#> 759 45.58250 Black or African American Female   TRT   VIS3   PT190         1
#> 760 62.96989 Black or African American Female   TRT   VIS4   PT190         1
#> 761 30.78252                     White   Male   TRT   VIS1   PT191         1
#> 764 44.69667                     White   Male   TRT   VIS4   PT191         1
#> 765 32.72491                     White Female   TRT   VIS1   PT192         1
#> 766 45.78702                     White Female   TRT   VIS2   PT192         1
#> 767 48.74886                     White Female   TRT   VIS3   PT192         1
#> 768 84.08449                     White Female   TRT   VIS4   PT192         1
#> 770 30.19495                     Asian   Male   PBO   VIS2   PT193         1
#> 771 36.78573                     Asian   Male   PBO   VIS3   PT193         1
#> 772 61.03588                     Asian   Male   PBO   VIS4   PT193         1
#> 773 20.36749 Black or African American   Male   PBO   VIS1   PT194         1
#> 774 35.22480 Black or African American   Male   PBO   VIS2   PT194         1
#> 775 37.42847 Black or African American   Male   PBO   VIS3   PT194         1
#> 776 30.20501 Black or African American   Male   PBO   VIS4   PT194         1
#> 778 49.12862                     White Female   TRT   VIS2   PT195         1
#> 779 47.31234                     White Female   TRT   VIS3   PT195         1
#> 781 19.28388                     Asian   Male   PBO   VIS1   PT196         1
#> 782 30.00682                     Asian   Male   PBO   VIS2   PT196         1
#> 784 49.21768                     Asian   Male   PBO   VIS4   PT196         1
#> 788 40.13353 Black or African American   Male   PBO   VIS4   PT197         1
#> 789 42.34534 Black or African American   Male   TRT   VIS1   PT198         1
#> 790 52.32575 Black or African American   Male   TRT   VIS2   PT198         1
#> 792 69.26254 Black or African American   Male   TRT   VIS4   PT198         1
#> 797 35.70341 Black or African American   Male   PBO   VIS1   PT200         1
#> 798 41.64454 Black or African American   Male   PBO   VIS2   PT200         1
#> 800 54.25081 Black or African American   Male   PBO   VIS4   PT200         1
#> 
#> $xlev
#> $xlev$RACE
#> [1] "Asian"                     "Black or African American"
#> [3] "White"                    
#> 
#> $xlev$SEX
#> [1] "Male"   "Female"
#> 
#> $xlev$ARMCD
#> [1] "PBO" "TRT"
#> 
#> $xlev$AVISIT
#> [1] "VIS1" "VIS2" "VIS3" "VIS4"
#> 
#> 
#> $contrasts
#> $contrasts$RACE
#>                           Black or African American White
#> Asian                                             0     0
#> Black or African American                         1     0
#> White                                             0     1
#> 
#> $contrasts$SEX
#>        Female
#> Male        0
#> Female      1
#> 
#> $contrasts$ARMCD
#>     TRT
#> PBO   0
#> TRT   1
#> 
#> $contrasts$AVISIT
#>      VIS2 VIS3 VIS4
#> VIS1    0    0    0
#> VIS2    1    0    0
#> VIS3    0    1    0
#> VIS4    0    0    1
#> 
#> 
# Get convergence code and message.
component(fit, c("convergence", "conv_message"))
#> $convergence
#> [1] 0
#> 
#> $conv_message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
#> 
# Get modeled formula as a string.
component(fit, c("formula"))
#> [1] "FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)"
```
