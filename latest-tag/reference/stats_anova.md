# Analysis of Variance for `mmrm` Fits

If supplied only one model fit, the function will calculate and return
the significance of the model terms. If supplied more than one model
fit, standard diagnostics will be returned for each model, optionally
including likelihood ratio test (LRT) results for adjacent models.

## Usage

``` r
# S3 method for class 'mmrm'
anova(object, ..., test = TRUE, refit = FALSE)
```

## Arguments

- object:

  (`mmrm`)  
  an `mmrm` model fit.

- ...:

  (`mmrm`)  
  optional `mmrm` model fits. If left empty, the significance of each
  term in `object` will be calculated.

- test:

  (`flag`)  
  indicating whether the output should include likelihood ratio test
  (LRT) results comparing the model fits to one another. Defaults to
  `TRUE`. Ignored if `...` is empty.

- refit:

  (`flag`)  
  indicating whether the models should be refitted with the dataset
  consisting of their shared set of observations before performing
  diagnostics and testing. This is ignored if the models already share
  the same dataset. If `refit = FALSE` and the models have different
  underlying data sets, an error will be thrown. Defaults to `FALSE`.
  Ignored if `...` is empty.

## Value

A data frame with a row for each supplied model. If `...` is empty, this
will be the the returned value of
[`h_anova_single_mmrm_model()`](https://openpharma.github.io/mmrm/reference/h_anova_single_mmrm_model.md)
with `object` supplied as its argument. Otherwise, the resulting data
frame will have the following columns:

- `Model`: the sequence number of the model according to the order in
  which the models were supplied to this function.

- `refit`: logical, indicating whether or not the model was refitted. If
  the `refit` argument was `FALSE`, all values will be `FALSE`.

- `REML`: logical, indicating whether or not the model was fitted using
  restricted maximum likelihood (REML) estimation. If `FALSE`, the model
  was fitted using maximum likelihood (ML) estimation.

- `n_param`: the number of variance parameters in the model fit,
  obtained via
  [`logLik.mmrm_tmb()`](https://openpharma.github.io/mmrm/reference/mmrm_tmb_methods.md).

- `n_coef`: the number of estimated coefficients in the model fit,
  obtained via
  [`logLik.mmrm_tmb()`](https://openpharma.github.io/mmrm/reference/mmrm_tmb_methods.md).

- `df`: degrees of freedom of the model fit, obtained via
  [`logLik.mmrm_tmb()`](https://openpharma.github.io/mmrm/reference/mmrm_tmb_methods.md).

- `AIC`: Akaike's "An Information Criterion" of the model fit, obtained
  via [`stats::AIC()`](https://rdrr.io/r/stats/AIC.html).

- `BIC`: the Bayesian Information Criterion of the model fit, obtained
  via [`stats::BIC()`](https://rdrr.io/r/stats/AIC.html).

- `logLik`: the log likelihood of the model fit, obtained via
  [`logLik.mmrm_tmb()`](https://openpharma.github.io/mmrm/reference/mmrm_tmb_methods.md).

- `call`: the [call](https://rdrr.io/r/base/call.html) that created the
  model fit, obtained via
  [`component()`](https://openpharma.github.io/mmrm/reference/component.md)
  with `name = "call"`, which is passed to
  [`deparse1()`](https://rdrr.io/r/base/deparse.html). If the model was
  refitted (i.e., if its `refit` entry in this table is `TRUE`), this
  `call` will be different from the `call` component of the pre-refitted
  model fit.

The data frame will have these additional columns inserted before `call`
if `test = TRUE`. Note that since each of these columns describe the
results of a likelihood ratio test (LRT) between the previous row's and
current row's model fits, the first element of each of these columns
will be `NA`.

- `test`: character, indicating which two model fits were compared.
  Values are of the form `{Model - 1} vs {Model}`.

- `log_likelihood_ratio`: the logarithm of the likelihood ratio between
  the two models being compared.

- `p_value`: the p-value of the `log_likelihood_ratio`.

## Details

When `test = FALSE` (or, when only one model is supplied), this function
will process any `mmrm` fits, related or unrelated.

When supplying multiple models and `test = TRUE`, adjacent pairs of
models are tested sequentially. In other words, the order of the
supplied models matters. Furthermore, there are are multiple
requirements for successful LRT. See the section "Requirements for LRT"
below.

## Requirements for LRT

1.  Each supplied model fit must have more degrees of freedom than the
    preceding model fit.

2.  If all supplied models were estimated using maximum likelihood (ML),
    the models must have nested covariates in order to undergo LRT. In
    other words, the set of covariates for each model must be a subset
    of the covariates of the next model. However, if any of the supplied
    models were estimated using restricted maximum likelihood (REML),
    all models must have the same covariates.

3.  The covariance structure of each model must be either (a) the same
    as that of the next model or (b) a special case of that of the next
    model. See the section "Covariance structure nesting hierarchy"
    below.

4.  All supplied model fits must either already use the same data or be
    refitted using `refit = TRUE`, which refits all models to the
    dataset of common observations between all models' respective data
    sets.

## Covariance structure nesting hierarchy

### Structured nests within unstructured

Tautologically, all covariance structures are special cases of an
unstructured covariance, and a model *with* a covariance structure can
be considered "nested" within an model *without* a covariance structure
(assuming that the covariates are also nested).

### Homogeneous nests within analogous heterogeneous

All homogeneous covariance structures are nested within their
corresponding heterogeneous counterparts. For instance, the homogeneous
Toeplitz covariance structure is nested within the heterogeneous
Toeplitz covariance structure.

### Other nested structures

Some different covariance structure types are also nested:

- First-order auto-regressive (`ar1` / `ar1h`) is nested within:

  - ante-dependence (`ad` / `adh`)

  - Toeplitz (`toep` / `toeph`)

- Compound symmetry (`cs` / `csh`) is nested within Toeplitz (`toep` /
  `toeph`)

## See also

For details on the single model operation of this function, see
[`h_anova_single_mmrm_model()`](https://openpharma.github.io/mmrm/reference/h_anova_single_mmrm_model.md).
For details on the generic, see
[`stats::anova()`](https://rdrr.io/r/stats/anova.html).

## Examples

``` r
# Create a few model fits, only adding terms from one to the next.
# Notice also that each covariance structure is a special case of the one
# that follows.
fit_sex_ar1 <-
  mmrm(FEV1 ~ FEV1_BL + SEX + ARMCD + ar1(AVISIT | USUBJID),
    data = fev_data,
    reml = FALSE
  )
fit_sex_race_toeph <-
  mmrm(
    FEV1 ~ FEV1_BL + SEX + RACE + ARMCD + toeph(AVISIT | USUBJID),
    data = fev_data,
    reml = FALSE
  )
fit_interaction_us <-
  mmrm(
    FEV1 ~ FEV1_BL + SEX * RACE + ARMCD + us(AVISIT | USUBJID),
    data = fev_data,
    reml = FALSE
  )

# Single model fit, showing significance of model terms:
anova(fit_interaction_us)
#>             num_df denom_df      f_stat        p_val
#> (Intercept)      1 169.7869 629.7216209 5.141580e-59
#> FEV1_BL          1 188.0191  33.1933418 3.359280e-08
#> SEX              1 140.5733   0.3276806 5.679424e-01
#> RACE             2 157.7997  16.1012466 4.330465e-07
#> ARMCD            1 159.9380  43.1828265 6.684586e-10
#> SEX:RACE         2 157.9950   0.1999710 8.189614e-01

# Multiple model fits, with diagnostics for each fit and likelihood ratio
# testing (LRT) for each adjacent pair. LRT is possible because when the fits
# are in this order, their covariates and covariance structures are nested.
anova(fit_sex_ar1, fit_sex_race_toeph, fit_interaction_us)
#>   Model refit  REML n_param n_coef df      AIC      BIC    logLik   test
#> 1     1 FALSE FALSE       2      4  6 3857.846 3877.545 -1922.923   <NA>
#> 2     2 FALSE FALSE       7      6 13 3646.468 3689.150 -1810.234 1 vs 2
#> 3     3 FALSE FALSE      10      8 18 3615.923 3675.021 -1789.961 2 vs 3
#>   log_likelihood_ratio      p_value
#> 1                   NA           NA
#> 2            112.68862 1.046903e-46
#> 3             20.27281 9.895512e-07
#>                                                                                                           call
#> 1          mmrm(formula = FEV1 ~ FEV1_BL + SEX + ARMCD + ar1(AVISIT | USUBJID), data = fev_data, reml = FALSE)
#> 2 mmrm(formula = FEV1 ~ FEV1_BL + SEX + RACE + ARMCD + toeph(AVISIT | USUBJID), data = fev_data, reml = FALSE)
#> 3    mmrm(formula = FEV1 ~ FEV1_BL + SEX * RACE + ARMCD + us(AVISIT | USUBJID), data = fev_data, reml = FALSE)

# We can only change the order if we forego LRT using test = FALSE.
anova(fit_sex_race_toeph, fit_interaction_us, fit_sex_ar1, test = FALSE)
#>   Model refit  REML n_param n_coef df      AIC      BIC    logLik
#> 1     1 FALSE FALSE       7      6 13 3646.468 3689.150 -1810.234
#> 2     2 FALSE FALSE      10      8 18 3615.923 3675.021 -1789.961
#> 3     3 FALSE FALSE       2      4  6 3857.846 3877.545 -1922.923
#>                                                                                                           call
#> 1 mmrm(formula = FEV1 ~ FEV1_BL + SEX + RACE + ARMCD + toeph(AVISIT | USUBJID), data = fev_data, reml = FALSE)
#> 2    mmrm(formula = FEV1 ~ FEV1_BL + SEX * RACE + ARMCD + us(AVISIT | USUBJID), data = fev_data, reml = FALSE)
#> 3          mmrm(formula = FEV1 ~ FEV1_BL + SEX + ARMCD + ar1(AVISIT | USUBJID), data = fev_data, reml = FALSE)

# Create a subset of fev_data set with the 4th visit removed.
fev_subset <- droplevels(fev_data[fev_data$VISITN < 4, ])

# Recreate fit_sex_race_toeph but this time based off fev_subset:
fit_sex_race_toeph_sub <-
  mmrm(
    FEV1 ~ FEV1_BL + SEX + RACE + ARMCD + toeph(AVISIT | USUBJID),
    data = fev_subset,
    reml = FALSE
  )

# If a model was created with a different data set, refit = TRUE is needed.
anova(fit_sex_ar1, fit_sex_race_toeph_sub, fit_interaction_us, refit = TRUE)
#>   Model refit  REML n_param n_coef df      AIC      BIC    logLik   test
#> 1     1  TRUE FALSE       2      4  6 2682.473 2702.018 -1335.236   <NA>
#> 2     2 FALSE FALSE       5      6 11 2600.928 2636.761 -1289.464 1 vs 2
#> 3     3  TRUE FALSE       6      8 14 2585.429 2631.034 -1278.714 2 vs 3
#>   log_likelihood_ratio      p_value
#> 1                   NA           NA
#> 2             45.77211 1.020588e-19
#> 3             10.74981 6.515941e-04
#>                                                                                                             call
#> 1              mmrm(formula = FEV1 ~ FEV1_BL + SEX + ARMCD + ar1(AVISIT | USUBJID), data = data.1, reml = FALSE)
#> 2 mmrm(formula = FEV1 ~ FEV1_BL + SEX + RACE + ARMCD + toeph(AVISIT | USUBJID), data = fev_subset, reml = FALSE)
#> 3        mmrm(formula = FEV1 ~ FEV1_BL + SEX * RACE + ARMCD + us(AVISIT | USUBJID), data = data.1, reml = FALSE)
```
