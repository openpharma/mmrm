# Fit an MMRM

**\[stable\]**

This is the main function fitting the MMRM.

## Usage

``` r
mmrm(
  formula,
  data,
  weights = NULL,
  covariance = NULL,
  reml = TRUE,
  control = mmrm_control(...),
  ...
)
```

## Arguments

- formula:

  (`formula`)  
  the model formula, see details.

- data:

  (`data`)  
  the data to be used for the model.

- weights:

  (`vector`)  
  an optional vector of weights to be used in the fitting process.
  Should be `NULL` or a numeric vector.

- covariance:

  (`cov_struct`)  
  a covariance structure type definition as produced with
  [`cov_struct()`](https://openpharma.github.io/mmrm/reference/cov_struct.md),
  or value that can be coerced to a covariance structure using
  [`as.cov_struct()`](https://openpharma.github.io/mmrm/reference/as.cov_struct.md).
  If no value is provided, a structure is derived from the provided
  formula.

- reml:

  (`flag`)  
  whether restricted maximum likelihood (REML) estimation is used,
  otherwise maximum likelihood (ML) is used.

- control:

  (`mmrm_control`)  
  fine-grained fitting specifications list created with
  [`mmrm_control()`](https://openpharma.github.io/mmrm/reference/mmrm_control.md).

- ...:

  arguments passed to
  [`mmrm_control()`](https://openpharma.github.io/mmrm/reference/mmrm_control.md).

## Value

An `mmrm` object.

## Details

The `formula` typically looks like:
`FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)` so specifies
response and covariates as usual, and exactly one special term defines
which covariance structure is used and what are the time point and
subject variables. The covariance structures in the formula can be found
in
[`covariance_types`](https://openpharma.github.io/mmrm/reference/covariance_types.md).

The time points have to be unique for each subject. That is, there
cannot be time points with multiple observations for any subject. The
rationale is that these observations would need to be correlated, but it
is not possible within the currently implemented covariance structure
framework to do that correctly. Moreover, for non-spatial covariance
structures, the time variable must be a factor variable.

When optimizer is not set, first the default optimizer (`L-BFGS-B`) is
used to fit the model. If that converges, this is returned. If not, the
other available optimizers from
[`h_get_optimizers()`](https://openpharma.github.io/mmrm/reference/h_get_optimizers.md),
including `BFGS`, `CG` and `nlminb` are tried (in parallel if `n_cores`
is set and not on Windows). If none of the optimizers converge, then the
function fails. Otherwise the best fit is returned.

Note that fine-grained control specifications can either be passed
directly to the `mmrm` function, or via the `control` argument for
bundling together with the
[`mmrm_control()`](https://openpharma.github.io/mmrm/reference/mmrm_control.md)
function. Both cannot be used together, since this would delete the
arguments passed via `mmrm`.

## Note

The `mmrm` object is also an `mmrm_fit` and an `mmrm_tmb` object,
therefore corresponding methods also work (see
[`mmrm_tmb_methods`](https://openpharma.github.io/mmrm/reference/mmrm_tmb_methods.md)).

Additional contents depend on `vcov` (see
[`mmrm_control()`](https://openpharma.github.io/mmrm/reference/mmrm_control.md)):

- If Kenward-Roger covariance matrix is used, `kr_comp` contains
  necessary components and `beta_vcov_adj` includes the adjusted
  coefficients covariance matrix.

- If Empirical covariance matrix is used, `beta_vcov_adj` contains the
  corresponding coefficients covariance matrix estimate. In addition,
  `empirical_g_mat` contains the empirical g matrix, which is used to
  calculate the Satterthwaite degrees of freedom. The
  `score_per_subject` contains the empirical score per subject.

- If Asymptotic covariance matrix is used in combination with
  Satterthwaite d.f. adjustment, the Jacobian information `jac_list` is
  included.

Note that these additional elements might change over time and are to be
considered internal implementation details rather than part of the
public user interface of the package.

Use of the package `emmeans` is supported, see
[`emmeans_support`](https://openpharma.github.io/mmrm/reference/emmeans_support.md).

NA values are always omitted regardless of `na.action` setting.

When the number of visit levels is large, it usually requires large
memory to create the covariance matrix. By default, the maximum allowed
visit levels is 100, and if there are more visit levels, a confirmation
is needed if run interactively. You can use
`options(mmrm.max_visits = <target>)` to increase the maximum allowed
number of visit levels. In non-interactive sessions the confirmation is
not raised and will directly give you an error if the number of visit
levels exceeds the maximum.

## Examples

``` r
fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)

# Direct specification of control details:
fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  weights = fev_data$WEIGHTS,
  method = "Kenward-Roger"
)

# Alternative specification via control argument (but you cannot mix the
# two approaches):
fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  control = mmrm_control(method = "Kenward-Roger")
)
```
