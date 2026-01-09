# Fitting an MMRM with Single Optimizer

**\[stable\]**

This function helps to fit an MMRM using `TMB` with a single optimizer,
while capturing messages and warnings.

## Usage

``` r
fit_single_optimizer(
  formula,
  data,
  weights,
  reml = TRUE,
  covariance = NULL,
  tmb_data,
  formula_parts,
  ...,
  control = mmrm_control(...)
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

- reml:

  (`flag`)  
  whether restricted maximum likelihood (REML) estimation is used,
  otherwise maximum likelihood (ML) is used.

- covariance:

  (`cov_struct`)  
  a covariance structure type definition as produced with
  [`cov_struct()`](https://openpharma.github.io/mmrm/reference/cov_struct.md),
  or value that can be coerced to a covariance structure using
  [`as.cov_struct()`](https://openpharma.github.io/mmrm/reference/as.cov_struct.md).
  If no value is provided, a structure is derived from the provided
  formula.

- tmb_data:

  (`mmrm_tmb_data`)  
  object.

- formula_parts:

  (`mmrm_tmb_formula_parts`)  
  object.

- ...:

  Additional arguments to pass to
  [`mmrm_control()`](https://openpharma.github.io/mmrm/reference/mmrm_control.md).

- control:

  (`mmrm_control`)  
  object.

## Value

The `mmrm_fit` object, with additional attributes containing warnings,
messages, optimizer used and convergence status in addition to the
`mmrm_tmb` contents.

## Details

`fit_single_optimizer` will fit the `mmrm` model using the `control`
provided. If there are multiple optimizers provided in `control`, only
the first optimizer will be used. If `tmb_data` and `formula_parts` are
both provided, `formula`, `data`, `weights`, `reml`, and `covariance`
are ignored.

## Examples

``` r
mod_fit <- fit_single_optimizer(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  weights = rep(1, nrow(fev_data)),
  optimizer = "nlminb"
)
attr(mod_fit, "converged")
#> [1] TRUE
```
