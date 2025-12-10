# Low-Level Fitting Function for MMRM

**\[stable\]**

This is the low-level function to fit an MMRM. Note that this does not
try different optimizers or adds Jacobian information etc. in contrast
to [`mmrm()`](https://openpharma.github.io/mmrm/reference/mmrm.md).

## Usage

``` r
fit_mmrm(
  formula,
  data,
  weights,
  reml = TRUE,
  covariance = NULL,
  tmb_data,
  formula_parts,
  control = mmrm_control()
)
```

## Arguments

- formula:

  (`formula`)  
  model formula with exactly one special term specifying the visits
  within subjects, see details.

- data:

  (`data.frame`)  
  input data containing the variables used in `formula`.

- weights:

  (`vector`)  
  input vector containing the weights.

- reml:

  (`flag`)  
  whether restricted maximum likelihood (REML) estimation is used,
  otherwise maximum likelihood (ML) is used.

- covariance:

  (`cov_struct`)  
  A covariance structure type definition, or value that can be coerced
  to a covariance structure using
  [`as.cov_struct()`](https://openpharma.github.io/mmrm/reference/as.cov_struct.md).
  If no value is provided, a structure is derived from the provided
  formula.

- tmb_data:

  (`mmrm_tmb_data`)  
  object.

- formula_parts:

  (`mmrm_tmb_formula_parts`)  
  list with formula parts from
  [`h_mmrm_tmb_formula_parts()`](https://openpharma.github.io/mmrm/reference/h_mmrm_tmb_formula_parts.md).

- control:

  (`mmrm_control`)  
  list of control options produced by
  [`mmrm_control()`](https://openpharma.github.io/mmrm/reference/mmrm_control.md).

## Value

List of class `mmrm_tmb`, see
[`h_mmrm_tmb_fit()`](https://openpharma.github.io/mmrm/reference/h_mmrm_tmb_fit.md)
for details. In addition, it contains elements `call` and `optimizer`.

## Details

The `formula` typically looks like:

`FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)`

which specifies response and covariates as usual, and exactly one
special term defines which covariance structure is used and what are the
visit and subject variables.

Always use only the first optimizer if multiple optimizers are provided.

## Examples

``` r
formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
data <- fev_data
system.time(result <- fit_mmrm(formula, data, rep(1, nrow(fev_data))))
#>    user  system elapsed 
#>   0.038   0.000   0.038 
```
