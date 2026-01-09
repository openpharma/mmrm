# Processing the Formula for `TMB` Fit

Processing the Formula for `TMB` Fit

## Usage

``` r
h_mmrm_tmb_formula_parts(
  formula,
  covariance = as.cov_struct(formula, warn_partial = FALSE)
)
```

## Arguments

- formula:

  (`formula`)  
  Original formula.

- covariance:

  (`cov_struct`)  
  A covariance structure from which additional formula parts should be
  added.

## Value

List of class `mmrm_tmb_formula_parts` with elements:

- `formula`: the original input.

- `model_formula`: `formula` with the covariance term is removed.

- `model_formula`: `formula` with the covariance term removed.

- `full_formula`: same as `model_formula` but includes the covariance
  structure's subject, visit and (optionally) group variables.

- `cov_type`: `string` with covariance term type (e.g. `"us"`).

- `is_spatial`: `flag` indicator of whether the covariance structure is
  spatial

- `visit_var`: `character` with the visit variable name.

- `subject_var`: `string` with the subject variable name.

- `group_var`: `string` with the group variable name. If no group
  specified, this element is `NULL`.

- `model_var`: `character` with the variables names of the formula,
  except `subject_var`.

- `response_var`: `string` with the response variable name.
