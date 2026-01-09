# Data for `TMB` Fit

Data for `TMB` Fit

## Usage

``` r
h_mmrm_tmb_data(
  formula_parts,
  data,
  weights,
  reml,
  singular = c("drop", "error", "keep"),
  drop_visit_levels,
  allow_na_response = FALSE,
  drop_levels = TRUE,
  xlev = NULL,
  contrasts = NULL
)
```

## Arguments

- formula_parts:

  (`mmrm_tmb_formula_parts`)  
  list with formula parts from
  [`h_mmrm_tmb_formula_parts()`](https://openpharma.github.io/mmrm/reference/h_mmrm_tmb_formula_parts.md).

- data:

  (`data.frame`)  
  which contains variables used in `formula_parts`.

- weights:

  (`vector`)  
  weights to be used in the fitting process.

- reml:

  (`flag`)  
  whether restricted maximum likelihood (REML) estimation is used,
  otherwise maximum likelihood (ML) is used.

- singular:

  (`string`)  
  choices of method deal with rank-deficient matrices. "error" to stop
  the function return the error, "drop" to drop these columns, and
  "keep" to keep all the columns.

- drop_visit_levels:

  (`flag`)  
  whether to drop levels for visit variable, if visit variable is a
  factor.

- allow_na_response:

  (`flag`)  
  whether NA in response is allowed.

- drop_levels:

  (`flag`)  
  whether drop levels for covariates. If not dropped could lead to
  singular matrix.

## Value

List of class `mmrm_tmb_data` with elements:

- `full_frame`: `data.frame` with `n` rows containing all variables
  needed in the model.

- `data`: `data.frame` of input dataset.

- `x_matrix`: `matrix` with `n` rows and `p` columns specifying the
  overall design matrix.

- `x_cols_aliased`: `logical` with potentially more than `p` elements
  indicating which columns in the original design matrix have been left
  out to obtain a full rank `x_matrix`.

- `y_vector`: length `n` `numeric` specifying the overall response
  vector.

- `weights_vector`: length `n` `numeric` specifying the weights vector.

- `n_visits`: `int` with the number of visits, which is the dimension of
  the covariance matrix.

- `n_subjects`: `int` with the number of subjects.

- `subject_zero_inds`: length `n_subjects` `integer` containing the
  zero-based start indices for each subject.

- `subject_n_visits`: length `n_subjects` `integer` containing the
  number of observed visits for each subjects. So the sum of this vector
  equals `n`.

- `cov_type`: `string` value specifying the covariance type.

- `is_spatial_int`: `int` specifying whether the covariance structure is
  spatial(1) or not(0).

- `reml`: `int` specifying whether REML estimation is used (1),
  otherwise ML (0).

- `subject_groups`: `factor` specifying the grouping for each subject.

- `n_groups`: `int` with the number of total groups

## Details

Note that the `subject_var` must not be factor but can also be
character. If it is character, then it will be converted to factor
internally. Here the levels will be the unique values, sorted
alphabetically and numerically if there is a common string prefix of
numbers in the character elements. For full control on the order please
use a factor.
