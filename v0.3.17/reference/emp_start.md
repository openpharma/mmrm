# Empirical Starting Value

Obtain empirical start value for unstructured covariance

## Usage

``` r
emp_start(
  y_vector,
  x_matrix,
  full_frame,
  visit_var,
  subject_var,
  subject_groups,
  ...
)
```

## Arguments

- y_vector:

  (`numeric`) response variable.

- x_matrix:

  (`matrix`) design matrix.

- full_frame:

  (`data.frame`) full data frame used for model fitting.

- visit_var:

  (`string`)  
  visit variable.

- subject_var:

  (`string`)  
  subject id variable.

- subject_groups:

  (`factor`)  
  subject group assignment.

- ...:

  not used.

## Value

A numeric vector of starting values.

## Details

This `emp_start` only works for unstructured covariance structure. It
uses linear regression to first obtain the coefficients and use the
residuals to obtain the empirical variance-covariance, and it is then
used to obtain the starting values.
