# Reconcile Possible Covariance Structure Inputs

Reconcile Possible Covariance Structure Inputs

## Usage

``` r
h_reconcile_cov_struct(formula = NULL, covariance = NULL)
```

## Arguments

- formula:

  (`formula`)  
  the model formula, see details.

- covariance:

  (`cov_struct`)  
  a covariance structure type definition as produced with
  [`cov_struct()`](https://openpharma.github.io/mmrm/reference/cov_struct.md),
  or value that can be coerced to a covariance structure using
  [`as.cov_struct()`](https://openpharma.github.io/mmrm/reference/as.cov_struct.md).
  If no value is provided, a structure is derived from the provided
  formula.

## Value

The value `covariance` if it's provided or a covariance structure
derived from the provided `formula` otherwise. An error is raised of
both are provided.
