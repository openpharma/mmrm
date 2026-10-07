# Obtain Theta from Covariance Matrix

Obtain unstructured theta from covariance matrix.

## Usage

``` r
h_get_theta_from_cov(covariance)
```

## Arguments

- covariance:

  (`matrix`) of covariance matrix values.

## Value

Numeric vector of the theta values.

## Details

If the covariance matrix has `NA` in some of the elements, they will be
replaced by 0 (non-diagonal) and 1 (diagonal). This ensures that the
matrix is positive definite.
