# Obtain List of Jacobian Matrix Entries for Covariance Matrix

Obtain the Jacobian matrices given the covariance function and variance
parameters.

## Usage

``` r
h_jac_list(tmb_data, theta_est, beta_vcov)
```

## Arguments

- tmb_data:

  (`mmrm_tmb_data`)  
  produced by
  [`h_mmrm_tmb_data()`](https://openpharma.github.io/mmrm/reference/h_mmrm_tmb_data.md).

- theta_est:

  (`numeric`)  
  variance parameters point estimate.

- beta_vcov:

  (`matrix`)  
  vairance covariance matrix of coefficients.

## Value

List with one element per variance parameter containing a matrix of the
same dimensions as the covariance matrix. The values are the derivatives
with regards to this variance parameter.
