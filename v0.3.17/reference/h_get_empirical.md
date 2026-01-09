# Obtain Empirical/Jackknife/Bias-Reduced Covariance

Obtain the empirical or Jackknife covariance for \\\beta\\. Used in
`mmrm` fitting if method is "Empirical", "Empirical-Jackknife" or
"Empirical-Bias-Reduced".

## Usage

``` r
h_get_empirical(tmb_data, theta, beta, beta_vcov, type)
```

## Arguments

- tmb_data:

  (`mmrm_tmb_data`)  
  produced by
  [`h_mmrm_tmb_data()`](https://openpharma.github.io/mmrm/reference/h_mmrm_tmb_data.md).

- theta:

  (`numeric`)  
  theta estimate.

- beta:

  (`numeric`)  
  beta estimate.

- beta_vcov:

  (`matrix`)  
  covariance of beta estimate.

- type:

  (`string`)  
  type of empirical method, including "Empirical", "Empirical-Jackknife"
  and "Empirical-Bias-Reduced".

## Value

Named list with elements:

- `cov`: `matrix` empirical covariance.

- `g_mat`: `matrix` to calculate Satterthwaite degrees of freedom.

## Note

This function used to return `df_mat`, which was equivalent to
`crossproduct(g_mat)`. However, executing the cross product in C++ was a
costly matrix multiplication, in particular when the number of
coefficients and/or the number of subjects was large. Therefore this is
now avoided and `g_mat` is returned instead.
