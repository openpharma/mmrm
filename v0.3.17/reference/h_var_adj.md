# Obtain the Adjusted Covariance Matrix

Obtains the Kenward-Roger adjusted covariance matrix for the coefficient
estimates. Used in
[`mmrm()`](https://openpharma.github.io/mmrm/reference/mmrm.md) fitting
if method is "Kenward-Roger" or "Kenward-Roger-Linear".

## Usage

``` r
h_var_adj(v, w, p, q, r, linear = FALSE)
```

## Arguments

- v:

  (`matrix`)  
  unadjusted covariance matrix.

- w:

  (`matrix`)  
  hessian matrix.

- p:

  (`matrix`)  
  P matrix from
  [`h_get_kr_comp()`](https://openpharma.github.io/mmrm/reference/h_get_kr_comp.md).

- q:

  (`matrix`)  
  Q matrix from
  [`h_get_kr_comp()`](https://openpharma.github.io/mmrm/reference/h_get_kr_comp.md).

- r:

  (`matrix`)  
  R matrix from
  [`h_get_kr_comp()`](https://openpharma.github.io/mmrm/reference/h_get_kr_comp.md).

- linear:

  (`flag`)  
  whether to use linear Kenward-Roger approximation.

## Value

The matrix of adjusted covariance matrix.
