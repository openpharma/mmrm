# Obtain the Contracted Linear Adjusted Covariance Matrix

Obtains the linear Kenward-Roger adjusted covariance matrix for the
coefficient estimates from the `P` matrices and the contracted `S_Q`.
Used in [`mmrm()`](https://openpharma.github.io/mmrm/reference/mmrm.md)
fitting if vcov is "Kenward-Roger-Linear".

## Usage

``` r
h_var_adj_contracted(v, w, p, s_q)
```

## Arguments

- v:

  (`matrix`)\
  unadjusted covariance matrix.

- w:

  (`matrix`)\
  covariance matrix of the estimated covariance parameters.

- p:

  (`matrix`)\
  P matrix from
  [`h_get_kr_comp()`](https://openpharma.github.io/mmrm/reference/h_get_kr_comp.md).

- s_q:

  (`matrix`)\
  contracted Q sum `S_Q` from
  [`h_get_kr_comp()`](https://openpharma.github.io/mmrm/reference/h_get_kr_comp.md)
  called with `w`.

## Value

The matrix of adjusted covariance matrix.

## Details

See the section "Contracted linear covariance adjustment" in
[`vignette("kenward", package = "mmrm")`](https://openpharma.github.io/mmrm/articles/kenward.md).
The full `w` is used, so that cross-group covariance parameter
covariances contribute.
