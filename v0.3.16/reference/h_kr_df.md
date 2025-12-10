# Obtain the Adjusted Kenward-Roger degrees of freedom

Obtains the adjusted Kenward-Roger degrees of freedom and F statistic
scale parameter. Used in
[`h_df_md_kr()`](https://openpharma.github.io/mmrm/reference/h_df_md_kr.md)
or
[h_df_1d_kr](https://openpharma.github.io/mmrm/reference/h_df_1d_kr.md).

## Usage

``` r
h_kr_df(v0, l, w, p)
```

## Arguments

- v0:

  (`matrix`)  
  unadjusted covariance matrix.

- l:

  (`matrix`)  
  linear combination matrix.

- w:

  (`matrix`)  
  hessian matrix.

- p:

  (`matrix`)  
  P matrix from
  [`h_get_kr_comp()`](https://openpharma.github.io/mmrm/reference/h_get_kr_comp.md).

## Value

Named list with elements:

- `m`: `numeric` degrees of freedom.

- `lambda`: `numeric` F statistic scale parameter.
