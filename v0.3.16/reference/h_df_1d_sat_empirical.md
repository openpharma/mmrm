# Helper for Calculation of Satterthwaite with Empirical Covariance Matrix

Used in
[`h_df_1d_sat()`](https://openpharma.github.io/mmrm/reference/h_df_1d_sat.md)
and
[`h_df_md_sat()`](https://openpharma.github.io/mmrm/reference/h_df_md_sat.md)
if empirical covariance matrix is used.

## Usage

``` r
h_df_1d_sat_empirical(object, contrast_matrix)
```

## Arguments

- object:

  (`mmrm`)  
  the MMRM fit.

- contrast_matrix:

  (`matrix`)  
  contrast matrix with number of subjects times number of coefficients
  as the number of columns.

## Value

Adjusted degrees of freedom value.
