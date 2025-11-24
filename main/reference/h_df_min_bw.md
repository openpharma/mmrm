# Assign Minimum Degrees of Freedom Given Involved Coefficients

Used in
[`h_df_1d_bw()`](https://openpharma.github.io/mmrm/reference/h_df_1d_bw.md)
and
[`h_df_md_bw()`](https://openpharma.github.io/mmrm/reference/h_df_md_bw.md).

## Usage

``` r
h_df_min_bw(bw_calc, is_coef_involved)
```

## Arguments

- bw_calc:

  (`list`)  
  from
  [`h_df_bw_calc()`](https://openpharma.github.io/mmrm/reference/h_df_bw_calc.md).

- is_coef_involved:

  (`logical`)  
  whether each coefficient is involved in the contrast.

## Value

The minimum of the degrees of freedom assigned to each involved
coefficient according to its between-within categorization.
