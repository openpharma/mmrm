# Calculation of Satterthwaite Degrees of Freedom for Multi-Dimensional Contrast

Used in
[`df_md()`](https://openpharma.github.io/mmrm/reference/df_md.md) if
method is "Satterthwaite".

## Usage

``` r
h_df_md_sat(object, contrast)
```

## Arguments

- object:

  (`mmrm`)  
  the MMRM fit.

- contrast:

  (`matrix`)  
  numeric contrast matrix, if given a `numeric` then this is coerced to
  a row vector. Note that this should not include elements for singular
  coefficient estimates, i.e. only refer to the actually estimated
  coefficients.

## Value

List with `num_df`, `denom_df`, `f_stat` and `p_val` (2-sided p-value).
