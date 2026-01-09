# Calculation of Kenward-Roger Degrees of Freedom for One-Dimensional Contrast

Used in
[`df_1d()`](https://openpharma.github.io/mmrm/reference/df_1d.md) if
method is "Kenward-Roger" or "Kenward-Roger-Linear".

## Usage

``` r
h_df_1d_kr(object, contrast)
```

## Arguments

- object:

  (`mmrm`)  
  the MMRM fit.

- contrast:

  (`numeric`)  
  contrast vector. Note that this should not include elements for
  singular coefficient estimates, i.e. only refer to the actually
  estimated coefficients.

## Value

List with `est`, `se`, `df`, `t_stat` and `p_val`.
