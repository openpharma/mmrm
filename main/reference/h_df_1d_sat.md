# Calculation of Satterthwaite Degrees of Freedom for One-Dimensional Contrast

Used in
[`df_1d()`](https://openpharma.github.io/mmrm/reference/df_1d.md) if
method is "Satterthwaite".

## Usage

``` r
h_df_1d_sat(object, contrast)
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
