# Calculation of Between-Within Degrees of Freedom

Used in
[`h_df_1d_bw()`](https://openpharma.github.io/mmrm/reference/h_df_1d_bw.md)
and
[`h_df_md_bw()`](https://openpharma.github.io/mmrm/reference/h_df_md_bw.md).

## Usage

``` r
h_df_bw_calc(object)
```

## Arguments

- object:

  (`mmrm`)  
  the fitted MMRM.

## Value

List with:

- `coefs_between_within` calculated via
  [`h_within_or_between()`](https://openpharma.github.io/mmrm/reference/h_within_or_between.md)

- `ddf_between`

- `ddf_within`
