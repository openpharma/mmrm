# Creating F-Statistic Results from One-Dimensional Contrast

Creates multi-dimensional result from one-dimensional contrast from
[`df_1d()`](https://openpharma.github.io/mmrm/reference/df_1d.md).

## Usage

``` r
h_df_md_from_1d(object, contrast)
```

## Arguments

- object:

  (`mmrm`)  
  model fit.

- contrast:

  (`numeric`)  
  one-dimensional contrast.

## Value

The one-dimensional degrees of freedom are calculated and then based on
that the p-value is calculated.
