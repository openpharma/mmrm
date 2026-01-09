# Coefficients Table for MMRM Fit

This is used by
[`summary.mmrm()`](https://openpharma.github.io/mmrm/reference/mmrm_methods.md)
to obtain the coefficients table.

## Usage

``` r
h_coef_table(object)
```

## Arguments

- object:

  (`mmrm`)  
  model fit.

## Value

Matrix with one row per coefficient and columns `Estimate`,
`Std. Error`, `df`, `t value` and `Pr(>|t|)`.
