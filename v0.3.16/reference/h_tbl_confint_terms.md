# Extract `tibble` with Confidence Intervals and Term Names

This is used in
[`tidy.mmrm()`](https://openpharma.github.io/mmrm/reference/mmrm_tidiers.md).

## Usage

``` r
h_tbl_confint_terms(x, ...)
```

## Arguments

- x:

  (`mmrm`)  
  fit object.

- ...:

  passed to [`stats::confint()`](https://rdrr.io/r/stats/confint.html),
  hence not used at the moment.

## Value

A `tibble` with `term`, `conf.low`, `conf.high` columns.
