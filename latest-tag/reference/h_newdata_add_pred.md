# Add Prediction Results to New Data

This is used in
[`augment.mmrm()`](https://openpharma.github.io/mmrm/reference/mmrm_tidiers.md).

## Usage

``` r
h_newdata_add_pred(x, newdata, se_fit, interval, ...)
```

## Arguments

- x:

  (`mmrm`)  
  fit.

- newdata:

  (`data.frame`)  
  data to predict.

- se_fit:

  (`flag`)  
  whether to return standard error of prediction, can only be used when
  `interval` is not "none".

- interval:

  (`string`)  
  type of interval.

- ...:

  passed to
  [`predict.mmrm_tmb()`](https://openpharma.github.io/mmrm/reference/mmrm_tmb_methods.md).

## Value

The `newdata` as a `tibble` with additional columns `.fitted`, `.lower`,
`.upper` (if interval is not `none`) and `.se.fit` (if `se_fit`
requested).
