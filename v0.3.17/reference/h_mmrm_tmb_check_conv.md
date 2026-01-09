# Checking the `TMB` Optimization Result

Checking the `TMB` Optimization Result

## Usage

``` r
h_mmrm_tmb_check_conv(tmb_opt, mmrm_tmb)
```

## Arguments

- tmb_opt:

  (`list`)  
  optimization result.

- mmrm_tmb:

  (`mmrm_tmb`)  
  result from
  [`h_mmrm_tmb_fit()`](https://openpharma.github.io/mmrm/reference/h_mmrm_tmb_fit.md).

## Value

Nothing, only used to generate warnings in case that the model did not
converge.
