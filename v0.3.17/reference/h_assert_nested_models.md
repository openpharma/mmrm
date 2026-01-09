# Ensure Two Models Are Nested

Throws an error if `model_basic` isn't nested within `model_augmented`.

## Usage

``` r
h_assert_nested_models(model_basic, model_augmented, any_reml)
```

## Arguments

- model_basic, model_augmented:

  (`mmrm`)  
  model fits.

- any_reml:

  (`flag`)  
  `TRUE` or `FALSE` indicating whether or not either model used REML
  estimation.

## Value

`TRUE` if `model_basic` is nested within `model_augmented`. Otherwise,
an error is thrown.

## Details

The following checks are applied in this order, and an error is thrown
if any of the conditions are not met:

1.  The fits must have the same visit, subject, and grouping variables.

2.  The covariates of `model_basic` must be the same as or a subset of
    the covariates of `model_augmented`.

3.  The covariance structure of `model_basic` must be the same as or a
    special case of the covariance structure of `model_augmented`.

Finally, if all these checks were passed, a warning is thrown if the two
fits have identical covariates and covariance structures.
