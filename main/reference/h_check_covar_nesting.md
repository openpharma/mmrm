# Ensure Two Models' Covariates Are Nested

Throws an error if the covariates of `model_basic` aren't the same as or
a subset of the covariates of `model_augmented`.

## Usage

``` r
h_check_covar_nesting(model_basic, model_augmented)
```

## Arguments

- model_basic, model_augmented:

  (`mmrm_tmb_formula_parts`)  
  the `formula_parts` element of an `mmrm` model fit.

## Value

`"identical"` if `model_basic` and `model_augmented` have the same set
of covariates. `"nesting"` if the covariates of `model_basic` are a
subset of the covariates of `model_augmented`.

## Details

For upstream coding brevity, this function accepts the `formula_parts`
element of two `mmrm` model fits rather than `mmrm` objects. Such
objects are of class `mmrm_tmb_formula_parts`.
