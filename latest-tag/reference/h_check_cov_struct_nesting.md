# Ensure Two Models' Covariance Structures Are Nested

Throws an error if the covariance structure of `model_basic` isn't the
same as or a special case of the covariance structure of
`model_augmented`.

## Usage

``` r
h_check_cov_struct_nesting(model_basic, model_augmented)
```

## Arguments

- model_basic, model_augmented:

  (`mmrm_tmb_formula_parts`)  
  the `formula_parts` element of an `mmrm` model fit.

## Value

`"identical"` if `model_basic` and `model_augmented` have the same
covariance structure. `"nesting"` if the covariance structure of
`model_basic` is a special case of the covariance structure of
`model_augmented`.

## Details

The check for "nesting" is a check against a mathematically determined
hierarchy of the available
[cov_types](https://openpharma.github.io/mmrm/reference/covariance_types.md):
if restricting an aspect of a covariance structure can result in another
covariance structure, the latter structure is nested within the former
structure.

For upstream coding brevity, this function accepts the `formula_parts`
element of two `mmrm` model fits rather than `mmrm` objects. Such
objects are of class `mmrm_tmb_formula_parts`.
