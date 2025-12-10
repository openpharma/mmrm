# Ensure LRT Is Appropriate for `list` of `mmrm` Fits

Throws an error if the degrees of freedom aren't monotonically
increasing, if the models aren't nested, or if `refit = FALSE` and the
models have different underlying data.

## Usage

``` r
h_assert_lrt_suitability(fits, refit, dfs, is_reml)
```

## Arguments

- fits:

  (`list`)  
  list of `mmrm` fits.

- refit:

  (`flag`)  
  `TRUE` or `FALSE` indicating whether or not the user gave permission
  to refit models to make all models suitable for LRT.

- dfs:

  (`numeric`)  
  vector of the degrees of freedom for each element of `fits`.

- is_reml:

  (`logical`)  
  vector indicating whether or not REML was used for each element of
  `fits`.

## Value

`TRUE` if the list of `fits` are suitable for LRT. Otherwise, an error
is thrown.
