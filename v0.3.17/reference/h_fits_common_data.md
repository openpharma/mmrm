# Combine the Datasets from `mmrm` Fits

Take the data columns used in each `mmrm` fit and
[`merge()`](https://rdrr.io/r/base/merge.html) them.

## Usage

``` r
h_fits_common_data(fits)
```

## Arguments

- fits:

  (`list`)  
  list of `mmrm` fits.

## Value

A data frame combining all the common observations among the datasets
underlying the elements of `fits`.

## Details

All default arguments for [`merge()`](https://rdrr.io/r/base/merge.html)
are used, resulting in a "natural join": the result will only contain
the observations found in all datasets.

[`droplevels()`](https://rdrr.io/r/base/droplevels.html) is applied to
the final product to prevent extraneous warnings.
