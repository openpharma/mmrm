# Covariance Type Database

An internal constant for covariance type information.

## Usage

``` r
COV_TYPES
```

## Format

A data frame with 5 variables and one record per covariance type:

- name:

  The long-form name of the covariance structure type

- abbr:

  The abbreviated name of the covariance structure type

- habbr:

  The abbreviated name of the heterogeneous version of a covariance
  structure type (The abbreviated name (`abbr`) with a trailing `"h"` if
  the structure has a heterogeneous implementation or `NA` otherwise).

- heterogeneous:

  A logical value indicating whether the covariance structure has a
  heterogeneous counterpart.

- spatial:

  A logical value indicating whether the covariance structure is
  spatial.
