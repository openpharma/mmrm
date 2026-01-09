# Add Individual Covariance Variables As Terms to Formula

Add Individual Covariance Variables As Terms to Formula

## Usage

``` r
h_add_covariance_terms(f, covariance)
```

## Arguments

- f:

  (`formula`)  
  a formula to which covariance structure terms should be added.

- covariance:

  (`cov_struct`)  
  a covariance structure object from which additional variables should
  be sourced.

## Value

A new formula with included covariance terms.

## Details

[`stats::update()`](https://rdrr.io/r/stats/update.html) is used to
append the covariance structure and the environment attribute will not
be changed. This ensures the returned formula and the input formula have
the same environment.
