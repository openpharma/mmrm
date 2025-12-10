# Drop Formula Terms used for Covariance Structure Definition

Drop Formula Terms used for Covariance Structure Definition

## Usage

``` r
h_drop_covariance_terms(f)
```

## Arguments

- f:

  (`formula`)  
  a formula from which covariance terms should be dropped.

## Value

The formula without accepted covariance terms.

## Details

`terms` is used and it will preserve the environment attribute. This
ensures the returned formula and the input formula have the same
environment.
