# Standard Starting Value

Obtain standard start values.

## Usage

``` r
std_start(cov_type, n_visits, n_groups, ...)
```

## Arguments

- cov_type:

  (`string`)  
  name of the covariance structure.

- n_visits:

  (`int`)  
  number of visits.

- n_groups:

  (`int`)  
  number of groups.

- ...:

  not used.

## Value

A numeric vector of starting values.

## Details

`std_start` will try to provide variance parameter from identity matrix.
However, for `ar1` and `ar1h` the corresponding values are not ideal
because the \\\rho\\ is usually a positive number thus using 0 as
starting value can lead to incorrect optimization result, and we use 0.5
as the initial value of \\\rho\\.
