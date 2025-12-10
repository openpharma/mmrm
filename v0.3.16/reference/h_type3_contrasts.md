# Obtain Type 3 Contrast for All Effects

This is support function to obtain contrast matrices for type III
testing.

## Usage

``` r
h_type3_contrasts(object, tol = sqrt(.Machine$double.eps))
```

## Arguments

- object:

  (`mmrm`)  
  the fitted MMRM.

- tol:

  (`numeric`) threshold below which values are treated as 0.

## Value

A `list` of contrast matrices, one per effect.
