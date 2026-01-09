# Obtain Type 2 Contrast for One Specified Effect

This is support function to obtain contrast matrix for type II testing.

## Usage

``` r
h_type2_contrast(object, effect, tol = sqrt(.Machine$double.eps))
```

## Arguments

- object:

  (`mmrm`)  
  the fitted MMRM.

- effect:

  (`string`) the name of the effect.

- tol:

  (`numeric`) threshold below which values are treated as 0.

## Value

A `matrix` of the contrast.
