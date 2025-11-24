# Drop Items from an Indexible

Drop elements from an indexible object (`vector`, `list`, etc.).

## Usage

``` r
drop_elements(x, n)
```

## Arguments

- x:

  Any object that can be consumed by
  [`seq_along()`](https://rdrr.io/r/base/seq.html) and indexed by a
  logical vector of the same length.

- n:

  (`integer`)  
  the number of terms to drop.

## Value

A subset of `x`.
