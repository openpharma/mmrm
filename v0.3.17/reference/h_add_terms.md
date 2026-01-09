# Add Formula Terms with Character

Add formula terms from the original formula with character
representation.

## Usage

``` r
h_add_terms(f, adds, drop_response = FALSE)
```

## Arguments

- f:

  (`formula`)  
  a formula to be updated.

- adds:

  (`character`)  
  representation of elements to be added.

- drop_response:

  (`flag`)  
  whether response should be dropped.

## Value

A new formula with elements in `drops` removed.

## Details

Elements in `adds` will be added from the formula, while the environment
of the formula is unchanged. If `adds` is `NULL` or `character(0)`, the
formula is unchanged.
