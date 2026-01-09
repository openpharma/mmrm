# Extract Right-Hand-Side (rhs) from Formula

Extract Right-Hand-Side (rhs) from Formula

## Usage

``` r
formula_rhs(f)
```

## Arguments

- f:

  (`formula`)  
  a formula.

## Value

A formula without a response, derived from the right-hand-side of the
formula, `f`.

    formula_rhs(a ~ b + c)
    formula_rhs(~ b + c)
