# Validate mmrm Formula

Validate mmrm Formula

## Usage

``` r
h_valid_formula(formula)
```

## Arguments

- formula:

  (`formula`)  
  to check.

## Details

In mmrm models, `.` is not allowed as it introduces ambiguity of
covariates to be used, so it is not allowed to be in formula.
