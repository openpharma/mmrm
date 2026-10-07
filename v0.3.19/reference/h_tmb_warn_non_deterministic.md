# Warn if TMB is Configured to Use Non-Deterministic Hash for Tape Optimizer

This function checks the TMB configuration for the
`tmbad_deterministic_hash` setting If it is set to `FALSE`, a warning is
issued indicating that this may lead to unreproducible results.

## Usage

``` r
h_tmb_warn_non_deterministic()
```

## Value

No return value, called for side effects.
