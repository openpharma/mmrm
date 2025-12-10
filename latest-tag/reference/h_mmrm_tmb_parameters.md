# Start Parameters for `TMB` Fit

Start Parameters for `TMB` Fit

## Usage

``` r
h_mmrm_tmb_parameters(formula_parts, tmb_data, start, n_groups = 1L)
```

## Arguments

- formula_parts:

  (`mmrm_tmb_formula_parts`)  
  produced by
  [`h_mmrm_tmb_formula_parts()`](https://openpharma.github.io/mmrm/reference/h_mmrm_tmb_formula_parts.md).

- tmb_data:

  (`mmrm_tmb_data`)  
  produced by
  [`h_mmrm_tmb_data()`](https://openpharma.github.io/mmrm/reference/h_mmrm_tmb_data.md).

- start:

  (`numeric` or `NULL`)  
  optional start values for variance parameters.

- n_groups:

  (`int`)  
  number of groups.

## Value

List with element `theta` containing the start values for the variance
parameters.
