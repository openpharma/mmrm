# Extract covariance matrix from `TMB` report and input data

This helper does some simple post-processing to extract covariance
matrix or named list of covariance matrices if the fitting is using
grouped covariance matrices.

## Usage

``` r
h_mmrm_tmb_extract_cov(tmb_report, tmb_data, visit_var, is_spatial)
```

## Arguments

- tmb_report:

  (`list`)  
  report created with
  [`TMB::MakeADFun()`](https://rdrr.io/pkg/TMB/man/MakeADFun.html)
  report function.

- tmb_data:

  (`mmrm_tmb_data`)  
  produced by
  [`h_mmrm_tmb_data()`](https://openpharma.github.io/mmrm/reference/h_mmrm_tmb_data.md).

- visit_var:

  (`character`)  
  character vector of the visit variable

- is_spatial:

  (`flag`)  
  indicator whether the covariance structure is spatial.

## Value

Return a simple covariance matrix if there is no grouping, or a named
list of estimated grouped covariance matrices, with its name equal to
the group levels.
