# Build `TMB` Fit Result List

This helper does some simple post-processing of the `TMB` object and
optimization results, including setting names, inverting matrices etc.

## Usage

``` r
h_mmrm_tmb_fit(
  tmb_object,
  tmb_opt,
  formula_parts,
  tmb_data,
  disable_theta_vcov = FALSE
)
```

## Arguments

- tmb_object:

  (`list`)  
  created with
  [`TMB::MakeADFun()`](https://rdrr.io/pkg/TMB/man/MakeADFun.html).

- tmb_opt:

  (`list`)  
  optimization result.

- formula_parts:

  (`mmrm_tmb_formula_parts`)  
  produced by
  [`h_mmrm_tmb_formula_parts()`](https://openpharma.github.io/mmrm/reference/h_mmrm_tmb_formula_parts.md).

- tmb_data:

  (`mmrm_tmb_data`)  
  produced by
  [`h_mmrm_tmb_data()`](https://openpharma.github.io/mmrm/reference/h_mmrm_tmb_data.md).

- disable_theta_vcov:

  (`flag`)  
  whether calculation of `theta_vcov` was disabled.

## Value

List of class `mmrm_tmb` with:

- `cov`: estimated covariance matrix, or named list of estimated group
  specific covariance matrices.

- `beta_est`: vector of coefficient estimates.

- `beta_vcov`: Variance-covariance matrix for coefficient estimates.

- `beta_vcov_inv_L`: Lower triangular matrix `L` of the inverse
  variance-covariance matrix decomposition.

- `beta_vcov_inv_D`: vector of diagonal matrix `D` of the inverse
  variance-covariance matrix decomposition.

- `theta_est`: vector of variance parameter estimates.

- `theta_vcov`: variance-covariance matrix for variance parameter
  estimates (if not disabled).

- `neg_log_lik`: obtained negative log-likelihood.

- `formula_parts`: input.

- `data`: input.

- `weights`: input.

- `reml`: input as a flag.

- `opt_details`: list with optimization details including convergence
  code.

- `tmb_object`: original `TMB` object created with
  [`TMB::MakeADFun()`](https://rdrr.io/pkg/TMB/man/MakeADFun.html).

- `tmb_data`: input.

## Details

Instead of inverting or decomposing `beta_vcov`, it can be more
efficient to use its robust Cholesky decomposition `LDL^T`, therefore we
return the corresponding two components `L` and `D` as well since they
have been available on the `C++` side already.
