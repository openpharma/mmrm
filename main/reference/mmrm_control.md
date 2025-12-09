# Control Parameters for Fitting an MMRM

**\[stable\]** Fine-grained specification of the MMRM fit details is
possible using this control function.

## Usage

``` r
mmrm_control(
  n_cores = 1L,
  method = c("Satterthwaite", "Kenward-Roger", "Residual", "Between-Within"),
  vcov = NULL,
  start = std_start,
  accept_singular = TRUE,
  drop_visit_levels = TRUE,
  disable_theta_vcov = FALSE,
  ...,
  optimizers = h_get_optimizers(...)
)
```

## Arguments

- n_cores:

  (`count`)  
  number of cores to be used.

- method:

  (`string`)  
  adjustment method for degrees of freedom.

- vcov:

  (`string`)  
  coefficients covariance matrix adjustment method.

- start:

  (`NULL`, `numeric` or `function`)  
  optional start values for variance parameters. See details for more
  information.

- accept_singular:

  (`flag`)  
  whether singular design matrices are reduced to full rank
  automatically and additional coefficient estimates will be missing.

- drop_visit_levels:

  (`flag`)  
  whether to drop levels for visit variable, if visit variable is a
  factor, see details.

- disable_theta_vcov:

  (`flag`)  
  whether to disable calculation of variance-covariance matrix for
  variance parameters. This can speed up fitting when there are many
  variance parameters, see details.

- ...:

  additional arguments passed to
  [`h_get_optimizers()`](https://openpharma.github.io/mmrm/reference/h_get_optimizers.md).

- optimizers:

  (`list`)  
  optimizer specification, created with
  [`h_get_optimizers()`](https://openpharma.github.io/mmrm/reference/h_get_optimizers.md).
  Please note that optimizers using the Hessian will not be compatible
  with `disable_theta_vcov = TRUE` and an error will be raised in that
  case.

## Value

List of class `mmrm_control` with the control parameters.

## Details

For example, if the data only has observations at visits `VIS1`, `VIS3`
and `VIS4`, by default they are treated to be equally spaced, the
distance from `VIS1` to `VIS3`, and from `VIS3` to `VIS4`, are
identical. However, you can manually convert this visit into a factor,
with `levels = c("VIS1", "VIS2", "VIS3", "VIS4")`, and also use
`drop_visits_levels = FALSE`, then the distance from `VIS1` to `VIS3`
will be double, as `VIS2` is a valid visit. However, please be cautious
because this can lead to convergence failure when using an unstructured
covariance matrix and there are no observations at the missing visits.

- The `method` and `vcov` arguments specify the degrees of freedom and
  coefficients covariance matrix adjustment methods, respectively.

  - Allowed `vcov` includes: "Asymptotic", "Kenward-Roger",
    "Kenward-Roger-Linear", "Empirical" (CR0), "Empirical-Jackknife"
    (CR3), and "Empirical-Bias-Reduced" (CR2).

  - Allowed `method` includes: "Satterthwaite", "Kenward-Roger",
    "Between-Within" and "Residual".

  - If `method` is "Kenward-Roger" then only "Kenward-Roger" or
    "Kenward-Roger-Linear" are allowed for `vcov`.

- The `vcov` argument can be `NULL` to use the default covariance method
  depending on the `method` used for degrees of freedom, see the
  following table:

  |                |                |
  |----------------|----------------|
  | `method`       | Default `vcov` |
  | Satterthwaite  | Asymptotic     |
  | Kenward-Roger  | Kenward-Roger  |
  | Residual       | Empirical      |
  | Between-Within | Asymptotic     |

- Please note that "Kenward-Roger" for "Unstructured" covariance gives
  different results compared to SAS; Use "Kenward-Roger-Linear" for
  `vcov` instead for better matching of the SAS results.

- The argument `start` is used to facilitate the choice of initial
  values for fitting the model. If `function` is provided, make sure its
  parameter is a valid element of `mmrm_tmb_data` or
  `mmrm_tmb_formula_parts` and it returns a numeric vector. By default
  or if `NULL` is provided, `std_start` will be used. Other implemented
  methods include `emp_start`.

- Disabling the `theta_vcov` calculation can speed up the fitting
  process when there are many variance parameters. However, this has
  also drawbacks. We can no longer check that the variance parameter
  estimates are indeed at a local maximum of the log-likelihood surface,
  and therefore convergence issues might go unnoticed. In addition, some
  covariance matrix adjustment methods (e.g., Kenward-Roger) require
  `theta_vcov` to be calculated. These will then raise errors.

## Examples

``` r
mmrm_control(
  optimizer_fun = stats::optim,
  optimizer_args = list(method = "L-BFGS-B")
)
#> $optimizers
#> $optimizers$custom_optimizer
#> function (par, fn, gr = NULL, ..., method = c("Nelder-Mead", 
#>     "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"), lower = -Inf, 
#>     upper = Inf, control = list(), hessian = FALSE) 
#> {
#>     fn1 <- function(par) fn(par, ...)
#>     gr1 <- if (!is.null(gr)) 
#>         function(par) gr(par, ...)
#>     method <- match.arg(method)
#>     if ((any(lower > -Inf) || any(upper < Inf)) && !any(method == 
#>         c("L-BFGS-B", "Brent"))) {
#>         warning("bounds can only be used with method L-BFGS-B (or Brent)")
#>         method <- "L-BFGS-B"
#>     }
#>     npar <- length(par)
#>     con <- list(trace = 0, fnscale = 1, parscale = rep.int(1, 
#>         npar), ndeps = rep.int(0.001, npar), maxit = 100L, abstol = -Inf, 
#>         reltol = sqrt(.Machine$double.eps), alpha = 1, beta = 0.5, 
#>         gamma = 2, REPORT = 10, warn.1d.NelderMead = TRUE, type = 1, 
#>         lmm = 5, factr = 1e+07, pgtol = 0, tmax = 10, temp = 10)
#>     nmsC <- names(con)
#>     if (method == "Nelder-Mead") 
#>         con$maxit <- 500
#>     if (method == "SANN") {
#>         con$maxit <- 10000
#>         con$REPORT <- 100
#>     }
#>     con[(namc <- names(control))] <- control
#>     if (length(noNms <- namc[!namc %in% nmsC])) 
#>         warning("unknown names in control: ", paste(noNms, collapse = ", "))
#>     if (con$trace < 0) 
#>         warning("read the documentation for 'trace' more carefully")
#>     else if (method == "SANN" && con$trace && as.integer(con$REPORT) == 
#>         0) 
#>         stop("'trace != 0' needs 'REPORT >= 1'")
#>     if (method == "L-BFGS-B" && any(!is.na(match(c("reltol", 
#>         "abstol"), namc)))) 
#>         warning("method L-BFGS-B uses 'factr' (and 'pgtol') instead of 'reltol' and 'abstol'")
#>     if (npar == 1 && method == "Nelder-Mead" && isTRUE(con$warn.1d.NelderMead)) 
#>         warning("one-dimensional optimization by Nelder-Mead is unreliable:\nuse \"Brent\" or optimize() directly")
#>     if (npar > 1 && method == "Brent") 
#>         stop("method = \"Brent\" is only available for one-dimensional optimization")
#>     lower <- as.double(rep_len(lower, npar))
#>     upper <- as.double(rep_len(upper, npar))
#>     res <- if (method == "Brent") {
#>         if (any(!is.finite(c(upper, lower)))) 
#>             stop("'lower' and 'upper' must be finite values")
#>         res <- optimize(function(par) fn(par, ...)/con$fnscale, 
#>             lower = lower, upper = upper, tol = con$reltol)
#>         names(res)[names(res) == c("minimum", "objective")] <- c("par", 
#>             "value")
#>         res$value <- res$value * con$fnscale
#>         c(res, list(counts = c(`function` = NA, gradient = NA), 
#>             convergence = 0L, message = NULL))
#>     }
#>     else .External2(C_optim, par, fn1, gr1, method, con, lower, 
#>         upper)
#>     if (hessian) 
#>         res$hessian <- .External2(C_optimhess, res$par, fn1, 
#>             gr1, con)
#>     res
#> }
#> <bytecode: 0x55ecb0a346c0>
#> <environment: namespace:stats>
#> attr(,"args")
#> attr(,"args")$control
#> list()
#> 
#> attr(,"args")$method
#> [1] "L-BFGS-B"
#> 
#> attr(,"class")
#> [1] "partial"  "function"
#> 
#> 
#> $start
#> function(cov_type, n_visits, n_groups, ...) {
#>   assert_string(cov_type)
#>   assert_subset(cov_type, cov_types(c("abbr", "habbr")))
#>   assert_int(n_visits, lower = 1L)
#>   assert_int(n_groups, lower = 1L)
#>   start_value <- switch(
#>     cov_type,
#>     us = rep(0, n_visits * (n_visits + 1) / 2),
#>     toep = rep(0, n_visits),
#>     toeph = rep(0, 2 * n_visits - 1),
#>     ar1 = c(0, 0.5),
#>     ar1h = c(rep(0, n_visits), 0.5),
#>     ad = rep(0, n_visits),
#>     adh = rep(0, 2 * n_visits - 1),
#>     cs = rep(0, 2),
#>     csh = rep(0, n_visits + 1),
#>     sp_exp = rep(0, 2)
#>   )
#>   rep(start_value, n_groups)
#> }
#> <bytecode: 0x55ecb7f8d180>
#> <environment: namespace:mmrm>
#> 
#> $accept_singular
#> [1] TRUE
#> 
#> $method
#> [1] "Satterthwaite"
#> 
#> $vcov
#> [1] "Asymptotic"
#> 
#> $n_cores
#> [1] 1
#> 
#> $drop_visit_levels
#> [1] TRUE
#> 
#> $disable_theta_vcov
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "mmrm_control"
```
