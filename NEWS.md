# mmrm 0.2.2

### New Features

- Add support for Kenward-Roger adjusted coefficients covariance matrix and
  degrees of freedom in `mmrm` function call with argument `method`.
  Options are "Kenward-Roger", "Kenward-Roger-Linear" and "Satterthwaite"
  (which is still the default). Subsequent methods calls
  will respect this initial choice, e.g. `vcov(fit)` will return the adjusted
  coefficients covariance matrix if a Kenward-Roger method has been used.
- Update the `mmrm` arguments to allow users more fine-grained control, e.g.
  `mmrm(..., start = start, optimizer = c("BFGS", "nlminb"))` to set the
  starting values for the variance estimates and to choose the available optimizers.
  These arguments will be passed to the new function `mmrm_control`.
- Add new argument `drop_visit_levels` to allow users to keep all levels in visits,
  even when they are not observed in the data. Dropping unobserved levels was done
  silently previously, and now a message will be given. See `?mmrm_control`
  for more details.

### Bug Fixes

- Previously duplicate time points could be present for a single subject,
  and this could lead to segmentation faults if more than the total number of
  unique time points were available for any subject. Now it is checked that
  there are no duplicate time points per subject, and this is explained also in the
  function documentation and the introduction vignette.
- Previously in `mmrm` calls, the `weights` object in the environment where the
  formula is defined was replaced by the `weights` used internally.
  Now this behavior is removed and your variable
  `weights` e.g. in the global environment will no longer be replaced.

### Miscellaneous

- Deprecated `free_cores()` in favor of `parallelly::availableCores(omit = 1)`.
- Deprecated `optimizer = "automatic"` in favor of not specifying the `optimizer`.
  By default, all remaining optimizers will be tried if the first optimizer fails
  to reach convergence.

# mmrm 0.1.5

- First CRAN version of the package.
- The package fits mixed models for repeated measures
  (MMRM) based on the marginal linear model without random effects.
- The motivation for this package is to have a fast, reliable (in terms of
  convergence behavior) and feature complete implementation of MMRM in R.

### New Features

- Currently 10 covariance structures are supported (unstructured; as well as
  homogeneous and heterogeneous versions of Toeplitz, auto-regressive order one,
  ante-dependence, compound symmetry; and spatial exponential).
- Fast C++ implementation of Maximum Likelihood (ML) and Restricted Maximum
  Likelihood (REML) estimation.
- Currently Satterthwaite adjusted degrees of freedom calculation is supported.
- Interface to the `emmeans` package for computing estimated marginal means
  (also called least-square means) for the coefficients.
- Multiple optimizers are run to reach convergence in as many cases as possible.
- Flexible formula based model specification and support for standard S3 methods such
  as `summary`, `logLik`, etc.
