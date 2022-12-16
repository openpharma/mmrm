# mmrm 0.1.5.9015

### New Features

- Add support for Kenward-Roger adjusted covariance and degree of freedom
  in `mmrm` function call with argument `method`. It now accepts "Kenward-Roger",
  "Kenward-Roger-Linear" or "Satterthwaite" (which is still the default). Subsequent `summary`, `print` or `component` calls will
  also be based on this `method` argument, e.g. `component(fit, "beta_vcov")` will return the
   adjusted covariance matrix if a Kenward-Roger method has been used.
- Update the `mmrm` arguments to allow users to input the control arguments directly in `mmrm` function, e.g.
  `mmrm(..., start = start, optimizer = c("BFGS", "nlminb"))`. These arguments will go into `mmrm_control`.
  In addition, `optimizer = "automatic"` is deprecated and will give you warning. It is a default behavior now to
  try all optimizers (if multiple optimizer provided) if the first optimizer fails.
- Add new arguments to `mmrm_control` to allow users to keep all levels in visits. This allows user to specify the distance of
  visits, e.g., for ar1 covariance strucuture, if you have VIS1, VIS3 and VIS4 as available visits, by default all visits
  are equaly spaced, the distance from VIS1 to VIS3, and the distance from VIS3 to VIS4 are both 1. 
  However, you can convert the visit into factors, with `levels = c("VIS1", "VIS2", "VIS3", "VIS4)`, and use
  `drop_visit_levels = FALSE`, the visits observed will not be equally spaced:
  the distance from VIS1 to VIS3 is 2, while the distance from VIS3 to VIS4 is 1.
  Please note that for some covariance structure, like unstructured, there will be warnings with missed visits because this kept empty level without observation will lead to inestimable result. 
### Bug Fixes

- Previously duplicate time points could be present for a single subject,
  and this could lead to segmentation faults if more than the total number of
  unique time points were available for any subject. Now it is checked that there are
  no duplicate time points per subject, and this is explained also in the
  function documentation and the introduction vignette.
- Previously in `mmrm` calls, the `weights` object in the environment where formula is
  defined will be replaced by the `weights` used internally. Now this behavior is removed and your variable
  `weights` e.g. in the global environment will not be replaced.
### Miscellaneous

- Deprecated `free_cores()` in favor of `parallelly::availableCores(omit = 1)`.

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
