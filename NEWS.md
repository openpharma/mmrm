# mmrm 0.1.5.9007

### Bug Fixes

- Previously duplicate time points could be present for a single subject,
  and this could lead to segmentation faults if more than the total number of
  unique time points were available for any subject. Now it is checked that there are
  no duplicate time points per subject, and this is explained also in the
  function documentation and the introduction vignette.

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
