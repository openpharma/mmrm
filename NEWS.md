# mmrm 0.1.0.9016

- First version of the package for fitting mixed models for repeated measures
  (MMRM) based on Template Model Builder (TMB).
- The motivation for this package is to have a fast, reliable (in terms of
  convergence behavior) and feature complete implementation of MMRM in R.

### New Features

- Currently 9 covariance structures are supported (unstructured, as well as
  homogeneous and heterogeneous versions of Toeplitz, auto-regressive order one,
  ante-dependence, compound symmetry).
- Fast C++ implementation of Maximum Likelihood (ML) and Restricted Maximum
  Likelihood (REML) estimation.
- Satterthwaite adjusted degrees of freedom calculation.
- Interface to the `emmeans` package for computing estimated marginal means
  (also called least-square means) for the coefficients.
- Multiple optimizers are run to allow for as much convergence chance as possible.
- Flexible formula based model specification and support for standard S3 methods such
  as `summary`, `logLik`, etc.
