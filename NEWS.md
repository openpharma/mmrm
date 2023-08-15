# mmrm 0.2.2.9034

### New Features

- Add argument `covariance` to `mmrm()` to allow for easier programmatic access
  to specifying the model's covariance structure and to expose covariance
  customization through the `tidymodels` interface.
- Add Kenward-Roger support for spatial covariance structures.
- Add support for `residuals` method with a `type` argument allowing for
  raw, Pearson and normalized residuals to be calculated from an `mmrm` fit.
- Add empirical, empirical Jackknife and empirical bias-reduced adjusted coefficients covariance matrix.
  In addition, the argument `method` now only specifies the method used
  for the degrees of freedom, another argument `vcov` is added to specify the
  method used to adjust the coefficients covariance matrix. Empirical, empirical Jackknife
  and empirical bias-reduced covariance support residual and Satterthwaite degrees of freedom.
- Add optional `tidymodels` framework support.
- Add confirmation if the number of visit levels is too large. Use `options(mmrm.max_visits = )`
  to specify the maximum number of visits allowed in non-interactive mode, or asked in interactive
  sessions.
- Add `predict` method to obtain conditional mean estimates and prediction intervals, both with or without previous observations.
- Use automatic differentiation to calculate Satterthwaite adjusted degrees of freedom calculation, resulting in 10-fold speed-up of the Satterthwaite calculations after the initial model fit.
- Add methods `model.matrix()` and `terms()` often available for modeling functions that assist in post-processing of a model object.

### Miscellaneous

- Removed `free_cores()` in favor of `parallelly::availableCores(omit = 1)`.
- The `model.frame()` method has been updated to return a data frame the size of 
  the number of observations utilized in the model for all combinations of the 
  `model.frame(include)` argument when `na.action='na.omit'` (the default).
- The `model.frame(include=)` method argument's default has been updated 
  from `include=NULL` to `include=c("subject_var", "visit_var", "group_var", "response_var")`
  such that by default all relevant variables are returned.

### Bug Fixes

- Previously `mmrm` fit follows the global option `na.action` and if it is set
  other than "na.omit" error will happen. This is now fixed and `NA` values are
  always removed prior to model fitting.
- Previously `model.frame` call on `mmrm` object with transformed terms, or new
  given data, e.g. `model.frame(mmrm(Y ~ log(X) + ar1(VISIT|ID), data = <new data>)`,
  will cause errors. This is now fixed and `model.frame` returns the data correctly.
  Now `na.action` argument can be "na.omit" or "na.pass", `subset` argument is not used.
  The `full` argument is deprecated and the `include` can be used instead.
- Previously `mmrm` always requires `data` argument. Now fitting `mmrm` can use
  environment variables, instead of requiring `data` argument. `fit_mmrm` is not affected.
- Previously `emmeans` does not work if fixed effect does not contain visit variable.
  This is now fixed.
- Previously `mmrm` can provide non-finite values in Jacobian calculations.
  This will raise an error now.

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
