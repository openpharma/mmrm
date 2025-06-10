# mmrm 0.3.15

### New Features

- `mmrm` now returns score per subject in empirical covariance. It can be accessed by `component(obj, name = "score_per_subject")`. 

### Bug Fixes

- Previously, when fitting a model with empirical covariance matrix estimation to a data set with a large number of subjects and/or a large number of coefficients, the model fitting could take very long and exhaust the memory in the worst case. This was due to an inefficient implementation of a matrix needed only in case of Satterthwaite degrees of freedom adjustment. This is fixed now, by returning the matrix `empirical_g_mat` in the `mmrm` object, instead of the previous `empirical_df_mat` matrix. The model fit is now much faster and does not exhaust the memory anymore. If old model fit objects are used, the `empirical_df_mat` will still be used correctly, however a deprecation warning will be issued. Please consider re-fitting the model to get the new `empirical_g_mat` matrix.
- Previously, when compiling `mmrm` from source using a `TMB` version below 1.9.15, and installing a newer `TMB` of version 1.9.15 or above, would render the `mmrm` package unusable. This is fixed now, by checking in the dynamic library of `mmrm` whether the version of `TMB` has been sufficient.

# mmrm 0.3.14

### Bug Fixes

- In version 0.3.13, when the tape optimizer from `TMB` was switched on, a warning would be given by `fit_mmrm()`, instructing users to turn off the tape optimizer. However, this is not necessary for reproducible results. Instead, it is now checked whether the deterministic hash for the `TMB` tape optimizer is used, and a warning is issued otherwise. 
- In version 0.3.13, the above described warning by `fit_mmrm()` was not visible to the user when calling `mmrm()` because it was caught internally, causing the first fit in each session to fail for the first tried optimizer and falling back to the other optimizers. The warning is now issued directly by `mmrm()`. This change ensures that the first model fit is consistent regarding the chosen optimizer (and thus numeric results) with subsequent model fits, avoiding discrepancies observed in version 0.3.13.

# mmrm 0.3.13

### Bug Fixes

- When running with `TMB` package versions below 1.9.15, MMRM fit results are not completely reproducible. While this may not be relevant for most applications, because the numerical differences are very small, we now issue a warning to the user if this is the case. We advise users to upgrade their `TMB` package versions to 1.9.15 or higher to ensure reproducibility.
- Previously, `mmrm` ignored contrasts defined for covariates in the input data set. This is fixed now.
- Previously, `predict` always required the response to be valid, even for unconditional predictions. This is fixed now and unconditional prediction does not require the response to be valid or present any longer.
- `model.frame` has been updated to ensure that the `na.action` works correctly.
- Previously `emmeans::emmeans` returned `NA` for spatial covariance structures. This is fixed now.
- Previously `car::Anova` gave incorrect results if an interaction term is included and the covariate of interest was not the first categorical variable. This is fixed now.
- Previously `car::Anova` failed if the model did not contain an intercept. This is fixed now.

### Miscellaneous

- Upon fitting an MMRM, it is checked whether a not reproducible optimization feature of `TMB` is turned on. If so, a warning is issued to the user once per session.
- `mmrm` now checks on the positive definiteness of the covariance matrix `theta_vcov`. If it is not positive definite, non-convergence is messaged appropriately.
- `model.matrix` has been updated to ensure that the `NA` values are dropped. Additionally, an argument `use_response` is added to decide whether records with `NA` values in the response should be discarded.
- `predict` has been updated to allow duplicated subject IDs for unconditional prediction.

# mmrm 0.3.12

### New Features

- Add parameter `conditional` for `predict` method to control whether the prediction is conditional on the observation or not.

### Bug Fixes

- Previously if the left hand side of a model formula is an expression, `predict` and `simulate` will fail. This is fixed now.

# mmrm 0.3.11

### Bug Fixes

- Previously if a secondary optimizer fails `mmrm` will fail. This is fixed now.
- Previously character covariate variables will make `Anova` fail. This is fixed now.

# mmrm 0.3.10

### Miscellaneous

- Fix internal test skipping functions for MacOS R. 

# mmrm 0.3.9

### Miscellaneous

- Fix internal test skipping functions for R versions older than 4.3. 

# mmrm 0.3.8

### New Features

- `Anova` is implemented for `mmrm` models and available upon loading the `car` package. It supports type II and III hypothesis testing.
- The argument `start` for `mmrm_control()` is updated to allow better choices of initial values.
- `confint` on `mmrm` models will give t-based confidence intervals now, instead of the normal approximation.  

### Bug Fixes

- Previously if the first optimizer failed, the best successful fit among the remaining optimizers was not returned correctly. This is fixed now.

### Miscellaneous

- In documentation of `mmrm_control()`, the allowed `vcov` definition is corrected to "Empirical-Jackknife" (CR3), and "Empirical-Bias-Reduced" (CR2).
- Fixed a compiler warning related to missing format specification.
- If an empty contrast matrix is provided to `df_md`, it will return statistics with `NA` values.

# mmrm 0.3.7

### New Features

- The argument `method` of `mmrm()` now only specifies the method used for the 
  degrees of freedom adjustment.
- Add empirical, empirical Jackknife and empirical bias-reduced adjusted coefficients
  covariance estimates, which can be specified via the new `vcov` argument of `mmrm()`.
- Add residual and between-within degrees of freedom methods.
- Add Kenward-Roger support for spatial covariance structures.
- Add `model.matrix()` and `terms()` methods to assist in post-processing.
- Add `predict()` method to obtain conditional mean estimates and prediction intervals.
- Add `simulate()` method to simulate observations from the predictive distribution.
- Add `residuals()` method to obtain raw, Pearson or normalized residuals.
- Add `tidy()`, `glance()` and `augment()` methods to tidy the fit results into summary tables.
- Add `tidymodels` framework support via a `parsnip` interface.
- Add argument `covariance` to `mmrm()` to allow for easier programmatic access
  to specifying the model's covariance structure and to expose covariance
  customization through the `tidymodels` interface.

### Bug Fixes

- Previously `mmrm()` follows the global option `na.action` and if it is set
  other than `"na.omit"` an assertion would fail. This is now fixed and hence `NA`
  values are always removed prior to model fitting, independent of the global
  `na.action` option.
- Previously a `model.frame()` call on an `mmrm` object with transformed terms, or new
  data, e.g. `model.frame(mmrm(Y ~ log(X) + ar1(VISIT|ID), data = new_data)`,
  would fail. This is now fixed.
- Previously `mmrm()` always required a `data` argument. Now fitting `mmrm` can also use
  environment variables instead of requiring `data` argument. (Note that
  `fit_mmrm` is not affected.)
- Previously `emmeans()` failed when using transformed terms or not including the visit 
  variable in the model formula. This is now fixed.
- Previously `mmrm()` might provide non-finite values in the Jacobian calculations, 
  leading to errors in the Satterthwaite degrees of freedom calculations. This will raise 
  an error now and thus alert the user that the model fit was not successful.

### Miscellaneous

- Use automatic differentiation to calculate Satterthwaite adjusted degrees of freedom, 
  resulting in 10-fold speed-up of the Satterthwaite calculations after the initial model fit.
- Add an interactive confirmation step if the number of visit levels is too large 
  for non-spatial covariance structures. Use `options(mmrm.max_visits = )` to specify the 
  maximum number of visits allowed in non-interactive mode.
- Removed `free_cores()` in favor of `parallelly::availableCores(omit = 1)`.
- The `model.frame()` method has been updated: The `full` argument is deprecated and
  the `include` argument can be used instead; by default all relevant variables are
  returned. Furthermore, it returns a `data.frame` the size of the number of observations 
  utilized in the model for all combinations of the  `include` argument 
  when `na.action= "na.omit"`.
- Overall, seven vignettes have been added to the package. All vignettes have a slightly
  different look now to reduce the size of the overall R package on CRAN.
- The used optimizer is now available via `component(., "optimizer")` instead of previously
  `attr(., "optimizer")`.

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
