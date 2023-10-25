#' Fitting an MMRM with Single Optimizer
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function helps to fit an MMRM using `TMB` with a single optimizer,
#' while capturing messages and warnings.
#'
#' @inheritParams mmrm
#' @param control (`mmrm_control`)\cr object.
#' @param tmb_data (`mmrm_tmb_data`)\cr object.
#' @param formula_parts (`mmrm_tmb_formula_parts`)\cr object.
#' @param ... Additional arguments to pass to [mmrm_control()].
#'
#' @details
#' `fit_single_optimizer` will fit the `mmrm` model using the `control` provided.
#' If there are multiple optimizers provided in `control`, only the first optimizer
#' will be used.
#' If `tmb_data` and `formula_parts` are both provided, `formula`, `data`, `weights`,
#' `reml`, and `covariance` are ignored.
#'
#' @return The `mmrm_fit` object, with additional attributes containing warnings,
#'   messages, optimizer used and convergence status in addition to the
#'   `mmrm_tmb` contents.
#' @export
#'
#' @examples
#' mod_fit <- fit_single_optimizer(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fev_data,
#'   weights = rep(1, nrow(fev_data)),
#'   optimizer = "nlminb"
#' )
#' attr(mod_fit, "converged")
fit_single_optimizer <- function(formula,
                                 data,
                                 weights,
                                 reml = TRUE,
                                 covariance = NULL,
                                 tmb_data,
                                 formula_parts,
                                 ...,
                                 control = mmrm_control(...)) {
  to_remove <- list(
    # Transient visit to invalid parameters.
    warnings = c("NA/NaN function evaluation")
  )
  as_diverged <- list(
    errors = c(
      "NA/NaN Hessian evaluation",
      "L-BFGS-B needs finite values of 'fn'"
    )
  )
  if (missing(tmb_data) || missing(formula_parts)) {
    h_valid_formula(formula)
    assert_data_frame(data)
    assert_numeric(weights, any.missing = FALSE, lower = .Machine$double.xmin)
    assert_flag(reml)
    assert_class(control, "mmrm_control")
    assert_list(control$optimizers, names = "unique", types = c("function", "partial"))
    quiet_fit <- h_record_all_output(
      fit_mmrm(
        formula = formula,
        data = data,
        weights = weights,
        reml = reml,
        covariance = covariance,
        control = control
      ),
      remove = to_remove,
      divergence = as_diverged
    )
  } else {
    assert_class(tmb_data, "mmrm_tmb_data")
    assert_class(formula_parts, "mmrm_tmb_formula_parts")
    quiet_fit <- h_record_all_output(
      fit_mmrm(
        formula_parts = formula_parts,
        tmb_data = tmb_data,
        control = control
      ),
      remove = to_remove,
      divergence = as_diverged
    )
  }
  if (length(quiet_fit$errors)) {
    stop(quiet_fit$errors)
  }
  converged <- (length(quiet_fit$warnings) == 0L) &&
    (length(quiet_fit$divergence) == 0L) &&
    isTRUE(quiet_fit$result$opt_details$convergence == 0)
  structure(
    quiet_fit$result,
    warnings = quiet_fit$warnings,
    messages = quiet_fit$messages,
    divergence = quiet_fit$divergence,
    converged = converged,
    class = c("mmrm_fit", class(quiet_fit$result))
  )
}

#' Summarizing List of Fits
#'
#' @param all_fits (`list` of `mmrm_fit` or `try-error`)\cr list of fits.
#'
#' @return List with `warnings`, `messages`, `log_liks` and `converged` results.
#' @keywords internal
h_summarize_all_fits <- function(all_fits) {
  assert_list(all_fits, types = c("mmrm_fit", "try-error"))
  is_error <- vapply(all_fits, is, logical(1), class2 = "try-error")

  warnings <- messages <- vector(mode = "list", length = length(all_fits))
  warnings[is_error] <- lapply(all_fits[is_error], as.character)
  warnings[!is_error] <- lapply(all_fits[!is_error], attr, which = "warnings")
  messages[!is_error] <- lapply(all_fits[!is_error], attr, which = "messages")
  log_liks <- as.numeric(rep(NA, length.out = length(all_fits)))
  log_liks[!is_error] <- vapply(all_fits[!is_error], stats::logLik, numeric(1L))
  converged <- rep(FALSE, length.out = length(all_fits))
  converged[!is_error] <- vapply(all_fits[!is_error], attr, logical(1), which = "converged")

  list(
    warnings = warnings,
    messages = messages,
    log_liks = log_liks,
    converged = converged
  )
}

#' Refitting MMRM with Multiple Optimizers
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param fit (`mmrm_fit`)\cr original model fit from [fit_single_optimizer()].
#' @param ... Additional arguments passed to [mmrm_control()].
#' @param control (`mmrm_control`)\cr object.
#'
#' @return The best (in terms of log likelihood) fit which converged.
#'
#' @note For Windows, no parallel computations are currently implemented.
#' @export
#'
#' @examples
#' fit <- fit_single_optimizer(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fev_data,
#'   weights = rep(1, nrow(fev_data)),
#'   optimizer = "nlminb"
#' )
#' best_fit <- refit_multiple_optimizers(fit)
refit_multiple_optimizers <- function(fit,
                                      ...,
                                      control = mmrm_control(...)) {
  assert_class(fit, "mmrm_fit")
  assert_class(control, "mmrm_control")

  n_cores_used <- ifelse(
    .Platform$OS.type == "windows",
    1L,
    min(
      length(control$optimizers),
      control$n_cores
    )
  )
  controls <- h_split_control(
    control,
    start = fit$theta_est
  )

  # Take the results from old fit as starting values for new fits.
  all_fits <- suppressWarnings(parallel::mcmapply(
    FUN = fit_single_optimizer,
    control = controls,
    MoreArgs = list(
      tmb_data = fit$tmb_data,
      formula_parts = fit$formula_parts
    ),
    mc.cores = n_cores_used,
    mc.silent = TRUE,
    SIMPLIFY = FALSE
  ))
  all_fits <- c(all_fits, list(old_result = fit))

  # Find the results that are ok and return best in terms of log-likelihood.
  all_fits_summary <- h_summarize_all_fits(all_fits)
  is_ok <- all_fits_summary$converged
  if (!any(is_ok)) {
    stop(
      "No optimizer led to a successful model fit. ",
      "Please try to use a different covariance structure or other covariates."
    )
  }
  best_optimizer <- which.max(all_fits_summary$log_liks[is_ok])
  all_fits[[best_optimizer]]
}

#' Control Parameters for Fitting an MMRM
#'
#' @description `r lifecycle::badge("experimental")`
#' Fine-grained specification of the MMRM fit details is possible using this
#' control function.
#'
#' @param n_cores (`count`)\cr number of cores to be used.
#' @param method (`string`)\cr adjustment method for degrees of freedom.
#' @param vcov (`string`)\cr coefficients covariance matrix adjustment method.
#' @param start (`numeric` or `NULL`)\cr optional start values for variance
#'   parameters.
#' @param accept_singular (`flag`)\cr whether singular design matrices are reduced
#'   to full rank automatically and additional coefficient estimates will be missing.
#' @param optimizers (`list`)\cr optimizer specification, created with [h_get_optimizers()].
#' @param drop_visit_levels (`flag`)\cr whether to drop levels for visit variable,
#'   if visit variable is a factor, see details.
#' @param ... additional arguments passed to [h_get_optimizers()].
#'
#' @details
#  - The `drop_visit_levels` flag will decide whether unobserved visits will be kept for analysis.
#'   For example, if the data only has observations at visits `VIS1`, `VIS3` and `VIS4`, by default
#'   they are treated to be equally spaced, the distance from `VIS1` to `VIS3`, and from `VIS3` to `VIS4`,
#'   are identical. However, you can manually convert this visit into a factor, with
#'   `levels = c("VIS1", "VIS2", "VIS3", "VIS4")`, and also use `drop_visits_levels = FALSE`,
#'   then the distance from `VIS1` to `VIS3` will be double, as `VIS2` is a valid visit.
#'   However, please be cautious because this can lead to convergence failure
#'   when using an unstructured covariance matrix and there are no observations
#'   at the missing visits.
#' - The `method` and `vcov` arguments specify the degrees of freedom and coefficients
#'   covariance matrix adjustment methods, respectively.
#'   - Allowed `vcov` includes: "Asymptotic", "Kenward-Roger", "Kenward-Roger-Linear", "Empirical" (CR0),
#'     "Empirical-Jackknife" (CR2), and "Empirical-Bias-Reduced" (CR3).
#'   - Allowed `method` includes: "Satterthwaite", "Kenward-Roger", "Between-Within" and "Residual".
#'   - If `method` is "Kenward-Roger" then only "Kenward-Roger" or "Kenward-Roger-Linear" are allowed for `vcov`.
#' - The `vcov` argument can be `NULL` to use the default covariance method depending on the `method`
#'   used for degrees of freedom, see the following table:
#'
#'    | `method`  |  Default `vcov`|
#'    |-----------|----------|
#'    |Satterthwaite| Asymptotic|
#'    |Kenward-Roger| Kenward-Roger|
#'    |Residual| Empirical|
#'    |Between-Within| Asymptotic|
#'
#' - Please note that "Kenward-Roger" for "Unstructured" covariance gives different results
#'   compared to SAS; Use "Kenward-Roger-Linear" for `vcov` instead for better matching
#'   of the SAS results.
#'
#' @return List of class `mmrm_control` with the control parameters.
#' @export
#'
#' @examples
#' mmrm_control(
#'   optimizer_fun = stats::optim,
#'   optimizer_args = list(method = "L-BFGS-B")
#' )
mmrm_control <- function(n_cores = 1L,
                         method = c("Satterthwaite", "Kenward-Roger", "Residual", "Between-Within"),
                         vcov = NULL,
                         start = NULL,
                         accept_singular = TRUE,
                         drop_visit_levels = TRUE,
                         ...,
                         optimizers = h_get_optimizers(...)) {
  assert_count(n_cores, positive = TRUE)
  assert_character(method)
  assert_numeric(start, null.ok = TRUE)
  assert_flag(accept_singular)
  assert_flag(drop_visit_levels)
  assert_list(optimizers, names = "unique", types = c("function", "partial"))
  assert_string(vcov, null.ok = TRUE)
  method <- match.arg(method)
  if (is.null(vcov)) {
    vcov <- h_get_cov_default(method)
  }
  assert_subset(
    vcov,
    c(
      "Asymptotic",
      "Empirical",
      "Empirical-Bias-Reduced",
      "Empirical-Jackknife",
      "Kenward-Roger",
      "Kenward-Roger-Linear"
    )
  )
  if (xor(identical(method, "Kenward-Roger"), vcov %in% c("Kenward-Roger", "Kenward-Roger-Linear"))) {
    stop(paste(
      "Kenward-Roger degrees of freedom must work together with Kenward-Roger",
      "or Kenward-Roger-Linear covariance!"
    ))
  }
  structure(
    list(
      optimizers = optimizers,
      start = start,
      accept_singular = accept_singular,
      method = method,
      vcov = vcov,
      n_cores = as.integer(n_cores),
      drop_visit_levels = drop_visit_levels
    ),
    class = "mmrm_control"
  )
}

#' Fit an MMRM
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This is the main function fitting the MMRM.
#'
#' @param formula (`formula`)\cr the model formula, see details.
#' @param data (`data`)\cr the data to be used for the model.
#' @param weights (`vector`)\cr an optional vector of weights to be used in
#'   the fitting process. Should be `NULL` or a numeric vector.
#' @param reml (`flag`)\cr whether restricted maximum likelihood (REML)
#'   estimation is used, otherwise maximum likelihood (ML) is used.
#' @param covariance (`cov_struct`)\cr a covariance structure type definition
#'   as produced with [cov_struct()], or value that can be coerced to a
#'   covariance structure using [as.cov_struct()]. If no value is provided,
#'   a structure is derived from the provided formula.
#' @param control (`mmrm_control`)\cr fine-grained fitting specifications list
#'   created with [mmrm_control()].
#' @param ... arguments passed to [mmrm_control()].
#'
#' @details
#' The `formula` typically looks like:
#' `FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)`
#' so specifies response and covariates as usual, and exactly one special term
#' defines which covariance structure is used and what are the time point and
#' subject variables. The covariance structures in the formula can be
#' found in [`covariance_types`].
#'
#' The time points have to be unique for each subject. That is,
#' there cannot be time points with multiple observations for any subject.
#' The rationale is that these observations would need to be correlated, but it
#' is not possible within the currently implemented covariance structure framework
#' to do that correctly. Moreover, for non-spatial covariance structures, the time
#' variable must be a factor variable.
#'
#' When optimizer is not set, first the default optimizer
#' (`L-BFGS-B`) is used to fit the model. If that converges, this is returned.
#' If not, the other available optimizers from [h_get_optimizers()],
#' including `BFGS`, `CG` and `nlminb` are
#' tried (in parallel if `n_cores` is set and not on Windows).
#' If none of the optimizers converge, then the function fails. Otherwise
#' the best fit is returned.
#'
#' Note that fine-grained control specifications can either be passed directly
#' to the `mmrm` function, or via the `control` argument for bundling together
#' with the [mmrm_control()] function. Both cannot be used together, since
#' this would delete the arguments passed via `mmrm`.
#'
#' @return An `mmrm` object.
#'
#' @note The `mmrm` object is also an `mmrm_fit` and an `mmrm_tmb` object,
#' therefore corresponding methods also work (see [`mmrm_tmb_methods`]).
#'
#' Additional contents depend on the choice of the adjustment `method`:
#' - If Satterthwaite adjustment is used, the Jacobian information `jac_list`
#' is included.
#' - If Kenward-Roger adjustment is used, `kr_comp` contains necessary
#' components and `beta_vcov_adj` includes the adjusted coefficients covariance
#' matrix.
#'
#' Use of the package `emmeans` is supported, see [`emmeans_support`].
#'
#' NA values are always omitted regardless of `na.action` setting.
#'
#' When the number of visit levels is large, it usually requires large memory to create the
#' covariance matrix. By default, the maximum allowed visit levels is 100, and if there are more
#' visit levels, a confirmation is needed if run interactively.
#' You can use `options(mmrm.max_visits = <target>)` to increase the maximum allowed number of visit
#' levels. In non-interactive sessions the confirmation is not raised and will directly give you an error if
#' the number of visit levels exceeds the maximum.
#'
#' @export
#'
#' @examples
#' fit <- mmrm(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fev_data
#' )
#'
#' # Direct specification of control details:
#' fit <- mmrm(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fev_data,
#'   weights = fev_data$WEIGHTS,
#'   method = "Kenward-Roger"
#' )
#'
#' # Alternative specification via control argument (but you cannot mix the
#' # two approaches):
#' fit <- mmrm(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fev_data,
#'   control = mmrm_control(method = "Kenward-Roger")
#' )
mmrm <- function(formula,
                 data,
                 weights = NULL,
                 covariance = NULL,
                 reml = TRUE,
                 control = mmrm_control(...),
                 ...) {
  assert_false(!missing(control) && !missing(...))
  assert_class(control, "mmrm_control")
  assert_list(control$optimizers, min.len = 1)

  if (control$method %in% c("Kenward-Roger", "Kenward-Roger-Linear") && !reml) {
    stop("Kenward-Roger only works for REML")
  }
  h_valid_formula(formula)
  covariance <- h_reconcile_cov_struct(formula, covariance)
  formula_parts <- h_mmrm_tmb_formula_parts(formula, covariance)

  if (!missing(data)) {
    attr(data, which = "dataname") <- toString(match.call()$data)
  } else {
    # na.action set to na.pass to allow data to be full; will be futher trimmed later
    data <- model.frame(formula_parts$full_formula, na.action = "na.pass")
  }

  if (is.null(weights)) {
    weights <- rep(1, nrow(data))
  } else {
    attr(weights, which = "dataname") <- deparse(match.call()$weights)
  }
  tmb_data <- h_mmrm_tmb_data(
    formula_parts, data, weights, reml,
    singular = if (control$accept_singular) "drop" else "error",
    drop_visit_levels = control$drop_visit_levels,
    allow_na_response = FALSE
  )
  fit <- list()
  names_all_optimizers <- names(control$optimizers)
  while ((length(fit) == 0) && length(control$optimizers) > 0) {
    fit <- fit_single_optimizer(
      tmb_data = tmb_data,
      formula_parts = formula_parts,
      control = control
    )
    if (length(fit) == 0) {
      warning(paste0(
        "Divergence with optimizer ", names(control$optimizers[1L]), " due to problems: ",
        toString(attr(fit, "divergence"))
      ))
    }
    control$optimizers <- control$optimizers[-1]
  }
  if (!attr(fit, "converged")) {
    more_optimizers <- length(control$optimizers) >= 1L
    if (more_optimizers) {
      fit <- refit_multiple_optimizers(
        fit = fit,
        control = control
      )
    } else {
      all_problems <- unlist(
        attributes(fit)[c("errors", "warnings")],
        use.names = FALSE
      )
      stop(paste0(
        "Chosen optimizers '", toString(names_all_optimizers), "' led to problems during model fit:\n",
        paste(paste0(seq_along(all_problems), ") ", all_problems), collapse = ";\n"), "\n",
        "Consider trying multiple or different optimizers."
      ))
    }
  }
  fit_msg <- attr(fit, "messages")
  if (!is.null(fit_msg)) {
    message(paste(fit_msg, collapse = "\n"))
  }
  fit$call <- match.call()
  fit$call$formula <- formula
  fit$method <- control$method
  fit$vcov <- control$vcov
  if (control$vcov %in% c("Kenward-Roger", "Kenward-Roger-Linear")) {
    fit$kr_comp <- h_get_kr_comp(fit$tmb_data, fit$theta_est)
    fit$beta_vcov_adj <- h_var_adj(
      v = fit$beta_vcov,
      w = component(fit, "theta_vcov"),
      p = fit$kr_comp$P,
      q = fit$kr_comp$Q,
      r = fit$kr_comp$R,
      linear = (control$vcov == "Kenward-Roger-Linear")
    )
  } else if (control$vcov %in% c("Empirical", "Empirical-Bias-Reduced", "Empirical-Jackknife")) {
    empirical_comp <- h_get_empirical(
      fit$tmb_data, fit$theta_est, fit$beta_est, fit$beta_vcov, control$vcov
    )
    fit$beta_vcov_adj <- empirical_comp$cov
    fit$empirical_df_mat <- empirical_comp$df_mat
    dimnames(fit$beta_vcov_adj) <- dimnames(fit$beta_vcov)
  } else if (identical(control$vcov, "Asymptotic")) {
    # Note that we only need the Jacobian list under Asymptotic covariance method,
    # cf. the Satterthwaite vignette.
    if (identical(fit$method, "Satterthwaite")) {
      fit$jac_list <- h_jac_list(fit$tmb_data, fit$theta_est, fit$beta_vcov)
    }
  } else {
    stop("Unrecognized coefficent variance-covariance method!")
  }

  class(fit) <- c("mmrm", class(fit))
  fit
}
