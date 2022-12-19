#' Fitting an MMRM with Single Optimizer
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function helps to fit an MMRM using `TMB` with a single optimizer,
#' while capturing messages and warnings.
#'
#' @inheritParams mmrm
#' @param control (`mmrm_control`)\cr object.
#' @param ... Additional arguments to pass to [mmrm_control()].
#'
#' @details
#' `fit_single_optimizer` will fit the `mmrm` model using the `control` provided.
#' If there are multiple optimizers provided in `control`, only the first optimizer
#' will be used.
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
                                 ...,
                                 control = mmrm_control(...)) {
  assert_formula(formula)
  assert_data_frame(data)
  assert_vector(weights)
  assert_flag(reml)
  assert_class(control, "mmrm_control")
  assert_list(control$optimizers, names = "unique", types = c("function", "partial"))
  quiet_fit <- h_record_all_output(
    fit_mmrm(
      formula = formula,
      data = data,
      weights = weights,
      reml = reml,
      control = control
    ),
    remove = list(
      warning = c("NA/NaN function evaluation") # Transient visit to invalid parameters.
    )
  )
  if (length(quiet_fit$errors)) {
    stop(quiet_fit$errors)
  }
  converged <- (length(quiet_fit$warnings) == 0L) &&
    (quiet_fit$result$opt_details$convergence == 0)
  structure(
    quiet_fit$result,
    warnings = quiet_fit$warnings,
    messages = quiet_fit$messages,
    optimizer = names(control$optimizers)[1],
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
  log_liks <- as.numeric(rep(NA, length = length(all_fits)))
  log_liks[!is_error] <- vapply(all_fits[!is_error], stats::logLik, numeric(1L))
  converged <- rep(FALSE, length = length(all_fits))
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

  # Extract the components of the original fit.
  old_formula <- formula(fit)
  old_data <- fit$data
  old_weights <- fit$weights

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
      formula = old_formula,
      data = old_data,
      weights = old_weights,
      reml = fit$reml
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
#' @param n_cores (`int`)\cr number of cores to be used.
#' @param method (`string`)\cr adjustment method for degrees of freedom and
#'   coefficients covariance matrix.
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
#' The `drop_visit_levels` flag will decide whether unobserved visits will be kept for analysis.
#' For example, if the data only has observations at visits `VIS1`, `VIS3` and `VIS4`, by default
#' they are treated to be equally spaced, the distance from `VIS1` to `VIS3`, and from `VIS3` to `VIS4`,
#' are identical. However, you can manually convert this visit into a factor, with
#' `levels = c("VIS1", "VIS2", "VIS3", "VIS4")`, and also use `drop_visits_levels = FALSE`,
#' then the distance from `VIS1` to `VIS3` will be double, as `VIS2` is a valid visit.
#' However, please be cautious because this can lead to convergence failure
#' when using an unstructured covariance matrix and there are no observations
#' at the missing visits.
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
                         method = c("Satterthwaite", "Kenward-Roger", "Kenward-Roger-Linear"),
                         start = NULL,
                         accept_singular = TRUE,
                         drop_visit_levels = TRUE,
                         ...,
                         optimizers = h_get_optimizers(...)) {
  assert_int(n_cores, lower = 1L)
  assert_character(method)
  assert_numeric(start, null.ok = TRUE)
  assert_flag(accept_singular)
  assert_flag(drop_visit_levels)
  assert_list(optimizers, names = "unique", types = c("function", "partial"))
  method <- match.arg(method)
  structure(
    list(
      optimizers = optimizers,
      start = start,
      accept_singular = accept_singular,
      method = method,
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
#' @param weights (`vector`)\cr an optional vector of weights to be used in the fitting process.
#'   Should be NULL or a numeric vector.
#' @param reml (`flag`)\cr whether restricted maximum likelihood (REML) estimation is used,
#'   otherwise maximum likelihood (ML) is used.
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
#' to do that correctly.
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
                 reml = TRUE,
                 control = mmrm_control(...),
                 ...) {
  assert_false(!missing(control) && !missing(...))
  assert_class(control, "mmrm_control")
  assert_list(control$optimizers, min.len = 1)
  if (control$method %in% c("Kenward-Roger", "Kenward-Roger-Linear") && !reml) {
    stop("Kenward-Roger only works for REML")
  }
  attr(data, which = "dataname") <- toString(match.call()$data)

  if (is.null(weights)) {
    weights <- rep(1, nrow(data))
  } else {
    attr(weights, which = "dataname") <- deparse(match.call()$weights)
  }

  fit <- fit_single_optimizer(
    formula = formula,
    data = data,
    weights = weights,
    reml = reml,
    control = control
  )
  if (!attr(fit, "converged")) {
    use_multiple <- length(control$optimizers) > 1L
    if (use_multiple) {
      control_remain <- control
      control_remain$optimizers <- control$optimizers[-1]
      fit <- refit_multiple_optimizers(
        fit = fit,
        control = control_remain
      )
    } else {
      all_problems <- unlist(
        attributes(fit)[c("errors", "warnings")],
        use.names = FALSE
      )
      stop(paste0(
        "Chosen optimizer '", toString(names(control$optimizer)), "' led to problems during model fit:\n",
        paste(paste0(seq_along(all_problems), ") ", all_problems), collapse = ";\n"), "\n",
        "Consider trying multiple optimizers."
      ))
    }
  }
  fit_msg <- attr(fit, "messages")
  if (!is.null(fit_msg)) {
    message(paste(fit_msg, collapse = "\n"))
  }
  fit$method <- control$method
  if (control$method == "Satterthwaite") {
    covbeta_fun <- h_covbeta_fun(fit)
    fit$jac_list <- h_jac_list(covbeta_fun, fit$theta_est)
  } else {
    fit$kr_comp <- h_get_kr_comp(fit$tmb_data, fit$theta_est)
    fit$beta_vcov_adj <- h_var_adj(
      v = fit$beta_vcov,
      w = component(fit, "theta_vcov"),
      p = fit$kr_comp$P,
      q = fit$kr_comp$Q,
      r = fit$kr_comp$R,
      linear = (control$method == "Kenward-Roger-Linear")
    )
  }
  class(fit) <- c("mmrm", class(fit))
  fit
}
