#' Methods for `mmrm_tmb` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM object.
#' @param x (`mmrm_tmb`)\cr same as `object`.
#' @param formula (`mmrm_tmb`)\cr same as `object`.
#' @param complete (`flag`)\cr whether to include potential non-estimable
#'   coefficients.
#' @param ... not used.
#' @return Depends on the method, see Functions.
#'
#' @name mmrm_tmb_methods
#'
#' @examples
#' formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
#' object <- fit_mmrm(formula, fev_data, weights = rep(1, nrow(fev_data)))
NULL

#' @describeIn mmrm_tmb_methods obtains the estimated coefficients.
#' @importFrom stats coef
#' @exportS3Method
#' @examples
#' # Estimated coefficients:
#' coef(object)
coef.mmrm_tmb <- function(object, complete = TRUE, ...) {
  assert_flag(complete)
  nm <- if (complete) "beta_est_complete" else "beta_est"
  component(object, name = nm)
}

#' @describeIn mmrm_tmb_methods obtains the fitted values.
#' @importFrom stats fitted
#' @exportS3Method
#' @examples
#' # Fitted values:
#' fitted(object)
fitted.mmrm_tmb <- function(object, ...) {
  fitted_col <- component(object, "x_matrix") %*% component(object, "beta_est")
  fitted_col[, 1L, drop = TRUE]
}

#' @describeIn mmrm_tmb_methods obtains the model frame.
#' @param exclude (`character`)\cr names of variable to exclude.
#' @param full (`flag`) indicator whether to return full model frame (deprecated).
#' @importFrom stats model.frame
#' @exportS3Method
#'
#' @details
#' `exclude` argument controls the variables the returned model frame will exclude.
#' Possible options are "subject_var", "visit_var" and "group_var", representing the
#' subject variable, visit variable or group variable.
#'
#' @examples
#' # Model frame:
#' model.frame(object)
#' model.frame(object, exclude = "subject_var")
model.frame.mmrm_tmb <- function(formula, exclude = "subject_var", full, ...) {
  if (!missing(full) && identical(full, TRUE)) {
    lifecycle::deprecate_warn("0.3", "model.frame.mmrm_tmb(full)")
    exclude <- NULL
  }
  assert_subset(exclude, c("subject_var", "visit_var", "group_var"))
  dots <- list(...)
  if (!identical(h_default_value(dots$na.action, getOption("na.action")), "na.omit")) {
    warning("na.action is always set to `na.omit` for `mmrm`!")
  }
  if (!is.null(dots$subset) || !is.null(dots$weights)) {
    warning("subset and weights are not valid arguments for `mmrm` models.")
  }
  if (is.null(dots$data) && length(exclude) == 0L) {
    formula$tmb_data$full_frame
  } else {
    drop_vars <- unlist(formula$formula_parts[exclude])
    new_formula <- h_drop_terms(formula$formula_parts$full_formula, drop_vars)
    model.frame(
      formula = new_formula,
      data = h_default_value(dots$data, formula$data),
      na.action = "na.omit"
    )
  }
}

#' @describeIn mmrm_tmb_methods obtains the attained log likelihood value.
#' @importFrom stats logLik
#' @exportS3Method
#' @examples
#' # Log likelihood given the estimated parameters:
#' logLik(object)
logLik.mmrm_tmb <- function(object, ...) {
  -component(object, "neg_log_lik")
}

#' @describeIn mmrm_tmb_methods obtains the used formula.
#' @importFrom stats formula
#' @exportS3Method
#' @examples
#' # Formula which was used:
#' formula(object)
formula.mmrm_tmb <- function(x, ...) {
  x$formula_parts$formula
}

#' @describeIn mmrm_tmb_methods obtains the variance-covariance matrix estimate
#'   for the coefficients.
#' @importFrom stats vcov
#' @exportS3Method
#' @examples
#' # Variance-covariance matrix estimate for coefficients:
#' vcov(object)
vcov.mmrm_tmb <- function(object, complete = TRUE, ...) {
  assert_flag(complete)
  nm <- if (complete) "beta_vcov_complete" else "beta_vcov"
  component(object, name = nm)
}

#' @describeIn mmrm_tmb_methods obtains the variance-covariance matrix estimate
#'   for the residuals.
#' @param sigma cannot be used (this parameter does not exist in MMRM).
#' @importFrom nlme VarCorr
#' @export VarCorr
#' @aliases VarCorr
#' @exportS3Method
#' @examples
#' # Variance-covariance matrix estimate for residuals:
#' VarCorr(object)
VarCorr.mmrm_tmb <- function(x, sigma = NA, ...) { # nolint
  assert_scalar_na(sigma)

  component(x, name = "varcor")
}

#' @describeIn mmrm_tmb_methods obtains the deviance, which is defined here
#'   as twice the negative log likelihood, which can either be integrated
#'   over the coefficients for REML fits or the usual one for ML fits.
#' @importFrom stats deviance
#' @exportS3Method
#' @examples
#' # REML criterion (twice the negative log likelihood):
#' deviance(object)
deviance.mmrm_tmb <- function(object, ...) {
  2 * component(object, "neg_log_lik")
}

#' @describeIn mmrm_tmb_methods obtains the Akaike Information Criterion,
#'   where the degrees of freedom are the number of variance parameters (`n_theta`).
#'   If `corrected`, then this is multiplied with `m / (m - n_theta - 1)` where
#'   `m` is the number of observations minus the number of coefficients, or
#'   `n_theta + 2` if it is smaller than that \insertCite{hurvich1989regression,burnham1998practical}{mmrm}.
#' @param corrected (`flag`)\cr whether corrected AIC should be calculated.
#' @param k (`number`)\cr the penalty per parameter to be used; default `k = 2`
#'   is the classical AIC.
#' @importFrom stats AIC
#' @exportS3Method
#' @examples
#' # AIC:
#' AIC(object)
#' AIC(object, corrected = TRUE)
#' @references
#' - \insertRef{hurvich1989regression}{mmrm}
#' - \insertRef{burnham1998practical}{mmrm}
AIC.mmrm_tmb <- function(object, corrected = FALSE, ..., k = 2) {
  # nolint
  assert_flag(corrected)
  assert_number(k, lower = 1)

  n_theta <- length(component(object, "theta_est"))
  df <- if (!corrected) {
    n_theta
  } else {
    n_obs <- length(component(object, "y_vector"))
    n_beta <- length(component(object, "beta_est"))
    m <- max(n_theta + 2, n_obs - n_beta)
    n_theta * (m / (m - n_theta - 1))
  }

  2 * component(object, "neg_log_lik") + k * df
}

#' @describeIn mmrm_tmb_methods obtains the Bayesian Information Criterion,
#'   which is using the natural logarithm of the number of subjects for the
#'   penalty parameter `k`.
#' @importFrom stats BIC
#' @exportS3Method
#' @examples
#' # BIC:
#' BIC(object)
BIC.mmrm_tmb <- function(object, ...) {
  # nolint
  k <- log(component(object, "n_subjects"))
  AIC(object, corrected = FALSE, k = k)
}


#' @describeIn mmrm_tmb_methods prints the object.
#' @exportS3Method
print.mmrm_tmb <- function(x,
                           ...) {
  cat("mmrm fit\n\n")

  h_print_call(
    component(x, "call"), component(x, "n_obs"),
    component(x, "n_subjects"), component(x, "n_timepoints")
  )
  h_print_cov(component(x, "cov_type"), component(x, "n_theta"), component(x, "n_groups"))

  cat("Inference:   ")
  cat(ifelse(component(x, "reml"), "REML", "ML"))
  cat("\n")
  cat("Deviance:    ")
  cat(deviance(x))

  cat("\n\nCoefficients: ")
  n_singular_coefs <- sum(component(x, "beta_aliased"))
  if (n_singular_coefs > 0) {
    cat("(", n_singular_coefs, " not defined because of singularities)", sep = "")
  }
  cat("\n")
  print(coef(x, complete = TRUE))

  cat("\nModel Inference Optimization:")

  cat(ifelse(component(x, "convergence") == 0, "\nConverged", "\nFailed to converge"))
  cat(
    " with code", component(x, "convergence"),
    "and message:", tolower(component(x, "conv_message"))
  )
  cat("\n")
  invisible(x)
}


#' @describeIn mmrm_tmb_methods to obtain residuals - either unscaled ('response'), 'pearson' or 'normalized'.
#' @param type (`string`)\cr unscaled (`response`), `pearson` or `normalized`. Default is `response`,
#' and this is the only type available for use with models with a spatial covariance structure.
#' @importFrom stats residuals
#' @exportS3Method
#' @examples
#' # residuals:
#' residuals(object, type = "response")
#' residuals(object, type = "pearson")
#' residuals(object, type = "normalized")
#' @references
#' - \insertRef{galecki2013linear}{mmrm}
residuals.mmrm_tmb <- function(object, type = c("response", "pearson", "normalized"), ...) {
  type <- match.arg(type)
  if (identical(object$tmb_data$is_spatial, 1L) && !identical(type, "response")) {
    stop("Only 'respons'residuals are available for models with spatial covariance structures.")
  }
  switch(type,
    "response" = h_residuals_response(object),
    "pearson" = h_residuals_pearson(object),
    "normalized" = h_residuals_normalized(object)
  )
}
#' Calculate Pearson Residuals
#'
#' This is used by [residuals.mmrm_tmb()] to calculate Pearson residuals.
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM.
#'
#' @return Vector of residuals.
#'
#' @keywords internal
h_residuals_pearson <- function(object) {
  assert_class(object, "mmrm_tmb")
  resids_unscaled <- h_residuals_response(object)
  visit_sigmas <- if (component(object, "n_groups") == 1) {
    sqrt(diag(unname(object$cov)))
  } else {
    sqrt(unlist(lapply(object$cov, diag), use.names = FALSE))
  }

  subject_grps <- rep(
    as.integer(object$tmb_data$subject_groups) - 1,
    object$tmb_data$subject_n_visits
  )
  index <- subject_grps * component(object, "n_timepoints") + object$tmb_data$visits_zero_inds + 1
  resids_unscaled * sqrt(object$tmb_data$weights_vector) / visit_sigmas[index]
}

#' Calculate normalized residuals
#'
#' This is used by [residuals.mmrm_tmb()] to calculate normalized / scaled residuals.
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM.
#'
#' @return Vector of residuals
#'
#' @keywords internal
h_residuals_normalized <- function(object) {
  assert_class(object, "mmrm_tmb")
  object$tmb_object$report()$epsilonTilde
}
#' Calculate response residuals.
#'
#' This is used by [residuals.mmrm_tmb()] to calculate response residuals.
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM.
#'
#' @return Vector of residuals
#'
#' @keywords internal
h_residuals_response <- function(object) {
  assert_class(object, "mmrm_tmb")
  component(object, "y_vector") - unname(fitted(object))
}
