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

#' @describeIn mmrm_tmb_methods predict conditional means for new data;
#'  optionally with standard errors and confidence or prediction intervals.
#'  Returns a vector of predictions if `se.fit == FALSE` and
#'  `interval == "none"`; otherwise it returns a data.frame with multiple
#'  columns and one row per input data row.
#' @importFrom stats predict
#' @exportS3Method
#' @examples
#' stop("implement example")
predict.mmrm_tmb <- function(
    object, newdata, se.fit = FALSE,
    interval = c("none", "confidence"), level = 0.95,
    na.action = na.pass, ...) {
  interval <- match.arg(interval)
  # compute X_new times beta_hat for new data X (conditional mean predictions)
  x_matrix <- h_get_x_matrix(object, newdata)
  res <- x_matrix %*% component(object, "beta_est")
  colnames(res) <- "fit"
  if (interval != "none" || se.fit) { # compute standard errors of fit
    se <- sqrt(diag(x_matrix %*% component(object, "beta_vcov") %*% t(x_matrix)))
    if (se.fit) { # save?
      res <- cbind(res, se = se)
    }
  }
  if (interval != "none") { # compute confidence interval
    alpha <- 1 - level
    # TODO: need to determine df, work with normal for now
    if (interval == "confidence") {
      res <- cbind(res,
          lwr = res[, "fit"] - qnorm(1 - alpha/2) * se,
          upr = res[, "fit"] + qnorm(1 - alpha/2) * se
        )
    }
  }
  if (ncol(res) == 1) { # return vector if only fit is computed
    res <- res[, "fit"]
  }
  return(res)
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
  resids_unscaled <- component(object, "y_vector") - unname(fitted(object))
  if (type == "response") {
    resids_unscaled
  } else {
    if (object$formula_parts$is_spatial) {
      stop("Only 'response' residuals are available for models with spatial covariance structures.")
    }
    if (type == "pearson") {
      h_residuals_pearson(object, resids_unscaled)
    } else if (type == "normalized") {
      h_residuals_normalized(object, resids_unscaled)
    }
  }
}

#' Calculate Pearson Residuals
#'
#' This is used by [residuals.mmrm_tmb()] to calculate Pearson residuals.
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM.
#' @param resids_unscaled (`numeric`)\cr the response residuals.
#'
#' @return Vector of residuals.
#'
#' @keywords internal
h_residuals_pearson <- function(object, resids_unscaled) {
  assert_class(object, "mmrm_tmb")
  assert_numeric(resids_unscaled)
  visits <- as.numeric(object$tmb_data$full_frame[[object$formula_parts$visit_var]])
  cov_list <- if (component(object, "n_groups") == 1) {
    list(object$cov)
  } else {
    object$cov
  }
  visit_sigmas <- lapply(cov_list, function(x) sqrt(diag(x, names = FALSE)))
  nobs <- nrow(object$tmb_data$full_frame)
  subject_grps <- if (component(object, "n_groups") == 1) {
    rep(1, times = nobs)
  } else {
    object$tmb_data$full_frame[[object$formula_parts$group_var]]
  }
  sapply(1:nobs, function(x) {
    resids_unscaled[x] / visit_sigmas[[subject_grps[x]]][visits[x]] * sqrt(object$tmb_data$weights_vector[x])
  })
}

#' Calculate normalized residuals
#'
#' This is used by [residuals.mmrm_tmb()] to calculate normalized / scaled residuals.
#'
#' @param object (`mmrm_tmb`)\cr the fitted MMRM.
#' @param resids_unscaled (`numeric`)\cr the raw/response residuals.
#'
#' @return Vector of residuals
#'
#' @keywords internal
h_residuals_normalized <- function(object, resids_unscaled) {
  assert_class(object, "mmrm_tmb")
  assert_numeric(resids_unscaled)

  resid_df <- data.frame(
    subject = object$tmb_data$full_frame[[object$formula_parts$subject_var]],
    time = as.numeric(object$tmb_data$full_frame[[object$formula_parts$visit_var]]),
    residual = resids_unscaled,
    weights = object$tmb_data$weights_vector
  )

  subject_list <- split(resid_df, resid_df$subject)

  lower_chol_list <- if (component(object, "n_groups") == 1) {
    lapply(seq_along(subject_list), function(x) {
      weighted_cov <- object$cov[subject_list[[x]]$time, subject_list[[x]]$time] /
        sqrt(tcrossprod(matrix(subject_list[[x]]$weights, ncol = 1)))

      solve(t(chol(weighted_cov)))
    })
  } else {
    groups <- data.frame(
      subject = object$tmb_data$full_frame[[object$formula_parts$subject_var]],
      group = object$tmb_data$full_frame[[object$formula_parts$group_var]]
    )
    groups <- groups[!duplicated(groups), ]
    lapply(seq_along(subject_list), function(x) {
      this_cov <- object$cov[[groups$group[x]]]

      weighted_cov <- this_cov[subject_list[[x]]$time, subject_list[[x]]$time] /
        sqrt(tcrossprod(matrix(subject_list[[x]]$weights, ncol = 1)))

      solve(t(chol(weighted_cov)))
    })
  }
  unlist(lapply(seq_along(subject_list), function(x) {
    lower_chol_list[[x]] %*% matrix(subject_list[[x]]$residual, ncol = 1)
  }))
}
